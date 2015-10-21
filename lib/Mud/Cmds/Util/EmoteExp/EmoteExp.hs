{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Cmds.Util.EmoteExp.EmoteExp ( adminChanEmotify
                                       , adminChanExpCmdify
                                       , adminChanTargetify
                                       , emotify
                                       , expCmdify
                                       , targetify ) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Util.CmdPrefixes
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Msgs.Advice
import Mud.Cmds.Util.Msgs.Sorry
import Mud.Data.Misc
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (_1, _2, both, each, view, views)
import Control.Lens.Operators ((%~), (&), (<>~))
import Data.Char (isLetter)
import Data.Either (isLeft)
import Data.List ((\\), delete, intersperse, nub)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.EmoteExp.EmoteExp"


-- ==================================================


targetify :: Id -> ChanContext -> [(Id, T.Text, T.Text)] -> T.Text -> Either T.Text (Either () [Broadcast])
targetify i cc triples msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isBracketed ws               = sorryBracketedMsg
  | isHeDon't chanTargetChar msg = Left sorryWtf
  | c == chanTargetChar          = fmap Right . procChanTarget i cc triples . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right . Left $ ()


procChanTarget :: Id -> ChanContext -> [(Id, T.Text, T.Text)] -> Args -> Either T.Text [Broadcast]
procChanTarget i cc triples ((T.toLower -> target):rest)
  | ()# rest  = Left sorryChanMsg
  | otherwise = case findFullNameForAbbrev target . map (views _2 T.toLower) $ triples of
    Nothing -> Left . sorryChanTargetNameFromContext target $ cc
    Just n  -> let targetId    = getIdForMatch n
                   tunedIds    = map (view _1) triples
                   msg         = capitalizeMsg . T.unwords $ rest
                   formatMsg x = parensQuote ("to " <> x) <> " " <> msg
               in Right [ (formatMsg . embedId $ targetId,                               pure i                    )
                        , (formatMsg . embedId $ targetId,                               targetId `delete` tunedIds)
                        , (formatMsg . quoteWith' (emoteTargetColor, dfltColor) $ "you", pure targetId             ) ]
  where
    getIdForMatch match  = view _1 . head . filter (views _2 ((== match) . T.toLower)) $ triples
procChanTarget _ _ _ as = patternMatchFail "procChanTarget" as


-----


emotify :: Id -> MudState -> ChanContext -> [(Id, T.Text, T.Text)] -> T.Text -> Either [T.Text] (Either () [Broadcast])
emotify i ms cc triples msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isHeDon't emoteChar msg = Left . pure $ sorryWtf
  | c == emoteChar          = fmap Right . procEmote i ms cc triples . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right . Left $ ()


procEmote :: Id -> MudState -> ChanContext -> [(Id, T.Text, T.Text)] -> Args -> Either [T.Text] [Broadcast]
procEmote _ _  cc _       as | hasYou as = Left . pure . adviceYouEmoteChar . pp $ cc
procEmote i ms cc triples as             =
    let me                      = (getSing i ms, embedId i, embedId i)
        xformed                 = xformArgs True as
        xformArgs _      []     = []
        xformArgs _      [x]
          | (h, t) <- headTail x
          , h == emoteNameChar
          , all isPunc . T.unpack $ t
          = pure . mkRightForNonTargets $ me & each <>~ t
        xformArgs isHead (x:xs) = (: xformArgs False xs) $ if
          | x == enc            -> mkRightForNonTargets me
          | x == enc's          -> mkRightForNonTargets (me & each <>~ "'s")
          | enc `T.isInfixOf` x -> Left . adviceEnc $ cc'
          | x == etc            -> Left . adviceEtc $ cc'
          | T.take 1 x == etc   -> isHead ? Left adviceEtcHead :? (procTarget . T.tail $ x)
          | etc `T.isInfixOf` x -> Left . adviceEtc $ cc'
          | isHead, hasEnc as   -> mkRightForNonTargets . dup3 . capitalizeMsg $ x
          | isHead              -> mkRightForNonTargets (me & each <>~ (" " <> x))
          | otherwise           -> mkRightForNonTargets . dup3 $ x
    in case filter isLeft xformed of
      []      -> let (toSelf, toOthers, targetIds, toTargetBs) = happy ms xformed
                 in Right $ (toSelf, pure i) : (toOthers, tunedIds \\ targetIds) : toTargetBs
      advices -> Left . intersperse "" . map fromLeft . nub $ advices
  where
    cc'             = pp cc <> " " <> T.singleton emoteChar
    procTarget word =
        case swap . (both %~ T.reverse) . T.span isPunc . T.reverse $ word of
          ("",   _) -> Left . adviceEtc $ cc'
          ("'s", _) -> Left adviceEtcEmptyPoss
          (w,    p) ->
            let (isPoss, target) = ("'s" `T.isSuffixOf` w ? (True, T.dropEnd 2) :? (False, id)) & _2 %~ (w |&|)
                notFound         = Left . sorryChanTargetNameFromContext target $ cc
                found match      =
                    let targetId = view _1 . head . filter (views _2 ((== match) . T.toLower)) $ triples
                        txt      = addSuffix isPoss p . embedId $ targetId
                    in Right ( txt
                             , [ mkEmoteWord isPoss p targetId, ForNonTargets txt ]
                             , txt )
            in findFullNameForAbbrev (T.toLower target) (map (views _2 T.toLower) triples) |&| maybe notFound found
    addSuffix   isPoss p = (<> p) . (isPoss ? (<> "'s") :? id)
    mkEmoteWord isPoss   = isPoss ? ForTargetPoss :? ForTarget
    tunedIds             = map (view _1) triples


-----


expCmdify :: Id -> MudState -> ChanContext -> [(Id, T.Text, T.Text)] -> T.Text -> Either T.Text ([Broadcast], T.Text)
expCmdify i ms cc triples msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isHeDon't expCmdChar msg = Left sorryWtf
  | c == expCmdChar          = fmap format . procExpCmd i ms cc triples . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right (pure (msg, i : map (view _1) triples), msg)
  where
    format xs = xs & _1 %~ map (_1 %~ angleBracketQuote)
                   & _2 %~ angleBracketQuote


procExpCmd :: Id -> MudState -> ChanContext -> [(Id, T.Text, T.Text)] -> Args -> Either T.Text ([Broadcast], T.Text)
procExpCmd _ _  _  _       (_:_:_:_) = sorryExpCmdLen
procExpCmd i ms cc triples (map T.toLower . unmsg -> [cn, target]) =
    findFullNameForAbbrev cn expCmdNames |&| maybe notFound found
  where
    found match =
        let ExpCmd _ ct = getExpCmdByName match
            tunedIds    = map (view _1) triples
        in case ct of
          NoTarget toSelf toOthers -> if ()# target
            then Right ( (format Nothing toOthers, tunedIds) : mkBroadcast i toSelf
                       , toSelf )
            else Left . sorryExpCmdIllegalTarget $ match
          HasTarget toSelf toTarget toOthers -> if ()# target
            then Left . sorryExpCmdRequiresTarget $ match
            else case findTarget of
              Nothing -> Left . sorryChanTargetNameFromContext target $ cc
              Just n  -> let targetId = getIdForMatch n
                             toSelf'  = format (Just targetId) toSelf
                         in Right ( (colorizeYous . format Nothing $ toTarget, pure targetId             ) :
                                    (format (Just targetId) toOthers,          targetId `delete` tunedIds) :
                                    mkBroadcast i toSelf'
                                  , toSelf' )
          Versatile toSelf toOthers toSelfWithTarget toTarget toOthersWithTarget -> if ()# target
            then Right ( (format Nothing toOthers, tunedIds) : mkBroadcast i toSelf
                       , toSelf )
            else case findTarget of
              Nothing -> Left . sorryChanTargetNameFromContext target $ cc
              Just n  -> let targetId          = getIdForMatch n
                             toSelfWithTarget' = format (Just targetId) toSelfWithTarget
                         in Right ( (colorizeYous . format Nothing $ toTarget,  pure targetId             ) :
                                    (format (Just targetId) toOthersWithTarget, targetId `delete` tunedIds) :
                                    mkBroadcast i toSelfWithTarget'
                                  , toSelfWithTarget' )
    notFound             = sorryExpCmdName cn
    findTarget           = findFullNameForAbbrev target . map (views _2 T.toLower) $ triples
    getIdForMatch match  = view _1 . head . filter (views _2 ((== match) . T.toLower)) $ triples
    format maybeTargetId =
        let substitutions = [ ("%", embedId i), ("^", heShe), ("&", hisHer), ("*", himHerself) ]
        in replace (substitutions ++ maybe [] (pure . ("@", ) . embedId) maybeTargetId)
    (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
    colorizeYous                = T.unwords . map helper . T.words
      where
        helper w = let (a, b) = T.break isLetter w
                       (c, d) = T.span  isLetter b
                   in T.toLower c `elem` yous ? (a <> quoteWith' (emoteTargetColor, dfltColor) c <> d) :? w
procExpCmd _ _ _ _ as = patternMatchFail "procExpCmd" as


-----


adminChanTargetify :: Inv -> [Sing] -> T.Text -> Either T.Text (Either () [Broadcast])
adminChanTargetify tunedIds tunedSings msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isBracketed ws               = sorryBracketedMsg
  | isHeDon't chanTargetChar msg = Left sorryWtf
  | c == chanTargetChar          = fmap Right . adminChanProcChanTarget tunedIds tunedSings . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right . Left $ ()


adminChanProcChanTarget :: Inv -> [Sing] -> Args -> Either T.Text [Broadcast]
adminChanProcChanTarget tunedIds tunedSings ((capitalize . T.toLower -> target):rest) =
    ()# rest ? Left sorryChanMsg :? (findFullNameForAbbrev target tunedSings |&| maybe notFound found)
  where
    notFound         = Left . sorryAdminChanTargetName $ target
    found targetSing =
        let targetId    = fst . head . filter ((== targetSing) . snd) . zip tunedIds $ tunedSings
            msg         = capitalizeMsg . T.unwords $ rest
            formatMsg x = parensQuote ("to " <> x) <> " " <> msg
        in Right [ (formatMsg targetSing,                                         targetId `delete` tunedIds)
                 , (formatMsg . quoteWith' (emoteTargetColor, dfltColor) $ "you", pure targetId             ) ]
adminChanProcChanTarget _ _ as = patternMatchFail "adminChanProcChanTarget" as


-----


adminChanEmotify :: Id -> MudState -> Inv -> [Sing] -> T.Text -> Either [T.Text] (Either () [Broadcast])
adminChanEmotify i ms tunedIds tunedSings msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isHeDon't emoteChar msg = Left . pure $ sorryWtf
  | c == emoteChar          = fmap Right . adminChanProcEmote i ms tunedIds tunedSings . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right . Left $ ()


adminChanProcEmote :: Id -> MudState -> Inv -> [Sing] -> Args -> Either [T.Text] [Broadcast]
adminChanProcEmote _ _  _        _          as | hasYou as = Left . pure . adviceYouEmoteChar . prefixAdminCmd $ "admin"
adminChanProcEmote i ms tunedIds tunedSings as =
    let s                       = getSing i ms
        xformed                 = xformArgs True as
        xformArgs _      []     = []
        xformArgs _      [x]
          | (h, t) <- headTail x
          , h == emoteNameChar
          , all isPunc . T.unpack $ t
          = pure . mkRightForNonTargets . dup3 $ s <> t
        xformArgs isHead (x:xs) = (: xformArgs False xs) $ if
          | x == enc            -> mkRightForNonTargets . dup3 $ s
          | x == enc's          -> mkRightForNonTargets . dup3 $ s <> "'s"
          | enc `T.isInfixOf` x -> Left . adviceEnc $ cn
          | x == etc            -> Left . adviceEtc $ cn
          | T.take 1 x == etc   -> isHead ? Left adviceEtcHead :? (procTarget . T.tail $ x)
          | etc `T.isInfixOf` x -> Left . adviceEtc $ cn
          | isHead, hasEnc as   -> mkRightForNonTargets . dup3 . capitalizeMsg $ x
          | isHead              -> mkRightForNonTargets . dup3 $ s <> " " <> x
          | otherwise           -> mkRightForNonTargets . dup3 $ x
    in case filter isLeft xformed of
      [] -> let (toSelf, toOthers, targetIds, toTargetBs) = happy ms xformed
            in Right $ (toSelf, pure i) : (toOthers, tunedIds \\ (i : targetIds)) : toTargetBs
      advices -> Left . intersperse "" . map fromLeft . nub $ advices
  where
    cn              = prefixAdminCmd "admin" <> " " <> T.singleton emoteChar
    procTarget word =
        case swap . (both %~ T.reverse) . T.span isPunc . T.reverse $ word of
          ("",   _) -> Left . adviceEtc $ cn
          ("'s", _) -> Left adviceEtcEmptyPoss
          (w,    p) ->
            let (isPoss, target) = ("'s" `T.isSuffixOf` w ? (True, T.dropEnd 2) :? (False, id)) & _2 %~ (w |&|)
                target'          = capitalize . T.toLower $ target
                notFound         = Left . sorryAdminChanTargetName $ target
                found targetSing@(addSuffix isPoss p -> targetSing') =
                    let targetId = head . filter ((== targetSing) . (`getSing` ms)) $ tunedIds
                    in Right ( targetSing'
                             , [ mkEmoteWord isPoss p targetId, ForNonTargets targetSing' ]
                             , targetSing' )
            in findFullNameForAbbrev target' (getSing i ms `delete` tunedSings) |&| maybe notFound found
    addSuffix   isPoss p = (<> p) . (isPoss ? (<> "'s") :? id)
    mkEmoteWord isPoss   = isPoss ? ForTargetPoss :? ForTarget


-----


adminChanExpCmdify :: Id -> MudState -> Inv -> [Sing] -> T.Text -> Either T.Text ([Broadcast], T.Text)
adminChanExpCmdify i ms tunedIds tunedSings msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isHeDon't expCmdChar msg = Left sorryWtf
  | c == expCmdChar          = fmap format . adminChanProcExpCmd i ms tunedIds tunedSings . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right (pure (msg, tunedIds), msg)
  where
    format xs = xs & _1 %~ map (_1 %~ angleBracketQuote)
                   & _2 %~ angleBracketQuote


adminChanProcExpCmd :: Id -> MudState -> Inv -> [Sing] -> Args -> Either T.Text ([Broadcast], T.Text)
adminChanProcExpCmd _ _ _ _ (_:_:_:_) = sorryExpCmdLen
adminChanProcExpCmd i ms tunedIds tunedSings (map T.toLower . unmsg -> [cn, target]) =
    findFullNameForAbbrev cn expCmdNames |&| maybe notFound found
  where
    found match =
        let ExpCmd _ ct = getExpCmdByName match
        in case ct of
          NoTarget toSelf toOthers -> if ()# target
            then Right ( (format Nothing toOthers, i `delete` tunedIds) : mkBroadcast i toSelf
                       , toSelf )
            else Left . sorryExpCmdIllegalTarget $ match
          HasTarget toSelf toTarget toOthers -> if ()# target
            then Left . sorryExpCmdRequiresTarget $ match
            else case findTarget of
              Nothing -> Left . sorryAdminChanTargetName $ target
              Just n  -> let targetId = getIdForPCSing n ms
                             toSelf'  = format (Just n) toSelf
                         in Right ( (colorizeYous . format Nothing $ toTarget, pure targetId              ) :
                                    (format (Just n) toOthers,                 tunedIds \\ [ i, targetId ]) :
                                    mkBroadcast i toSelf'
                                  , toSelf' )
          Versatile toSelf toOthers toSelfWithTarget toTarget toOthersWithTarget -> if ()# target
            then Right ( (format Nothing toOthers, i `delete` tunedIds) : mkBroadcast i toSelf
                       , toSelf )
            else case findTarget of
              Nothing -> Left . sorryAdminChanTargetName $ target
              Just n  -> let targetId          = getIdForPCSing n ms
                             toSelfWithTarget' = format (Just n) toSelfWithTarget
                         in Right ( (colorizeYous . format Nothing $ toTarget, pure targetId              ) :
                                    (format (Just n) toOthersWithTarget,       tunedIds \\ [ i, targetId ]) :
                                    mkBroadcast i toSelfWithTarget'
                                  , toSelfWithTarget' )
    notFound   = sorryExpCmdName cn
    findTarget = findFullNameForAbbrev (capitalize target) $ getSing i ms `delete` tunedSings
    format maybeTargetSing =
        let substitutions = [ ("%", s), ("^", heShe), ("&", hisHer), ("*", himHerself) ]
        in replace (substitutions ++ maybe [] (pure . ("@", )) maybeTargetSing)
    s                           = getSing i ms
    (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
    colorizeYous                = T.unwords . map helper . T.words
      where
        helper w = let (a, b) = T.break isLetter w
                       (c, d) = T.span  isLetter b
                   in T.toLower c `elem` yous ? (a <> quoteWith' (emoteTargetColor, dfltColor) c <> d) :? w
adminChanProcExpCmd _ _ _ _ as = patternMatchFail "adminChanProcExpCmd" as
