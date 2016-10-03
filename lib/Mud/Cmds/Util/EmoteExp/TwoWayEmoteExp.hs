{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Cmds.Util.EmoteExp.TwoWayEmoteExp ( emotifyTwoWay
                                             , expCmdifyTwoWay ) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Msgs.Advice
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (_1)
import Control.Lens.Operators ((%~))
import Data.Either (lefts, rights)
import Data.List (intersperse, nub)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.EmoteExp.TwoWayEmoteExp"


-- ==================================================


emotifyTwoWay :: Text -> Id -> MudState -> Id -> Text -> Either [Text] (Either () [Broadcast])
emotifyTwoWay cn i ms targetId msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isBracketed ws          = Left . pure $ sorryBracketedMsg
  | isHeDon't emoteChar msg = Left . pure $ sorryWtf
  | c == emoteChar          = fmap Right . procTwoWayEmote cn i ms targetId . parseOutDenotative ws $ rest
  | otherwise               = Right . Left $ ()


procTwoWayEmote :: Text -> Id -> MudState -> Id -> Args -> Either [Text] [Broadcast]
procTwoWayEmote cn i ms targetId as =
    let s       = getSing i ms
        xformed = xformArgs True as
        xformArgs _      []     = []
        xformArgs _      [x]
          | (h, t) <- headTail x
          , h == emoteNameChar
          , all isPunc . T.unpack $ t
          = pure . Right $ s <> t
        xformArgs isHead (x:xs) = (: xformArgs False xs) $ if
          | x == enc            -> Right s
          | x == enc's          -> Right $ s <> "'s"
          | enc `T.isInfixOf` x -> Left . adviceEnc $ cn'
          | etc `T.isInfixOf` x -> Left . adviceEtcInTwoWay cn $ cn'
          | isHead, hasEnc as   -> Right . capitalizeMsg $ x
          | isHead              -> Right $ s |<>| x
          | otherwise           -> Right x
    in case lefts xformed of
      []      -> let msg = bracketQuote . T.unwords . rights $ xformed
                 in Right [ (msg, pure i), (msg, pure targetId) ]
      advices -> Left . intersperse "" . nub $ advices
  where
    cn' = cn |<>| T.singleton emoteChar


-----


expCmdifyTwoWay :: Id -> MudState -> Id -> Sing -> Text -> Either Text [Broadcast]
expCmdifyTwoWay i ms targetId targetSing msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | isHeDon't expCmdChar msg = Left sorryWtf
  | c == expCmdChar          = procExpCmdTwoWay i ms targetId targetSing . parseOutDenotative ws $ rest
  | otherwise                = Right [ (msg, pure i), (msg, pure targetId) ]


procExpCmdTwoWay :: Id -> MudState -> Id -> Sing -> Args -> Either Text [Broadcast]
procExpCmdTwoWay _ _  _        _          (_:_:_:_)                               = Left sorryExpCmdLen
procExpCmdTwoWay i ms targetId targetSing (map T.toLower . unmsg -> [cn, target]) =
    findFullNameForAbbrev cn expCmdNames |&| maybe notFound found
  where
    found match = let ExpCmd _ ct _ _ = getExpCmdByName match in map (_1 %~ angleBracketQuote) <$> case ct of
      NoTarget toSelf toOthers -> if ()# target
        then Right [ (toSelf,                  pure i       )
                   , (format Nothing toOthers, pure targetId) ]
        else Left . sorryExpCmdIllegalTarget $ match
      HasTarget toSelf toTarget _ ->
          let good  = Right [ (format (Just targetId) toSelf,   pure i       )
                            , (format Nothing         toTarget, pure targetId) ]
              sorry = Left . sorryTwoWayTargetName match $ targetSing
          in ()# target ?  good
                        :? (target `T.isPrefixOf` uncapitalize targetSing ?  good
                                                                          :? sorry)
      Versatile toSelf toOthers toSelfWithTarget toTarget _
        | ()# target -> Right [ (toSelf,                  pure i       )
                              , (format Nothing toOthers, pure targetId) ]
        | target `T.isPrefixOf` uncapitalize targetSing ->
            Right [ (format (Just targetId) toSelfWithTarget, pure i       )
                  , (format Nothing         toTarget,         pure targetId) ]
        | otherwise -> Left . sorryTwoWayTargetName match $ targetSing
    notFound             = Left . sorryExpCmdName $ cn
    format maybeTargetId = let substitutions = [ ("%", s), ("^", heShe), ("&", hisHer), ("*", himHerself) ]
                           in replace (substitutions ++ maybe [] (const . pure $ ("@", targetSing)) maybeTargetId)
    s                    = getSing i ms
    (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
procExpCmdTwoWay _ _ _ _ as = patternMatchFail "procExpCmdTwoWay" . showText $ as
