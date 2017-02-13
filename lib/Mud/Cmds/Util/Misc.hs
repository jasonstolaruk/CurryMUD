{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, TupleSections, ViewPatterns #-}

-- This module contains helper functions used by multiple modules under "Mud.Cmds".

module Mud.Cmds.Util.Misc ( asterisk
                          , awardExp
                          , consume
                          , dispCmdList
                          , dispMatches
                          , embedId
                          , expandEmbeddedIds
                          , expandEmbeddedIdsToSings
                          , fakeClientInput
                          , formatChanMsg
                          , formatQuestion
                          , getAllChanIdNames
                          , getChanIdNames
                          , getChanStyleds
                          , getPCChans
                          , getQuestionStyleds
                          , getTunedQuestionIds
                          , happyTimes
                          , hasEnc
                          , hasType
                          , hasYou
                          , initPropNamesTbl
                          , initWordsTbl
                          , inOut
                          , isActingAny
                          , isAlive
                          , isAttacking
                          , isBracketed
                          , isDblLinked
                          , isDrinking
                          , isDrinkingEating
                          , isEating
                          , isHeDon't
                          , isHostBanned
                          , isLinked
                          , isOutside
                          , isPCBanned
                          , isPunc
                          , isSacrificing
                          , locateHelper
                          , loggedInOut
                          , loggedInOutColorize
                          , mkActionParams
                          , mkChanReport
                          , mkCmdListText
                          , mkCmdTriplesForStyling
                          , mkHimHer
                          , mkHolySymbolDesc
                          , mkInterfaceList
                          , mkNameTypeIdDesc
                          , mkPossPro
                          , mkPros
                          , mkReflexPro
                          , mkRetainedMsgFromPerson
                          , mkRightForNonTargets
                          , mkRndmVector
                          , mkSingleTarget
                          , mkThrPerPro
                          , mkWhoHeader
                          , onOff
                          , pager
                          , parseOutDenotative
                          , ppMaybe
                          , punc
                          , questionChanContext
                          , sendGenericErrorMsg
                          , showTime
                          , tunedInOut
                          , tunedInOutColorize
                          , unmsg
                          , updateRndmName
                          , withDbExHandler
                          , withDbExHandler_
                          , withoutArgs ) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Util.Abbrev
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Effect
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Interp.Pager
import Mud.Misc.ANSI
import Mud.Misc.CurryTime
import Mud.Misc.Database
import Mud.Misc.LocPref
import Mud.TheWorld.Zones.AdminZoneIds (iNecropolis)
import Mud.Threads.Misc
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Padding
import Mud.Util.List hiding (headTail)
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), (&&&), first)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (+~), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), forM, join, mplus, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Char (isDigit, isLetter)
import Data.Either (rights)
import Data.Function (on)
import Data.List (delete, groupBy, intercalate, nub, partition, sortBy, unfoldr)
import Data.Monoid ((<>), Any(..), Sum(..))
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Stack (HasCallStack)
import Prelude hiding (exp)
import qualified Data.IntMap.Strict as IM (IntMap, empty, filter, foldlWithKey', foldr, fromList, keys, map, mapWithKey)
import qualified Data.Map.Strict as M ((!), elems, keys, lookup, member, null, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Data.Vector.Unboxed as V (Vector, splitAt, toList)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN module ("HLint: ignore Use ||"        :: String) #-}


-----


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Cmds.Util.Misc"


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Misc"


-----


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Util.Misc"


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Util.Misc"


-- ==================================================


asterisk :: Text
asterisk = colorWith asteriskColor "*"


-----


awardExp :: HasCallStack => Exp -> Text -> Id -> MudStack ()
awardExp amt reason i = getLvlExp i <$> getState >>= \(l, x) -> let diff = calcLvlForExp (x + amt) - l in
    rndmVector (diff * noOfLvlUpRndmInts) >>= \v -> helper v |&| modifyState >=> \(ms, (msgs, logMsgs)) -> do
        let logMsg = T.concat [ "awarded "
                              , commaShow amt
                              , " exp "
                              , parensQuote reason
                              , "."
                              , logMsgs |!| (spcL . capitalize . prd . slashes $ logMsgs) ]
        when (isNpc i ms || isLoggedIn (getPla i ms)) . logPla "awardExp" i $ logMsg
        mapM_ (retainedMsg i ms) msgs
  where
    helper v ms =
        let oldLvl = getLvl i ms
            ms'    = ms & mobTbl.ind i.exp +~ amt
            newLvl = calcLvl i ms'
            diff   = newLvl - oldLvl
            ms''   | diff <= 0 = ms'
                   | otherwise = lvlUp i ms' v oldLvl newLvl & mobTbl.ind i.lvl .~ newLvl
            f 0    = Nothing
            f seed = Just ((colorWith lvlUpColor lvlUpMsg, mkLogMsg), pred seed)
              where
                mkLogMsg = ("gained a level " <>) . parensQuote $ "now level " <> showText (newLvl - seed + 1)
        in (ms'', (ms'', bool (unzip . unfoldr f $ diff) mempties $ diff <= 0))


noOfLvlUpRndmInts :: Int
noOfLvlUpRndmInts = 5


lvlUp :: HasCallStack => Id -> MudState -> V.Vector Int -> Lvl -> Lvl -> MudState
lvlUp i = helper
  where
    helper ms v oldLvl newLvl
      | oldLvl >= newLvl = ms
      | otherwise        = let (V.toList -> [ a, b, c, d, e ], v') = V.splitAt noOfLvlUpRndmInts v
                               myMob = mobTbl.ind i
                               ms'   = upd ms [ myMob.maxHp          +~ calcLvlUpHp       i ms a
                                              , myMob.maxMp          +~ calcLvlUpMp       i ms b
                                              , myMob.maxPp          +~ calcLvlUpPp       i ms c
                                              , myMob.maxFp          +~ calcLvlUpFp       i ms d
                                              , pcTbl.ind i.skillPts +~ calcLvlUpSkillPts i ms e ]
                           in helper ms' v' (succ oldLvl) newLvl


-----


consume :: HasCallStack => Id -> [StomachCont] -> MudStack ()
consume _ []     = unit
consume i newScs = do now <- liftIO getCurrentTime
                      modifyState (helper now) >>= procEffectList i
                      logPla "consume" i . prd $ "consumed " <> commas (map pp newScs)
  where
    helper now ms =
        let scs   = getStomach i ms ++ newScs
            pairs = map (dupSecond getConsumpEffects) scs :: [(StomachCont, Maybe ConsumpEffects)]
            getConsumpEffects sc = case sc^.distinctId of
              Left  (DistinctLiqId  x) -> f liqEdibleEffects  . getDistinctLiq  $ x
              Right (DistinctFoodId x) -> f foodEdibleEffects . getDistinctFood $ x
              where
                f a b = view (a.consumpEffects) . b $ ms
            (others, consumpEffectingPairs) = foldr g mempties pairs
              where
                g (sc, Nothing) = _1 %~ (sc       :)
                g (sc, Just ce) = _2 %~ ((sc, ce) :)
            (valids, invalids)   = partition isValid consumpEffectingPairs
            isValid pair@(sc, _) = sc^.hasCausedConsumpEffect.to not && isNotExpired pair
              where
                isNotExpired (view consumpTime -> t, ConsumpEffects _ secs _) = round (now `diffUTCTime` t) <= secs
            groups     = groupBy ((==) `on` (view distinctId . fst)) valids
            (scs', el) = foldr h ([], EffectList []) groups
            h [] acc = acc
            h grp@((sc, ConsumpEffects amt _ (EffectList thisEl)):_) acc@(_, EffectList accEl)
              | length grp >= amt = let (x, m) = length grp `divMod` amt
                                    in acc & _1 <>~ replicate (x * amt) (sc & hasCausedConsumpEffect .~ True) ++
                                                    replicate m sc
                                           & _2 .~  EffectList (concat (replicate x thisEl) ++ accEl)
              | otherwise = acc & _1 %~ (map fst grp ++)
            others' = others ++ map fst invalids
        in (ms & mobTbl.ind i.stomach .~ others' ++ scs', el)


-----


dispCmdList :: HasCallStack => [Cmd] -> ActionFun
dispCmdList cmds (NoArgs i mq cols) = pager i mq Nothing . concatMap (wrapIndent cmdNamePadding cols) . mkCmdListText $ cmds
dispCmdList cmds p                  = dispMatches p cmdNamePadding . mkCmdListText $ cmds


mkCmdListText :: HasCallStack => [Cmd] -> [Text]
mkCmdListText cmds = let zipped = zip (styleCmdAbbrevs cmds) [ cmdDesc cmd | cmd <- cmds ]
                     in [ uncurry (<>) . first padCmdName $ pair | pair@(_, d) <- zipped, ()!# d ]


styleCmdAbbrevs :: HasCallStack => [Cmd] -> [Text]
styleCmdAbbrevs = map f . mkCmdTriplesForStyling
  where
    f (_,  Nothing,  scn) = scn
    f (cn, Just cpa, _  ) = uncurry (<>) . first (colorWith abbrevColor) . maybe (cn, "") (cpa, ) $ cpa `T.stripPrefix` cn


mkCmdTriplesForStyling :: HasCallStack => [Cmd] -> [(CmdName, Maybe CmdPriorityAbbrevTxt, Text)]
mkCmdTriplesForStyling cmds = let cmdNames       = [ cmdName           cmd | cmd <- cmds ]
                                  cmdPAs         = [ cmdPriorityAbbrev cmd | cmd <- cmds ]
                                  styledCmdNames = styleAbbrevs Don'tQuote cmdNames
                              in zip3 cmdNames cmdPAs styledCmdNames


-----


dispMatches :: HasCallStack => ActionParams -> Int -> [Text] -> MudStack ()
dispMatches (LowerNub i mq cols needles) indent haystack = let (dropEmpties -> matches) = map grep needles in
    if ()# matches
      then wrapSend mq cols sorrySearch
      else pager i mq Nothing . concatMap (wrapIndent indent cols) . intercalate [""] $ matches
  where
    grep needle = let haystack' = [ (hay, hay') | hay <- haystack, let hay' = T.toLower . dropANSI $ hay ]
                  in [ fst match | match <- haystack', needle `T.isInfixOf` snd match ]
dispMatches p _ _ = patternMatchFail "dispMatches" . showText $ p


-----


embedId :: Id -> Text
embedId = quoteWith (T.singleton plaIdDelimiter) . showText


-----


expandEmbeddedIds :: HasCallStack => MudState -> ChanContext -> [Broadcast] -> MudStack [Broadcast]
expandEmbeddedIds ms ChanContext { revealAdminNames } = concatMapM helper
  where
    helper a@(msg, is) = case breakIt msg of
      (_, "")                                        -> unadulterated a
      (x, breakIt . T.tail -> (numTxt, T.tail -> y)) ->
          let embeddedId = read . T.unpack $ numTxt :: Int
              f i | g . isLinked ms $ (i, embeddedId) = return (rebuild . getSing embeddedId $ ms, pure i)
                  | otherwise = ((, pure i) . rebuild . underline) <$> updateRndmName i embeddedId
              g       = onTrue revealAdminNames (isAdminId embeddedId ms ||)
              rebuild = quoteWith' (x, y)
          in mapM f is >>= concatMapM helper


breakIt :: Text -> (Text, Text)
breakIt = T.break (== plaIdDelimiter)


expandEmbeddedIdsToSings :: HasCallStack => MudState -> Text -> Text
expandEmbeddedIdsToSings ms = helper
  where
    helper msg = case breakIt msg of
      (_, ""                                       ) -> msg
      (x, breakIt . T.tail -> (numTxt, T.tail -> y)) -> let embeddedId = read . T.unpack $ numTxt :: Int
                                                        in helper . quoteWith' (x, y) . getSing embeddedId $ ms


-----


fakeClientInput :: HasCallStack => MsgQueue -> Text -> MudStack ()
fakeClientInput mq = writeMsg mq . FromClient . nl


-----


formatChanMsg :: Text -> Text -> Text -> Text
formatChanMsg cn n msg = T.concat [ parensQuote cn, " ", n, ": ", msg ]


-----


formatQuestion :: HasCallStack => Id  -> MudState -> Broadcast -> MudStack [Broadcast]
formatQuestion i ms (txt, is)
  | i `elem` is = let pair = i |&| (flip (formatChanMsg "Question") txt . (`getSing` ms) &&& pure)
                  in (pair :) <$> mkBsWithStyled (i `delete` is)
  | otherwise   = mkBsWithStyled is
  where
    mkBsWithStyled is' = mapM getStyled is' >>= \styleds ->
        return [ (formatChanMsg "Question" styled txt, pure i') | i' <- is' | styled <- styleds ]
    getStyled targetId = view _3 . head . filter (views _1 (== i)) <$> getQuestionStyleds targetId ms


-----


getAllChanIdNames :: HasCallStack => Id -> MudState -> MudStack (IM.IntMap [(Id, Text)])
getAllChanIdNames i ms = let tunedChans = foldr helper [] . getPCChans i $ ms in
    IM.fromList . zipWith (\chan -> (chan^.chanId, )) tunedChans <$> forM tunedChans (flip (getChanIdNames i) ms)
  where
    helper chan acc = views chanConnTbl (M.! getSing i ms) chan ? (chan : acc) :? acc


getChanIdNames :: HasCallStack => Id -> Chan -> MudState -> MudStack [(Id, Text)]
getChanIdNames i c ms = let (linkeds, nonLinkedIds) = getChanLinkeds_nonLinkedIds i c ms in
    sortBy (compare `on` snd) . (linkeds ++) . zip nonLinkedIds <$> mapM (updateRndmName i) nonLinkedIds


getChanLinkeds_nonLinkedIds :: HasCallStack => Id -> Chan -> MudState -> ([(Id, Sing)], Inv)
getChanLinkeds_nonLinkedIds i c ms =
    let s                     = getSing i ms
        others                = views chanConnTbl (filter h . map g . filter f . M.toList) c
        f (s', isTuned)       = s' /= s && isTuned
        g (s', _      )       = ((`getIdForPCSing` ms) &&& id) s'
        h                     = (`isAwake` ms) . fst
        (linkeds, nonLinkeds) = partition (isLinked ms . (i, ) . fst) others
        nonLinkedIds          = map fst nonLinkeds
    in (linkeds, nonLinkedIds)


getChanStyleds :: HasCallStack => Id -> Chan -> MudState -> MudStack [(Id, Text, Text)]
getChanStyleds i c ms = let (linkeds, nonLinkedIds) = getChanLinkeds_nonLinkedIds i c ms in
    mapM (updateRndmName i) nonLinkedIds >>= \rndmNames ->
        let nonLinkeds' = zip nonLinkedIds rndmNames
            combo       = sortBy (compare `on` snd) $ linkeds ++ nonLinkeds'
            styleds     = styleAbbrevs Don'tQuote . map snd $ combo
            helper (x, y) styled | x `elem` nonLinkedIds = a & _3 %~ underline
                                 | otherwise             = a
              where
                a = (x, y, styled)
        in return . zipWith helper combo $ styleds


-----


getPCChans :: HasCallStack => Id -> MudState -> [Chan]
getPCChans i ms = views chanTbl (IM.foldr helper []) ms
  where
    helper chan acc = getSing i ms `elem` (chan^.chanConnTbl.to M.keys) ? (chan : acc) :? acc


-----


getQuestionStyleds :: HasCallStack => Id -> MudState -> MudStack [(Id, Text, Text)]
getQuestionStyleds i ms = let (plaIds,    adminIds) = getTunedQuestionIds i ms
                              (linkedIds, otherIds) = partition (isLinked ms . (i, )) plaIds
                          in mapM (updateRndmName i) otherIds >>= \rndmNames ->
                              let rndms   = zip otherIds rndmNames
                                  f       = map (dupSecond (`getSing` ms))
                                  linkeds = f linkedIds
                                  admins  = f adminIds
                                  combo   = sortBy (compare `on` snd) $ rndms ++ nubSort (linkeds ++ admins)
                                  styleds = styleAbbrevs Don'tQuote . map snd $ combo
                                  helper (x, y) styled | x `elem` otherIds = a & _3 %~ underline
                                                       | otherwise         = a
                                    where
                                      a = (x, y, styled)
                              in return . zipWith helper combo $ styleds


-----


getTunedQuestionIds :: HasCallStack => Id -> MudState -> (Inv, Inv)
getTunedQuestionIds i ms =
    (getLoggedInPlaIds &&& getNonIncogLoggedInAdminIds) ms & both %~ filter (`isTunedQuestionId` ms) . (i `delete`)


-----


happyTimes :: HasCallStack => MudState -> [Either Text (Text, [EmoteWord], Text)] -> (Text, Text, Inv, [Broadcast])
happyTimes ms xformed =
    let (toSelf, toTargets, toOthers)               = unzip3 . rights $ xformed
        targetIds                                   = nub . foldr extractIds [] $ toTargets
        extractIds [ForNonTargets _           ] acc = acc
        extractIds (ForTarget     _ targetId:_) acc = targetId : acc
        extractIds (ForTargetPoss _ targetId:_) acc = targetId : acc
        extractIds xs                           _   = patternMatchFail "happyTimes extractIds" . showText $ xs
        msgMap  = foldr (\targetId -> at targetId ?~ []) IM.empty targetIds
        msgMap' = foldr consWord msgMap toTargets
        consWord [ ForNonTargets word                           ] = IM.map (word :)
        consWord [ ForTarget     p targetId, ForNonTargets word ] = selectiveCons p targetId False word
        consWord [ ForTargetPoss p targetId, ForNonTargets word ] = selectiveCons p targetId True  word
        consWord xs = const . patternMatchFail "happyTimes consWord" . showText $ xs
        selectiveCons p targetId isPoss word = IM.mapWithKey helper
          where
            helper k v = let targetSing = onTrue isPoss (<> "'s") . getSing k $ ms
                         in (: v) $ if k == targetId
                           then colorWith emoteTargetColor targetSing <> p
                           else word
        toTargetBs = IM.foldlWithKey' helper [] msgMap'
          where
            helper acc k = (: acc) . (formatMsg *** pure) . (, k)
        formatMsg = bracketQuote . punctuateMsg . T.unwords
        _ = ()
    in (formatMsg toSelf, formatMsg toOthers, targetIds, toTargetBs)


-----


hasEnc :: HasCallStack => Args -> Bool
hasEnc [] = False
hasEnc as = ((||) <$> any (`elem` [ enc, enc's ]) <*> (== prd enc) . last) as


-----


hasType :: HasCallStack => Id -> MudState -> Bool
hasType i = views typeTbl ((i `elem`) . IM.keys)


-----


hasYou :: [Text] -> Bool
hasYou = any (`elem` yous) . map (T.dropAround (not . isLetter) . T.toLower)


-----


initPropNamesTbl :: HasCallStack => MudStack () -- Used by the "!propnames" debug cmd.
initPropNamesTbl = initTblHelper "initPropNamesTbl" "prop_names" (lookupPropName "jason") insertPropNames propNamesFileFun


initTblHelper :: HasCallStack => FunName -> Text -> IO (Maybe Text) -> (Text -> IO ()) -> FilePathFun -> MudStack ()
initTblHelper fn (dblQuote -> tblName) lookupFun insertFun fpf = liftIO (mkMudFilePath fpf) >>= \fp ->
    liftIO (T.readFile fp) |&| try >=> either (emptied . fileIOExHandler fn) proceed
  where
    logHelper   = logNotice fn
    proceed txt = join <$> withDbExHandler fn lookupFun >>= \case
      Nothing -> logHelper ("initializing the " <> tblName <> " table.") >> withDbExHandler_ fn (insertFun txt)
      Just _  -> logHelper $ the tblName <> " table has already been initialized."


initWordsTbl :: HasCallStack => MudStack () -- Used by the "!words" debug cmd.
initWordsTbl = initTblHelper "initWordsTbl" "words" (lookupWord "a") insertWords wordsFileFun


-----


isActingAny :: HasCallStack => Id -> MudState -> Bool
isActingAny i = not . M.null . getActMap i


isAttacking :: HasCallStack => Id -> MudState -> Bool
isAttacking = isActing Attacking


isActing :: HasCallStack => ActType -> Id -> MudState -> Bool
isActing act i = M.member act . getActMap i


isDrinking :: HasCallStack => Id -> MudState -> Bool
isDrinking = isActing Drinking


isEating :: HasCallStack => Id -> MudState -> Bool
isEating = isActing Eating


isDrinkingEating :: HasCallStack => Id -> MudState -> (Bool, Bool)
isDrinkingEating i = (isDrinking `fanUncurry` isEating) . (i, )


isSacrificing :: HasCallStack => Id -> MudState -> Bool
isSacrificing = isActing Sacrificing


-----


isAlive :: HasCallStack => Id -> MudState -> Bool
isAlive i = (i `notElem`) . getInv iNecropolis


-----


isBracketed :: [Text] -> Bool
isBracketed ws = or [ T.head (head ws) `elem` ("[<" :: String)
                    , "]." `T.isSuffixOf` last ws
                    , ">." `T.isSuffixOf` last ws ]


-----


isHeDon't :: Char -> Text -> Bool
isHeDon't c = (== prd (T.singleton c))


-----


isHostBanned :: HasCallStack => Text -> IO Any
isHostBanned host = isBanned host <$> (getDbTblRecs "ban_host" :: IO [BanHostRec])


isBanned :: (HasCallStack, BanRecord a) => Text -> [a] -> Any
isBanned target = helper . reverse
  where
    helper []                             = Any False
    helper (x:xs) | recTarget x == target = Any . recIsBanned $ x
                  | otherwise             = helper xs


-----


isLinked :: HasCallStack => MudState -> (Id, Id) -> Bool
isLinked = helperIsLinked (||)


helperIsLinked :: HasCallStack => (Bool -> Bool -> Bool) -> MudState -> (Id, Id) -> Bool
helperIsLinked f ms (i, i') = let s                = getSing i  ms
                                  s'               = getSing i' ms
                                  targetLinkedToMe = s' `elem` getLinked i  ms
                                  meLinkedToTarget = s  `elem` getLinked i' ms
                              in noNpcs && (targetLinkedToMe `f` meLinkedToTarget)
  where
    noNpcs = not ((||) <$> isNpc i <*> isNpc i' $ ms)


isDblLinked :: HasCallStack => MudState -> (Id, Id) -> Bool
isDblLinked = helperIsLinked (&&)


-----


isOutside :: Id -> MudState -> Bool
isOutside i = views rmEnv (== OutsideEnv) . getMobRm i


-----


isPCBanned :: HasCallStack => Sing -> IO Any
isPCBanned banSing = isBanned banSing <$> (getDbTblRecs "ban_pc" :: IO [BanPCRec])


-----


locateHelper :: HasCallStack => MudState -> [Text] -> Id -> (Id, Text)
locateHelper ms txts i = case getType i ms of
  RmType -> (i, commas txts)
  _      -> maybe oops (uncurry . locateHelper $ ms) $ searchInvs `mplus` searchEqs
  where
    searchInvs = views invTbl (fmap (mkDescId "in"         ) . listToMaybe . IM.keys . IM.filter ( i `elem`)           ) ms
    searchEqs  = views eqTbl  (fmap (mkDescId "equipped by") . listToMaybe . IM.keys . IM.filter ((i `elem`) . M.elems)) ms
    mkDescId txt targetId = ((txts ++) . pure $ txt |<>| mkNameTypeIdDesc targetId ms, targetId)
    oops                  = blowUp "locateHelper" "ID is in limbo" . showText $ i


-----


loggedInOut :: Bool -> Text
loggedInOut = loggedInOutHelper id


loggedInOutHelper :: HasCallStack => (Text -> Text) -> Bool -> Text
loggedInOutHelper f = ("logged " <>) . f . inOut


inOut :: Bool -> Text
inOut True  = "in"
inOut False = "out"


loggedInOutColorize :: Bool -> Text
loggedInOutColorize True  = loggedInOutHelper (colorWith loggedInColor) True
loggedInOutColorize False = loggedInOutHelper id                        False


-----


mkHimHer :: Sex -> Text
mkHimHer Male   = "him"
mkHimHer Female = "her"
mkHimHer NoSex  = "it"


-----


mkHolySymbolDesc :: GodName -> Text -- TODO
mkHolySymbolDesc Aule      = "holysymbol"
mkHolySymbolDesc Caila     = "holysymbol"
mkHolySymbolDesc Celoriel  = "holysymbol"
mkHolySymbolDesc Dellio    = "holysymbol"
mkHolySymbolDesc Drogo     = "holysymbol"
mkHolySymbolDesc Iminye    = "holysymbol"
mkHolySymbolDesc Itulvatar = "holysymbol"
mkHolySymbolDesc Murgorhd  = "holysymbol"
mkHolySymbolDesc Rha'yk    = "holysymbol"
mkHolySymbolDesc Rumialys  = "holysymbol"


-----


mkInterfaceList :: HasCallStack => IO Text
mkInterfaceList = NI.getNetworkInterfaces >>= \ns -> return . commas $ [ T.concat [ showText . NI.name $ n
                                                                                  , ": "
                                                                                  , showText . NI.ipv4 $ n ]
                                                                       | n <- ns ]


-----


mkNameTypeIdDesc :: HasCallStack => Id -> MudState -> Text
mkNameTypeIdDesc i ms = let (n, typeTxt) = case getType i ms of RmType -> (getRmName i ms, pp RmType)
                                                                t      -> (getSing   i ms, pp t     )
                        in n <> spaced (parensQuote typeTxt) <> bracketQuote (showText i)


-----


mkActionParams :: HasCallStack => Id -> MudState -> Args -> ActionParams
mkActionParams i ms as = ActionParams { myId        = i
                                      , plaMsgQueue = getMsgQueue i ms
                                      , plaCols     = getColumns  i ms
                                      , args        = as }


-----


mkChanReport :: HasCallStack => Id -> MudState -> Chan -> [Text]
mkChanReport i ms (Chan ci cn cct tappers) =
    let desc    = commas . map descPla . f $ [ (s, t, l) | (s, t) <- M.toList cct
                                                         , let l = isAwake (getIdForPCSing s ms) ms ]
        tapping = getSing i ms `elem` tappers |?| spcL . parensQuote $ "wiretapped"
    in [ T.concat [ bracketQuote . showText $ ci, " ", dblQuote cn, tapping, ":" ], desc ]
  where
    descPla (s, t, l) = T.concat [ underline s, ": ", tunedInOutColorize t, " / ", loggedInOutColorize l ]
    f                 = sortBy (compare `on` view _1)


-----


mkPossPro :: Sex -> Text
mkPossPro Male   = "his"
mkPossPro Female = "her"
mkPossPro NoSex  = "its"


-----


mkPros :: Sex -> (Text, Text, Text)
mkPros sexy = (mkThrPerPro, mkPossPro, mkReflexPro) & each %~ (sexy |&|)


-----


mkReflexPro :: Sex -> Text
mkReflexPro Male   = "himself"
mkReflexPro Female = "herself"
mkReflexPro NoSex  = "itself"


-----


mkRetainedMsgFromPerson :: Sing -> Text -> Text
mkRetainedMsgFromPerson s msg = fromPersonMarker `T.cons` (quoteWith "__" s |<>| msg)


-----


mkRightForNonTargets :: (Text, Text, Text) -> Either Text (Text, [EmoteWord], Text)
mkRightForNonTargets = Right . (_2 %~ (pure . ForNonTargets))


-----


mkRndmVector :: HasCallStack => MudStack (V.Vector Int)
mkRndmVector = rndmVector rndmVectorLen


-----


mkSingleTarget :: HasCallStack => MsgQueue -> Cols -> Text -> Text -> SingleTarget
mkSingleTarget mq cols target (sorryIgnoreLocPref -> sorryMsg) =
    SingleTarget { strippedTarget   = capitalize   t
                 , strippedTarget'  = uncapitalize t
                 , sendFun          = hlp ? (multiWrapSend mq cols . (sorryMsg :) . pure) :? wrapSend mq cols
                 , multiSendFun     = hlp ? (multiWrapSend mq cols . (sorryMsg :)       ) :? multiWrapSend mq cols
                 , consLocPrefMsg   = hlp ? (sorryMsg :)                                  :? id
                 , consLocPrefBcast = hlp ? f                                             :? const id }
  where
    hlp = hasLocPref . uncapitalize $ target
    t   = hlp ? T.tail (T.tail target) :? target
    f i = ((sorryMsg, pure i) :)


-----


mkThrPerPro :: Sex -> Text
mkThrPerPro Male   = "he"
mkThrPerPro Female = "she"
mkThrPerPro NoSex  = "it"


-----


mkWhoHeader :: Bool -> [Text]
mkWhoHeader b = [ T.concat [ padName "Name", b |?| padId "Id", padSex  "Sex", padRace "Race", "Level" ], divider cols ]
  where
    cols = namePadding + getSum x + sexPadding + racePadding + lvlPadding
    x    = b |?| Sum idPadding


-----


onOff :: Bool -> Text
onOff True  = "on"
onOff False = "off"


-----


pager :: HasCallStack => Id -> MsgQueue -> Maybe Fun -> [Text] -> MudStack ()
pager i mq mf txt@(length -> txtLen) = getState >>= \ms -> let pl = getPageLines i ms in if txtLen + 3 <= pl
  then send mq . nl . T.unlines $ txt
  else let pair@(page, _) = splitAt (pl - 2) txt in do
      send mq . T.unlines $ page
      sendPagerPrompt mq (pl - 2) txtLen
      setInterp i . Just . interpPager mf pl txtLen $ pair


-----


parseOutDenotative :: [Text] -> Text -> [Text]
parseOutDenotative ws rest = onFalse (()# rest) (rest :) . tail $ ws


-----


ppMaybe :: (Pretty a) => Maybe a -> Text
ppMaybe = maybe none pp


-----


punc :: String
punc = "!\"),./:;?"


isPunc :: Char -> Bool
isPunc = (`elem` punc)


-----


questionChanContext :: ChanContext
questionChanContext = ChanContext "question" Nothing True


-----


sendGenericErrorMsg :: HasCallStack => MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = wrapSend mq cols genericErrorMsg


-----


showTime :: HasCallStack => MsgQueue -> Cols -> MudStack Text
showTime mq cols = liftIO getCurryTime >>= \CurryTime { .. } ->
    ((>>) <$> wrapSend mq cols <*> return) . (curryHour |&|) $ if isNight curryHour
      then case getMoonPhaseForDayOfMonth curryDayOfMonth of Nothing    -> const sorryTimeUnknown
                                                             Just phase -> mkTimeDescNight phase
      else mkTimeDescDay


mkTimeDescDay :: Hour -> Text
mkTimeDescDay {- morning   -} 6  = "The sun is rising in the east; a new day is dawning. It's about 6:00."
mkTimeDescDay {- morning   -} 7  = mkTimeDescDayHelper "it's early morning."
mkTimeDescDay {- morning   -} 8  = mkTimeDescDayHelper "it's mid-morning."
mkTimeDescDay {- morning   -} 9  = mkTimeDescDayHelper "it's late morning."
mkTimeDescDay {- afternoon -} 10 = mkTimeDescDayHelper "it's about midday, or 10:00."
mkTimeDescDay {- afternoon -} 11 = mkTimeDescDayHelper "it's early afternoon."
mkTimeDescDay {- afternoon -} 12 = mkTimeDescDayHelper "it's midafternoon, or about 12:00."
mkTimeDescDay {- afternoon -} 13 = mkTimeDescDayHelper "it's past midafternoon."
mkTimeDescDay {- afternoon -} 14 = mkTimeDescDayHelper "it's late afternoon."
mkTimeDescDay {- evening   -} 15 = mkTimeDescDayHelper "it's now evening, or about 15:00."
mkTimeDescDay {- evening   -} 16 = mkTimeDescDayHelper "it's mid evening."
mkTimeDescDay {- evening   -} 17 = mkTimeDescDayHelper "it's late in the evening."
mkTimeDescDay                 x  = patternMatchFail "mkTimeDescDay" . showText $ x


mkTimeDescDayHelper :: Text -> Text
mkTimeDescDayHelper = ("Judging by the position of the sun in the sky, " <>)


mkTimeDescNight :: MoonPhase -> Hour -> Text
mkTimeDescNight NewMoon _  = "Given that the moon is altogether absent in the sky, you can't tell what time of night it is."
mkTimeDescNight phase   0  = mkTimeDescNightHelper phase "it's about midnight."
mkTimeDescNight phase   1  = mkTimeDescNightHelper phase "it's shortly after midnight."
mkTimeDescNight phase   2  = mkTimeDescNightHelper phase "it's the middle of the night."
mkTimeDescNight phase   3  = mkTimeDescNightHelper phase "the night is more than half over."
mkTimeDescNight phase   4  = mkTimeDescNightHelper phase "it's less than 2 hours to sunrise."
mkTimeDescNight phase   5  = mkTimeDescNightHelper phase "the sun will soon be rising."
mkTimeDescNight _       18 = "The sun has finished setting. It's about 18:00."
mkTimeDescNight phase   19 = mkTimeDescNightHelper phase "night has only just begun."
mkTimeDescNight _       x  = patternMatchFail "mkTimeDescNight" . showText $ x


mkTimeDescNightHelper :: MoonPhase -> Text -> Text
mkTimeDescNightHelper phase t = T.concat [ "Judging by the position of the ", pp phase, " moon in the sky, ", t ]


-----


tunedInOut :: Bool -> Text
tunedInOut = tunedInOutHelper id


tunedInOutHelper :: (Text -> Text) -> Bool -> Text
tunedInOutHelper f = ("tuned " <>) . f . inOut


tunedInOutColorize :: Bool -> Text
tunedInOutColorize True  = tunedInOutHelper (colorWith tunedInColor) True
tunedInOutColorize False = tunedInOutHelper id                       False


-----


unmsg :: [Text] -> [Text]
unmsg [cn        ] = [ T.init cn, ""            ]
unmsg [cn, target] = [ cn,        T.init target ]
unmsg xs           = patternMatchFail "unmsg" . showText $ xs


-----


updateRndmName :: HasCallStack => Id -> Id -> MudStack Sing
updateRndmName i targetId = do
    rndmNames <- T.lines <$> readRndmNames
    rndmName  <- ()# rndmNames ? return "xyz" :? rndmElem rndmNames
    let helper ms = let targetSing = getSing targetId ms
                        rnt        = getRndmNamesTbl i ms
                        notFound   = let existing = M.elems rnt
                                         checkLength n | T.length n > maxNameLen = mkUniqueName "xyz" existing
                                                       | otherwise               = n
                                         rndmName' = checkLength . mkUniqueName rndmName $ existing
                                         ms'       = ms & rndmNamesMstrTbl.ind i.at targetSing ?~ rndmName'
                                     in (ms', rndmName')
                        found match = (ms, match)
                    in maybe notFound found . M.lookup targetSing $ rnt
    modifyState helper
  where
    readRndmNames = let f = emptied . fileIOExHandler "updateRndmName"
                    in liftIO (T.readFile =<< mkMudFilePath rndmNamesFileFun) |&| try >=> eitherRet f
    mkUniqueName rndmName existing
      | rndmName `notElem` existing = rndmName
      | otherwise = case sortBy (flip compare) . filter (rndmName `T.isPrefixOf`) $ existing of
        [_]             -> rndmName <> "2"
        (head -> match) -> let (name, readNum -> num) = T.break isDigit match
                           in name <> showText (succ num)


-----


withDbExHandler :: HasCallStack => Text -> IO a -> MudStack (Maybe a)
withDbExHandler fn f = (Just <$> dbOperation f) `catch` (\e -> dbExHandler fn e >> return Nothing)


withDbExHandler_ :: HasCallStack => Text -> IO () -> MudStack ()
withDbExHandler_ fn f = dbOperation f `catch` dbExHandler fn


-----


withoutArgs :: HasCallStack => ActionFun -> ActionParams -> MudStack ()
withoutArgs f p = ignore p >> f p { args = [] }


ignore :: HasCallStack => ActionFun
ignore (Ignoring mq cols as) = wrapSend1Nl mq cols . parensQuote . thrice prd $ "Ignoring " <> as
ignore p                     = patternMatchFail "ignore" . showText $ p
