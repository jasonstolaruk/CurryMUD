{-# LANGUAGE FlexibleContexts, LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, TupleSections, ViewPatterns #-}

-- This module contains helper functions used by multiple modules under "Mud.Cmds".

module Mud.Cmds.Util.Misc ( asterisk
                          , awardExp
                          , consume
                          , descMaybeId
                          , descMaybeSingId
                          , descSingId
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
                          , handleDeath
                          , happy
                          , hasEnc
                          , hasYou
                          , inOut
                          , isAlive
                          , isAttacking
                          , isAwake
                          , isBracketed
                          , isDblLinked
                          , isDrinking
                          , isDrinkingEating
                          , isEating
                          , isHeDon't
                          , isHostBanned
                          , isLinked
                          , isMoving
                          , isPCBanned
                          , isPunc
                          , locateHelper
                          , loggedInOut
                          , loggedInOutColorize
                          , mkActionParams
                          , mkChanReport
                          , mkCmdListText
                          , mkHimHer
                          , mkInterfaceList
                          , mkNameTypeIdDesc
                          , mkPossPro
                          , mkPrettifiedSexRaceLvl
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
                          , punc
                          , questionChanContext
                          , sendGenericErrorMsg
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
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Interp.Pager
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.LocPref
import Mud.TheWorld.Zones.AdminZoneIds
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

import Control.Arrow ((***), first, second)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (+~), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), forM, mplus, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (setBit, zeroBits)
import Data.Char (isDigit, isLetter)
import Data.Either (rights)
import Data.Function (on)
import Data.List (delete, groupBy, intercalate, nub, partition, sortBy, unfoldr)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..), Sum(..))
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Prelude hiding (exp)
import qualified Data.IntMap.Lazy as IM (IntMap, empty, filter, foldlWithKey', foldr, fromList, keys, map, mapWithKey)
import qualified Data.Map.Lazy as M ((!), elems, empty, keys, lookup, member, toList)
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


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Util.Misc"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Util.Misc"


-- ==================================================


asterisk :: Text
asterisk = colorWith asteriskColor "*"


-----


awardExp :: Exp -> Text -> Id -> MudStack ()
awardExp amt reason i = getLvlExp i <$> getState >>= \(l, x) -> let diff = calcLvlForExp (x + amt) - l in
    rndmVector (diff * noOfLvlUpRndmInts) >>= \v -> helper v |&| modifyState >=> \(ms, (msgs, logMsgs)) -> do
        mapM_ (retainedMsg i ms) msgs
        let logMsg = T.concat [ "awarded "
                              , commaShow amt
                              , " exp "
                              , parensQuote reason
                              , "."
                              , logMsgs |!| " " <> (capitalize . prd . slashes $ logMsgs) ]
            b = isNpc i ms ? True :? isLoggedIn (getPla i ms)
        when b . logPla "awardExp" i $ logMsg
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
        in (ms'', (ms'', if diff <= 0 then dupIdentity else unzip . unfoldr f $ diff))


noOfLvlUpRndmInts :: Int
noOfLvlUpRndmInts = 5


lvlUp :: Id -> MudState -> V.Vector Int -> Lvl -> Lvl -> MudState
lvlUp i = helper
  where
    helper ms v oldLvl newLvl
      | oldLvl >= newLvl = ms
      | otherwise        = let (V.toList -> [ a, b, c, d, e ], v') = V.splitAt noOfLvlUpRndmInts v
                               myMob = mobTbl.ind i
                               ms'   = ms & myMob.maxHp          +~ calcLvlUpHp       i ms a
                                          & myMob.maxMp          +~ calcLvlUpMp       i ms b
                                          & myMob.maxPp          +~ calcLvlUpPp       i ms c
                                          & myMob.maxFp          +~ calcLvlUpFp       i ms d
                                          & pcTbl.ind i.skillPts +~ calcLvlUpSkillPts i ms e
                           in helper ms' v' (succ oldLvl) newLvl


-----


consume :: Id -> [StomachCont] -> MudStack ()
consume _ []     = unit
consume i newScs = do
    logPla "consume" i . prd $ "consuming " <> commas (map pp newScs)
    now <- liftIO getCurrentTime
    procEffectList i =<< modifyState (helper now)
  where
    helper now ms =
        let scs   = getStomach i ms ++ newScs
            pairs = map (second getConsumpEffects . dup) scs :: [(StomachCont, Maybe ConsumpEffects)]
            getConsumpEffects sc = case sc^.distinctId of
              Left  (DistinctLiqId  x) -> f liqEdibleEffects  . getDistinctLiq  $ x
              Right (DistinctFoodId x) -> f foodEdibleEffects . getDistinctFood $ x
              where
                f a b = view (a.consumpEffects) . b $ ms
            (others, consumpEffectingPairs) = foldr g ([], []) pairs
              where
                g (sc, Nothing) = _1 %~ (sc       :)
                g (sc, Just ce) = _2 %~ ((sc, ce) :)
            (valids, invalids) = partition isValid consumpEffectingPairs
            isValid (sc, ce)   = sc^.hasCausedConsumpEffect.to not && isNotExpired (sc, ce)
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


descSingId :: Id -> MudState -> Text
descSingId i ms = quoteWith' (getSing i ms, parensQuote . showText $ i) " "


descMaybeId :: MudState -> Maybe Id -> Text
descMaybeId ms = maybe none (`descSingId` ms)


descMaybeSingId :: Maybe Id -> MudState -> Text
descMaybeSingId Nothing  _  = none
descMaybeSingId (Just x) ms = descSingId x ms


-----


dispCmdList :: [Cmd] -> ActionFun
dispCmdList cmds (NoArgs i mq cols) = pager i mq Nothing . concatMap (wrapIndent cmdNamePadding cols) . mkCmdListText $ cmds
dispCmdList cmds p                  = dispMatches p cmdNamePadding . mkCmdListText $ cmds


mkCmdListText :: [Cmd] -> [Text]
mkCmdListText cmds = let zipped = zip (styleCmdAbbrevs cmds) [ cmdDesc cmd | cmd <- cmds ]
                     in [ padCmdName n <> d | (n, d) <- zipped, ()!# d ]


styleCmdAbbrevs :: [Cmd] -> [Text]
styleCmdAbbrevs cmds = let cmdNames       = [ cmdName           cmd | cmd <- cmds ]
                           cmdPAs         = [ cmdPriorityAbbrev cmd | cmd <- cmds ]
                           styledCmdNames = styleAbbrevs Don'tQuote cmdNames
                       in [ checkProrityAbbrev a | a <- zip3 cmdNames cmdPAs styledCmdNames ]
  where
    checkProrityAbbrev (_,  Nothing,  scn) = scn
    checkProrityAbbrev (cn, Just cpa, _  ) = colorWith abbrevColor cpa <> (fromJust . T.stripPrefix cpa $ cn)


-----


dispMatches :: ActionParams -> Int -> [Text] -> MudStack ()
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


expandEmbeddedIds :: MudState -> ChanContext -> [Broadcast] -> MudStack [Broadcast]
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


expandEmbeddedIdsToSings :: MudState -> Text -> Text
expandEmbeddedIdsToSings ms = helper
  where
    helper msg = case breakIt msg of
      (_, "")                                        -> msg
      (x, breakIt . T.tail -> (numTxt, T.tail -> y)) -> let embeddedId = read . T.unpack $ numTxt :: Int
                                                        in helper . quoteWith' (x, y) . getSing embeddedId $ ms


-----


fakeClientInput :: MsgQueue -> Text -> MudStack ()
fakeClientInput mq = writeMsg mq . FromClient . nl


-----


formatChanMsg :: Text -> Text -> Text -> Text
formatChanMsg cn n msg = T.concat [ parensQuote cn
                                  , " "
                                  , n
                                  , ": "
                                  , msg ]


-----


formatQuestion :: Id  -> MudState -> Broadcast -> MudStack [Broadcast]
formatQuestion i ms (txt, is)
  | i `elem` is = ((formatChanMsg "Question" (getSing i ms) txt, pure i) :) <$> mkBsWithStyled (i `delete` is)
  | otherwise   = mkBsWithStyled is
  where
    mkBsWithStyled is' = mapM getStyled is' >>= \styleds ->
        return [ (formatChanMsg "Question" styled txt, pure i') | i' <- is' | styled <- styleds ]
    getStyled targetId = view _3 . head . filter (views _1 (== i)) <$> getQuestionStyleds targetId ms


-----


getAllChanIdNames :: Id -> MudState -> MudStack (IM.IntMap [(Id, Text)])
getAllChanIdNames i ms = let tunedChans = foldr helper [] . getPCChans i $ ms in
    IM.fromList . zipWith (\chan -> (chan^.chanId, )) tunedChans <$> forM tunedChans (flip (getChanIdNames i) ms)
  where
    helper chan acc = views chanConnTbl (M.! getSing i ms) chan ? (chan : acc) :? acc


getChanIdNames :: Id -> Chan -> MudState -> MudStack [(Id, Text)]
getChanIdNames i c ms = let (linkeds, nonLinkedIds) = getChanLinkeds_nonLinkedIds i c ms in
    sortBy (compare `on` snd) . (linkeds ++) . zip nonLinkedIds <$> mapM (updateRndmName i) nonLinkedIds


getChanLinkeds_nonLinkedIds :: Id -> Chan -> MudState -> ([(Id, Sing)], Inv)
getChanLinkeds_nonLinkedIds i c ms =
    let s                     = getSing i ms
        others                = views chanConnTbl (filter h . map g . filter f . M.toList) c
        f (s', isTuned)       = s' /= s && isTuned
        g (s', _      )       = (getIdForMobSing s' ms, s')
        h                     = (`isAwake` ms) . fst
        (linkeds, nonLinkeds) = partition (isLinked ms . (i, ) . fst) others
        nonLinkedIds          = map fst nonLinkeds
    in (linkeds, nonLinkedIds)


getChanStyleds :: Id -> Chan -> MudState -> MudStack [(Id, Text, Text)]
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


getPCChans :: Id -> MudState -> [Chan]
getPCChans i ms = views chanTbl (IM.foldr helper []) ms
  where
    helper chan acc = getSing i ms `elem` (chan^.chanConnTbl.to M.keys) ? (chan : acc) :? acc


-----


getQuestionStyleds :: Id -> MudState -> MudStack [(Id, Text, Text)]
getQuestionStyleds i ms =
    let (plaIds,    adminIds) = getTunedQuestionIds i ms
        (linkedIds, otherIds) = partition (isLinked ms . (i, )) plaIds
    in mapM (updateRndmName i) otherIds >>= \rndmNames ->
        let rndms   = zip otherIds rndmNames
            f       = map (second (`getSing` ms) . dup)
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


getTunedQuestionIds :: Id -> MudState -> (Inv, Inv)
getTunedQuestionIds i ms = let pair = (getLoggedInPlaIds ms, getNonIncogLoggedInAdminIds ms)
                           in pair & both %~ filter (`isTunedQuestionId` ms) . (i `delete`)


-----


{-
When Taro dies:
Taro's PC becomes a disembodied spirit.
Taro's corpse is created. Items are transferred from spirit to corpse.
Those who are linked with Taro are notified of his death (via retained message?).
Taro's spirit is sent to the Necropolis when it passes into the beyond.

About spirits:
A player has a certain amount of time as a spirit, depending on level.
A spirit can move freely about with no FP cost.
A spirit retains a certain number of two-way links, depending on PS. A spirit may continue to communicate telepathically over its retained links, with no cost to PP.
Those links with the greatest volume of messages are retained. If the deceased PC's top links are all asleep, its spirit gets to retain a bonus link with a PC who is presently awake.
-}
handleDeath :: Id -> MudStack ()
handleDeath i = helper |&| modifyState >=> sequence_
  where
    helper ms = let (ms',  fs ) = mkCorpse  i ms
                    (ms'', fs') = spiritize i ms'
                in (ms'', logPla "handleDeath" i "handling death." : fs ++ fs')


mkCorpse :: Id -> MudState -> (MudState, Funs)
mkCorpse i ms = let et = EntTemplate (Just "corpse")
                                     s p
                                     (getEntDesc i ms)
                                     Nothing -- TODO: Smell.
                                     zeroBits
                    ot = ObjTemplate (getCorpseWeight i ms)
                                     (getCorpseVol    i ms)
                                     Nothing -- TODO: Taste.
                                     zeroBits
                    ct = ConTemplate (getCorpseCapacity i ms `max` calcCarriedVol i ms)
                                     (setBit zeroBits . fromEnum $ IsCorpse)
                    is = M.elems (getEqMap i ms) ++ getInv i ms
                    c  = getCoins i ms
                    (_, ms', fs) = newCon ms et ot ct (is, c) . getRmId i $ ms
                in ( ms' & eqTbl .ind i .~ M.empty
                         & invTbl.ind i .~ []
                   , logPla "mkCorpse" i "corpse created." : fs )
      where
        (s, p) = (("corpse of " <>) *** ("corpses of " <>)) $ if isPC i ms
          then second (<> "s") . dup . mkSerializedNonStdDesig i ms s' A $ Don'tCap
          else first aOrAnOnLower pair
          where
            pair@(s', _) = getBothGramNos i ms


spiritize :: Id -> MudState -> (MudState, Funs) -- TODO: Delete NPCs.
spiritize i ms = if isPC i ms
  then (ms & plaTbl.ind i %~ setPlaFlag IsSpirit True, pure . logPla "spiritize" i $ "spirit created.")
  else (ms, pure . logNotice "spiritize" . T.concat $ [ getSing i ms, " ", parensQuote (showText i), " has died." ])


-----


happy :: MudState -> [Either Text (Text, [EmoteWord], Text)] -> (Text, Text, Inv, [Broadcast])
happy ms xformed =
    let (toSelf, toTargets, toOthers)               = unzip3 . rights $ xformed
        targetIds                                   = nub . foldr extractIds [] $ toTargets
        extractIds [ForNonTargets _           ] acc = acc
        extractIds (ForTarget     _ targetId:_) acc = targetId : acc
        extractIds (ForTargetPoss _ targetId:_) acc = targetId : acc
        extractIds xs                           _   = patternMatchFail "happy extractIds" . showText $ xs
        msgMap  = foldr (\targetId -> at targetId ?~ []) IM.empty targetIds
        msgMap' = foldr consWord msgMap toTargets
        consWord [ ForNonTargets word                           ] = IM.map (word :)
        consWord [ ForTarget     p targetId, ForNonTargets word ] = selectiveCons p targetId False word
        consWord [ ForTargetPoss p targetId, ForNonTargets word ] = selectiveCons p targetId True  word
        consWord xs                                               = const . patternMatchFail "happy consWord" . showText $ xs
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


hasEnc :: Args -> Bool
hasEnc [] = False
hasEnc as = any (`elem` [ enc, enc's ]) as || last as == prd enc


-----


hasYou :: [Text] -> Bool
hasYou = any (`elem` yous) . map (T.dropAround (not . isLetter) . T.toLower)


-----


isAlive :: Id -> MudState -> Bool
isAlive i ms = i `notElem` getInv iNecropolis ms


-----


isAttacking :: Id -> MudState -> Bool
isAttacking = isActing Attacking


isDrinking :: Id -> MudState -> Bool
isDrinking = isActing Drinking


isActing :: ActType -> Id -> MudState -> Bool
isActing actType i = M.member actType . getActMap i


isEating :: Id -> MudState -> Bool
isEating = isActing Eating


isDrinkingEating :: Id -> MudState -> (Bool, Bool)
isDrinkingEating i = (uncurry isDrinking *** uncurry isEating) . dup . (,) i


isMoving :: Id -> MudState -> Bool
isMoving = isActing Moving


-----


isAwake :: Id -> MudState -> Bool
isAwake = onPla (uncurry (&&) . (isLoggedIn *** not . isIncognito) . dup) True


-----


isBracketed :: [Text] -> Bool
isBracketed ws = or [ (T.head . head $ ws) `elem` ("[<" :: String)
                    , "]." `T.isSuffixOf` last ws
                    , ">." `T.isSuffixOf` last ws ]


-----


isHeDon't :: Char -> Text -> Bool
isHeDon't c = (== prd (T.singleton c))


-----


isHostBanned :: Text -> IO Any
isHostBanned host = isBanned host <$> (getDbTblRecs "ban_host" :: IO [BanHostRec])


isBanned :: (BanRecord a) => Text -> [a] -> Any
isBanned target = helper . reverse
  where
    helper [] = Any False
    helper (x:xs) | recTarget x == target = Any . recIsBanned $ x
                  | otherwise             = helper xs


-----


isLinked :: MudState -> (Id, Id) -> Bool
isLinked = helperIsLinked (||)


helperIsLinked :: (Bool -> Bool -> Bool) -> MudState -> (Id, Id) -> Bool
helperIsLinked f ms ids@(i, i') = let s                = getSing i  ms
                                      s'               = getSing i' ms
                                      targetLinkedToMe = s' `elem` getLinked i  ms
                                      meLinkedToTarget = s  `elem` getLinked i' ms
                                  in noNpcs && (targetLinkedToMe `f` meLinkedToTarget)
  where
    noNpcs | uncurry (||) . ((🍭) *** (🍭)) $ ids = False
           | otherwise = otherwise
    (🍭) = (`isNpc` ms)


isDblLinked :: MudState -> (Id, Id) -> Bool
isDblLinked = helperIsLinked (&&)


-----


isPCBanned :: Sing -> IO Any
isPCBanned banSing = isBanned banSing <$> (getDbTblRecs "ban_pc" :: IO [BanPCRec])


-----


locateHelper :: MudState -> [Text] -> Id -> (Id, Text)
locateHelper ms txts i = case getType i ms of
  RmType -> (i, commas txts)
  _      -> maybe oops (uncurry . locateHelper $ ms) $ searchInvs `mplus` searchEqs
  where
    searchInvs = views invTbl (fmap (mkDescId "in"         ) . listToMaybe . IM.keys . IM.filter ( i `elem`)           ) ms
    searchEqs  = views eqTbl  (fmap (mkDescId "equipped by") . listToMaybe . IM.keys . IM.filter ((i `elem`) . M.elems)) ms
    mkDescId txt targetId = ((txts ++) . pure $ txt <> " " <> mkNameTypeIdDesc targetId ms, targetId)
    oops                  = blowUp "locateHelper" "ID is in limbo" . showText $ i


-----


loggedInOut :: Bool -> Text
loggedInOut = loggedInOutHelper id


loggedInOutHelper :: (Text -> Text) -> Bool -> Text
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


mkInterfaceList :: IO Text
mkInterfaceList = NI.getNetworkInterfaces >>= \ns -> return . commas $ [ T.concat [ showText . NI.name $ n
                                                                                  , ": "
                                                                                  , showText . NI.ipv4 $ n ]
                                                                       | n <- ns ]


-----


mkNameTypeIdDesc :: Id -> MudState -> Text
mkNameTypeIdDesc i ms = let (n, typeTxt) = case getType i ms of RmType -> (getRmName i ms, pp RmType)
                                                                t      -> (getSing   i ms, pp t     )
                        in n <> spaced (parensQuote typeTxt) <> bracketQuote (showText i)


-----


mkActionParams :: Id -> MudState -> Args -> ActionParams
mkActionParams i ms as = ActionParams { myId        = i
                                      , plaMsgQueue = getMsgQueue i ms
                                      , plaCols     = getColumns  i ms
                                      , args        = as }


-----


mkChanReport :: Id -> MudState -> Chan -> [Text]
mkChanReport i ms (Chan ci cn cct tappers) =
    let desc    = commas . map descPla . f $ [ (s, t, l) | (s, t) <- M.toList cct
                                                         , let p = getPla (getIdForMobSing s ms) ms
                                                         , let l = isLoggedIn p && (not . isIncognito $ p) ]
        tapping = getSing i ms `elem` tappers |?| (" " <> parensQuote "wiretapped")
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


mkPrettifiedSexRaceLvl :: Id -> MudState -> (Text, Text, Text)
mkPrettifiedSexRaceLvl i ms = let (s, r, l) = getSexRaceLvl i ms
                              in (pp s, pp r, showText l)


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
mkRetainedMsgFromPerson s msg = fromPersonMarker `T.cons` (quoteWith "__" s <> " " <> msg)


-----


mkRightForNonTargets :: (Text, Text, Text) -> Either Text (Text, [EmoteWord], Text)
mkRightForNonTargets = Right . (_2 %~ (pure . ForNonTargets))


-----


mkRndmVector :: MudStack (V.Vector Int)
mkRndmVector = rndmVector rndmVectorLen


-----


mkSingleTarget :: MsgQueue -> Cols -> Text -> Text -> SingleTarget
mkSingleTarget mq cols target (sorryIgnoreLocPref -> sorryMsg) =
    SingleTarget { strippedTarget   = capitalize   t
                 , strippedTarget'  = uncapitalize t
                 , sendFun          = hlp ? (multiWrapSend mq cols . (sorryMsg :) . pure) :? wrapSend mq cols
                 , multiSendFun     = hlp ? (multiWrapSend mq cols . (sorryMsg :)       ) :? multiWrapSend mq cols
                 , consLocPrefMsg   = hlp ? (sorryMsg :)                                  :? id
                 , consLocPrefBcast = hlp ? f                                             :? const id }
  where
    hlp = hasLocPref . uncapitalize $ target
    t   = hlp ? (T.tail . T.tail $ target) :? target
    f i = ((sorryMsg, pure i) :)


-----


mkThrPerPro :: Sex -> Text
mkThrPerPro Male   = "he"
mkThrPerPro Female = "she"
mkThrPerPro NoSex  = "it"


-----


mkWhoHeader :: Bool -> [Text]
mkWhoHeader b = T.concat [ padName "Name"
                         , b |?| padId "Id"
                         , padSex  "Sex"
                         , padRace "Race"
                         , "Level" ] : [ T.replicate (namePadding + getSum x + sexPadding + racePadding + lvlPadding) "=" ]
  where
    x = b |?| Sum idPadding


-----


onOff :: Bool -> Text
onOff True  = "on"
onOff False = "off"


-----


pager :: Id -> MsgQueue -> Maybe Fun -> [Text] -> MudStack ()
pager i mq mf txt@(length -> txtLen) = getState >>= \ms -> let pl = getPageLines i ms in if txtLen + 3 <= pl
  then send mq . nl . T.unlines $ txt
  else let (page, rest) = splitAt (pl - 2) txt in do
      send mq . T.unlines $ page
      sendPagerPrompt mq (pl - 2) txtLen
      setInterp i . Just . interpPager mf pl txtLen $ (page, rest)


-----


parseOutDenotative :: [Text] -> Text -> [Text]
parseOutDenotative ws rest = onTrue (()!# rest) (rest :) . tail $ ws


-----


punc :: String
punc = "!\"),./:;?"


isPunc :: Char -> Bool
isPunc = (`elem` punc)


-----


questionChanContext :: ChanContext
questionChanContext = ChanContext "question" Nothing True


-----


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = wrapSend mq cols genericErrorMsg


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


updateRndmName :: Id -> Id -> MudStack Sing
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
    readRndmNames = (liftIO . T.readFile $ rndmNamesFile) |&| try >=> eitherRet
      (emptied . fileIOExHandler "updateRndmName")
    mkUniqueName rndmName existing
      | rndmName `notElem` existing = rndmName
      | otherwise = case sortBy (flip compare) . filter (rndmName `T.isPrefixOf`) $ existing of
        [_]             -> rndmName <> "2"
        (head -> match) -> let (name, readNum -> num) = T.break isDigit match
                           in name <> (showText . succ $ num)


-----


withDbExHandler :: (Monoid a) => Text -> IO a -> MudStack (Maybe a)
withDbExHandler fn f = liftIO (Just <$> f) `catch` (emptied . dbExHandler fn)


withDbExHandler_ :: Text -> IO () -> MudStack ()
withDbExHandler_ fn f = liftIO f `catch` dbExHandler fn


-----


withoutArgs :: ActionFun -> ActionParams -> MudStack ()
withoutArgs f p = ignore p >> f p { args = [] }


ignore :: ActionFun
ignore (Ignoring mq cols as) = wrapSend1Nl mq cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" . showText $ p
