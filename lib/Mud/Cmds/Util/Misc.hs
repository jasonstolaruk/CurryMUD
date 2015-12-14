{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, TupleSections, ViewPatterns #-}

-- This module contains helper functions used by multiple modules under "Mud.Cmds".

module Mud.Cmds.Util.Misc ( asterisk
                          , awardExp
                          , dbExHandler
                          , descSingId
                          , dispCmdList
                          , dispMatches
                          , embedId
                          , expandEmbeddedIds
                          , expandEmbeddedIdsToSings
                          , fakeClientInput
                          , fileIOExHandler
                          , formatChanMsg
                          , formatQuestion
                          , getAllChanIdNames
                          , getChanIdNames
                          , getChanStyleds
                          , getLvl
                          , getLvlExp
                          , getPCChans
                          , getQuestionStyleds
                          , getTunedQuestionIds
                          , happy
                          , hasEnc
                          , hasYou
                          , inOut
                          , isAwake
                          , isBracketed
                          , isDblLinked
                          , isHeDon't
                          , isHostBanned
                          , isLinked
                          , isPlaBanned
                          , isPunc
                          , locateHelper
                          , loggedInOut
                          , loggedInOutColorize
                          , mkActionParams
                          , mkChanReport
                          , mkCmdListText
                          , mkInterfaceList
                          , mkNameTypeIdDesc
                          , mkPossPro
                          , mkPrettifiedSexRaceLvl
                          , mkPros
                          , mkReflexPro
                          , mkRetainedMsgFromPerson
                          , mkRightForNonTargets
                          , mkSingleTarget
                          , mkThrPerPro
                          , mkWhoHeader
                          , pager
                          , punc
                          , questionChanContext
                          , sendGenericErrorMsg
                          , throwToListenThread
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
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Interp.Pager
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.LocPref
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
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logPla)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), second)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException, SomeException, toException)
import Control.Exception.Lifted (catch, throwTo, try)
import Control.Lens (_1, _2, _3, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (+~), (?~), (^.))
import Control.Monad ((>=>), forM, mplus, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit, isLetter)
import Data.Either (rights)
import Data.Function (on)
import Data.List (delete, intercalate, nub, partition, sortBy, unfoldr)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..))
import Prelude hiding (exp)
import qualified Data.IntMap.Lazy as IM (IntMap, empty, filter, foldlWithKey', foldr, fromList, keys, map, mapWithKey)
import qualified Data.Map.Lazy as M ((!), elems, keys, lookup, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN module ("HLint: ignore Use ||"        :: String) #-}


-----


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Cmds.Util.Misc"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Misc"


-----


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Cmds.Util.Misc"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Util.Misc"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Util.Misc"


-- ==================================================


asterisk :: T.Text
asterisk = colorWith asteriskColor "*"


-----


awardExp :: Exp -> T.Text -> Id -> MudStack ()
awardExp amt reason i = helper |&| modifyState >=> \(ms, (msgs, logMsgs)) -> do
    mapM_ (retainedMsg i ms) msgs
    let logMsg = T.concat [ "awarded "
                          , showText amt
                          , " exp "
                          , parensQuote reason
                          , "."
                          , logMsgs |!| " " <> (capitalize . (<> ".") . slashes $ logMsgs) ]
        b = isNpc i ms ? True :? isLoggedIn (getPla i ms)
    when b . logPla "awardExp" i $ logMsg
  where
    helper ms =
        let oldLvl = getLvl i ms
            ms'    = ms & mobTbl.ind i.exp +~ amt
            newLvl = getLvl i ms'
            f 0    = Nothing
            f seed = Just ((lvlMsg, mkLogMsg), pred seed)
              where
                mkLogMsg = ("gained a level " <>) . parensQuote $ "now level " <> showText (newLvl - seed + 1)
        in (ms', (ms', unzip . unfoldr f $ newLvl - oldLvl))


-----


dbExHandler :: T.Text -> SomeException -> MudStack ()
dbExHandler fn e =
    logExMsg "dbExHandler" (rethrowExMsg $ "during a database operation in " <> dblQuote fn) e >> throwToListenThread e


-----


descSingId :: Id -> MudState -> T.Text
descSingId i ms = quoteWith' (getSing i ms, parensQuote . showText $ i) " "


-----


dispCmdList :: [Cmd] -> ActionFun
dispCmdList cmds (NoArgs i mq cols) = pager i mq . concatMap (wrapIndent cmdNamePadding cols) . mkCmdListText $ cmds
dispCmdList cmds p                  = dispMatches p cmdNamePadding . mkCmdListText $ cmds


mkCmdListText :: [Cmd] -> [T.Text]
mkCmdListText cmds = let zipped = zip (styleCmdAbbrevs cmds) [ cmdDesc cmd | cmd <- cmds ]
                     in [ padCmdName n <> d | (n, d) <- zipped, ()!# d ]


styleCmdAbbrevs :: [Cmd] -> [T.Text]
styleCmdAbbrevs cmds = let cmdNames       = [ cmdName           cmd | cmd <- cmds ]
                           cmdPAs         = [ cmdPriorityAbbrev cmd | cmd <- cmds ]
                           styledCmdNames = styleAbbrevs Don'tQuote cmdNames
                       in [ checkProrityAbbrev a | a <- zip3 cmdNames cmdPAs styledCmdNames ]
  where
    checkProrityAbbrev (_,  Nothing,  scn) = scn
    checkProrityAbbrev (cn, Just cpa, _  ) = colorWith abbrevColor cpa <> (fromJust . T.stripPrefix cpa $ cn)


-----


dispMatches :: ActionParams -> Int -> [T.Text] -> MudStack ()
dispMatches (LowerNub i mq cols needles) indent haystack = let (dropEmpties -> matches) = map grep needles in
    if ()# matches
      then wrapSend mq cols sorrySearch
      else pager i mq . concatMap (wrapIndent indent cols) . intercalate [""] $ matches
  where
    grep needle = let haystack' = [ (hay, hay') | hay <- haystack, let hay' = T.toLower . dropANSI $ hay ]
                  in [ fst match | match <- haystack', needle `T.isInfixOf` snd match ]
dispMatches p indent haystack = patternMatchFail "dispMatches" [ showText p, showText indent, showText haystack ]


-----


embedId :: Id -> T.Text
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
              g       = revealAdminNames ? (isAdminId embeddedId ms || ) :? id
              rebuild = quoteWith' (x, y)
          in mapM f is >>= concatMapM helper


breakIt :: T.Text -> (T.Text, T.Text)
breakIt = T.break (== plaIdDelimiter)


expandEmbeddedIdsToSings :: MudState -> T.Text -> T.Text
expandEmbeddedIdsToSings ms = helper
  where
    helper msg = case breakIt msg of
      (_, "")                                        -> msg
      (x, breakIt . T.tail -> (numTxt, T.tail -> y)) -> let embeddedId = read . T.unpack $ numTxt :: Int
                                                        in helper . quoteWith' (x, y) . getSing embeddedId $ ms


-----


fakeClientInput :: MsgQueue -> T.Text -> MudStack ()
fakeClientInput mq = liftIO . atomically . writeTQueue mq . FromClient . nl


-----


fileIOExHandler :: T.Text -> IOException -> MudStack ()
fileIOExHandler fn e = do
    logIOEx fn e
    let rethrow = throwToListenThread . toException $ e
    unless (any (e |&|) [ isAlreadyInUseError, isDoesNotExistError, isPermissionError ]) rethrow


throwToListenThread :: SomeException -> MudStack ()
throwToListenThread e = flip throwTo e . getListenThreadId =<< getState


-----


formatChanMsg :: T.Text -> T.Text -> T.Text -> T.Text
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


getAllChanIdNames :: Id -> MudState -> MudStack (IM.IntMap [(Id, T.Text)])
getAllChanIdNames i ms = let tunedChans = foldr helper [] . getPCChans i $ ms in
    IM.fromList . zipWith (\chan -> (chan^.chanId, )) tunedChans <$> forM tunedChans (flip (getChanIdNames i) ms)
  where
    helper chan acc = views chanConnTbl (M.! getSing i ms) chan ? (chan : acc) :? acc


getChanIdNames :: Id -> Chan -> MudState -> MudStack [(Id, T.Text)]
getChanIdNames i c ms = let (linkeds, nonLinkedIds) = getChanLinkeds_nonLinkedIds i c ms in
    sortBy (compare `on` snd) . (linkeds ++) . zip nonLinkedIds <$> mapM (updateRndmName i) nonLinkedIds


getChanLinkeds_nonLinkedIds :: Id -> Chan -> MudState -> ([(Id, Sing)], [Id])
getChanLinkeds_nonLinkedIds i c ms =
    let s                     = getSing i ms
        others                = views chanConnTbl (filter h . map g . filter f . M.toList) c
        f (s', isTuned)       = s' /= s && isTuned
        g (s', _      )       = (getIdForMobSing s' ms, s')
        h                     = (`isAwake` ms) . fst
        (linkeds, nonLinkeds) = partition (isLinked ms . (i, ) . fst) others
        nonLinkedIds          = map fst nonLinkeds
    in (linkeds, nonLinkedIds)


getChanStyleds :: Id -> Chan -> MudState -> MudStack [(Id, T.Text, T.Text)]
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


getLvl :: Id -> MudState -> Lvl
getLvl i ms = let myExp                            = getExp i ms
                  helper ((l, x):rest) | myExp < x = pred l
                                       | otherwise = helper rest
                  helper xs                        = patternMatchFail "getLvl" [ showText xs ]
              in helper calcLvlExps


getLvlExp :: Id -> MudState -> LvlExp
getLvlExp i = (getLvl i *** getExp i) . dup


-----


getPCChans :: Id -> MudState -> [Chan]
getPCChans i ms = views chanTbl (IM.foldr helper []) ms
  where
    helper chan acc = getSing i ms `elem` (chan^.chanConnTbl.to M.keys) ? (chan : acc) :? acc


-----


getQuestionStyleds :: Id -> MudState -> MudStack [(Id, T.Text, T.Text)]
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


getSexRaceLvl :: Id -> MudState -> (Sex, Race, Lvl)
getSexRaceLvl i ms | (s, r) <- getSexRace i ms = (s, r, getLvl i ms)


-----


getTunedQuestionIds :: Id -> MudState -> (Inv, Inv)
getTunedQuestionIds i ms = let pair = (getLoggedInPlaIds ms, getNonIncogLoggedInAdminIds ms)
                           in pair & both %~ filter (`isTunedQuestionId` ms) . (i `delete`)


-----


happy :: MudState -> [Either T.Text (T.Text, [EmoteWord], T.Text)] -> (T.Text, T.Text, [Id], [Broadcast])
happy ms xformed = let (toSelf, toTargets, toOthers) = unzip3 . rights $ xformed
                       targetIds = nub . foldr extractIds [] $ toTargets
                       extractIds [ForNonTargets _           ] acc = acc
                       extractIds (ForTarget     _ targetId:_) acc = targetId : acc
                       extractIds (ForTargetPoss _ targetId:_) acc = targetId : acc
                       extractIds xs                           _   = patternMatchFail "happy extractIds" [ showText xs ]
                       msgMap  = foldr (\targetId -> at targetId ?~ []) IM.empty targetIds
                       msgMap' = foldr consWord msgMap toTargets
                       consWord [ ForNonTargets word                           ] = IM.map (word :)
                       consWord [ ForTarget     p targetId, ForNonTargets word ] = selectiveCons p targetId False word
                       consWord [ ForTargetPoss p targetId, ForNonTargets word ] = selectiveCons p targetId True  word
                       consWord xs = const . patternMatchFail "happy consWord" $ [ showText xs ]
                       selectiveCons p targetId isPoss word = IM.mapWithKey helper
                         where
                           helper k v = let targetSing = getSing k ms |&| (isPoss ? (<> "'s") :? id)
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
hasEnc as = any (`elem` [ enc, enc's ]) as || last as == enc <> "."


-----


hasYou :: [T.Text] -> Bool
hasYou = any (`elem` yous) . map (T.dropAround (not . isLetter) . T.toLower)


-----


isAwake :: Id -> MudState -> Bool
isAwake = onPla (uncurry (&&) . (isLoggedIn *** not . isIncognito) . dup) True


-----


isHeDon't :: Char -> T.Text -> Bool
isHeDon't c msg = msg == T.singleton c <> "."


-----


isBracketed :: [T.Text] -> Bool
isBracketed ws = or [ (T.head . head $ ws) `elem` ("[<" :: String)
                    , "]." `T.isSuffixOf` last ws
                    , ">." `T.isSuffixOf` last ws ]


-----


isHostBanned :: T.Text -> IO Any
isHostBanned host = isBanned host <$> (getDbTblRecs "ban_host" :: IO [BanHostRec])


isBanned :: (BanRecord a) => T.Text -> [a] -> Any
isBanned target banRecs = helper . reverse $ banRecs
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
    noNpcs | uncurry (||) . (g *** g) $ ids = False
           | otherwise                      = otherwise
    g = (`isNpc` ms)


isDblLinked :: MudState -> (Id, Id) -> Bool
isDblLinked = helperIsLinked (&&)


-----


isPlaBanned :: Sing -> IO Any
isPlaBanned banSing = isBanned banSing <$> (getDbTblRecs "ban_pla" :: IO [BanPlaRec])


-----


locateHelper :: MudState -> [T.Text] -> Id -> (Id, T.Text)
locateHelper ms txts i = case getType i ms of
  RmType -> (i, commas txts)
  _      -> maybe oops (uncurry (locateHelper ms)) $ searchInvs `mplus` searchEqs
  where
    searchInvs = views invTbl (fmap (mkDescId "in"         ) . listToMaybe . IM.keys . IM.filter ( i `elem`)           ) ms
    searchEqs  = views eqTbl  (fmap (mkDescId "equipped by") . listToMaybe . IM.keys . IM.filter ((i `elem`) . M.elems)) ms
    mkDescId txt targetId = ((txts ++) . pure $ txt <> " " <> mkNameTypeIdDesc targetId ms, targetId)
    oops                  = blowUp "locateHelper" "ID is in limbo" [ showText i ]


-----


loggedInOut :: Bool -> T.Text
loggedInOut = loggedInOutHelper id


loggedInOutHelper :: (T.Text -> T.Text) -> Bool -> T.Text
loggedInOutHelper f = ("logged " <>) . f . inOut


inOut :: Bool -> T.Text
inOut True  = "in"
inOut False = "out"


loggedInOutColorize :: Bool -> T.Text
loggedInOutColorize True  = loggedInOutHelper (colorWith loggedInColor) True
loggedInOutColorize False = loggedInOutHelper id                        False


-----


mkInterfaceList :: IO T.Text
mkInterfaceList = NI.getNetworkInterfaces >>= \ns -> return . commas $ [ T.concat [ showText . NI.name $ n
                                                                                  , ": "
                                                                                  , showText . NI.ipv4 $ n ]
                                                                       | n <- ns ]


-----


mkNameTypeIdDesc :: Id -> MudState -> T.Text
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


mkChanReport :: Id -> MudState -> Chan -> [T.Text]
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


mkPossPro :: Sex -> T.Text
mkPossPro Male   = "his"
mkPossPro Female = "her"
mkPossPro NoSex  = "its"


-----


mkPrettifiedSexRaceLvl :: Id -> MudState -> (T.Text, T.Text, T.Text)
mkPrettifiedSexRaceLvl i ms = let (s, r, l) = getSexRaceLvl i ms
                              in (pp s, pp r, showText l)


-----


mkPros :: Sex -> (T.Text, T.Text, T.Text)
mkPros sexy = (mkThrPerPro, mkPossPro, mkReflexPro) & each %~ (sexy |&|)


-----


mkReflexPro :: Sex -> T.Text
mkReflexPro Male   = "himself"
mkReflexPro Female = "herself"
mkReflexPro NoSex  = "itself"


-----


mkRetainedMsgFromPerson :: Sing -> T.Text -> T.Text
mkRetainedMsgFromPerson s msg = fromPersonMarker `T.cons` (quoteWith "__" s <> " " <> msg)


-----


mkRightForNonTargets :: (T.Text, T.Text, T.Text) -> Either T.Text (T.Text, [EmoteWord], T.Text)
mkRightForNonTargets = Right . (_2 %~ (pure . ForNonTargets))


-----


mkSingleTarget :: MsgQueue -> Cols -> T.Text -> T.Text -> SingleTarget
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


mkThrPerPro :: Sex -> T.Text
mkThrPerPro Male   = "he"
mkThrPerPro Female = "she"
mkThrPerPro NoSex  = "it"


-----


mkWhoHeader :: [T.Text]
mkWhoHeader = T.concat [ padName "Name"
                       , padSex  "Sex"
                       , padRace "Race"
                       , "Level" ] : [ T.replicate (namePadding + sexPadding + racePadding + lvlPadding) "=" ]


-----


pager :: Id -> MsgQueue -> [T.Text] -> MudStack ()
pager i mq txt@(length -> txtLen) = getState >>= \ms -> let pl = getPageLines i ms in if txtLen + 3 <= pl
  then send mq . nl . T.unlines $ txt
  else let (page, rest) = splitAt (pl - 2) txt in do
      send mq . T.unlines $ page
      sendPagerPrompt mq (pl - 2) txtLen
      setInterp i . Just $ interpPager pl txtLen (page, rest)


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


tunedInOut :: Bool -> T.Text
tunedInOut = tunedInOutHelper id


tunedInOutHelper :: (T.Text -> T.Text) -> Bool -> T.Text
tunedInOutHelper f = ("tuned " <>) . f . inOut


tunedInOutColorize :: Bool -> T.Text
tunedInOutColorize True  = tunedInOutHelper (colorWith tunedInColor) True
tunedInOutColorize False = tunedInOutHelper id                       False


-----


unmsg :: [T.Text] -> [T.Text]
unmsg [cn        ] = [ T.init cn, ""            ]
unmsg [cn, target] = [ cn,        T.init target ]
unmsg xs           = patternMatchFail "unmsg" xs


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


withDbExHandler :: (Monoid a) => T.Text -> IO a -> MudStack (Maybe a)
withDbExHandler fn f = liftIO (Just <$> f) `catch` (emptied . dbExHandler fn)


withDbExHandler_ :: T.Text -> IO () -> MudStack ()
withDbExHandler_ fn f = liftIO f `catch` dbExHandler fn


-----


withoutArgs :: ActionFun -> ActionParams -> MudStack ()
withoutArgs act p = ignore p >> act p { args = [] }


ignore :: ActionFun
ignore (Ignoring mq cols as) = send mq . wrapUnlines cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" [ showText p ]
