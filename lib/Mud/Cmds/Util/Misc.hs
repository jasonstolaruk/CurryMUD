{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, TupleSections, ViewPatterns #-}

module Mud.Cmds.Util.Misc ( asterisk
                          , dbExHandler
                          , dispCmdList
                          , dispMatches
                          , embedId
                          , expandEmbeddedIds
                          , expandEmbeddedIdsToSings
                          , fileIOExHandler
                          , formatChanMsg
                          , formatQuestion
                          , getQuestionStyleds
                          , getTunedQuestionIds
                          , happy
                          , hasEnc
                          , hasYou
                          , inOut
                          , inOutOnOffs
                          , inOutOrOnOff
                          , isBracketed
                          , isDblLinked
                          , isHeDon't
                          , isHostBanned
                          , isLinked
                          , isPlaBanned
                          , isPunc
                          , loggedInOut
                          , loggedInOutColorize
                          , mkActionParams
                          , mkChanReport
                          , mkInterfaceList
                          , mkPossPro
                          , mkPrettifiedSexRaceLvl
                          , mkPros
                          , mkReflexPro
                          , mkRightForNonTargets
                          , mkSingleTarget
                          , mkThrPerPro
                          , mkWhoHeader
                          , pager
                          , plusRelated
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

import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Sorry
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Data.State.Util.Set
import Mud.Interp.Pager
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.LocPref
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Msgs
import Mud.TopLvlDefs.Padding
import Mud.Util.List hiding (headTail)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***), second)
import Control.Exception (IOException, SomeException, toException)
import Control.Exception.Lifted (catch, throwTo, try)
import Control.Lens (_1, _2, _3, at, both, each, view, views)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad ((>=>), unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit, isLetter)
import Data.Function (on)
import Data.List (delete, intercalate, nub, partition, sortBy)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..))
import qualified Data.IntMap.Lazy as IM (empty, foldlWithKey', map, mapWithKey)
import qualified Data.Map.Lazy as M (elems, lookup, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN module ("HLint: ignore Use ||"        :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Misc"


-----


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Cmds.Util.Misc"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Util.Misc"


-- ==================================================


asterisk :: T.Text
asterisk = asteriskColor <> "*" <> dfltColor


-----


dbExHandler :: T.Text -> SomeException -> MudStack ()
dbExHandler fn e = let msg = T.concat [ "exception caught during a database operation in "
                                      , dblQuote fn
                                      , "; rethrowing to listen thread" ]
                   in logExMsg "dbExHandler" msg e >> throwToListenThread e


-----


dispCmdList :: [Cmd] -> Action
dispCmdList cmds (NoArgs i mq cols) = pager i mq . concatMap (wrapIndent cmdNamePadding cols) . mkCmdListText $ cmds
dispCmdList cmds p                  = dispMatches p cmdNamePadding . mkCmdListText $ cmds


mkCmdListText :: [Cmd] -> [T.Text]
mkCmdListText cmds = let zipped = zip (styleCmdAbbrevs cmds) [ cmdDesc cmd | cmd <- cmds ]
                     in [ padCmdName n <> d | (n, d) <- zipped, ()!# d ]


styleCmdAbbrevs :: [Cmd] -> [T.Text]
styleCmdAbbrevs cmds = let cmdNames       = [ cmdName           cmd | cmd <- cmds ]
                           cmdPAs         = [ cmdPriorityAbbrev cmd | cmd <- cmds ]
                           styledCmdNames = styleAbbrevs Don'tBracket cmdNames
                       in [ checkProrityAbbrev a | a <- zip3 cmdNames cmdPAs styledCmdNames ]
  where
    checkProrityAbbrev (_,  Nothing,  scn) = scn
    checkProrityAbbrev (cn, Just cpa, _  ) = T.concat [ abbrevColor, cpa, dfltColor, fromJust . T.stripPrefix cpa $ cn ]


-----


dispMatches :: ActionParams -> Int -> [T.Text] -> MudStack ()
dispMatches (LowerNub i mq cols needles) indent haystack = let (dropEmpties -> matches) = map grep needles in
    if ()# matches
      then wrapSend mq cols "No matches found."
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
expandEmbeddedIds ms (ChanContext { revealAdminNames }) = concatMapM helper
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
            styleds = styleAbbrevs Don'tBracket . map snd $ combo
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


happy :: MudState -> [Either T.Text (T.Text, [EmoteWord], T.Text)] -> (T.Text, T.Text, [Id], [Broadcast])
happy ms xformed = let (toSelf, toTargets, toOthers) = unzip3 . map fromRight $ xformed
                       targetIds = nub . foldr extractIds [] $ toTargets
                       extractIds [ForNonTargets _           ] acc = acc
                       extractIds (ForTarget     _ targetId:_) acc = targetId : acc
                       extractIds (ForTargetPoss _ targetId:_) acc = targetId : acc
                       extractIds xs                           _   = patternMatchFail "happy extractIds" [ showText xs ]
                       msgMap  = foldr (\targetId -> at targetId .~ Just []) IM.empty targetIds
                       msgMap' = foldr consWord msgMap toTargets
                       consWord [ ForNonTargets word                           ] = IM.map (word :)
                       consWord [ ForTarget     p targetId, ForNonTargets word ] = selectiveCons p targetId False word
                       consWord [ ForTargetPoss p targetId, ForNonTargets word ] = selectiveCons p targetId True  word
                       consWord xs = const . patternMatchFail "happy consWord" $ [ showText xs ]
                       selectiveCons p targetId isPoss word = IM.mapWithKey helper
                         where
                           helper k v = let targetSing = getSing k ms |&| (isPoss ? (<> "'s") :? id)
                                        in (: v) $ if k == targetId
                                          then T.concat [ emoteTargetColor, targetSing, dfltColor, p ]
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


inOutOnOffs :: [(T.Text, Bool)]
inOutOnOffs = [ ("i",   True )
              , ("in",  True )
              , ("o",   False)
              , ("of",  False)
              , ("off", False)
              , ("on",  True )
              , ("ou",  False)
              , ("out", False) ]


-----


inOutOrOnOff :: T.Text
inOutOrOnOff = T.concat [ dblQuote "in"
                        , "/"
                        , dblQuote "out"
                        , " or "
                        , dblQuote "on"
                        , "/"
                        , dblQuote "off" ]


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
helperIsLinked f ms (i, i') = let s                = getSing i  ms
                                  s'               = getSing i' ms
                                  targetLinkedToMe = s' `elem` getLinked i  ms
                                  meLinkedToTarget = s  `elem` getLinked i' ms
                              in targetLinkedToMe `f` meLinkedToTarget


isDblLinked :: MudState -> (Id, Id) -> Bool
isDblLinked = helperIsLinked (&&)


-----


isPlaBanned :: Sing -> IO Any
isPlaBanned banSing = isBanned banSing <$> (getDbTblRecs "ban_pla" :: IO [BanPlaRec])


-----


loggedInOut :: Bool -> T.Text
loggedInOut = loggedInOutHelper id


loggedInOutHelper :: (T.Text -> T.Text) -> Bool -> T.Text
loggedInOutHelper f = ("logged " <>) . f . inOut


inOut :: Bool -> T.Text
inOut True  = "in"
inOut False = "out"


loggedInOutColorize :: Bool -> T.Text
loggedInOutColorize True  = loggedInOutHelper (quoteWith' (loggedInColor, dfltColor)) True
loggedInOutColorize False = loggedInOutHelper id                                      False


-----


mkInterfaceList :: IO T.Text
mkInterfaceList = NI.getNetworkInterfaces >>= \ns -> return . commas $ [ T.concat [ showText . NI.name $ n
                                                                                  , ": "
                                                                                  , showText . NI.ipv4 $ n ]
                                                                       | n <- ns ]


-----


mkActionParams :: Id -> MudState -> Args -> ActionParams
mkActionParams i ms as = ActionParams { plaId       = i
                                      , plaMsgQueue = getMsgQueue i ms
                                      , plaCols     = getColumns  i ms
                                      , args        = as }


-----


mkChanReport :: MudState -> Chan -> [T.Text]
mkChanReport ms (Chan ci cn cct) =
    let desc = commas . map descPla . f $ [ (s, t, l) | (s, t) <- M.toList cct
                                                      , let p = getPla (getIdForPCSing s ms) ms
                                                      , let l = isLoggedIn p && (not . isIncognito $ p) ]
    in [ T.concat [ parensQuote . showText $ ci, " ", dblQuote cn, ":" ], desc ]
  where
    descPla (s, t, l) = T.concat [ underline s, ": ", tunedInOutColorize t, " / ", loggedInOutColorize l ]
    f                 = sortBy (compare `on` view _1)


-----


mkPossPro :: Sex -> T.Text
mkPossPro Male   = "his"
mkPossPro Female = "her"
mkPossPro s      = patternMatchFail "mkPossPro" [ showText s ]


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
mkReflexPro s      = patternMatchFail "mkReflexPro" [ showText s ]


-----


mkRightForNonTargets :: (T.Text, T.Text, T.Text) -> Either T.Text (T.Text, [EmoteWord], T.Text)
mkRightForNonTargets = Right . mkForNonTargets
  where
    mkForNonTargets = _2 %~ (pure . ForNonTargets)


-----


mkSingleTarget :: MsgQueue -> Cols -> T.Text -> T.Text -> SingleTarget
mkSingleTarget mq cols target (sorryIgnoreLocPref -> sorryMsg) =
    SingleTarget { strippedTarget     = capitalize   t
                 , strippedTarget'    = uncapitalize t
                 , sendFun            = hlp ? (multiWrapSend mq cols . (sorryMsg :) . pure) :? wrapSend mq cols
                 , multiSendFun       = hlp ? (multiWrapSend mq cols . (sorryMsg :)       ) :? multiWrapSend mq cols
                 , consSorry          = hlp ? (sorryMsg :)                                  :? id
                 , consSorryBroadcast = hlp ? f                                             :? const id }
  where
    hlp = hasLocPref . uncapitalize $ target
    t   = hlp ? (T.tail . T.tail $ target) :? target
    f i = ((sorryMsg, pure i) :)


-----


mkThrPerPro :: Sex -> T.Text
mkThrPerPro Male   = "he"
mkThrPerPro Female = "she"
mkThrPerPro s      = patternMatchFail "mkThrPerPro" [ showText s ]


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


plusRelated :: T.Text -> T.Text
plusRelated = (<> ".") . (<> parensQuote "plus related functionality") . (<> " ")


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
tunedInOutColorize True  = tunedInOutHelper (quoteWith' (tunedInColor, dfltColor)) True
tunedInOutColorize False = tunedInOutHelper id                                     False


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
                                         ms'       = ms & rndmNamesMstrTbl.ind i.at targetSing .~ Just rndmName'
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


withoutArgs :: Action -> ActionParams -> MudStack ()
withoutArgs act p = ignore p >> act p { args = [] }


ignore :: Action
ignore (Ignoring mq cols as) = send mq . wrapUnlines cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" [ showText p ]
