{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Cmds.Util.Misc ( adviceEnc
                          , adviceEtc
                          , adviceEtcEmptyPoss
                          , adviceEtcHead
                          , advise
                          , asterisk
                          , dbExHandler
                          , dispCmdList
                          , dispMatches
                          , fileIOExHandler
                          , inOutOnOffs
                          , isHostBanned
                          , isPlaBanned
                          , mkActionParams
                          , mkInterfaceList
                          , mkPossPro
                          , mkPrettifiedSexRaceLvl
                          , mkPros
                          , mkReflexPro
                          , mkSingleTarget
                          , mkThrPerPro
                          , mkWhoHeader
                          , pager
                          , prefixCmd
                          , sendGenericErrorMsg
                          , sorryDbEx
                          , sorryIgnoreLocPref
                          , sorryIgnoreLocPrefPlur
                          , throwToListenThread
                          , updateRndmName
                          , withDbExHandler
                          , withDbExHandler_
                          , withoutArgs ) where

import Mud.Cmds.Util.Abbrev
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
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
import Mud.Util.List
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Exception (IOException, SomeException, toException)
import Control.Exception.Lifted (catch, throwTo, try)
import Control.Lens (at, each)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad ((>=>), unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.List (intercalate, sortBy)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Any(..))
import qualified Data.Map.Lazy as M (elems, lookup)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Misc"


-----


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Cmds.Util.Misc"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Util.Misc"


-- ==================================================


enc, etc :: T.Text
enc = T.singleton emoteNameChar
etc = T.singleton emoteTargetChar


adviceEnc :: T.Text -> T.Text
adviceEnc cn = T.concat [ dblQuote enc
                        , " must either be used alone, or with a "
                        , dblQuote "'s"
                        , " suffix "
                        , parensQuote "to create a possessive noun"
                        , ", as in "
                        , quoteColor
                        , dblQuote . T.concat $ [ cn
                                                , "shielding her eyes from the sun, "
                                                , enc
                                                , " looks out across the plains" ]
                        , dfltColor
                        , ", or "
                        , quoteColor
                        , dblQuote $ cn <> enc <> "'s leg twitches involuntarily as she laughs with gusto"
                        , dfltColor
                        , "." ]


adviceEtc :: T.Text -> T.Text
adviceEtc cn = T.concat [ dblQuote etc
                        , " must be immediately followed by the name of the person you wish to target, as in "
                        , quoteColor
                        , dblQuote . T.concat $ [ cn
                                                , "slowly turns her head to look directly at "
                                                , etc
                                                , "taro" ]
                        , dfltColor
                        , ". To create a possessive noun, append "
                        , dblQuote "'s"
                        , " to the target name, as in "
                        , quoteColor
                        , dblQuote . T.concat $ [ cn
                                                , "places her hand firmly on "
                                                , etc
                                                , "taro's shoulder" ]
                        , dfltColor
                        , "." ]


adviceEtcEmptyPoss :: T.Text
adviceEtcEmptyPoss = T.concat [ "You must specify the name of the person you want to target between "
                              , dblQuote etc
                              , " and "
                              , dblQuote "'s"
                              , "." ]


adviceEtcHead :: T.Text
adviceEtcHead = "You can't begin an emote with a target."


-----


advise :: ActionParams -> [HelpName] -> T.Text -> MudStack ()
advise (Advising mq cols) []  msg = wrapSend mq cols msg
advise (Advising mq cols) [h] msg = multiWrapSend mq cols [ msg, T.concat [ "For more information, type "
                                                          , quoteColor
                                                          , dblQuote $ "help " <> h
                                                          , dfltColor
                                                          , "." ] ]
advise (Advising mq cols) (dblQuote . T.intercalate (dblQuote ", ") -> helpTopics) msg =
    multiWrapSend mq cols [ msg, "For more information, see the following help articles: " <> helpTopics <> "." ]
advise p hs msg = patternMatchFail "advise" [ showText p, showText hs, msg ]


-----


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


fileIOExHandler :: T.Text -> IOException -> MudStack ()
fileIOExHandler fn e = do
    logIOEx fn e
    let rethrow = throwToListenThread . toException $ e
    unless (any (e |&|) [ isAlreadyInUseError, isDoesNotExistError, isPermissionError ]) rethrow


throwToListenThread :: SomeException -> MudStack ()
throwToListenThread e = flip throwTo e . getListenThreadId =<< getState


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


isHostBanned :: T.Text -> IO Any
isHostBanned host = isBanned host <$> (getDbTblRecs "ban_host" :: IO [BanHostRec])


isBanned :: (BanRecord a) => T.Text -> [a] -> Any
isBanned target banRecs = helper . reverse $ banRecs
  where
    helper [] = Any False
    helper (x:xs) | recTarget x == target = Any . recIsBanned $ x
                  | otherwise             = helper xs


-----


isPlaBanned :: Sing -> IO Any
isPlaBanned banSing = isBanned banSing <$> (getDbTblRecs "ban_pla" :: IO [BanPlaRec])


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


mkSingleTarget :: MsgQueue -> Cols -> T.Text -> T.Text -> SingleTarget
mkSingleTarget mq cols target (sorryIgnoreLocPref -> sorryMsg) =
    SingleTarget { strippedTarget     = capitalize   t
                 , strippedTarget'    = uncapitalize t
                 , sendFun            = hlp ? (multiWrapSend mq cols . (sorryMsg :) . pure) :? wrapSend mq cols
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


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd (T.singleton -> prefix) cn = prefix <> cn


-----


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = wrapSend mq cols genericErrorMsg


-----


sorryDbEx :: MsgQueue -> Cols -> MudStack ()
sorryDbEx mq cols = wrapSend mq cols "There was an error when reading the database."


-----


sorryIgnoreLocPref :: T.Text -> T.Text
sorryIgnoreLocPref msg = parensQuote $ msg <> " need not be given a location prefix. The location prefix you provided \
                                              \will be ignored."


sorryIgnoreLocPrefPlur :: T.Text -> T.Text
sorryIgnoreLocPrefPlur msg = parensQuote $ msg <> " need not be given location prefixes. The location prefixes you \
                                                  \provided will be ignored."


-----


updateRndmName :: Id -> Id -> MudStack Sing
updateRndmName i targetId = do
    rndmNames <- T.lines <$> readRndmNames
    rndmName  <- ()# rndmNames ? return "curry" :? rndmElem rndmNames
    let helper ms = let targetSing = getSing targetId ms
                        rnt        = getRndmNamesTbl i ms
                        notFound   = let existing = M.elems rnt
                                         checkLength n | T.length n > maxNameLen = mkUniqueName "curry" existing
                                                       | otherwise               = n
                                         rndmName' = checkLength . mkUniqueName rndmName $ existing
                                     in ( ms & rndmNamesMstrTbl.ind i.at targetSing .~ Just rndmName'
                                        , rndmName' )
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
