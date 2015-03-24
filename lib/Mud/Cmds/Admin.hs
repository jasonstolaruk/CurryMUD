{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, TransformListComp, TupleSections, ViewPatterns #-}

module Mud.Cmds.Admin (adminCmds) where

import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Msgs
import Mud.Util.List (headLast)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, massLogPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, over, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (^.))
import Control.Monad ((>=>), forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import GHC.Exts (sortWith)
import Prelude hiding (pi)
import System.Directory (doesFileExist)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import qualified Data.IntMap.Lazy as IM (filter, keys)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, readFile)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Admin"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Admin"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Admin"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Admin"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Admin"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Admin"


massLogPla :: T.Text -> T.Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds.Admin"


-- ==================================================


adminCmds :: [Cmd]
adminCmds =
    [ mkAdminCmd "?"         adminDispCmdList "Display or search this command list."
    , mkAdminCmd "announce"  adminAnnounce    "Send a message to all players."
    , mkAdminCmd "boot"      adminBoot        "Boot a player, optionally with a custom message."
    , mkAdminCmd "bug"       adminBug         "Dump the bug log."
    , mkAdminCmd "date"      adminDate        "Display the current system date."
    , mkAdminCmd "peep"      adminPeep        "Start or stop peeping one or more players."
    , mkAdminCmd "print"     adminPrint       "Print a message to the server console."
    , mkAdminCmd "profanity" adminProfanity   "Dump the profanity log."
    , mkAdminCmd "shutdown"  adminShutdown    "Shut down CurryMUD, optionally with a custom message."
    , mkAdminCmd "tell"      adminTell        "Send a message to a player."
    , mkAdminCmd "time"      adminTime        "Display the current system time."
    , mkAdminCmd "typo"      adminTypo        "Dump the typo log."
    , mkAdminCmd "uptime"    adminUptime      "Display the system uptime."
    , mkAdminCmd "who"       adminWho         "Display or search a list of the players who are currently connected." ]


mkAdminCmd :: T.Text -> Action -> CmdDesc -> Cmd
mkAdminCmd (prefixAdminCmd -> cn) act cd = Cmd { cmdName           = cn
                                               , cmdPriorityAbbrev = Nothing
                                               , cmdFullName       = cn
                                               , action            = act
                                               , cmdDesc           = cd }


prefixAdminCmd :: T.Text -> CmdName
prefixAdminCmd = prefixCmd adminCmdChar


-----


adminAnnounce :: Action
adminAnnounce p@AdviseNoArgs = advise p [ prefixAdminCmd "announce" ] advice
  where
    advice = T.concat [ "You must provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "announce" <> " CurryMUD will be shutting down for maintenance in 30 \
                                                  \minutes"
                      , dfltColor
                      , "." ]
adminAnnounce (Msg i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    ok mq
    massSend $ announceColor <> msg <> dfltColor
    logPla    "adminAnnounce" i $       "announced "  <> dblQuote msg
    logNotice "adminAnnounce"   $ s <> " announced, " <> dblQuote msg
adminAnnounce p = patternMatchFail "adminAnnounce" [ showText p ]


-----


adminBoot :: Action
adminBoot p@AdviseNoArgs = advise p [ prefixAdminCmd "boot" ] "Please specify the full PC name of the player you wish \
                                                              \to boot, followed optionally by a custom message."
adminBoot (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == target ] of
      []       -> wrapSend mq cols $ "No PC by the name of " <> dblQuote target <> " is currently connected. (Note \
                                     \that you must specify the full PC name of the player you wish to boot.)"
      [bootId] -> let selfSing = getSing i ms in if selfSing == target
                    then wrapSend mq cols "You can't boot yourself."
                    else let bootMq = getMsgQueue bootId ms
                             f      = T.null msg ? dfltMsg :? customMsg
                         in ok mq >> (sendMsgBoot bootMq =<< f bootId selfSing)
      xs       -> patternMatchFail "adminBoot" [ showText xs ]
  where
    dfltMsg   bootId s = emptied $ do
        logPla "adminBoot dfltMsg"   i      $ T.concat [ "booted ", target, " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   bootId $ T.concat [ "booted by ", s,   " ", parensQuote "no message given", "." ]
    customMsg bootId s = do
        logPla "adminBoot customMsg" i      $ T.concat [ "booted ", target, "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" bootId $ T.concat [ "booted by ", s,   "; message: ", dblQuote msg ]
        return . Just $ msg
adminBoot p = patternMatchFail "adminBoot" [ showText p ]


-----


adminBug :: Action
adminBug (NoArgs i mq cols) =
    dumpLog mq cols bugLogFile ("bug", "bugs") >> logPlaExec (prefixAdminCmd "bug") i
adminBug p = withoutArgs adminBug p


dumpLog :: MsgQueue -> Cols -> FilePath -> BothGramNos -> MudStack ()
dumpLog mq cols logFile (s, p) = send mq =<< helper
  where
    helper  = liftIO readLog |$| try >=> eitherRet handler
    readLog = mIf (doesFileExist logFile)
                  (return . multiWrapNl   cols . T.lines =<< T.readFile logFile)
                  (return . wrapUnlinesNl cols $ "No " <> p <> " have been logged.")
    handler e = do
        fileIOExHandler "dumpLog" e
        return . wrapUnlinesNl cols $ "Unfortunately, the " <> s <> " log could not be retrieved."


-----


adminDate :: Action
adminDate (NoArgs' i mq) = do
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
    logPlaExec (prefixAdminCmd "date") i
adminDate p = withoutArgs adminDate p


-----


adminDispCmdList :: Action
adminDispCmdList p@(LowerNub' i as) = dispCmdList adminCmds p >> logPlaExecArgs (prefixAdminCmd "?") as i
adminDispCmdList p                  = patternMatchFail "adminDispCmdList" [ showText p ]


-----


adminPeep :: Action
adminPeep p@AdviseNoArgs = advise p [ prefixAdminCmd "peep" ] "Please specify one or more PC names of the player(s) \
                                                              \you wish to start or stop peeping."
adminPeep (LowerNub i mq cols (map capitalize -> as)) = do
    (msgs, unzip -> (logMsgsSelf, logMsgsOthers)) <- modifyState helper
    multiWrapSend mq cols msgs
    logPla "adminPeep" i . (<> ".") . T.intercalate " / " $ logMsgsSelf
    forM_ logMsgsOthers $ uncurry (logPla "adminPeep")
  where
    helper ms = let (pt, msgs, logMsgs) = foldr (peep (getSing i ms) (mkPlaIdSingList ms)) (ms^.plaTbl, [], []) as
                in (ms & plaTbl .~ pt, (msgs, logMsgs))
    peep s plaIdSings target a@(pt, _, _) =
        let notFound = over _2 (sorry :) a
            sorry    = "No player by the name of " <> dblQuote target <> " is currently connected."
            found (peepId, peepSing) = if peepId `notElem` pt^.ind i.peeping
              then let pt'     = pt & ind i     .peeping %~ (peepId :)
                                    & ind peepId.peepers %~ (i      :)
                       msg     = "You are now peeping " <> peepSing <> "."
                       logMsgs = [("started peeping " <> peepSing, (peepId, s <> " started peeping."))]
                   in a & _1 .~ pt' & _2 %~ (msg :) & _3 <>~ logMsgs
              else let pt'     = pt & ind i     .peeping %~ (peepId `delete`)
                                    & ind peepId.peepers %~ (i      `delete`)
                       msg     = "You are no longer peeping " <> peepSing <> "."
                       logMsgs = [("stopped peeping " <> peepSing, (peepId, s <> " stopped peeping."))]
                   in a & _1 .~ pt' & _2 %~ (msg :) & _3 <>~ logMsgs
        in maybe notFound found . findFullNameForAbbrev target $ plaIdSings
adminPeep p = patternMatchFail "adminPeep" [ showText p ]


-----


adminPrint :: Action
adminPrint p@AdviseNoArgs = advise p [ prefixAdminCmd "print" ] advice
  where
    advice = T.concat [ "You must provide a message to print to the server console, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "print" <> " Is anybody home?"
                      , dfltColor
                      , "." ]
adminPrint (Msg i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    liftIO . T.putStrLn . T.concat $ [ bracketQuote s, " ", printConsoleColor, msg, dfltColor ]
    ok mq
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
adminPrint p = patternMatchFail "adminPrint" [ showText p ]


-----


adminProfanity :: Action
adminProfanity (NoArgs i mq cols) =
    dumpLog mq cols profanityLogFile ("profanity", "profanities") >> logPlaExec (prefixAdminCmd "profanity") i
adminProfanity p = withoutArgs adminProfanity p


-----


adminShutdown :: Action
adminShutdown (NoArgs' i mq) = shutdownHelper i mq Nothing
adminShutdown (Msg i mq msg) = shutdownHelper i mq . Just $ msg
adminShutdown p              = patternMatchFail "adminShutdown" [ showText p ]


shutdownHelper :: Id -> MsgQueue -> Maybe T.Text -> MudStack ()
shutdownHelper i mq maybeMsg = getState >>= \ms ->
    let s    = getSing i ms
        rest = maybe (" " <> parensQuote "no message given" <> ".") (("; message: " <>) . dblQuote) maybeMsg
    in do
        massSend $ shutdownMsgColor <> fromMaybe dfltShutdownMsg maybeMsg <> dfltColor
        logPla     "adminShutdown" i $ "initiating shutdown" <> rest
        massLogPla "adminShutdown"   $ "closing connection due to server shutdown initiated by " <> s <> rest
        logNotice  "adminShutdown"   $ "server shutdown initiated by "                           <> s <> rest
        liftIO . atomically . writeTQueue mq $ Shutdown


-----


adminTell :: Action
adminTell p@AdviseNoArgs = advise p [ prefixAdminCmd "tell" ] advice
  where
    advice = T.concat [ "Please specify the PC name of a player followed by a message, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "tell" <> " taro thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminTell p@(AdviseOneArg a) = advise p [ prefixAdminCmd "tell" ] advice
  where
    advice = T.concat [ "Please also provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "tell " <> a <> " thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminTell (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
    unless (null logMsgs) . forM_ logMsgs . uncurry $ logPla (prefixAdminCmd "tell")
  where
    helper ms =
        let s          = getSing i ms
            plaIdSings = mkPlaIdSingList ms
            notFound   = emptied . wrapSend mq cols $ "No player with the PC name of " <> dblQuote target <> " is \
                                                      \currently logged in."
            found (tellId, tellSing)
              | tellMq         <- getMsgQueue tellId ms
              , tellPla        <- getPla      tellId ms
              , tellCols       <- tellPla^.columns
              , sentLogMsg     <- (i,      T.concat [ "sent message to ", tellSing, ": ", dblQuote msg ])
              , receivedLogMsg <- (tellId, T.concat [ "received message from ", s, ": ",  dblQuote msg ]) = do
                  wrapSend mq cols . T.concat $ [ "You send ", tellSing, ": ", dblQuote msg ]
                  let targetMsg = T.concat [ bracketQuote s, " ", adminTellColor, msg, dfltColor ]
                  if getPlaFlag IsNotFirstAdminTell tellPla
                    then wrapSend tellMq tellCols targetMsg
                    else multiWrapSend tellMq tellCols =<< [ targetMsg : msgs | msgs <- firstAdminTell tellId s ]
                  return [ sentLogMsg, receivedLogMsg ]
        in maybe notFound found . findFullNameForAbbrev target $ plaIdSings
adminTell p = patternMatchFail "adminTell" [ showText p ]


firstAdminTell :: Id -> Sing -> MudStack [T.Text]
firstAdminTell tellId adminSing = modifyState $ (, msg) . (plaTbl.ind tellId %~ setPlaFlag IsNotFirstAdminTell True)
  where
    msg = [ T.concat [ hintANSI
                     , "Hint:"
                     , noHintANSI
                     , " the above is a message from "
                     , adminSing
                     , ", a CurryMUD administrator. To reply, type "
                     , quoteColor
                     , dblQuote $ "admin " <> adminSing <> " msg"
                     , dfltColor
                     , ", where "
                     , quoteColor
                     , dblQuote "msg"
                     , dfltColor
                     , " is the message you want to send to "
                     , adminSing
                     , "." ] ]


-----


adminTime :: Action
adminTime (NoArgs i mq cols) = do
    (ct, zt) <- liftIO $ (,) <$> formatThat `fmap` getCurrentTime <*> formatThat `fmap` getZonedTime
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
    logPlaExec (prefixAdminCmd "time") i
  where
    formatThat (T.words . showText -> wordy@(headLast -> (date, zone))) =
        let time = T.init . T.dropWhileEnd (/= '.') . head . tail $ wordy in T.concat [ zone, ": ", date, " ", time ]
adminTime p = withoutArgs adminTime p


-----


adminTypo :: Action
adminTypo (NoArgs i mq cols) = dumpLog mq cols typoLogFile ("typo", "typos") >> logPlaExec (prefixAdminCmd "typo") i
adminTypo p                  = withoutArgs adminTypo p


-----


adminUptime :: Action
adminUptime (NoArgs i mq cols) = do
    send mq . nl =<< liftIO uptime |$| try >=> eitherRet ((sendGenericErrorMsg mq cols >>) . logIOEx "adminUptime")
    logPlaExec (prefixAdminCmd "uptime") i
  where
    uptime = T.pack <$> readProcess "uptime" [] ""
adminUptime p = withoutArgs adminUptime p


-----


adminWho :: Action
adminWho (NoArgs i mq cols) = do
    pager i mq =<< [ concatMap (wrapIndent 20 cols) plaListTxt | plaListTxt <- mkPlaListTxt <$> getState ]
    logPlaExecArgs (prefixAdminCmd "who") [] i
adminWho p@(ActionParams { plaId, args }) =
    (dispMatches p 20 =<< mkPlaListTxt <$> getState) >> logPlaExecArgs (prefixAdminCmd "who") args plaId


mkPlaListTxt :: MudState -> [T.Text]
mkPlaListTxt ms = let is              = IM.keys . IM.filter (not . getPlaFlag IsAdmin) $ ms^.plaTbl
                      (is', ss)       = unzip [ (i, s) | i <- is, let s = getSing i ms, then sortWith by s ]
                      ias             = zip is' . styleAbbrevs Don'tBracket $ ss
                      mkPlaTxt (i, a) = let (pp *** pp -> (s, r)) = getSexRace i ms
                                        in T.concat [ pad 13 a, padOrTrunc 7 s, padOrTrunc 10 r ]
                  in map mkPlaTxt ias ++ [ mkNumOfPlayersTxt is <> " connected." ]
  where
    mkNumOfPlayersTxt (length -> nop) | nop == 1  = "1 player"
                                      | otherwise = showText nop <> " players"
