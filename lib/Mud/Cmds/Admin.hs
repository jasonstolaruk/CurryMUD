{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, TransformListComp, ViewPatterns #-}

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
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, at, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.List (delete)
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
    logNotice "adminAnnounce"   $ s <> " announced, " <> dblQuote msg
    logPla    "adminAnnounce" i $       "announced "  <> dblQuote msg
    ok mq
    massSend $ announceColor <> msg <> dfltColor
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
                             f      = T.null msg ? dlfMsg :? customMsg
                         in ok mq >> (sendMsgBoot bootMq =<< f bootId selfSing)
      xs       -> patternMatchFail "adminBoot" [ showText xs ]
  where
    dfltMsg   bootId s = do
        logPla "adminBoot dfltMsg"   i      $ T.concat [ "booted ", target, " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   bootId $ T.concat [ "booted by ", s,   " ", parensQuote "no message given", "." ]
        return Nothing
    customMsg bootId s = do
        logPla "adminBoot customMsg" i      $ T.concat [ "booted ", target, "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" bootId $ T.concat [ "booted by ", s,   "; message: ", dblQuote msg ]
        return . Just $ msg
adminBoot p = patternMatchFail "adminBoot" [ showText p ]


-----


adminBug :: Action
adminBug (NoArgs i mq cols) =
    logPlaExec (prefixAdminCmd "bug") i >> dumpLog mq cols bugLogFile ("bug", "bugs")
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
    logPlaExec (prefixAdminCmd "date") i
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
adminDate p = withoutArgs adminDate p


-----


adminDispCmdList :: Action
adminDispCmdList p@(LowerNub' i as) = logPlaExecArgs (prefixAdminCmd "?") as i >> dispCmdList adminCmds p
adminDispCmdList p                  = patternMatchFail "adminDispCmdList" [ showText p ]


-----


adminPeep :: Action
adminPeep p@AdviseNoArgs = advise p [ prefixAdminCmd "peep" ] "Please specify one or more PC names of the player(s) \
                                                              \you wish to start or stop peeping."
adminPeep (LowerNub i mq cols (map capitalize -> as)) = ask >>= \md -> do
    (msgs, logMsgs) <- liftIO . atomically . helperSTM $ md
    multiWrapSend mq cols msgs
    let (logMsgsSelf, logMsgsOthers) = unzip logMsgs
    logPla "adminPeep" i . (<> ".") . T.intercalate " / " $ logMsgsSelf
    forM_ logMsgsOthers $ uncurry (logPla "adminPeep")
  where
    helperSTM md = do
        (et, pt) <- (,) <$> readTVar (md^.entTblTVar) <*> readTVar (md^.plaTblTVar)
        let s                    = (et ! i)^.sing
            piss                 = mkPlaIdsSingsList et pt
            (pt', msgs, logMsgs) = foldr (peep s piss) (pt, [], []) as
        writeTVar (md^.plaTblTVar) pt'
        return (msgs, logMsgs)
    peep s piss target a@(pt, _, _) =
        let notFound    = over _2 (sorry :) a
            sorry       = "No player by the name of " <> dblQuote target <> " is currently connected."
            found match | (peepI, peepS) <- head . filter ((== match) . snd) $ piss
                        , thePeeper      <- pt ! i
                        , thePeeped      <- pt ! peepI = if peepI `notElem` thePeeper^.peeping
                          then let pt'     = pt & at i     ?~ over peeping (peepI :) thePeeper
                                                & at peepI ?~ over peepers (i     :) thePeeped
                                   msg     = "You are now peeping " <> peepS <> "."
                                   logMsgs = [("started peeping " <> peepS, (peepI, s <> " started peeping."))]
                               in a & _1 .~ pt' & over _2 (msg :) & _3 <>~ logMsgs
                          else let pt'     = pt & at i     ?~ over peeping (peepI `delete`) thePeeper
                                                & at peepI ?~ over peepers (i     `delete`) thePeeped
                                   msg     = "You are no longer peeping " <> peepS <> "."
                                   logMsgs = [("stopped peeping " <> peepS, (peepI, s <> " stopped peeping."))]
                               in a & _1 .~ pt' & over _2 (msg :) & _3 <>~ logMsgs
        in maybe notFound found . findFullNameForAbbrev target . map snd $ piss
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
adminPrint (Msg i mq msg) = ask >>= \md -> view sing . (! i) <$> (liftIO . readTVarIO $ md^.entTblTVar) >>= \s -> do
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
    liftIO . T.putStrLn . T.concat $ [ bracketQuote s, " ", printConsoleColor, msg, dfltColor ]
    ok mq
adminPrint p = patternMatchFail "adminPrint" [ showText p ]


-----


adminProfanity :: Action
adminProfanity (NoArgs i mq cols) =
    logPlaExec (prefixAdminCmd "profanity") i >> dumpLog mq cols profanityLogFile ("profanity", "profanities")
adminProfanity p = withoutArgs adminProfanity p


-----


adminShutdown :: Action
adminShutdown (NoArgs' i mq) = ask >>= \md -> view sing . (! i) <$> (liftIO . readTVarIO $ md^.entTblTVar) >>= \s -> do
    logPla "adminShutdown" i $ "initiating shutdown " <> parensQuote "no message given" <> "."
    massSend $ shutdownMsgColor <> dfltShutdownMsg <> dfltColor
    massLogPla "adminShutdown" $ T.concat [ "closing connection due to server shutdown initiated by "
                                          , s
                                          , " "
                                          , parensQuote "no message given"
                                          , "." ]
    logNotice  "adminShutdown" $ T.concat [ "server shutdown initiated by "
                                          , s
                                          , " "
                                          , parensQuote "no message given"
                                          , "." ]
    liftIO . atomically . writeTQueue mq $ Shutdown
adminShutdown (Msg i mq msg) = ask >>= \md -> view sing . (! i) <$> (liftIO . readTVarIO $ md^.entTblTVar) >>= \s -> do
    logPla "adminShutdown" i $ "initiating shutdown; message: " <> dblQuote msg
    massSend $ shutdownMsgColor <> msg <> dfltColor
    massLogPla "adminShutdown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                            , s
                                            , "; message: "
                                            , dblQuote msg ]
    logNotice  "adminShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; message: ", dblQuote msg ]
    liftIO . atomically . writeTQueue mq $ Shutdown
adminShutdown p = patternMatchFail "adminShutdown" [ showText p ]


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
adminTell (MsgWithTarget i mq cols target msg) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    let f = uncurry $ logPla (prefixAdminCmd "tell") in unless (null logMsgs) $ mapM_ f logMsgs
  where
    helperSTM md = (,,) <$> readTVar (md^.entTblTVar)
                        <*> readTVar (md^.msgQueueTblTVar)
                        <*> readTVar (md^.plaTblTVar) >>= \(et, mqt, pt) ->
        let s        = (et ! i)^.sing
            piss     = mkPlaIdsSingsList et pt
            notFound = do
                wrapSendSTM mq cols $ "No player with the PC name of " <> dblQuote target <> " is currently logged in."
                return []
            found match | (tellI, tellS) <- head . filter ((== match) . snd) $ piss
                        , tellMq         <- mqt ! tellI
                        , p              <- pt  ! tellI
                        , tellCols       <- p^.columns
                        , sentLogMsg     <- (i, T.concat [ "sent message to ", tellS, ": ", dblQuote msg ])
                        , receivedLogMsg <- (tellI, T.concat [ "received message from ", s, ": ", dblQuote msg ]) = do
                            wrapSendSTM mq cols . T.concat $ [ "You send ", tellS, ": ", dblQuote msg ]
                            let targetMsg = T.concat [ bracketQuote s, " ", adminTellColor, msg, dfltColor ]
                            if getPlaFlag IsNotFirstAdminTell p
                              then wrapSendSTM tellMq tellCols targetMsg
                              else multiWrapSendSTM tellMq tellCols . (targetMsg :) =<< firstAdminTellSTM tellI md pt p s
                            return [ sentLogMsg, receivedLogMsg ]
        in maybe notFound found . findFullNameForAbbrev target . map snd $ piss
adminTell p = patternMatchFail "adminTell" [ showText p ]


firstAdminTellSTM :: Id -> MudData -> PlaTbl -> Pla -> Sing -> STM [T.Text]
firstAdminTellSTM i md pt (setPlaFlag IsNotFirstAdminTell True -> p) s = do
    writeTVar (md^.plaTblTVar) (pt & at i ?~ p)
    return [ T.concat [ hintANSI
                      , "Hint:"
                      , noHintANSI
                      , " the above is a message from "
                      , s
                      , ", a CurryMUD administrator. To reply, type "
                      , quoteColor
                      , dblQuote $ "admin " <> s <> " msg"
                      , dfltColor
                      , ", where "
                      , quoteColor
                      , dblQuote "msg"
                      , dfltColor
                      , " is the message you want to send to "
                      , s
                      , "." ] ]


-----


adminTime :: Action
adminTime (NoArgs i mq cols) = do
    logPlaExec (prefixAdminCmd "time") i
    (ct, zt) <- liftIO $ (,) <$> formatThat `fmap` getCurrentTime <*> formatThat `fmap` getZonedTime
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
  where
    formatThat (T.words . showText -> wordy@(headLast -> (date, zone)))
      | time <- T.init . T.dropWhileEnd (/= '.') . head . tail $ wordy = T.concat [ zone, ": ", date, " ", time ]
adminTime p = withoutArgs adminTime p


-----


adminTypo :: Action
adminTypo (NoArgs i mq cols) = logPlaExec (prefixAdminCmd "typo") i >> dumpLog mq cols typoLogFile ("typo", "typos")
adminTypo p                  = withoutArgs adminTypo p


-----


adminUptime :: Action
adminUptime (NoArgs i mq cols) = do
    logPlaExec (prefixAdminCmd "uptime") i
    send mq . nl =<< liftIO uptime |$| try >=> eitherRet (\e -> logIOEx "adminUptime" e >> sendGenericErrorMsg mq cols)
  where
    uptime = T.pack <$> readProcess "uptime" [] ""
adminUptime p = withoutArgs adminUptime p


-----


adminWho :: Action
adminWho (NoArgs i mq cols) = do
    logPlaExecArgs (prefixAdminCmd "who") [] i
    pager i mq . concatMap (wrapIndent 20 cols) =<< mkPlaListTxt
adminWho p@(ActionParams { plaId, args }) = do
    logPlaExecArgs (prefixAdminCmd "who") args plaId
    dispMatches p 20 =<< mkPlaListTxt


mkPlaListTxt :: MudStack [T.Text]
mkPlaListTxt = ask >>= \md -> md |$| (liftIO . atomically . helperSTM) >=> \(et, mt, pcTbl, plaTbl) ->
    let pis              = IM.keys . IM.filter (not . getPlaFlag IsAdmin) $ plaTbl
        (pis', pss)      = unzip [ (pi, s) | pi <- pis, let s = (et ! pi)^.sing, then sortWith by s ]
        pias             = zip pis' . styleAbbrevs Don'tBracket $ pss
        mkPlaTxt (pi, a) = let ((pp *** pp) -> (s, r)) = getSexRace pi mt pcTbl
                           in T.concat [ pad 13 a, padOrTrunc 7 s, padOrTrunc 10 r ]
    in return $ map mkPlaTxt pias ++ [ mkNumOfPlayersTxt pis <> " connected." ]
  where
    helperSTM md = (,,,) <$> readTVar (md^.entTblTVar)
                         <*> readTVar (md^.mobTblTVar)
                         <*> readTVar (md^.pcTblTVar)
                         <*> readTVar (md^.plaTblTVar)
    mkNumOfPlayersTxt (length -> nop) | nop == 1  = "1 player"
                                      | otherwise = showText nop <> " players"
