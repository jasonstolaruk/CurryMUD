{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, TransformListComp, ViewPatterns #-}

module Mud.Cmds.Admin (adminCmds) where

import Mud.ANSI
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.Data.State.Util.STM
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Msgs
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, massLogPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, at, over)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((&), (?~), (^.))
import Control.Lens.Setter (set)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
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
import qualified Data.IntMap.Lazy as IM (IntMap, keys)
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
    [ Cmd { cmdName = prefixAdminCmd "?", action = adminDispCmdList, cmdDesc = "Display or search this command list." }
    , Cmd { cmdName = prefixAdminCmd "announce", action = adminAnnounce, cmdDesc = "Send a message to all players." }
    , Cmd { cmdName = prefixAdminCmd "boot", action = adminBoot, cmdDesc = "Boot a player, optionally with a custom \
                                                                           \message." }
    , Cmd { cmdName = prefixAdminCmd "date", action = adminDate, cmdDesc = "Display the current system date." }
    , Cmd { cmdName = prefixAdminCmd "peep", action = adminPeep, cmdDesc = "Start or stop peeping one or more \
                                                                           \players." }
    , Cmd { cmdName = prefixAdminCmd "print", action = adminPrint, cmdDesc = "Print a message to the server console." }
    , Cmd { cmdName = prefixAdminCmd "profanity", action = adminProfanity, cmdDesc = "Dump the profanity log." }
    , Cmd { cmdName = prefixAdminCmd "shutdown", action = adminShutdown, cmdDesc = "Shut down CurryMUD, optionally \
                                                                                   \with a custom message." }
    , Cmd { cmdName = prefixAdminCmd "tell", action = adminTell, cmdDesc = "Send a message to a player." }
    , Cmd { cmdName = prefixAdminCmd "time", action = adminTime, cmdDesc = "Display the current system time." }
    , Cmd { cmdName = prefixAdminCmd "uptime", action = adminUptime, cmdDesc = "Display the system uptime." }
    , Cmd { cmdName = prefixAdminCmd "who", action = adminWho, cmdDesc = "Display or search a list of the players who \
                                                                         \are currently connected." } ]


prefixAdminCmd :: CmdName -> T.Text
prefixAdminCmd = prefixCmd adminCmdChar


-----


adminAnnounce :: Action
adminAnnounce p@AdviseNoArgs   = advise p [ prefixAdminCmd "announce" ] advice
  where
    advice = T.concat [ "You must provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "announce" <> " CurryMUD will be shutting down for maintenance in 30 \
                                                  \minutes"
                      , dfltColor
                      , "." ]
adminAnnounce   (Msg i mq msg) = getEntSing i >>= \s -> do
    logPla    "adminAnnounce" i $       "announced "  <> dblQuote msg
    logNotice "adminAnnounce"   $ s <> " announced, " <> dblQuote msg
    ok mq
    massSend $ announceColor <> msg <> dfltColor
adminAnnounce p = patternMatchFail "adminAnnounce" [ showText p ]


-----


adminBoot :: Action
adminBoot p@AdviseNoArgs = advise p [ prefixAdminCmd "boot" ] "Please specify the full PC name of the player you wish \
                                                              \to boot, followed optionally by a custom message."
adminBoot   (MsgWithTarget i mq cols target msg) = do
    mqt@(IM.keys -> is) <- readTMVarInNWS msgQueueTblTMVar
    getEntTbl >>= \et -> case [ i' | i' <- is, (et ! i')^.sing == target ] of
      []   -> wrapSend mq cols $ "No PC by the name of " <> dblQuote target <> " is currently connected. (Note that \
                                 \you must specify the full PC name of the player you wish to boot.)"
      [i'] | s <- (et ! i)^.sing -> if s == target
             then wrapSend mq cols "You can't boot yourself."
             else let mq' = mqt ! i' in do
                 ok mq
                 case msg of "" -> dfltMsg   i' s mq'
                             _  -> customMsg i' s mq'
      xs   -> patternMatchFail "adminBoot" [ showText xs ]
  where
    dfltMsg   i' s mq' = do
        logPla "adminBoot dfltMsg"   i  $ T.concat [ "booted ", target, " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   i' $ T.concat [ "booted by ", s,   " ", parensQuote "no message given", "." ]
        sendMsgBoot mq' Nothing
    customMsg i' s mq' = do
        logPla "adminBoot customMsg" i  $ T.concat [ "booted ", target, "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" i' $ T.concat [ "booted by ", s,   "; message: ", dblQuote msg ]
        sendMsgBoot mq' . Just $ msg
adminBoot p = patternMatchFail "adminBoot" [ showText p ]


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
adminPeep   (LowerNub i mq cols (map capitalize -> as)) = helper >>= \(msgs, logMsgs) -> do
    multiWrapSend mq cols msgs
    let (logMsgsSelf, logMsgsOthers) = unzip logMsgs
    logPla "adminPeep" i . (<> ".") . T.intercalate " / " $ logMsgsSelf
    forM_ logMsgsOthers $ uncurry (logPla "adminPeep")
  where
    helper = getEntTbl >>= \et -> onNWS plaTblTMVar $ \(ptTMVar, pt) ->
        let s                    = (et ! i)^.sing
            piss                 = mkPlaIdsSingsList et pt
            (pt', msgs, logMsgs) = foldr (peep s piss) (pt, [], []) as
        in putTMVar ptTMVar pt' >> return (msgs, logMsgs)
    peep s piss target a@(pt, _, _) =
        let notFound    = over _2 ("No player by the name of " <> dblQuote target <> " is currently connected." :) a
            found match | (i', target') <- head . filter ((== match) . snd) $ piss
                        , thePeeper     <- pt ! i
                        , thePeeped     <- pt ! i' = if i' `notElem` thePeeper^.peeping
                          then let pt'     = pt & at i  ?~ over peeping (i' :) thePeeper
                                                & at i' ?~ over peepers (i  :) thePeeped
                                   msg     = "You are now peeping " <> target' <> "."
                                   logMsgs = [("started peeping " <> target', (i', s <> " started peeping."))]
                               in set _1 pt' . over _2 (msg :) . over _3 (logMsgs ++) $ a
                          else let pt'     = pt & at i  ?~ over peeping (i' `delete`) thePeeper
                                                & at i' ?~ over peepers (i  `delete`) thePeeped
                                   msg     = "You are no longer peeping " <> target' <> "."
                                   logMsgs = [("stopped peeping " <> target', (i', s <> " stopped peeping."))]
                               in set _1 pt' . over _2 (msg :) . over _3 (logMsgs ++) $ a
        in maybe notFound found . findFullNameForAbbrev target . map snd $ piss
adminPeep p = patternMatchFail "adminPeep" [ showText p ]


-----


adminPrint :: Action
adminPrint p@AdviseNoArgs   = advise p [ prefixAdminCmd "print" ] advice
  where
    advice = T.concat [ "You must provide a message to print to the server console, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "print" <> " Is anybody home?"
                      , dfltColor
                      , "." ]
adminPrint   (Msg i mq msg) = getEntSing i >>= \s -> do
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
    liftIO . T.putStrLn . T.concat $ [ bracketQuote s, " ", printConsoleColor, msg, dfltColor ]
    ok mq
adminPrint p = patternMatchFail "adminPrint" [ showText p ]


-----


adminProfanity :: Action
adminProfanity (NoArgs i mq cols) = logPlaExec (prefixAdminCmd "profanity") i >> showProfanityLog mq cols
adminProfanity p                  = withoutArgs adminProfanity p


showProfanityLog :: MsgQueue -> Cols -> MudStack ()
showProfanityLog mq cols = send mq =<< helper
  where
    helper           = (try . liftIO $ readProfanityLog) >>= eitherRet handler
    readProfanityLog = doesFileExist profanityLogFile >>= \case
      True  -> return . multiWrapNl   cols . T.lines =<< T.readFile profanityLogFile
      False -> return . wrapUnlinesNl cols $ "No profanities have been logged."
    handler e        = do
        fileIOExHandler "showProfanityLog" e
        return . wrapUnlinesNl cols $ "Unfortunately, the profanity log could not be retrieved."


-----


adminShutdown :: Action
adminShutdown (NoArgs' i mq) = getEntSing i >>= \s -> do
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
adminShutdown (Msg i mq msg) = getEntSing i >>= \s -> do
    logPla "adminShutdown" i $ "initiating shutdown; message: " <> dblQuote msg
    massSend $ shutdownMsgColor <> msg <> dfltColor
    massLogPla "adminShutdown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                            , s
                                            , "; message: "
                                            , dblQuote msg ]
    logNotice  "adminShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; message: ", dblQuote msg ]
    liftIO . atomically . writeTQueue mq $ Shutdown
adminShutdown _ = patternMatchFail "adminShutdown" []


-----


adminTell :: Action
adminTell p@AdviseNoArgs     = advise p [ prefixAdminCmd "tell" ] advice
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
adminTell   (MsgWithTarget i mq cols target msg) = do
    et        <- getEntTbl
    (mqt, pt) <- getMqtPt
    let (view sing -> s) = et ! i
    let piss             = mkPlaIdsSingsList et pt
    let notFound         = wrapSend mq cols $ "No player with the PC name of " <> dblQuote target <> " is currently \
                                              \logged in."
    let found match | (i', target') <- head . filter ((== match) . snd) $ piss
                    , mq'           <- mqt ! i'
                    , p             <- pt ! i'
                    , cols'         <- p^.columns = do
                       logPla (prefixAdminCmd "tell") i  . T.concat $ [ "sent message to "
                                                                      , target'
                                                                      , ": "
                                                                      , dblQuote msg ]
                       logPla (prefixAdminCmd "tell") i' . T.concat $ [ "received message from "
                                                                      , s
                                                                      , ": "
                                                                      , dblQuote msg ]
                       wrapSend mq cols . T.concat $ [ "You send ", target', ": ", dblQuote msg ]
                       let targetMsg = T.concat [ bracketQuote s, " ", adminTellColor, msg, dfltColor ]
                       if getPlaFlag IsNotFirstAdminTell p
                         then wrapSend mq' cols' targetMsg
                         else multiWrapSend mq' cols' . (targetMsg :) =<< firstAdminTell i' s
    maybe notFound found . findFullNameForAbbrev target . map snd $ piss
adminTell p = patternMatchFail "adminTell" [ showText p ]


firstAdminTell :: Id -> Sing -> MudStack [T.Text]
firstAdminTell i s = do
    void . modifyPlaFlag i IsNotFirstAdminTell $ True
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
    (ct, zt) <- (,) <$> liftIO (formatThat `fmap` getCurrentTime) <*> liftIO (formatThat `fmap` getZonedTime)
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
  where
    formatThat (T.words . showText -> wordy@((,) <$> head <*> last -> (date, zone)))
      | time <- T.init . T.reverse . T.dropWhile (/= '.') . T.reverse . head . tail $ wordy
      = T.concat [ zone, ": ", date, " ", time ]
adminTime p = withoutArgs adminTime p


-----


adminUptime :: Action
adminUptime (NoArgs i mq cols) = do
    logPlaExec (prefixAdminCmd "uptime") i
    (try . send mq . nl =<< liftIO runUptime) >>= eitherRet handler
  where
    runUptime = T.pack <$> readProcess "uptime" [] ""
    handler e = logIOEx "adminUptime" e >> sendGenericErrorMsg mq cols
adminUptime p = withoutArgs adminUptime p


-----


adminWho :: Action
adminWho   (NoArgs i mq cols)  = do
    logPlaExecArgs (prefixAdminCmd "who") [] i
    pager i mq . concatMap (wrapIndent 20 cols) =<< (mkPlaListTxt <$> readWSTMVar <*> readTMVarInNWS plaTblTMVar)
adminWho p@(WithArgs i _ _ as) = do
    logPlaExecArgs (prefixAdminCmd "who") as i
    dispMatches p 20 =<< (mkPlaListTxt <$> readWSTMVar <*> readTMVarInNWS plaTblTMVar)
adminWho _ = patternMatchFail "adminWho" []


mkPlaListTxt :: WorldState -> IM.IntMap Pla -> [T.Text]
mkPlaListTxt ws pt =
    let pis         = [ pi | pi <- IM.keys pt, not . getPlaFlag IsAdmin $ (pt ! pi) ]
        (pis', pss) = unzip [ (pi, s) | pi <- pis, let s = view sing $ (ws^.entTbl) ! pi, then sortWith by s ]
        pias        = zipWith (,) pis' . styleAbbrevs Don'tBracket $ pss -- [ (pi, a) | pi <- pis' | a <- styleAbbrevs Don'tBracket pss ]
    in map helper pias ++ [ numOfPlayers pis <> " connected." ]
  where
    helper (pi, a) = let ((pp *** pp) -> (s, r)) = getSexRace pi ws
                     in T.concat [ pad 13 a, padOrTrunc 7 s, padOrTrunc 10 r ]
    numOfPlayers (length -> nop) | nop == 1  = "1 player"
                                 | otherwise = showText nop <> " players"
