{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ParallelListComp, PatternSynonyms, ViewPatterns #-}

module Mud.Cmds.Admin (adminCmds) where

import Mud.ANSI
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
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
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.IntMap.Lazy ((!))
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
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
    , Cmd { cmdName = prefixAdminCmd "boot", action = adminBoot, cmdDesc = "Boot a player." }
    , Cmd { cmdName = prefixAdminCmd "date", action = adminDate, cmdDesc = "Display the current system date." }
    , Cmd { cmdName = prefixAdminCmd "print", action = adminPrint, cmdDesc = "Print a message to the server console." }
    , Cmd { cmdName = prefixAdminCmd "profanity", action = adminProfanity, cmdDesc = "Dump the profanity log." }
    , Cmd { cmdName = prefixAdminCmd "shutdown", action = adminShutdown, cmdDesc = "Shut down CurryMUD." }
    , Cmd { cmdName = prefixAdminCmd "time", action = adminTime, cmdDesc = "Display the current system time." }
    , Cmd { cmdName = prefixAdminCmd "uptime", action = adminUptime, cmdDesc = "Display the system uptime." }
    , Cmd { cmdName = prefixAdminCmd "who", action = adminWho, cmdDesc = "Display or search a list of all the players \
                                                                         \logged in." } ]


prefixAdminCmd :: CmdName -> T.Text
prefixAdminCmd = prefixCmd adminCmdChar


-----


adminAnnounce :: Action
adminAnnounce p@AdviseNoArgs         = advise p [ prefixAdminCmd "announce" ] advice
  where
    advice = T.concat [ "You must provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "announce" <> " CurryMUD will be shutting down for maintenance in 30 \
                                                  \minutes."
                      , dfltColor
                      , "." ]
adminAnnounce   (WithArgs i mq _ as) = do
    logPlaExecArgs (prefixAdminCmd "announce") as i
    ok mq
    massSend $ announceColor <> T.unwords as <> dfltColor
adminAnnounce p = patternMatchFail "adminAnnounce" [ showText p ]


-----


adminBoot :: Action
adminBoot p@AdviseNoArgs = advise p [ prefixAdminCmd "boot" ] "Please specify the full PC name of the player you wish \
                                                              \to boot, followed optionally by a custom message."
adminBoot (WithArgs i mq cols as@((capitalize . T.toLower -> n):rest)) = do
    mqt@(IM.keys -> is) <- readTMVarInNWS msgQueueTblTMVar
    getEntTbl >>= \et -> case [ i' | i' <- is, (et ! i')^.sing == n ] of
      []   -> wrapSend mq cols $ "No PC by the name of " <> dblQuote n <> " is currently logged in."
      [i'] | n'  <- (et  ! i )^.sing
           , mq' <- mqt ! i' -> if n' == n
             then wrapSend mq cols "You can't boot yourself."
             else do
                 logPlaExecArgs (prefixAdminCmd "boot") as i
                 ok mq
                 case rest of [] -> dfltMsg   i' n' mq'
                              _  -> customMsg i' n' mq'
      xs   -> patternMatchFail "adminBoot" [ showText xs ]
  where
    dfltMsg   i' n' mq' = do
        logPla "adminBoot dfltMsg" i' $ T.concat [ "booted by ", n', " ", parensQuote "no message given", "." ]
        sendMsgBoot mq' Nothing
    customMsg i' n' mq' | msg <- T.intercalate " " rest = do
        logPla "adminBoot customMsg" i' $ T.concat [ "booted by ", n', "; message: ", msg ]
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


adminPrint :: Action
adminPrint p@AdviseNoArgs         = advise p [ prefixAdminCmd "print" ] advice
  where
    advice = T.concat [ "You must provide a message to print to the server console, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "print" <> " Is anybody home?"
                      , dfltColor
                      , "." ]
adminPrint   (WithArgs i mq _ as) = do
    logPlaExecArgs (prefixAdminCmd "print") as i
    s <- getEntSing i
    liftIO . T.putStrLn . T.concat $ [ bracketQuote s, " ", printConsoleColor, T.intercalate " " as, dfltColor ]
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
adminShutdown (NoArgs' i mq) = do
    logPlaExecArgs (prefixAdminCmd "shutdown") [] i
    s <- getEntSing i
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
adminShutdown (WithArgs i mq _ as) = do
    logPlaExecArgs (prefixAdminCmd "shutdown") as i
    s <- getEntSing i
    let msg = T.intercalate " " as
    massSend $ shutdownMsgColor <> msg <> dfltColor
    massLogPla "adminShutdown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                            , s
                                            , "; message: "
                                            , msg ]
    logNotice  "adminShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; message: ", msg, "." ]
    liftIO . atomically . writeTQueue mq $ Shutdown
adminShutdown _ = patternMatchFail "adminShutdown" []


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
    (mkPlaListTxt <$> readWSTMVar <*> readTMVarInNWS plaTblTMVar) >>= pager i mq . concatMap (wrapIndent 20 cols)
adminWho p@(WithArgs i _ _ as) = do
    logPlaExecArgs (prefixAdminCmd "who") as i
    dispMatches p 20 =<< (mkPlaListTxt <$> readWSTMVar <*> readTMVarInNWS plaTblTMVar)
adminWho _ = patternMatchFail "adminWho" []


mkPlaListTxt :: WorldState -> IM.IntMap Pla -> [T.Text]
mkPlaListTxt ws pt =
    let pis  = [ pi | pi <- IM.keys pt, not $ (pt ! pi)^.isAdmin ]
        piss = sortBy (compare `on` snd) . zip pis $ [ view sing $ (ws^.entTbl) ! pi | pi <- pis ]
        pias = [ (pi, a) | (pi, _) <- piss | a <- styleAbbrevs Don'tBracket . map snd $ piss ]
    in map helper pias ++ [ numOfPlayers pis <> " logged in." ]
  where
    helper (pi, a) = let ((pp *** pp) -> (s, r)) = getSexRace pi ws
                     in T.concat [ pad 13 a, padOrTrunc 7 s, padOrTrunc 10 r ]
    numOfPlayers (length -> nop) | nop == 1  = "1 player"
                                 | otherwise = showText nop <> " players"
