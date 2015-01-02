{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Cmds.Wiz (wizCmds) where

import Mud.Cmds.Util
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Msgs
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, massLogPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Lazy ((!))
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import System.Directory (doesFileExist)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, readFile)


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Wiz"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Wiz"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Wiz"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Wiz"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Wiz"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Wiz"


massLogPla :: T.Text -> T.Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds.Wiz"


-- ==================================================


wizCmds :: [Cmd]
wizCmds =
    [ Cmd { cmdName = prefixWizCmd "?", action = wizDispCmdList, cmdDesc = "Display this command list." }
    , Cmd { cmdName = prefixWizCmd "boot", action = wizBoot, cmdDesc = "Boot a player." }
    , Cmd { cmdName = prefixWizCmd "date", action = wizDate, cmdDesc = "Display the date." }
    , Cmd { cmdName = prefixWizCmd "print", action = wizPrint, cmdDesc = "Print a message to the server console." }
    , Cmd { cmdName = prefixWizCmd "profanity", action = wizProfanity, cmdDesc = "Dump the profanity log." }
    , Cmd { cmdName = prefixWizCmd "shutdown", action = wizShutdown, cmdDesc = "Shut down CurryMUD." }
    , Cmd { cmdName = prefixWizCmd "start", action = wizStart, cmdDesc = "Display the MUD start time." }
    , Cmd { cmdName = prefixWizCmd "time", action = wizTime, cmdDesc = "Display the current system time." }
    , Cmd { cmdName = prefixWizCmd "uptime", action = wizUptime, cmdDesc = "Display the server uptime." } ]


prefixWizCmd :: CmdName -> T.Text
prefixWizCmd = prefixCmd wizCmdChar


-----


wizDispCmdList :: Action
wizDispCmdList p@(LowerNub' i as) = logPlaExecArgs (prefixWizCmd "?") as i >> dispCmdList wizCmds p
wizDispCmdList p = patternMatchFail "wizDispCmdList" [ showText p ]


-----


wizBoot :: Action
wizBoot p@AdviseNoArgs = advise p [ prefixWizCmd "boot" ] "Please specify the full name of the PC you wish to boot, \
                                                          \followed optionally by a message."
wizBoot (WithArgs i mq cols as@((capitalize . T.toLower -> n):rest)) = do
    mqt@(IM.keys -> is) <- readTMVarInNWS msgQueueTblTMVar
    getEntTbl >>= \et -> case [ i' | i' <- is, (et ! i')^.sing == n ] of
      []   -> wrapSend mq cols $ "No PC by the name of " <> dblQuote n <> " is currently logged in."
      [i'] -> let n'  = (et  ! i )^.sing
                  mq' = (mqt ! i')
              in do
                  logPlaExecArgs (prefixWizCmd "boot") as i
                  ok mq
                  case rest of [] -> dfltMsg   i' n' mq'
                               _  -> customMsg i' n' mq'
      xs   -> patternMatchFail "wizBoot" [ showText xs ]
  where
    dfltMsg   i' n' mq' = do
        logPla "wizBoot dfltMsg" i' $ T.concat [ "booted by ", n', " ", parensQuote "no message given", "." ]
        sendMsgBoot mq' Nothing
    customMsg i' n' mq' = let msg = T.intercalate " " rest in do
        logPla "wizBoot customMsg" i' $ T.concat [ "booted by ", n', "; message: ", msg ]
        sendMsgBoot mq' . Just $ msg
wizBoot p = patternMatchFail "wizBoot" [ showText p ]


-----


wizDate :: Action
wizDate (NoArgs' i mq) = do
    logPlaExec (prefixWizCmd "date") i
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
wizDate p = withoutArgs wizDate p


-----


wizPrint :: Action
wizPrint p@AdviseNoArgs         = advise p [ prefixWizCmd "print" ] advice
  where
    advice = "You must provide a message to print to the server console, as in " <> dblQuote (prefixWizCmd "print" <>
             " Is anybody home?") <> "."
wizPrint   (WithArgs i mq _ as) = do
    logPlaExecArgs (prefixWizCmd "print") as i
    s <- getEntSing i
    liftIO . T.putStrLn $ bracketQuote s <> " " <> T.intercalate " " as
    ok mq
wizPrint p = patternMatchFail "wizPrint" [ showText p ]


-----


wizProfanity :: Action
wizProfanity (NoArgs i mq cols) = logPlaExec (prefixWizCmd "profanity") i >> showProfanityLog mq cols
wizProfanity p                  = withoutArgs wizProfanity p


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


wizShutdown :: Action
wizShutdown (NoArgs' i mq) = do
    logPlaExecArgs (prefixWizCmd "shutdown") [] i
    s <- getEntSing i
    massSend dfltShutdownMsg
    massLogPla "wizShutdown" $ T.concat [ "closing connection due to server shutdown initiated by "
                                        , s
                                        , " "
                                        , parensQuote "no message given"
                                        , "." ]
    logNotice  "wizShutdown" $ T.concat [ "server shutdown initiated by "
                                        , s
                                        , " "
                                        , parensQuote "no message given"
                                        , "." ]
    liftIO . atomically . writeTQueue mq $ Shutdown
wizShutdown (WithArgs i mq _ as) = do
    logPlaExecArgs (prefixWizCmd "shutdown") as i
    s <- getEntSing i
    let msg = T.intercalate " " as
    massSend msg
    massLogPla "wizShutdown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                          , s
                                          , "; message: "
                                          , msg ]
    logNotice  "wizShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; message: ", msg, "." ]
    liftIO . atomically . writeTQueue mq $ Shutdown
wizShutdown _ = patternMatchFail "wizShutdown" []


-----


wizStart :: Action
wizStart (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "start") i
    wrapSend mq cols . showText =<< getNWSRec startTime
wizStart p = withoutArgs wizStart p


-----


wizTime :: Action
wizTime (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "time") i
    (ct, zt) <- (,) <$> liftIO (formatThat `fmap` getCurrentTime) <*> liftIO (formatThat `fmap` getZonedTime)
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
  where
    formatThat (T.words . showText -> wordy@((,) <$> head <*> last -> (date, zone)))
      | time <- T.init . T.reverse . T.dropWhile (/= '.') . T.reverse . head . tail $ wordy
      = T.concat [ zone, ": ", date, " ", time ]
wizTime p = withoutArgs wizTime p


-----


wizUptime :: Action
wizUptime (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "uptime") i
    (try . send mq . nl =<< liftIO runUptime) >>= eitherRet (\e -> logIOEx "wizUptime" e >> sendGenericErrorMsg mq cols)
  where
    runUptime = T.pack <$> readProcess "uptime" [] ""
wizUptime p = withoutArgs wizUptime p
