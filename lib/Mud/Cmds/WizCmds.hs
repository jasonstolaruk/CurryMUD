{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, ScopedTypeVariables, ViewPatterns #-}

module Mud.Cmds.WizCmds (wizCmds) where

import Mud.Cmds.CmdUtil
import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logIOEx, logNotice, logPlaExec, logPlaExecArgs, massLogPla)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Lazy ((!))
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.WizCmds"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.WizCmds"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.WizCmds"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.WizCmds"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.WizCmds"


massLogPla :: T.Text -> T.Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds.WizCmds"


-- ==================================================


wizCmds :: [Cmd]
wizCmds =
    [ Cmd { cmdName = prefixWizCmd "?", action = wizDispCmdList, cmdDesc = "Display this command list." }
    , Cmd { cmdName = prefixWizCmd "date", action = wizDate, cmdDesc = "Display the date." }
    , Cmd { cmdName = prefixWizCmd "name", action = wizName, cmdDesc = "Verify your PC name." }
    , Cmd { cmdName = prefixWizCmd "print", action = wizPrint, cmdDesc = "Print a message to the server console." }
    , Cmd { cmdName = prefixWizCmd "shutdown", action = wizShutdown, cmdDesc = "Shut down the MUD." }
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


wizShutdown :: Action
wizShutdown (NoArgs' i mq) = readWSTMVar >>= \ws ->
    let (view sing -> s) = (ws^.entTbl) ! i in do
        massSend "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"
        logPlaExecArgs (prefixWizCmd "shutdown") [] i
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
wizShutdown (WithArgs i mq _ as) = readWSTMVar >>= \ws ->
    let (view sing -> s) = (ws^.entTbl) ! i
        msg              = T.intercalate " " as
    in do
        massSend msg
        logPlaExecArgs (prefixWizCmd "shutdown") as i
        massLogPla "wizShutdown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                              , s
                                              , "; reason: "
                                              , msg
                                              , "." ]
        logNotice  "wizShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; reason: ", msg, "." ]
        liftIO . atomically . writeTQueue mq $ Shutdown
wizShutdown _ = patternMatchFail "wizShutdown" []


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


wizDate :: Action
wizDate (NoArgs' i mq) = do
    logPlaExec (prefixWizCmd "date") i
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
wizDate p = withoutArgs wizDate p


-----


wizUptime :: Action
wizUptime (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "uptime") i
    (try . send mq . nl =<< liftIO runUptime) >>= eitherRet (\e -> logIOEx "wizUptime" e >> sendGenericErrorMsg mq cols)
  where
    runUptime = T.pack <$> readProcess "uptime" [] ""
wizUptime p = withoutArgs wizUptime p


-----


wizStart :: Action
wizStart (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "start") i
    wrapSend mq cols . showText =<< getNWSRec startTime
wizStart p = withoutArgs wizStart p


-----


wizName :: Action
wizName (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "name") i
    readWSTMVar >>= \ws ->
        let (view sing -> s)    = (ws^.entTbl) ! i
            (pp -> s', pp -> r) = getSexRace i ws
        in wrapSend mq cols . T.concat $ [ "You are ", s, " (a ", s', " ", r, ")." ]
wizName p = withoutArgs wizName p


-----


wizPrint :: Action
wizPrint p@AdviseNoArgs       = advise p ["print"] $ "You must provide a message to print to the server console, as \
                                                     \in " <> dblQuote (prefixWizCmd "print" <> " Is anybody \
                                                     \home?") <> "."
wizPrint (WithArgs i mq _ as) = readWSTMVar >>= \ws ->
    let (view sing -> s) = (ws^.entTbl) ! i
    in do
        logPlaExecArgs (prefixWizCmd "print") as i
        liftIO . T.putStrLn $ bracketQuote s <> " " <> T.intercalate " " as
        ok mq
wizPrint p = patternMatchFail "wizPrint" [ showText p ]
