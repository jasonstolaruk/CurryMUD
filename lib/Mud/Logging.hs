{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Logging where

import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBQueue (readTBQueue, writeTBQueue)
import Control.Exception (IOException, SomeException)
import Control.Exception.Lifted (throwIO)
import Control.Lens.Operators ((^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.State (gets)
import Data.Text.Strict.Lens (packed)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, noticeM, setHandlers, setLevel, updateGlobalLogger)


closeLogs :: MudStack ()
closeLogs = do
    logNotice "Mud.Logging" "closeLogs" "closing the logs"
    nq <- gets (^.logQueues.noticeQueue)
    eq <- gets (^.logQueues.errorQueue)
    forM_ [nq, eq] closeIt
  where
    closeIt q = liftIO . atomically . writeTBQueue q $ Stop


initLogging :: MudStack ()
initLogging = do
    gets (^.logQueues.noticeQueue) >>= spawnLogger "notice.log" NOTICE "currymud.notice" noticeM
    gets (^.logQueues.errorQueue)  >>= spawnLogger "error.log"  ERROR  "currymud.error"  errorM


type LogName    = String
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> MudStack ()
spawnLogger fn p ln f q = liftIO initLog >>= void . liftIO . forkIO . loop
  where
    initLog = do
        gh <- fileHandler (logDir ++ fn) p
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        updateGlobalLogger ln (setHandlers [h] . setLevel p)
        return gh
    loop gh = (atomically . readTBQueue $ q) >>= \cmd ->
        case cmd of Stop  -> close gh
                    Msg m -> f ln m >> loop gh


registerMsg :: String -> LogQueue -> MudStack ()
registerMsg msg q = liftIO . atomically . writeTBQueue q . Msg $ msg


logNotice :: String -> String -> String -> MudStack ()
logNotice modName funName msg = gets (^.logQueues.noticeQueue) >>= registerMsg (concat [ modName, " ", funName, ": ", msg, "." ])


logError :: String -> MudStack ()
logError msg = gets (^.logQueues.errorQueue) >>= registerMsg msg


logIOEx :: String -> String -> IOException -> MudStack ()
logIOEx modName funName e = logError . concat $ [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]


logAndDispIOEx :: String -> String -> IOException -> MudStack ()
logAndDispIOEx modName funName e = let msg = concat [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]
                                   in logError msg >> output (msg^.packed)


logIOExRethrow :: String -> String -> IOException -> MudStack ()
logIOExRethrow modName funName e = do
    logError . concat $ [ modName, " ", funName, ": unexpected exception; rethrowing." ]
    liftIO . throwIO $ e


logExMsg :: String -> String -> String -> SomeException -> MudStack ()
logExMsg modName funName msg e = logError . concat $ [ modName, " ", funName, ": ", msg, ". ", dblQuoteStr . show $ e ]
