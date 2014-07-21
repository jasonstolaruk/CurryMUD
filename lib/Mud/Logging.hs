{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Logging where

import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util

import Control.Concurrent.Async (async, waitBoth)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (IOException, SomeException)
import Control.Exception.Lifted (throwIO)
import Control.Lens (_2, to)
import Control.Lens.Operators ((.=), (^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.State (gets)
import Data.Maybe (fromJust)
import Data.Text.Strict.Lens (packed)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, noticeM, setHandlers, setLevel, updateGlobalLogger)


closeLogs :: MudStack ()
closeLogs = do
    logNotice "Mud.Logging" "closeLogs" "closing the logs"
    (na, nq) <- gets (^.logServices.noticeLog.to fromJust) -- TODO: Helper functions?
    (ea, eq) <- gets (^.logServices.errorLog.to fromJust)
    forM_ [nq, eq] $ liftIO . atomically . flip writeTBQueue Stop
    liftIO . void . waitBoth na $ ea


initLogging :: MudStack ()
initLogging = do
    nq <- liftIO . newTBQueueIO $ logQueueMax
    eq <- liftIO . newTBQueueIO $ logQueueMax
    na <- spawnLogger "notice.log" NOTICE "currymud.notice" noticeM nq
    ea <- spawnLogger "error.log"  ERROR  "currymud.error"  errorM eq
    logServices.noticeLog .= Just (na, nq)
    logServices.errorLog  .= Just (ea, eq)


type LogName    = String
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> MudStack LogAsync
spawnLogger fn p ln f q = liftIO initLog >>= liftIO . async . loop
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
logNotice modName funName msg = gets (^.logServices.noticeLog.to fromJust._2) >>= registerMsg (concat [ modName, " ", funName, ": ", msg, "." ])


logError :: String -> MudStack ()
logError msg = gets (^.logServices.errorLog.to fromJust._2) >>= registerMsg msg


logExMsg :: String -> String -> String -> SomeException -> MudStack ()
logExMsg modName funName msg e = logError . concat $ [ modName, " ", funName, ": ", msg, ". ", dblQuoteStr . show $ e ]


logIOEx :: String -> String -> IOException -> MudStack ()
logIOEx modName funName e = logError . concat $ [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]


logAndDispIOEx :: String -> String -> IOException -> MudStack ()
logAndDispIOEx modName funName e = let msg = concat [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]
                                   in logError msg >> output (msg^.packed)


logIOExRethrow :: String -> String -> IOException -> MudStack ()
logIOExRethrow modName funName e = do
    logError . concat $ [ modName, " ", funName, ": unexpected exception; rethrowing." ]
    liftIO . throwIO $ e
