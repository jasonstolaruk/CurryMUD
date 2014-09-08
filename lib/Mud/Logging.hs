{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, RankNTypes #-}

module Mud.Logging ( closeLogs
                   , initLogging
                   , logAndDispIOEx
                   , logError
                   , logExMsg
                   , logIOEx
                   , logIOExRethrow
                   , logNotice) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util

import Control.Concurrent.Async (async, waitBoth)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (IOException, SomeException)
import Control.Exception.Lifted (throwIO)
import Control.Lens (_2)
import Control.Lens.Operators ((.=), (^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Text.Strict.Lens (packed)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, noticeM, setHandlers, setLevel, updateGlobalLogger)
import qualified Data.Text as T


getNoticeLog :: MudStack LogService
getNoticeLog = getLog noticeLog "notice"


getErrorLog :: MudStack LogService
getErrorLog = getLog errorLog "error"


getLogQueue :: MudStack LogService -> MudStack LogQueue
getLogQueue = fmap (^._2)


closeLogs :: MudStack ()
closeLogs = do
    logNotice "Mud.Logging" "closeLogs" "closing the logs"
    [(na, nq), (ea, eq)] <- sequence [getNoticeLog, getErrorLog]
    forM_ [nq, eq] $ liftIO . atomically . flip writeTQueue Stop
    liftIO . void . waitBoth na $ ea


initLogging :: MudStack ()
initLogging = do
    nq <- liftIO newTQueueIO
    eq <- liftIO newTQueueIO
    na <- spawnLogger "notice.log" NOTICE "currymud.notice" noticeM nq
    ea <- spawnLogger "error.log"  ERROR  "currymud.error"  errorM  eq
    nonWorldState.logServices.noticeLog .= Just (na, nq)
    nonWorldState.logServices.errorLog  .= Just (ea, eq)


type LogName    = String
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> MudStack LogAsync
spawnLogger fn p ln f q = liftIO (async . loop =<< initLog)
  where
    initLog = do
        gh <- fileHandler (logDir ++ fn) p
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        updateGlobalLogger ln (setHandlers [h] . setLevel p)
        return gh
    loop gh = (atomically . readTQueue $ q) >>= \case
      Stop  -> close gh
      Msg m -> f ln m >> loop gh


registerMsg :: String -> LogQueue -> MudStack ()
registerMsg msg q = liftIO . atomically . writeTQueue q . Msg $ msg


logNotice :: String -> String -> String -> MudStack ()
logNotice modName funName msg = (registerMsg . concat $ [modName, " ", funName, ": ", msg, "."]) =<< getLogQueue getNoticeLog


logError :: String -> MudStack ()
logError msg = registerMsg msg =<< getLogQueue getErrorLog


logExMsg :: String -> String -> String -> SomeException -> MudStack ()
logExMsg modName funName msg e = logError . concat $ [ modName, " ", funName, ": ", msg, ". ", dblQuoteStr . show $ e ]


logIOEx :: String -> String -> IOException -> MudStack ()
logIOEx modName funName e = logError . concat $ [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]


logAndDispIOEx :: MsgQueue -> Cols -> String -> String -> IOException -> MudStack ()
logAndDispIOEx mq cols modName funName e = let msg = concat [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]
                                           in logError msg >> (send mq . nl . T.unlines . wordWrap cols $ msg^.packed)


logIOExRethrow :: String -> String -> IOException -> MudStack ()
logIOExRethrow modName funName e = do
    logError . concat $ [modName, " ", funName, ": unexpected exception; rethrowing."]
    liftIO . throwIO $ e
