{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase, OverloadedStrings, RankNTypes #-}

module Mud.Logging ( closeLogs
                   , initLogging
                   , logAndDispIOEx
                   , logError
                   , logExMsg
                   , logIOEx
                   , logIOExRethrow
                   , logNotice) where

import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp)
import qualified Mud.Util as U (blowUp)

import Control.Applicative (Const)
import Control.Concurrent.Async (async, waitBoth)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (IOException, SomeException)
import Control.Exception.Lifted (throwIO)
import Control.Lens (_2, to)
import Control.Lens.Operators ((.=), (^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State (gets)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Strict.Lens (packed)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, noticeM, setHandlers, setLevel, updateGlobalLogger)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Logging"


getNoticeLog :: MudStack LogService
getNoticeLog = getLog noticeLog "notice"


getErrorLog :: MudStack LogService
getErrorLog = getLog errorLog "error"


getLog :: forall (m :: * -> *) . MonadState MudState m => ((Maybe LogService -> Const LogService (Maybe LogService)) -> LogServices -> Const LogService LogServices) -> T.Text -> m LogService
getLog l n = gets (^.nonWorldState.logServices.l.to (fromMaybeLogService n))


fromMaybeLogService :: T.Text -> Maybe LogService -> LogService
fromMaybeLogService n = fromMaybe (blowUp "fromMaybeLogService" (n <> " log service not initialized") [])


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
logNotice modName funName msg = registerMsg (concat [ modName, " ", funName, ": ", msg, "." ]) =<< getLogQueue getNoticeLog


logError :: String -> MudStack ()
logError msg = registerMsg msg =<< getLogQueue getErrorLog


logExMsg :: String -> String -> String -> SomeException -> MudStack ()
logExMsg modName funName msg e = logError . concat $ [ modName, " ", funName, ": ", msg, ". ", dblQuoteStr . show $ e ]


logIOEx :: String -> String -> IOException -> MudStack ()
logIOEx modName funName e = logError . concat $ [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]


logAndDispIOEx :: String -> String -> IOException -> MudStack ()
logAndDispIOEx modName funName e = let msg = concat [ modName, " ", funName, ": ", dblQuoteStr . show $ e ]
                                   in logError msg >> output (msg^.packed <> nlt <> nlt)


logIOExRethrow :: String -> String -> IOException -> MudStack ()
logIOExRethrow modName funName e = do
    logError . concat $ [ modName, " ", funName, ": unexpected exception; rethrowing." ]
    liftIO . throwIO $ e
