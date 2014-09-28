{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, RankNTypes #-}

module Mud.Logging ( closeLogs
                   , closePlaLog
                   , initLogging
                   , initPlaLog
                   , logAndDispIOEx
                   , logError
                   , logExMsg
                   , logIOEx
                   , logIOExRethrow
                   , logNotice
                   , logPla
                   , logPlaExec
                   , logPlaExecArgs ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (IOException, SomeException)
import Control.Exception.Lifted (throwIO)
import Control.Lens (at)
import Control.Lens.Operators ((&), (.=), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.State (gets)
import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, infoM, noticeM, setHandlers, setLevel, updateGlobalLogger)
import qualified Data.IntMap.Lazy as IM (elems)
import qualified Data.Text as T


closeLogs :: MudStack ()
closeLogs = do
    logNotice "Mud.Logging" "closeLogs" "closing the logs"
    [ (na, nq), (ea, eq) ] <- sequence [ fromJust <$> gets (^.nonWorldState.noticeLog), fromJust <$> gets (^.nonWorldState.errorLog) ]
    ls <- IM.elems <$> getNWS plaLogsTblTMVar
    mapM_ stopLog $ nq : eq : map snd ls
    mapM_ (liftIO . wait) $ na : ea : map fst ls


stopLog :: LogQueue -> MudStack ()
stopLog = liftIO . atomically . flip writeTQueue Stop


initLogging :: MudStack ()
initLogging = do
    nq <- liftIO newTQueueIO
    eq <- liftIO newTQueueIO
    na <- liftIO . spawnLogger "notice.log" NOTICE "currymud.notice" noticeM $ nq
    ea <- liftIO . spawnLogger "error.log"  ERROR  "currymud.error"  errorM  $ eq
    nonWorldState.noticeLog .= Just (na, nq)
    nonWorldState.errorLog  .= Just (ea, eq)


type LogName    = T.Text
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> IO LogAsync
spawnLogger fn p ln f q = async . loop =<< initLog
  where
    initLog = do
        gh <- fileHandler (logDir ++ fn) p
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        updateGlobalLogger (T.unpack ln) (setHandlers [h] . setLevel p)
        return gh
    loop gh = (atomically . readTQueue $ q) >>= \case
      Stop  -> close gh
      Msg m -> f (T.unpack ln) (T.unpack m) >> loop gh


registerMsg :: T.Text -> LogQueue -> MudStack ()
registerMsg msg q = liftIO . atomically . writeTQueue q . Msg $ msg


logNotice :: T.Text -> T.Text -> T.Text -> MudStack ()
logNotice modName funName msg = maybeVoid helper =<< gets (^.nonWorldState.noticeLog)
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg, "." ]) . snd


logError :: T.Text -> MudStack ()
logError msg = maybeVoid (registerMsg msg . snd) =<< gets (^.nonWorldState.errorLog)


logExMsg :: T.Text -> T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg modName funName msg e = logError . T.concat $ [ modName, " ", funName, ": ", msg, ". ", dblQuote . showText $ e ]


logIOEx :: T.Text -> T.Text -> IOException -> MudStack ()
logIOEx modName funName e = logError . T.concat $ [ modName, " ", funName, ": ", dblQuote . showText $ e ]


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols modName funName e = let msg = T.concat [ modName, " ", funName, ": ", dblQuote . showText $ e ]
                                           in logError msg >> (send mq . nl . T.unlines . wordWrap cols $ msg)


logIOExRethrow :: T.Text -> T.Text -> IOException -> MudStack ()
logIOExRethrow modName funName e = do
    logError . T.concat $ [ modName, " ", funName, ": unexpected exception; rethrowing." ]
    liftIO . throwIO $ e


initPlaLog :: Id -> Sing -> MudStack ()
initPlaLog i n = do
    q <- liftIO newTQueueIO
    a <- liftIO . spawnLogger (T.unpack $ n <> ".log") INFO ("currymud." <> n) infoM $ q
    modifyNWS plaLogsTblTMVar $ \plt -> plt & at i ?~ (a, q)


logPla :: T.Text -> T.Text -> Id -> T.Text -> MudStack ()
logPla modName funName i msg = helper =<< getPlaLogQueue i
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg, "." ])


logPlaExec :: T.Text -> CmdName -> Id -> MudStack ()
logPlaExec modName cn i = logPla modName cn i $ "executed " <> dblQuote cn


logPlaExecArgs :: T.Text -> CmdName -> Rest -> Id -> MudStack ()
logPlaExecArgs modName cn rs i = logPla modName cn i $ "executed " <> helper
  where
    helper = case rs of [] -> dblQuote cn <> " with no arguments"
                        _  -> dblQuote . T.intercalate " " $ cn : rs


closePlaLog :: Id -> MudStack ()
closePlaLog i = stopLog =<< getPlaLogQueue i
