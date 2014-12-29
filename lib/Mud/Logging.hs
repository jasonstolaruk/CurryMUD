{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Logging ( closeLogs
                   , closePlaLog
                   , initLogging
                   , initPlaLog
                   , logAndDispIOEx
                   , logError
                   , logExMsg
                   , logIOEx
                   , logNotice
                   , logPla
                   , logPlaExec
                   , logPlaExecArgs
                   , logPlaOut
                   , massLogPla ) where

import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race_, wait)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Lens (at)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((&), (.=), (?~))
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.State (gets)
import Data.IntMap.Lazy ((!))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Time (getZonedTime)
import System.Directory (doesFileExist, renameFile)
import System.IO (stderr)
import System.IO.Error (isAlreadyInUseError, isPermissionError)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, infoM, noticeM, removeAllHandlers, removeHandler, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import System.Posix.Files (fileSize, getFileStatus)
import qualified Data.IntMap.Lazy as IM (elems, lookup)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, hPutStrLn)


-- ==================================================
-- Starting logs:


initLogging :: MudStack ()
initLogging = do
    liftIO . updateGlobalLogger rootLoggerName $ removeHandler
    (nq, eq) <- (,) <$> liftIO newTQueueIO <*> liftIO newTQueueIO
    (na, ea) <- (,) <$> (liftIO . spawnLogger noticeLogFile NOTICE "currymud.notice" noticeM $ nq)
                    <*> (liftIO . spawnLogger errorLogFile  ERROR  "currymud.error"  errorM  $ eq)
    nonWorldState.noticeLog .= Just (na, nq)
    nonWorldState.errorLog  .= Just (ea, eq)


type LogName    = T.Text
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> IO LogAsync
spawnLogger fn p (T.unpack -> ln) f q =
    async $ race_ ((loop =<< initLog)   `catch` loggingThreadExHandler "spawnLogger")
                  (logRotationFlagger q `catch` loggingThreadExHandler "logRotationFlagger")
  where
    initLog = fileHandler fn p >>= \gh ->
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        in updateGlobalLogger ln (setHandlers [h] . setLevel p) >> return gh
    loop gh = (atomically . readTQueue $ q) >>= \case
      LogMsg (T.unpack -> msg) -> f ln msg >> loop gh
      RotateLog                -> rotateLog gh
      StopLog                  -> close gh
      Throw                    -> throwIO DivideByZero
    rotateLog gh = doesFileExist fn >>= \case
        True  -> (fileSize <$> getFileStatus fn) >>= \fs ->
            if fs >= maxLogSize then rotateIt else loop gh
        False -> close gh >> (loop =<< initLog)
      where
        rotateIt = getZonedTime >>= \(words . show -> wordy) ->
            let date = head wordy
                time = map replaceColons . init . reverse . dropWhile (/= '.') . reverse . head . tail $ wordy
            in do
                atomically . writeTQueue q . LogMsg $ "Mud.Logging spawnLogger rotateLog rotateIt: log rotated."
                close gh
                renameFile fn . concat $ [ dropExt fn, ".", date, "_", time, ".log" ]
                loop =<< initLog
        replaceColons ':' = '-'
        replaceColons x   = x
        dropExt           = reverse . drop 4 . reverse


loggingThreadExHandler :: T.Text -> SomeException -> IO ()
loggingThreadExHandler n e = case fromException e of
  Just ThreadKilled -> return ()
  _                 -> mkTimestamp >>= \ts ->
      let msg = T.concat [ ts
                         , " "
                         , "Mud.Logging loggingThreadExHandler: exception caught on logging thread "
                         , parensQuote $ "inside " <> dblQuote n
                         , ". "
                         , dblQuote . showText $ e ]
      in (T.appendFile loggingExLogFile . nl $ msg) `catch` handler msg
  where
    handler msg e' | isAlreadyInUseError e' = showIt
                   | isPermissionError   e' = showIt
                   | otherwise              = throwIO e'
      where
        showIt = T.hPutStrLn stderr msg


logRotationFlagger :: LogQueue -> IO ()
logRotationFlagger q = forever loop
  where
    loop = do
        threadDelay $ 10 ^ 6 * logRotationFlaggerDelay
        atomically . writeTQueue q $ RotateLog


initPlaLog :: Id -> Sing -> MudStack ()
initPlaLog i n@((logDir ++) . (++ ".log") . T.unpack -> fn) = do
    q <- liftIO newTQueueIO
    a <- liftIO . spawnLogger fn INFO ("currymud." <> n) infoM $ q
    modifyNWS plaLogTblTMVar $ \plt ->
        plt & at i ?~ (a, q)


-- ==================================================
-- Stopping/closing logs:


stopLog :: LogQueue -> MudStack ()
stopLog = liftIO . atomically . flip writeTQueue StopLog


closePlaLog :: Id -> MudStack ()
closePlaLog = flip doIfLogging stopLog


doIfLogging :: Id -> (LogQueue -> MudStack ()) -> MudStack ()
doIfLogging i f = (IM.lookup i <$> readTMVarInNWS plaLogTblTMVar) >>= \case
  Nothing     -> return ()
  Just (_, q) -> f q


closeLogs :: MudStack ()
closeLogs = do
    logNotice "Mud.Logging" "closeLogs" "closing the logs."
    [ (na, nq), (ea, eq) ] <- sequence [ fromJust <$> gets (view (nonWorldState.noticeLog))
                                       , fromJust <$> gets (view (nonWorldState.errorLog )) ]
    (unzip -> (as, qs)) <- IM.elems <$> readTMVarInNWS plaLogTblTMVar
    mapM_ stopLog         $ nq : eq : qs
    mapM_ (liftIO . wait) $ na : ea : as
    liftIO removeAllHandlers


-- ==================================================
-- Logging messages:


registerMsg :: T.Text -> LogQueue -> MudStack ()
registerMsg msg q = liftIO . atomically . writeTQueue q . LogMsg $ msg


logNotice :: T.Text -> T.Text -> T.Text -> MudStack ()
logNotice modName funName msg = maybeVoid helper =<< gets (view (nonWorldState.noticeLog))
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg ]) . snd


logError :: T.Text -> MudStack ()
logError msg = maybeVoid (registerMsg msg . snd) =<< gets (view (nonWorldState.errorLog))


logExMsg :: T.Text -> T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg modName funName msg (dblQuote . showText -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", msg, ". ", e ]


logIOEx :: T.Text -> T.Text -> IOException -> MudStack ()
logIOEx modName funName (dblQuote . showText -> e) = logError . T.concat $ [ modName, " ", funName, ": ", e ]


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols modName funName (dblQuote . showText -> e)
  | msg <- T.concat [ modName, " ", funName, ": ", e ] = logError msg >> wrapSend mq cols msg


logPla :: T.Text -> T.Text -> Id -> T.Text -> MudStack ()
logPla modName funName i msg = doIfLogging i $ registerMsg (T.concat [ modName, " ", funName, ": ", msg ])


logPlaExec :: T.Text -> CmdName -> Id -> MudStack ()
logPlaExec modName (dblQuote -> cn) i = logPla modName cn i $ "executed " <> cn <> "."


logPlaExecArgs :: T.Text -> CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs modName cn@(dblQuote -> cn') as i = logPla modName cn' i $ "executed " <> helper <> "."
  where
    helper = case as of [] -> cn' <> " with no arguments"
                        _  -> dblQuote . T.intercalate " " $ cn : as


logPlaOut :: T.Text -> CmdName -> Id -> [T.Text] -> MudStack ()
logPlaOut modName cn i (T.intercalate " / " -> msgs) = helper =<< getPlaLogQueue i
  where
    helper = registerMsg (T.concat [ modName, " ", cn, " (output): ", msgs ])


getPlaLogQueue :: Id -> MudStack LogQueue
getPlaLogQueue i = snd . (! i) <$> readTMVarInNWS plaLogTblTMVar


massLogPla :: T.Text -> T.Text -> T.Text -> MudStack ()
massLogPla modName funName msg = readTMVarInNWS plaLogTblTMVar >>= helper
  where
    helper (map snd . IM.elems -> logQueues) =
        forM_ logQueues $ registerMsg (T.concat [ modName, " ", funName, ": ", msg ])
