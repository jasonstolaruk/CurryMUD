{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race_, wait)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Lens (at)
import Control.Lens.Getter (use)
import Control.Lens.Operators ((&), (.=), (?~))
import Control.Monad ((>=>), forM_, forever, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.IntMap.Lazy ((!))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
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


default (Int)


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
spawnLogger fn@(T.pack -> fn') p (T.unpack -> ln) f q =
    async $ race_ ((loop =<< initLog)   `catch` loggingThreadExHandler "spawnLogger")
                  (logRotationFlagger q `catch` loggingThreadExHandler "logRotationFlagger")
  where
    initLog = p |$| fileHandler fn >=> \gh ->
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        in updateGlobalLogger ln (setHandlers [h] . setLevel p) >> return gh
    loop gh = q |$| atomically . readTQueue >=> \case
      LogMsg (T.unpack -> msg) -> f ln msg >> loop gh
      RotateLog                -> rotateLog gh
      StopLog                  -> close gh
      Throw                    -> throwIO DivideByZero
    rotateLog gh = mIf (doesFileExist fn)
                       (mIf ((>= maxLogSize) <$> fileSize `fmap` getFileStatus fn)
                            rotateIt
                            (loop gh))
                       (sequence_ [ close gh, loop =<< initLog ])
      where
        rotateIt = mkDateTimeTxt >>= \(date, time) -> do
            atomically . writeTQueue q . LogMsg $ "Mud.Logging spawnLogger rotateLog rotateIt: log rotated."
            close gh
            renameFile fn . T.unpack . T.concat $ [ dropExt fn', ".", date, "_", time, ".log" ]
            loop =<< initLog
        dropExt = T.reverse . T.drop 4 . T.reverse


loggingThreadExHandler :: T.Text -> SomeException -> IO ()
loggingThreadExHandler n e = guard (fromException e /= Just ThreadKilled) >> mkTimestamp >>= \ts ->
    let msg = T.concat [ ts
                       , " "
                       , "Mud.Logging loggingThreadExHandler: exception caught on logging thread "
                       , parensQuote $ "inside " <> dblQuote n
                       , ". "
                       , dblQuote . showText $ e ]
    in (T.appendFile loggingExLogFile . nl $ msg) `catch` handler msg
  where
    handler msg ex | isAlreadyInUseError ex = showIt
                   | isPermissionError   ex = showIt
                   | otherwise              = throwIO ex
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
doIfLogging i f = IM.lookup i <$> readTMVarInNWS plaLogTblTMVar >>= maybeVoid (f . snd)


closeLogs :: MudStack ()
closeLogs = do
    logNotice "Mud.Logging" "closeLogs" "closing the logs."
    [ (na, nq), (ea, eq) ] <- sequence [ fromJust <$> use (nonWorldState.noticeLog)
                                       , fromJust <$> use (nonWorldState.errorLog ) ]
    (unzip -> (as, qs)) <- IM.elems <$> readTMVarInNWS plaLogTblTMVar
    mapM_ stopLog         $ nq : eq : qs
    mapM_ (liftIO . wait) $ na : ea : as
    liftIO removeAllHandlers


-- ==================================================
-- Logging messages:


registerMsg :: T.Text -> LogQueue -> MudStack ()
registerMsg msg = liftIO . atomically . flip writeTQueue (LogMsg msg)


logNotice :: T.Text -> T.Text -> T.Text -> MudStack ()
logNotice modName (dblQuote -> funName) msg = maybeVoid helper =<< use (nonWorldState.noticeLog)
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg ]) . snd


logError :: T.Text -> MudStack ()
logError msg = maybeVoid (registerMsg msg . snd) =<< use (nonWorldState.errorLog)


logExMsg :: T.Text -> T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg modName (dblQuote -> funName) msg (dblQuote . showText -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", msg, ". ", e ]


logIOEx :: T.Text -> T.Text -> IOException -> MudStack ()
logIOEx modName (dblQuote -> funName) (dblQuote . showText -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", e ]


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols modName (dblQuote -> funName) (dblQuote . showText -> e)
  | msg <- T.concat [ modName, " ", funName, ": ", e ] = logError msg >> wrapSend mq cols msg


logPla :: T.Text -> T.Text -> Id -> T.Text -> MudStack ()
logPla modName (dblQuote -> funName) i msg =
    doIfLogging i . registerMsg . T.concat $ [ modName, " ", funName, ": ", msg ]


logPlaExec :: T.Text -> CmdName -> Id -> MudStack ()
logPlaExec modName cn i = logPla modName cn i $ "executed " <> dblQuote cn <> "."


logPlaExecArgs :: T.Text -> CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs modName cn as i = logPla modName cn i $ "executed " <> helper <> "."
  where
    helper = case as of [] -> dblQuote cn <> " with no arguments"
                        _  -> dblQuote . T.unwords $ cn : as


logPlaOut :: T.Text -> CmdName -> Id -> [T.Text] -> MudStack ()
logPlaOut modName (dblQuote -> cn) i (T.intercalate " / " -> msgs) = helper =<< getPlaLogQueue i
  where
    helper = registerMsg (T.concat [ modName, " ", cn, " (output): ", msgs ])


getPlaLogQueue :: Id -> MudStack LogQueue
getPlaLogQueue i = snd . (! i) <$> readTMVarInNWS plaLogTblTMVar


massLogPla :: T.Text -> T.Text -> T.Text -> MudStack ()
massLogPla modName (dblQuote -> funName) msg = plaLogTblTMVar |$| readTMVarInNWS >=> helper
  where
    helper (map snd . IM.elems -> logQueues) =
        forM_ logQueues $ registerMsg (T.concat [ modName, " ", funName, ": ", msg ])
