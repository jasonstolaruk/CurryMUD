{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Misc.Logging ( closeLogs
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
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Set
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Lens (view, views)
import Control.Lens.Operators ((^.))
import Control.Monad ((>=>), forM_, forever, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((<.>), (</>), replaceExtension)
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


initLogging :: IO (LogService, LogService)
initLogging = do
    updateGlobalLogger rootLoggerName removeHandler
    (nq, eq) <- (,) <$> newTQueueIO <*> newTQueueIO
    (na, ea) <- (,) <$> spawnLogger noticeLogFile NOTICE "currymud.notice" noticeM nq
                    <*> spawnLogger errorLogFile  ERROR  "currymud.error"  errorM  eq
    return ((na, nq), (ea, eq))


type LogName    = T.Text
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> IO LogAsync
spawnLogger fn p (T.unpack -> ln) f q =
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
        rotateIt = mkDateTimeTxt >>= \(T.unpack -> date, T.unpack . T.replace ":" "-" -> time) -> do
            atomically . writeTQueue q . LogMsg $ "Mud.Logging spawnLogger rotateLog rotateIt: log rotated."
            close gh
            renameFile fn . replaceExtension fn . concat $ [ date, "_", time, ".log" ]
            loop =<< initLog


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
        threadDelay $ logRotationDelay * 10 ^ 6
        atomically . writeTQueue q $ RotateLog


initPlaLog :: Id -> Sing -> MudStack ()
initPlaLog i n@(T.unpack -> n') = do
    q <- liftIO newTQueueIO
    a <- liftIO . spawnLogger (logDir </> n' <.> "log") INFO ("currymud." <> n) infoM $ q
    setLogService i (a, q)


-- ==================================================
-- Stopping/closing logs:


stopLog :: LogQueue -> MudStack ()
stopLog = liftIO . atomically . flip writeTQueue StopLog


closePlaLog :: Id -> MudStack ()
closePlaLog = flip doIfLogging stopLog


doIfLogging :: Id -> (LogQueue -> MudStack ()) -> MudStack ()
doIfLogging i f = getState >>= \(view plaLogTbl -> plt) -> maybeVoid (f . snd) . IM.lookup i $ plt


closeLogs :: MudStack ()
closeLogs = ask >>= \md -> do
    logNotice "Mud.Logging" "closeLogs" "closing the logs."
    let (na, nq) = md^.noticeLog
        (ea, eq) = md^.errorLog
    (as, qs) <- unzip . views plaLogTbl IM.elems <$> getState
    liftIO $ do
        atomically . mapM_ (`writeTQueue` StopLog) $ nq : eq : qs
        mapM_ wait $ na : ea : as
        removeAllHandlers


-- ==================================================
-- Logging messages:


registerMsg :: T.Text -> LogQueue -> MudStack ()
registerMsg msg = liftIO . atomically . flip writeTQueue (LogMsg msg)


logNotice :: T.Text -> T.Text -> T.Text -> MudStack ()
logNotice modName (dblQuote -> funName) msg = onEnv $ helper . snd . view noticeLog
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg ])


logError :: T.Text -> MudStack ()
logError msg = onEnv $ registerMsg msg . snd . view errorLog


logExMsg :: T.Text -> T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg modName (dblQuote -> funName) msg (dblQuote . showText -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", msg, ". ", e ]


logIOEx :: T.Text -> T.Text -> IOException -> MudStack ()
logIOEx modName (dblQuote -> funName) (dblQuote . showText -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", e ]


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols modName (dblQuote -> funName) (dblQuote . showText -> e) =
    let msg = T.concat [ modName, " ", funName, ": ", e ] in logError msg >> wrapSend mq cols msg


logPla :: T.Text -> T.Text -> Id -> T.Text -> MudStack ()
logPla modName (dblQuote -> funName) i msg =
    doIfLogging i . registerMsg . T.concat $ [ modName, " ", funName, ": ", msg ]


logPlaExec :: T.Text -> CmdName -> Id -> MudStack ()
logPlaExec modName cn i = logPla modName cn i $ "executed " <> dblQuote cn <> "."


logPlaExecArgs :: T.Text -> CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs modName cn as i = logPla modName cn i $ "executed " <> helper <> "."
  where
    helper | null as   = dblQuote cn <> " with no arguments"
           | otherwise = dblQuote . T.unwords $ cn : as


logPlaOut :: T.Text -> CmdName -> Id -> [T.Text] -> MudStack ()
logPlaOut modName (dblQuote -> cn) i (T.intercalate " / " -> msgs) = helper . getLogQueue i =<< getState
  where
    helper = registerMsg (T.concat [ modName, " ", cn, " (output): ", msgs ])


massLogPla :: T.Text -> T.Text -> T.Text -> MudStack ()
massLogPla modName (dblQuote -> funName) msg = liftIO . atomically . helperSTM =<< getState
  where
    helperSTM (views plaLogTbl (map snd . IM.elems) -> qs) =
        forM_ qs (`writeTQueue` (LogMsg . T.concat $ [ modName, " ", funName, ": ", msg ]))
