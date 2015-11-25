{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

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
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, handle, throwIO)
import Control.Lens (both, over, view, views)
import Control.Lens.Operators ((.~))
import Control.Monad ((>=>), forM_, forever, guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.IntMap.Lazy as IM (elems, lookup)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, hPutStrLn)
import System.Directory (doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.FilePath ((<.>), (</>), replaceExtension, takeBaseName)
import System.IO (stderr)
import System.IO.Error (isAlreadyInUseError, isPermissionError)
import System.Log (Priority(..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (close, setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger (errorM, infoM, noticeM, removeAllHandlers, removeHandler, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import System.Posix.Files (fileSize, getFileStatus)


default (Int)


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Misc.Logging"


-- ==================================================
-- Starting logs:


initLogging :: ShouldLog -> Maybe Lock -> IO (Maybe LogService, Maybe LogService)
initLogging Don'tLog _                = return (Nothing, Nothing)
initLogging DoLog    (Just logExLock) = do
    updateGlobalLogger rootLoggerName removeHandler
    (eq, nq) <- (,) <$> newTQueueIO <*> newTQueueIO
    (ea, na) <- (,) <$> spawnLogger errorLogFile  ERROR  "currymud.error"  errorM  eq logExLock
                    <*> spawnLogger noticeLogFile NOTICE "currymud.notice" noticeM nq logExLock
    return (Just (ea, eq), Just (na, nq))
initLogging DoLog Nothing = patternMatchFail "initLogging" [ showText DoLog, "Nothing" ]


type LogName    = T.Text
type LoggingFun = String -> String -> IO ()


spawnLogger :: FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> Lock -> IO LogAsync
spawnLogger fn p (T.unpack -> ln) f q logExLock =
    async $ race_ ((loop =<< initLog)   `catch` loggingThreadExHandler logExLock "spawnLogger")
                  (logRotationFlagger q `catch` loggingThreadExHandler logExLock "logRotationFlagger")
  where
    initLog = p |&| fileHandler fn >=> \gh ->
        let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
        in updateGlobalLogger ln (setHandlers (pure h) . setLevel p) >> return gh
    loop gh = q |&| atomically . readTQueue >=> \case
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
            cont <- dropIrrelevantFilenames . sort <$> getDirectoryContents logDir
            let matches = filter ((== takeBaseName fn) . takeWhile (/= '.')) cont
            when (length matches >= noOfLogFiles) . removeFile . (logDir </>) . head $ matches
            loop =<< initLog


loggingThreadExHandler :: Lock -> T.Text -> SomeException -> IO ()
loggingThreadExHandler logExLock n e = guard (fromException e /= Just ThreadKilled) >> mkTimestamp >>= \ts ->
    let msg = T.concat [ ts
                       , " "
                       , "Mud.Logging loggingThreadExHandler: exception caught on logging thread "
                       , parensQuote $ "inside " <> dblQuote n
                       , ". "
                       , dblQuote . showText $ e ]
    in handle (handler msg) . withLock logExLock . T.appendFile loggingExLogFile . nl $ msg
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
    logExLock <- onEnv $ views (locks.loggingExLock) return
    q         <- liftIO newTQueueIO
    a         <- liftIO . spawnLogger (logDir </> n' <.> "log") INFO ("currymud." <> n) infoM q $ logExLock
    tweak $ plaLogTbl.ind i .~ (a, q)


-- ==================================================
-- Stopping/closing logs:


stopLog :: LogQueue -> MudStack ()
stopLog = liftIO . atomically . flip writeTQueue StopLog


closePlaLog :: Id -> MudStack ()
closePlaLog = flip doIfLogging stopLog


doIfLogging :: Id -> (LogQueue -> MudStack ()) -> MudStack ()
doIfLogging i f = getState >>= \ms -> case getType i ms of
  PCType  -> maybeVoid (f . snd) . IM.lookup i . view plaLogTbl $ ms
  NpcType -> maybeVoid (`doIfLogging` f) . getPossessor i $ ms
  t       -> patternMatchFail "doIfLogging" [ showText t ]


closeLogs :: MudStack ()
closeLogs = asks mkBindings >>= \((ea, eq), (na, nq)) -> do
    logNotice "Mud.Logging" "closeLogs" "closing the logs."
    (as, qs) <- unzip . views plaLogTbl IM.elems <$> getState
    liftIO $ do
        atomically . mapM_ (`writeTQueue` StopLog) $ eq : nq : qs
        mapM_ wait $ ea : na : as
        removeAllHandlers
  where
    mkBindings = over both fromJust . (view errorLog *** view noticeLog) . dup


-- ==================================================
-- Logging messages:


registerMsg :: T.Text -> LogQueue -> MudStack ()
registerMsg msg = liftIO . atomically . flip writeTQueue (LogMsg msg)


logNotice :: T.Text -> T.Text -> T.Text -> MudStack ()
logNotice modName (dblQuote -> funName) msg = onEnv $ maybeVoid (helper . snd) . view noticeLog
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg ])


logError :: T.Text -> MudStack ()
logError msg = onEnv $ maybeVoid (registerMsg msg . snd) . view errorLog


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
    helper | ()# as    = dblQuote cn <> " with no arguments"
           | otherwise = dblQuote . T.unwords $ cn : as


logPlaOut :: T.Text -> CmdName -> Id -> [T.Text] -> MudStack ()
logPlaOut modName (dblQuote -> cn) i (slashes -> msgs) = helper . getLogQueue i =<< getState
  where
    helper = registerMsg (T.concat [ modName, " ", cn, " (output): ", msgs ])


massLogPla :: T.Text -> T.Text -> T.Text -> MudStack ()
massLogPla modName (dblQuote -> funName) msg = liftIO . atomically . helperSTM =<< getState
  where
    helperSTM (views plaLogTbl (map snd . IM.elems) -> qs) =
        forM_ qs (`writeTQueue` (LogMsg . T.concat $ [ modName, " ", funName, ": ", msg ]))
