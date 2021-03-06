{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ViewPatterns #-}

module Mud.Misc.Logging ( DoOrDon'tLog
                        , closeLogs
                        , closePlaLog
                        , initLogging
                        , initPlaLog
                        , logAndDispIOEx
                        , logErrorMsg
                        , logExMsg
                        , logIOEx
                        , logImpossible
                        , logNotice
                        , logPla
                        , logPlaExec
                        , logPlaExecArgs
                        , logPlaOut
                        , massLogPla
                        , spawnLogger
                        , writeLog ) where

import           Mud.Data.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Locks
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.TopLvlDefs.FilePaths
import           Mud.TopLvlDefs.Misc
import qualified Mud.Util.Misc as U (blowUp)
import           Mud.Util.Misc hiding (blowUp)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent.Async (AsyncCancelled(..), async, race_, wait)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import           Control.Exception (ArithException(..), IOException, SomeException, fromException)
import           Control.Exception.Lifted (catch, handle, throwIO)
import           Control.Lens (both, view, views)
import           Control.Lens.Operators ((.~), (&), (%~))
import           Control.Monad ((>=>), forM_, forever, guard, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import qualified Data.IntMap.Strict as IM (elems, lookup)
import           Data.List (sort)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (appendFile, hPutStrLn)
import           GHC.Stack (HasCallStack)
import           System.Directory (doesFileExist, getDirectoryContents, removeFile, renameFile)
import           System.FilePath ((<.>), (</>), replaceExtension, takeBaseName)
import           System.IO (stderr)
import           System.IO.Error (isAlreadyInUseError, isPermissionError)
import           System.Log (Priority(..))
import           System.Log.Formatter (simpleLogFormatter)
import           System.Log.Handler (close, setFormatter)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger (errorM, infoM, noticeM, removeAllHandlers, removeHandler, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import           System.Posix.Files (fileSize, getFileStatus)

blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Misc.Logging"

-- ==================================================
-- Starting logs:

type DoOrDon'tLog = Bool

initLogging :: HasCallStack => DoOrDon'tLog -> Maybe Lock -> IO (Maybe LogService, Maybe LogService)
initLogging False _                = return (Nothing, Nothing)
initLogging True  (Just logExLock) = do
    updateGlobalLogger rootLoggerName removeHandler
    (errorFile, noticeFile) <- (,) <$> mkMudFilePath errorLogFileFun
                                   <*> mkMudFilePath noticeLogFileFun
    (eq, nq) <- (,) <$> newTQueueIO
                    <*> newTQueueIO
    (ea, na) <- (,) <$> spawnLogger errorFile  ERROR  "currymud.error"  errorM  eq logExLock
                    <*> spawnLogger noticeFile NOTICE "currymud.notice" noticeM nq logExLock
    return ((,) (ea, eq) (na, nq) & both %~ Just)
initLogging True Nothing = blowUp "initLogging" "missing lock" ""

type LogName    = Text
type LoggingFun = String -> String -> IO ()

spawnLogger :: HasCallStack => FilePath -> Priority -> LogName -> LoggingFun -> LogQueue -> Lock -> IO LogAsync
spawnLogger fn p (T.unpack -> ln) f q logExLock =
    async $ race_ ((loop =<< initLog)   `catch` loggingThreadExHandler logExLock "spawnLogger"       )
                  (logRotationFlagger q `catch` loggingThreadExHandler logExLock "logRotationFlagger")
  where
    initLog = p |&| fileHandler fn >=> \gh -> let h = setFormatter gh . simpleLogFormatter $ "[$time $loggername] $msg"
                                              in updateGlobalLogger ln (setHandlers (pure h) . setLevel p) >> return gh
    loop gh = q |&| atomically . readTQueue >=> \case LogMsg (T.unpack -> msg) -> f ln msg >> loop gh
                                                      RotateLog                -> rotateLog gh
                                                      StopLog                  -> close     gh
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
            dir  <- mkMudFilePath logDirFun
            cont <- dropIrrelevantFiles . sort <$> getDirectoryContents dir
            let matches = filter ((== takeBaseName fn) . takeWhile (/= '.')) cont
            when (length matches >= noOfLogFiles) . removeFile . (dir </>) . head $ matches
            loop =<< initLog

loggingThreadExHandler :: HasCallStack => Lock -> Text -> SomeException -> IO ()
loggingThreadExHandler logExLock n e = guard (fromException e /= Just AsyncCancelled) >> mkTimestamp >>= \ts ->
    let txt = "Mud.Logging loggingThreadExHandler: exception caught on logging thread"
        msg = T.concat [ ts, spaced txt, parensQuote $ "inside " <> dblQuote n, ". ", dblQuote . showTxt $ e ]
    in do liftIO . printErrorMsg $ txt
          file <- mkMudFilePath loggingExLogFileFun
          handle (handler msg) . withLock logExLock . T.appendFile file . nl $ msg
  where
    handler msg ex = let showIt = T.hPutStrLn stderr msg in if | isAlreadyInUseError ex -> showIt
                                                               | isPermissionError   ex -> showIt
                                                               | otherwise              -> throwIO ex

logRotationFlagger :: HasCallStack => LogQueue -> IO ()
logRotationFlagger q = forever . sequence_ $ [ delaySecs logRotationDelay, atomically . writeTQueue q $ RotateLog ]

initPlaLog :: HasCallStack => Id -> Sing -> MudStack ()
initPlaLog i n@(T.unpack -> n') = do
    dir       <- liftIO . mkMudFilePath $ logDirFun
    logExLock <- getLock loggingExLock
    q         <- liftIO newTQueueIO
    a         <- liftIO . spawnLogger (dir </> n' <.> "log") INFO ("currymud." <> n) infoM q $ logExLock
    tweak $ plaLogTbl.ind i .~ (a, q)

-- ==================================================
-- Stopping/closing logs:

stopLog :: HasCallStack => LogQueue -> MudStack ()
stopLog = flip writeLog StopLog

closePlaLog :: HasCallStack => Id -> MudStack ()
closePlaLog = flip doIfLogging stopLog

doIfLogging :: HasCallStack => Id -> (LogQueue -> MudStack ()) -> MudStack ()
doIfLogging i f = getState >>= \ms ->
    let helper = \case PlaType -> views plaLogTbl (maybeVoid (f . snd) . (i `IM.lookup`)) ms
                       NpcType -> maybeVoid (`doIfLogging` f) . getPossessor i $ ms
                       _       -> unit
    in views typeTbl (maybeVoid helper . (i `IM.lookup`)) ms

closeLogs :: HasCallStack => MudStack ()
closeLogs = asks (errorLog `fanView` noticeLog) >>= \case
  (Just (ea, eq), Just (na, nq)) -> do logNotice "Mud.Logging" "closeLogs" "closing the logs."
                                       helper ([ ea, na ], [ eq, nq ])
  _                              -> helper mempty
  where
    helper (asyncs, queues) = views plaLogTbl (unzip . IM.elems) <$> getState >>= \(as, qs) ->
        liftIO $ do mapM_ (atomically . (`writeTQueue` StopLog)) (queues ++ qs)
                    mapM_ wait $ asyncs ++ as
                    removeAllHandlers

-- ==================================================
-- Logging messages:

registerMsg :: HasCallStack => Text -> LogQueue -> MudStack ()
registerMsg msg = flip writeLog (LogMsg msg)

logNotice :: HasCallStack => Text -> Text -> Text -> MudStack ()
logNotice modName (dblQuote -> funName) msg = maybeVoid (helper . snd) =<< asks (view noticeLog)
  where
    helper = registerMsg (T.concat [ modName, " ", funName, ": ", msg ])

logError :: HasCallStack => Text -> MudStack ()
logError msg = maybeVoid (registerMsg msg . snd) =<< asks (view errorLog)

logErrorMsg :: HasCallStack => Text -> Text -> Text -> MudStack ()
logErrorMsg modName (dblQuote -> funName) msg = logError . T.concat $ [ modName, " ", funName, ": ", msg ]

logExMsg :: HasCallStack => Text -> Text -> Text -> SomeException -> MudStack ()
logExMsg modName (dblQuote -> funName) msg (dblQuote . showTxt -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", msg, ". ", e ]

logImpossible :: HasCallStack => Text -> Text -> Text -> MudStack ()
logImpossible modName funName msg = logErrorMsg modName funName $ panicMsg |<>| msg

logIOEx :: HasCallStack => Text -> Text -> IOException -> MudStack ()
logIOEx modName (dblQuote -> funName) (dblQuote . showTxt -> e) =
    logError . T.concat $ [ modName, " ", funName, ": ", e ]

logAndDispIOEx :: HasCallStack => MsgQueue -> Cols -> Text -> Text -> IOException -> MudStack ()
logAndDispIOEx mq cols modName (dblQuote -> funName) (dblQuote . showTxt -> e) =
    let msg = T.concat [ modName, " ", funName, ": ", e ] in logError msg >> wrapSend mq cols msg

logPla :: HasCallStack => Text -> Text -> Id -> Text -> MudStack ()
logPla modName (dblQuote -> funName) i msg = doIfLogging i . registerMsg . T.concat $ [ modName, " ", funName, ": ", msg ]

logPlaExec :: HasCallStack => Text -> CmdName -> Id -> MudStack ()
logPlaExec modName cn i = logPla modName cn i . prd $ "executing " <> dblQuote cn

logPlaExecArgs :: HasCallStack => Text -> CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs modName cn as i = logPla modName cn i . prd $ "executing " <> helper
  where
    helper | ()# as    = dblQuote cn <> " with no arguments"
           | otherwise = dblQuote . T.unwords $ cn : as

logPlaOut :: HasCallStack => Text -> CmdName -> Id -> [Text] -> MudStack ()
logPlaOut modName cn i (slashes -> msgs) = logPla modName cn i $ "(output) " <> msgs

massLogPla :: HasCallStack => Text -> Text -> Text -> MudStack ()
massLogPla modName (dblQuote -> funName) msg = helper =<< getState
  where
    helper (views plaLogTbl (map snd . IM.elems) -> qs) =
        forM_ qs (`writeLog` (LogMsg . T.concat $ [ modName, " ", funName, ": ", msg ]))

writeLog :: HasCallStack => LogQueue -> LogCmd -> MudStack ()
writeLog lq = liftIO . atomically . writeTQueue lq
