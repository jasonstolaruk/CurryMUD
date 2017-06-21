{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Service.Logging ( closeRestServiceLog
                           , initRestServiceLogging
                           , logRestService
                           , logRestServiceSimple ) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Locks
import           Mud.Misc.Logging (DoOrDon'tLog, spawnLogger)
import           Mud.TopLvlDefs.FilePaths
import           Mud.Util.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent.Async (wait)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue)
import           Control.Lens (views)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack (HasCallStack)
import           System.Log (Priority(..))
import           System.Log.Logger (noticeM)


closeRestServiceLog :: MudState -> IO ()
closeRestServiceLog ms = views restServiceLogService helper ms
  where
    helper (Just (la, lq)) = do logRestServiceSimple ms "closeRestServiceLog" "closing the log."
                                writeLog lq StopLog
                                wait la
    helper Nothing         = unit


-----


doIfLogging :: HasCallStack => MudState -> (LogQueue -> IO ()) -> IO ()
doIfLogging ms f = views restServiceLogService (maybeVoid (f . snd)) ms


-----


initRestServiceLogging :: HasCallStack => DoOrDon'tLog -> IO (Maybe LogService)
initRestServiceLogging True = ((,,) <$> mkLock
                                    <*> mkMudFilePath restServiceLogFileFun
                                    <*> newTQueueIO) >>= \(lock, logFile, q) ->
    Just . (, q) <$> spawnLogger logFile NOTICE "currymud.service" noticeM q lock
initRestServiceLogging False = return Nothing


-----


logRestService :: HasCallStack => MudState -> Text -> Maybe Text -> Maybe Id -> Text -> IO ()
logRestService ms funName un i msg = doIfLogging ms . registerMsg $ msg'
  where
    msg'         = T.concat [ bracketQuote funName, " ", f un dblQuote, f i (parensQuote . showTxt), msg ]
    f (Just x) g = spcR . g $ x
    f Nothing  _ = ""


logRestServiceSimple :: HasCallStack => MudState -> Text -> Text -> IO ()
logRestServiceSimple ms fn = logRestService ms fn Nothing Nothing


-----


registerMsg :: HasCallStack => Text -> LogQueue -> IO ()
registerMsg msg = flip writeLog (LogMsg msg)


writeLog :: HasCallStack => LogQueue -> LogCmd -> IO ()
writeLog lq = atomically . writeTQueue lq
