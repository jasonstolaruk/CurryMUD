{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Service.Logging (initRestServiceLogging) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Locks
import Mud.Misc.Logging (spawnLogger)
import Mud.TopLvlDefs.FilePaths

import Control.Concurrent.STM.TQueue (newTQueueIO)
import GHC.Stack (HasCallStack)
import System.Log (Priority(..))
import System.Log.Logger (noticeM)


initRestServiceLogging :: HasCallStack => DoOrDon'tLog -> IO (Maybe LogService)
initRestServiceLogging DoLog = ((,,) <$> mkLock
                                     <*> mkMudFilePath restServiceLogFileFun
                                     <*> newTQueueIO) >>= \(lock, logFile, q) ->
    Just . (, q) <$> spawnLogger logFile NOTICE "currymud.service" noticeM q lock
initRestServiceLogging Don'tLog = return Nothing
