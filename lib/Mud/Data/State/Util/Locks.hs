{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Locks (withLock) where -- TODO: Add?

import Mud.Data.State.MudData

import Control.Exception.Lifted (bracket)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)


withLock :: Lock -> IO a -> IO a
withLock l f = bracket (atomically . takeTMVar $ l)
                       (\Done -> atomically . putTMVar l $ Done)
                       (\Done -> f)
