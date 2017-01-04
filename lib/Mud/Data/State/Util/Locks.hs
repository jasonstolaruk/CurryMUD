{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Mud.Data.State.Util.Locks ( getLock
                                 , mkLocks
                                 , withLock ) where

import Mud.Data.State.MudData

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newTMVarIO, putTMVar, takeTMVar)
import Control.Exception.Lifted (bracket)
import Control.Lens (Getter, view)
import Control.Monad (replicateM)
import Control.Monad.Reader (asks)


getLock :: Getter Locks Lock -> MudStack Lock
getLock l = asks . view $ locks.l


mkLocks :: IO [Lock]
mkLocks = replicateM 3 . newTMVarIO $ Done


withLock :: Lock -> IO a -> IO a
withLock l f = bracket (atomically . takeTMVar $ l)
                       (\Done -> atomically . putTMVar l $ Done)
                       (\Done -> f)
