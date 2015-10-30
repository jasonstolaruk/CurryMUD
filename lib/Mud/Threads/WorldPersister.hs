{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.WorldPersister (threadWorldPersister) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Set
import Mud.Misc.Persist
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T


default (Int)


-----


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.WorldPersister"


-- ==================================================


threadWorldPersister :: MudStack ()
threadWorldPersister = handle (threadExHandler "world persister") $ do
    setThreadType WorldPersister
    logNotice "threadWorldPersister" "world persister started."
    let loop = (liftIO . threadDelay $ worldPersisterDelay * 10 ^ 6) >> persist
    forever loop `catch` die "world persister" Nothing
