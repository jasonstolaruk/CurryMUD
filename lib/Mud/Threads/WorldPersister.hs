{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.WorldPersister (threadWorldPersister) where

import Mud.Data.State.MudData
import Mud.Misc.Persist
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Exception.Lifted (catch, handle)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.WorldPersister"


-- ==================================================


threadWorldPersister :: MudStack ()
threadWorldPersister = handle (threadExHandler Nothing "world persister") $ do
    setThreadType WorldPersister
    logNotice "threadWorldPersister" "world persister started."
    let loop = sequence_ [ liftIO . delaySecs $ worldPersisterDelay, persist ]
    forever loop `catch` die Nothing "world persister"
