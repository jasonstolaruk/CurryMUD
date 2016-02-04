{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.OpListMonitor (threadOpListMonitor) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Lens (view)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad ((>=>), forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.OpListMonitor"


-- ==================================================


threadOpListMonitor :: MudStack ()
threadOpListMonitor = handle (threadExHandler "operation list monitor") $ do
    setThreadType OpListMonitor
    logNotice "threadOpListMonitor" "operation list monitor started."
    forever loop `catch` die Nothing "operation list monitor"


loop :: MudStack ()
loop = view opList <$> getState >>= \case
  [] -> delay
  _  -> (helper |&| modifyState >=> mapM_ onNewThread) >> delay
  where
    delay     = liftIO . threadDelay $ 10 ^ 5 -- 0.1 secs
    helper ms = (ms & opList .~ [], ms^.opList)
