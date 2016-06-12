{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.FeelingTimer (threadFeelingTimer) where

-- import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
-- import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
-- import Mud.Util.Quoting
import Mud.Util.Text
-- import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (tryReadTMQueue)
-- import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (catch)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import System.Time.Utils (renderSecs)


default (Int)


-----


-- logNotice :: Text -> Text -> MudStack ()
-- logNotice = L.logNotice "Mud.Threads.FeelingTimer"
--
--
-- logPla :: Text -> Id -> Text -> MudStack ()
-- logPla = L.logPla "Mud.Threads.FeelingTimer"


-- ==================================================


threadFeelingTimer :: Id -> FeelingTag -> Seconds -> TimerQueue -> MudStack ()
threadFeelingTimer i _ dur tq = sequence_ [ setThreadType . FeelingTimer $ i
                                          , loop 0 `catch` threadExHandler ("feeling timer " <> showText i) ]
  where
    loop secs = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        tq |&| liftIO . atomically . tryReadTMQueue >=> \case
          Just Nothing | secs >= dur -> unit -- TODO: Remove the Feeling from the FeelingMap.
                       | otherwise   -> loop . succ $ secs
          Just (Just ResetTimer)     -> loop 0
          Nothing                    -> unit
