{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Mud.Threads.CurryTime ( notifyTime
                             , threadCurryTime ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Output
import           Mud.Misc.ANSI
import           Mud.Misc.CurryTime
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Threads.Misc
import           Mud.Util.Misc
import           Mud.Util.Text

import           Control.Exception.Lifted (catch, handle)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.CurryTime"

-- ==================================================

threadCurryTime :: HasCallStack => MudStack ()
threadCurryTime = handle (threadExHandler Nothing "curry time") $ do
    setThreadType CurryTimeThread
    logNotice "threadCurryTime" "curry time thread started."
    let loop = sequence_ [ liftIO . delaySecs $ 1, notifyTime =<< liftIO getCurryTime ]
    forever loop `catch` die Nothing "curry time"

notifyTime :: HasCallStack => CurryTime -> MudStack ()
notifyTime = maybeVoid bcastToOutsideMobs . mkNotification

{-
It's light out from 6:00 to 17:59 (12 hours).
It's dark out from 18:00 to 5:59 (8 hours).
-}
mkNotification :: HasCallStack => CurryTime -> Maybe Text
mkNotification ct | isHourMin ct 17 45 = helper . thrice prd $ "Very soon now it will be too dark outside to see \
                                                               \without aid"
                  | isHourMin ct 17 15 = helper "You figure there's about 45 minutes of daylight left."
                  | otherwise          = Nothing
  where
    helper = Just . colorWith timeNotificationColor

isHourMin :: HasCallStack => CurryTime -> Hour -> Min -> Bool
isHourMin CurryTime { .. } h m | curryHour == h, curryMin == m = currySec == 0
                               | otherwise                     = False
