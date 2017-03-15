{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.TrashDumpPurger (threadTrashDumpPurger) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Destroy
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.AdminZoneIds (iTrashDump)
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Exception.Lifted (catch, handle)
import Control.Lens.Operators ((.~))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.TrashDumpPurger"


-- ==================================================


threadTrashDumpPurger :: MudStack ()
threadTrashDumpPurger = handle (threadExHandler Nothing "trash dump purger") $ do
    setThreadType TrashDumpPurger
    logNotice "threadTrashDumpPurger" "trash dump purger started."
    let loop = sequence_ [ liftIO . delaySecs $ trashDumpPurgerDelay, purgeTrashDump ]
    forever loop `catch` die Nothing "trash dump purger"


purgeTrashDump :: MudStack ()
purgeTrashDump = getState >>= \ms -> do logNotice "purgeTrashDump" "purging the trash dump."
                                        destroy . filter (not . (`isPla` ms)) . getInv iTrashDump $ ms
                                        tweak $ coinsTbl.ind iTrashDump .~ mempty
