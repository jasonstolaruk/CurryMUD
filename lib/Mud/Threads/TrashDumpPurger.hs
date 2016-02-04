{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Lens.Operators ((&), (.~))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.TrashDumpPurger"


-- ==================================================


threadTrashDumpPurger :: MudStack ()
threadTrashDumpPurger = handle (threadExHandler "trash dump purger") $ do
    setThreadType TrashDumpPurger
    logNotice "threadTrashDumpPurger" "trash dump purger started."
    let loop = (liftIO . threadDelay $ trashDumpPurgerDelay * 10 ^ 6) >> purgeTrashDump
    forever loop `catch` die Nothing "trash dump purger"


purgeTrashDump :: MudStack ()
purgeTrashDump = do
    logNotice "purgeTrashDump" "purging the trash dump."
    tweak $ \ms -> destroy ms (getInv iTrashDump ms) & coinsTbl.ind iTrashDump .~ mempty
