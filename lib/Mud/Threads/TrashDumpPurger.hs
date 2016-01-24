{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.TrashDumpPurger ( threadTrashDumpPurger ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.TheWorld.Zones.AdminZoneIds (iTrashDump)
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Lens (at)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (map)


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
    tweak $ \ms -> (foldr helper ms . getInv iTrashDump $ ms) & coinsTbl.ind iTrashDump .~ mempty
  where
    helper i ms = case getType i ms of
      ArmType   -> ms & destroyEnt & destroyObj & destroyArm   & rest
      ClothType -> ms & destroyEnt & destroyObj & destroyCloth & rest
      ConType   -> (foldr helper ms . getInv i $ ms) & destroyEnt
                                                     & destroyObj
                                                     & destroyInv
                                                     & destroyCoins
                                                     & destroyCloth
                                                     & destroyCon
                                                     & rest
      ObjType   -> ms & destroyEnt & destroyObj              & rest
      WpnType   -> ms & destroyEnt & destroyObj & destroyWpn & rest
      _         -> ms
      where
        destroyArm   = armTbl  .at i .~ Nothing
        destroyCloth = clothTbl.at i .~ Nothing
        destroyCoins = coinsTbl.at i .~ Nothing
        destroyCon   = conTbl  .at i .~ Nothing
        destroyEnt   = entTbl  .at i .~ Nothing
        destroyInv   = invTbl  .at i .~ Nothing
        destroyObj   = objTbl  .at i .~ Nothing
        destroyType  = typeTbl .at i .~ Nothing
        destroyWpn   = wpnTbl  .at i .~ Nothing
        rest ms'     = ms' & destroyType & invTblHelper
        invTblHelper = invTbl %~ IM.map (i `delete`)
