{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Destroy where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Threads.Misc

import Control.Lens (at)
import Control.Lens.Operators ((%~), (&), (.~), (<>~))
import Data.List (delete)
import qualified Data.IntMap.Lazy as IM (map)


destroy :: MudState -> Inv -> MudState
destroy = foldr helper
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
        destroyObj ms' = ms' & objTbl.at i .~ Nothing
                             & opList <>~ pure (throwWaitBiodegrader i)
        destroyType  = typeTbl .at i .~ Nothing
        destroyWpn   = wpnTbl  .at i .~ Nothing
        rest ms'     = ms' & destroyType & invTblHelper
        invTblHelper = invTbl %~ IM.map (i `delete`)
