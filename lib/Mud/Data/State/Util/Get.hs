{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Data.State.Util.Get where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Applicative ((<$>))
import Control.Lens.Getter (view)
import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as IM (IntMap)


getEntTbl :: MudStack (IM.IntMap Ent)
getEntTbl = view entTbl <$> readWSTMVar


getEnt :: Id -> MudStack Ent
getEnt i = (! i) <$> getEntTbl


getEntSing :: Id -> MudStack Sing
getEntSing i = view sing <$> getEnt i


getEntSing' :: Id -> MudStack (WorldState, Sing)
getEntSing' i = readWSTMVar >>= \ws ->
    return (ws, view sing . (! i) . view entTbl $ ws)


-----


getObjTbl :: MudStack (IM.IntMap Obj)
getObjTbl = view objTbl <$> readWSTMVar


getObj :: Id -> MudStack Obj
getObj i = (! i) <$> getObjTbl


-----


getClothTbl :: MudStack (IM.IntMap Cloth)
getClothTbl = view clothTbl <$> readWSTMVar


getCloth :: Id -> MudStack Cloth
getCloth i = (! i) <$> getClothTbl


-----


getInvTbl :: MudStack (IM.IntMap Inv)
getInvTbl = view invTbl <$> readWSTMVar


getInv :: Id -> MudStack Inv
getInv i = (! i) <$> getInvTbl


-----


getCoinsTbl :: MudStack (IM.IntMap Coins)
getCoinsTbl = view coinsTbl <$> readWSTMVar


getCoins :: Id -> MudStack Coins
getCoins i = (! i) <$> getCoinsTbl


-----


getConTbl :: MudStack (IM.IntMap Con)
getConTbl = view conTbl <$> readWSTMVar


getCon :: Id -> MudStack Con
getCon i = (! i) <$> getConTbl


-----


getWpnTbl :: MudStack (IM.IntMap Wpn)
getWpnTbl = view wpnTbl <$> readWSTMVar


getWpn :: Id -> MudStack Wpn
getWpn i = (! i) <$> getWpnTbl


-----


getArmTbl :: MudStack (IM.IntMap Arm)
getArmTbl = view armTbl <$> readWSTMVar


getArm :: Id -> MudStack Arm
getArm i = (! i) <$> getArmTbl


-----


getEqTbl :: MudStack (IM.IntMap EqMap)
getEqTbl = view eqTbl <$> readWSTMVar


getEq :: Id -> MudStack EqMap
getEq i = (! i) <$> getEqTbl


-----


getMobTbl :: MudStack (IM.IntMap Mob)
getMobTbl = view mobTbl <$> readWSTMVar


getMob :: Id -> MudStack Mob
getMob i = (! i) <$> getMobTbl


-----


getPCTbl :: MudStack (IM.IntMap PC)
getPCTbl = view pcTbl <$> readWSTMVar


getPC :: Id -> MudStack PC
getPC i = (! i) <$> getPCTbl


-----


getRmTbl :: MudStack (IM.IntMap Rm)
getRmTbl = view rmTbl <$> readWSTMVar


getRm :: Id -> MudStack Rm
getRm i = (! i) <$> getRmTbl


-----


getTypeTbl :: MudStack (IM.IntMap Type)
getTypeTbl = view typeTbl <$> readWSTMVar


getType :: Id -> MudStack Type
getType i = (! i) <$> getTypeTbl
