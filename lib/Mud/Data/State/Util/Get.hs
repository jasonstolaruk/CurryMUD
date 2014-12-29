{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Get where

import Mud.Data.State.State
import Mud.Data.State.Util.STM
import Mud.Util

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Lens.Getter (view)
import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as IM (IntMap)


getEntTbl :: MudStack (IM.IntMap Ent)
getEntTbl = view entTbl <$> readWSTMVar


getEnt :: Id -> MudStack Ent
getEnt i = (! i) <$> getEntTbl


getEnt' :: Id -> MudStack (WorldState, Ent)
getEnt' i = readWSTMVar >>= \ws@((! i) . view entTbl -> e) ->
    return (ws, e)


getEntSing :: Id -> MudStack Sing
getEntSing i = view sing <$> getEnt i


getEntSing' :: Id -> MudStack (WorldState, Sing)
getEntSing' i = readWSTMVar >>= \ws@(view sing . (! i) . view entTbl -> s) ->
    return (ws, s)


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


getInvCoins :: Id -> MudStack (Inv, Coins)
getInvCoins i = getInvCoinsHelper i <$> readWSTMVar


getInvCoinsHelper :: Id -> WorldState -> (Inv, Coins)
getInvCoinsHelper i = (((! i) . view invTbl) *** ((! i) . view coinsTbl)) . dup


getInvCoins' :: Id -> MudStack (WorldState, (Inv, Coins))
getInvCoins' i = readWSTMVar >>= \ws@(getInvCoinsHelper i -> invCoins) ->
    return (ws, invCoins)


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


getEq' :: Id -> MudStack (WorldState, EqMap)
getEq' i = readWSTMVar >>= \ws@((! i) . view eqTbl -> em) ->
    return (ws, em)


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


getPCIntroduced :: Id -> MudStack [Sing]
getPCIntroduced i = view introduced <$> getPC i


getPCRmId :: Id -> MudStack Id
getPCRmId i = view rmId <$> getPC i


getPCRmId' :: Id -> MudStack (WorldState, Id)
getPCRmId' i = readWSTMVar >>= \ws@(view rmId . (! i) . view pcTbl -> ri) ->
    return (ws, ri)


getPCRm :: Id -> MudStack Rm
getPCRm i = getPCRmId' i >>= \(ws, ri) ->
    return . (! ri) . view rmTbl $ ws


getPCRm' :: Id -> MudStack (WorldState, Rm)
getPCRm' i = getPCRmId' i >>= \(ws, ri) ->
    return (ws, (! ri) . view rmTbl $ ws)


getPCRmIdRm :: Id -> MudStack (Id, Rm)
getPCRmIdRm i = getPCRmId' i >>= \(ws, ri) ->
    return (ri, (! ri) . view rmTbl $ ws)


getPCRmIdRm' :: Id -> MudStack (WorldState, (Id, Rm))
getPCRmIdRm' i = getPCRmId' i >>= \(ws, ri) ->
    return (ws, (ri, (! ri) . view rmTbl $ ws))


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
