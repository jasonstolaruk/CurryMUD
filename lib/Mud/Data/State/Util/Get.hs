{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Data.State.Util.Get where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Applicative ((<$>))
import Control.Lens.Getter (view)
import Data.IntMap.Lazy ((!))


getEnt :: Id -> MudStack Ent
getEnt i = (! i) . view entTbl <$> readWSTMVar


-----


getObj :: Id -> MudStack Obj
getObj i = (! i) . view objTbl <$> readWSTMVar


-----


getCloth :: Id -> MudStack Cloth
getCloth i = (! i) . view clothTbl <$> readWSTMVar


-----


getInv :: Id -> MudStack Inv
getInv i = (! i) . view invTbl <$> readWSTMVar


-----


getCoins :: Id -> MudStack Coins
getCoins i = (! i) . view coinsTbl <$> readWSTMVar


-----


getCon :: Id -> MudStack Con
getCon i = (! i) . view conTbl <$> readWSTMVar


-----


getWpn :: Id -> MudStack Wpn
getWpn i = (! i) . view wpnTbl <$> readWSTMVar


-----


getArm :: Id -> MudStack Arm
getArm i = (! i) . view armTbl <$> readWSTMVar


-----


getEq :: Id -> MudStack EqMap
getEq i = (! i) . view eqTbl <$> readWSTMVar


-----


getMob :: Id -> MudStack Mob
getMob i = (! i) . view mobTbl <$> readWSTMVar


-----


getPC :: Id -> MudStack PC
getPC i = (! i) . view pcTbl <$> readWSTMVar


-----


getRm :: Id -> MudStack Rm
getRm i = (! i) . view rmTbl <$> readWSTMVar


-----


getType :: Id -> MudStack Type
getType i = (! i) . view typeTbl <$> readWSTMVar
