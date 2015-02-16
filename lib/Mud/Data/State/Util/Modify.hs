{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Modify where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Lens (at)
import Control.Lens.Operators ((&), (?~), (^.))
import Control.Lens.Setter (ASetter, set)
import Data.IntMap.Lazy ((!))


modifyEnt :: Id -> ASetter Ent Ent a b -> b -> MudStack Ent
modifyEnt i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.entTbl) ! i
    in putTMVar t (ws & entTbl.at i ?~ e) >> return e


modifyObj :: Id -> ASetter Obj Obj a b -> b -> MudStack Obj
modifyObj i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.objTbl) ! i
    in putTMVar t (ws & objTbl.at i ?~ e) >> return e


modifyWpn :: Id -> ASetter Wpn Wpn a b -> b -> MudStack Wpn
modifyWpn i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.wpnTbl) ! i
    in putTMVar t (ws & wpnTbl.at i ?~ e) >> return e


modifyArm :: Id -> ASetter Arm Arm a b -> b -> MudStack Arm
modifyArm i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.armTbl) ! i
    in putTMVar t (ws & armTbl.at i ?~ e) >> return e


modifyMob :: Id -> ASetter Mob Mob a b -> b -> MudStack Mob
modifyMob i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.mobTbl) ! i
    in putTMVar t (ws & mobTbl.at i ?~ e) >> return e


modifyPC :: Id -> ASetter PC PC a b -> b -> MudStack PC
modifyPC i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.pcTbl) ! i
    in putTMVar t (ws & pcTbl.at i ?~ e) >> return e


modifyRm :: Id -> ASetter Rm Rm a b -> b -> MudStack Rm
modifyRm i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.rmTbl) ! i
    in putTMVar t (ws & rmTbl.at i ?~ e) >> return e
