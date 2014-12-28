{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Data.State.Util.Put where

import Mud.Data.State.State
import Mud.Data.State.Util.STM

import Control.Lens (at)
import Control.Lens.Operators ((&), (?~))


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ObjType & entTbl.at i ?~ e & objTbl.at i ?~ o


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ClothType & entTbl.at i ?~ e & objTbl.at i ?~ o & clothTbl.at i ?~ c


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Con -> MudStack ()
putCon i e o is coi con = modifyWS $ \ws ->
    ws & typeTbl.at i  ?~ ConType
       & entTbl.at i   ?~ e
       & objTbl.at i   ?~ o
       & invTbl.at i   ?~ is
       & coinsTbl.at i ?~ coi
       & conTbl.at i   ?~ con


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ WpnType & entTbl.at i ?~ e & objTbl.at i ?~ o & wpnTbl.at i ?~ w


-----


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ArmType & entTbl.at i ?~ e & objTbl.at i ?~ o & armTbl.at i ?~ a


-----


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = modifyWS $ \ws ->
    ws & typeTbl.at i  ?~ MobType
       & entTbl.at i   ?~ e
       & invTbl.at i   ?~ is
       & coinsTbl.at i ?~ c
       & eqTbl.at i    ?~ em
       & mobTbl.at i   ?~ m


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = modifyWS $ \ws ->
    ws & typeTbl.at i  ?~ PCType
       & entTbl.at i   ?~ e
       & invTbl.at i   ?~ is
       & coinsTbl.at i ?~ c
       & eqTbl.at i    ?~ em
       & mobTbl.at i   ?~ m
       & pcTbl.at i    ?~ p


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ RmType & invTbl.at i ?~ is & coinsTbl.at i ?~ c & rmTbl.at i ?~ r
