module Mud.Data.State.Util.Put where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((&), (.~), (?~))


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = modifyState helper
  where
    helper ms = let ms' = ms & armTbl.at  i ?~ a
                             & entTbl.at  i ?~ e
                             & objTbl.at  i ?~ o
                             & typeTbl.at i ?~ ArmType
                in (ms', ())


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = modifyState helper
  where
    helper ms = let ms' = ms & clothTbl.at i ?~ c
                             & entTbl.at   i ?~ e
                             & objTbl.at   i ?~ o
                             & typeTbl.at  i ?~ ClothType
                in (ms', ())


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = modifyState helper
  where
    helper ms = let ms' = ms & clothTbl.at i .~ mc
                             & coinsTbl.at i ?~ coi
                             & conTbl.at   i ?~ con
                             & entTbl.at   i ?~ e
                             & invTbl.at   i ?~ is
                             & objTbl.at   i ?~ o
                             & typeTbl.at  i ?~ ConType
                in (ms', ())


-----


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = modifyState helper
  where
    helper ms = let ms' = ms & coinsTbl.at i ?~ c
                             & entTbl.at   i ?~ e
                             & eqTbl.at    i ?~ em
                             & invTbl.at   i ?~ is
                             & mobTbl.at   i ?~ m
                             & typeTbl.at  i ?~ MobType
                in (ms', ())


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = modifyState helper
  where
    helper ms = let ms' = ms & entTbl.at  i ?~ e
                             & objTbl.at  i ?~ o
                             & typeTbl.at i ?~ ObjType
                in (ms', ())


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = modifyState helper
  where
    helper ms = let ms' = ms & coinsTbl.at i ?~ c
                             & entTbl.at   i ?~ e
                             & eqTbl.at    i ?~ em
                             & invTbl.at   i ?~ is
                             & mobTbl.at   i ?~ m
                             & pcTbl.at    i ?~ p
                             & typeTbl.at  i ?~ PCType
                in (ms', ())


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = modifyState helper
  where
    helper ms = let ms' = ms & coinsTbl.at i ?~ c
                             & invTbl.at   i ?~ is
                             & rmTbl.at    i ?~ r
                             & typeTbl.at  i ?~ RmType
                in (ms', ())


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = modifyState helper
  where
    helper ms = let ms' = ms & entTbl.at  i ?~ e
                             & objTbl.at  i ?~ o
                             & typeTbl.at i ?~ WpnType
                             & wpnTbl.at  i ?~ w
                in (ms', ())
