module Mud.Data.State.Util.Put where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((&), (.~))


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = modifyState $ \ms -> (ms & armTbl .ind i .~ a
                                          & entTbl .ind i .~ e
                                          & objTbl .ind i .~ o
                                          & typeTbl.ind i .~ ArmType, ())


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = modifyState $ \ms -> (ms & clothTbl.ind i .~ c
                                            & entTbl  .ind i .~ e
                                            & objTbl  .ind i .~ o
                                            & typeTbl .ind i .~ ClothType, ())


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = modifyState $ \ms -> (ms & clothTbl.at  i .~ mc
                                                      & coinsTbl.ind i .~ coi
                                                      & conTbl  .ind i .~ con
                                                      & entTbl  .ind i .~ e
                                                      & invTbl  .ind i .~ is
                                                      & objTbl  .ind i .~ o
                                                      & typeTbl .ind i .~ ConType, ())


-----


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = modifyState $ \ms -> (ms & coinsTbl.ind i .~ c
                                                & entTbl  .ind i .~ e
                                                & eqTbl   .ind i .~ em
                                                & invTbl  .ind i .~ is
                                                & mobTbl  .ind i .~ m
                                                & typeTbl .ind i .~ MobType, ())


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = modifyState $ \ms -> (ms & entTbl .ind i .~ e
                                        & objTbl .ind i .~ o
                                        & typeTbl.ind i .~ ObjType, ())


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = modifyState $ \ms -> (ms & coinsTbl.ind i .~ c
                                                 & entTbl  .ind i .~ e
                                                 & eqTbl   .ind i .~ em
                                                 & invTbl  .ind i .~ is
                                                 & mobTbl  .ind i .~ m
                                                 & pcTbl   .ind i .~ p
                                                 & typeTbl .ind i .~ PCType, ())


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = modifyState $ \ms -> (ms & coinsTbl.ind i .~ c
                                          & invTbl  .ind i .~ is
                                          & rmTbl   .ind i .~ r
                                          & typeTbl .ind i .~ RmType, ())


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = modifyState $ \ms -> (ms & entTbl .ind i .~ e
                                          & objTbl .ind i .~ o
                                          & typeTbl.ind i .~ WpnType
                                          & wpnTbl .ind i .~ w, ())
