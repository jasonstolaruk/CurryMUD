{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Put where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((.~))
import Data.Text (Text)


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = tweaks [ armTbl .ind i .~ a
                        , entTbl .ind i .~ e
                        , objTbl .ind i .~ o
                        , typeTbl.ind i .~ ArmType ]


-----


putChan :: Id -> Chan -> MudStack ()
putChan i c = tweak $ chanTbl.ind i .~ c


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = tweaks [ clothTbl.ind i .~ c
                          , entTbl  .ind i .~ e
                          , objTbl  .ind i .~ o
                          , typeTbl .ind i .~ ClothType ]


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = tweaks [ clothTbl.at  i .~ mc
                                    , coinsTbl.ind i .~ coi
                                    , conTbl  .ind i .~ con
                                    , entTbl  .ind i .~ e
                                    , invTbl  .ind i .~ is
                                    , objTbl  .ind i .~ o
                                    , typeTbl .ind i .~ ConType ]


-----


putNpc :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putNpc i e is c em m = tweaks [ coinsTbl.ind i .~ c
                              , entTbl  .ind i .~ e
                              , eqTbl   .ind i .~ em
                              , invTbl  .ind i .~ is
                              , mobTbl  .ind i .~ m
                              , typeTbl .ind i .~ NpcType ]


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = tweaks [ entTbl .ind i .~ e
                      , objTbl .ind i .~ o
                      , typeTbl.ind i .~ ObjType ]


-----


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> RndmNamesTbl -> TeleLinkTbl -> PC -> MudStack ()
putPC i e is c em m r t p = tweaks [ coinsTbl        .ind i .~ c
                                   , entTbl          .ind i .~ e
                                   , eqTbl           .ind i .~ em
                                   , invTbl          .ind i .~ is
                                   , mobTbl          .ind i .~ m
                                   , pcTbl           .ind i .~ p
                                   , rndmNamesMstrTbl.ind i .~ r
                                   , teleLinkMstrTbl .ind i .~ t
                                   , typeTbl         .ind i .~ PCType ]


-----


putPla :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> RndmNamesTbl -> TeleLinkTbl -> PC -> Pla -> MudStack ()
putPla i e is c em m r t pc pla = tweaks [ coinsTbl        .ind i .~ c
                                         , entTbl          .ind i .~ e
                                         , eqTbl           .ind i .~ em
                                         , invTbl          .ind i .~ is
                                         , mobTbl          .ind i .~ m
                                         , pcTbl           .ind i .~ pc
                                         , plaTbl          .ind i .~ pla
                                         , rndmNamesMstrTbl.ind i .~ r
                                         , teleLinkMstrTbl .ind i .~ t
                                         , typeTbl         .ind i .~ PCType ]


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = tweaks [ coinsTbl.ind i .~ c
                        , invTbl  .ind i .~ is
                        , rmTbl   .ind i .~ r
                        , typeTbl .ind i .~ RmType ]


-----


putRmTeleName :: Id -> Text -> MudStack ()
putRmTeleName i tn = tweak $ rmTeleNameTbl.ind i .~ tn


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = tweaks [ entTbl .ind i .~ e
                        , objTbl .ind i .~ o
                        , typeTbl.ind i .~ WpnType
                        , wpnTbl .ind i .~ w ]


-----


putWritable :: Id -> Ent -> Obj -> Writable -> MudStack ()
putWritable i e o w = tweaks [ entTbl     .ind i .~ e
                             , objTbl     .ind i .~ o
                             , typeTbl    .ind i .~ WritableType
                             , writableTbl.ind i .~ w ]

