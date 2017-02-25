{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Put where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Arrow (second)
import Control.Lens (at)
import Control.Lens.Operators ((?~), (.~), (^.))
import Data.Text (Text)


-- These functions are meant to be used in zone-defining code run at server startup.


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = tweaks [ armTbl             .ind i .~ a
                        , durationalEffectTbl.ind i .~ []
                        , entTbl             .ind i .~ e
                        , objTbl             .ind i .~ o
                        , pausedEffectTbl    .ind i .~ []
                        , typeTbl            .ind i .~ ArmType ]


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = tweaks [ clothTbl           .ind i .~ c
                          , durationalEffectTbl.ind i .~ []
                          , entTbl             .ind i .~ e
                          , objTbl             .ind i .~ o
                          , pausedEffectTbl    .ind i .~ []
                          , typeTbl            .ind i .~ ClothType ]


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = tweaks [ clothTbl           .at  i .~ mc
                                    , coinsTbl           .ind i .~ coi
                                    , conTbl             .ind i .~ con
                                    , durationalEffectTbl.ind i .~ []
                                    , entTbl             .ind i .~ e
                                    , invTbl             .ind i .~ is
                                    , objTbl             .ind i .~ o
                                    , pausedEffectTbl    .ind i .~ []
                                    , typeTbl            .ind i .~ ConType ]


-----


putHolySymbol :: Id -> Ent -> Obj -> HolySymbol -> MudStack ()
putHolySymbol i e o h = tweaks [ durationalEffectTbl.ind i .~ []
                               , entTbl             .ind i .~ e
                               , holySymbolTbl      .ind i .~ h
                               , objTbl             .ind i .~ o
                               , pausedEffectTbl    .ind i .~ []
                               , typeTbl            .ind i .~ HolySymbolType ]


-----


putNpc :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putNpc i e is c em m = tweaks [ coinsTbl           .ind i .~ c
                              , durationalEffectTbl.ind i .~ []
                              , entTbl             .ind i .~ e
                              , eqTbl              .ind i .~ em
                              , invTbl             .ind i .~ is
                              , mobTbl             .ind i .~ m
                              -- TODO , npcTbl             .ind i .~ n
                              , pausedEffectTbl    .ind i .~ []
                              , typeTbl            .ind i .~ NpcType ]


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = tweaks [ durationalEffectTbl.ind i .~ []
                      , entTbl             .ind i .~ e
                      , objTbl             .ind i .~ o
                      , pausedEffectTbl    .ind i .~ []
                      , typeTbl            .ind i .~ ObjType ]


-----


putPla :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> RndmNamesTbl -> TeleLinkTbl -> PC -> Pla -> MudStack ()
putPla i e is c em m r t pc pla = tweaks [ coinsTbl           .ind i .~ c
                                         , durationalEffectTbl.ind i .~ []
                                         , entTbl             .ind i .~ e
                                         , eqTbl              .ind i .~ em
                                         , invTbl             .ind i .~ is
                                         , mobTbl             .ind i .~ m
                                         , pausedEffectTbl    .ind i .~ []
                                         , pcSingTbl          .at (e^.sing) ?~ i
                                         , pcTbl              .ind i .~ pc
                                         , plaTbl             .ind i .~ pla
                                         , rndmNamesMstrTbl   .ind i .~ r
                                         , teleLinkMstrTbl    .ind i .~ t
                                         , typeTbl            .ind i .~ PCType ]


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = tweaks [ coinsTbl           .ind i .~ c
                        , durationalEffectTbl.ind i .~ []
                        , invTbl             .ind i .~ is
                        , pausedEffectTbl    .ind i .~ []
                        , rmTbl              .ind i .~ r
                        , typeTbl            .ind i .~ RmType ]


-----


putRmTeleName :: Id -> Text -> MudStack ()
putRmTeleName i tn = tweak $ rmTeleNameTbl.ind i .~ tn


-----


putVessel :: Id -> Ent -> Obj -> Maybe VesselCont -> MudStack ()
putVessel i e o c = tweaks [ durationalEffectTbl.ind i .~ []
                           , entTbl             .ind i .~ e
                           , objTbl             .ind i .~ o
                           , pausedEffectTbl    .ind i .~ []
                           , typeTbl            .ind i .~ VesselType
                           , vesselTbl          .ind i .~ mkVessel ]
  where
    mkVessel = let mouth = calcMaxMouthfuls o
                   c'    = second (min mouth) <$> c
               in Vessel mouth c'


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = tweaks [ durationalEffectTbl.ind i .~ []
                        , entTbl             .ind i .~ e
                        , objTbl             .ind i .~ o
                        , pausedEffectTbl    .ind i .~ []
                        , typeTbl            .ind i .~ WpnType
                        , wpnTbl             .ind i .~ w ]


-----


putWritable :: Id -> Ent -> Obj -> Writable -> MudStack ()
putWritable i e o w = tweaks [ durationalEffectTbl.ind i .~ []
                             , entTbl             .ind i .~ e
                             , objTbl             .ind i .~ o
                             , pausedEffectTbl    .ind i .~ []
                             , typeTbl            .ind i .~ WritableType
                             , writableTbl        .ind i .~ w ]
