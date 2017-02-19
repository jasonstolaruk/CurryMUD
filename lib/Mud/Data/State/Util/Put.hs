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
putArm i e o a = tweaks [ activeEffectsTbl.ind i .~ []
                        , armTbl          .ind i .~ a
                        , entTbl          .ind i .~ e
                        , objTbl          .ind i .~ o
                        , pausedEffectsTbl.ind i .~ []
                        , typeTbl         .ind i .~ ArmType ]


-----


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = tweaks [ activeEffectsTbl.ind i .~ []
                          , clothTbl        .ind i .~ c
                          , entTbl          .ind i .~ e
                          , objTbl          .ind i .~ o
                          , pausedEffectsTbl.ind i .~ []
                          , typeTbl         .ind i .~ ClothType ]


-----


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Maybe Cloth -> Con -> MudStack ()
putCon i e o is coi mc con = tweaks [ activeEffectsTbl.ind i .~ []
                                    , clothTbl        .at  i .~ mc
                                    , coinsTbl        .ind i .~ coi
                                    , conTbl          .ind i .~ con
                                    , entTbl          .ind i .~ e
                                    , invTbl          .ind i .~ is
                                    , objTbl          .ind i .~ o
                                    , pausedEffectsTbl.ind i .~ []
                                    , typeTbl         .ind i .~ ConType ]


-----


putHolySymbol :: Id -> Ent -> Obj -> HolySymbol -> MudStack ()
putHolySymbol i e o h = tweaks [ activeEffectsTbl.ind i .~ []
                               , entTbl          .ind i .~ e
                               , holySymbolTbl   .ind i .~ h
                               , objTbl          .ind i .~ o
                               , pausedEffectsTbl.ind i .~ []
                               , typeTbl         .ind i .~ HolySymbolType ]


-----


putNpc :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putNpc i e is c em m = tweaks [ activeEffectsTbl.ind i .~ []
                              , coinsTbl        .ind i .~ c
                              , entTbl          .ind i .~ e
                              , eqTbl           .ind i .~ em
                              , invTbl          .ind i .~ is
                              , mobTbl          .ind i .~ m
                              , pausedEffectsTbl.ind i .~ []
                              , typeTbl         .ind i .~ NpcType ]


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = tweaks [ activeEffectsTbl.ind i .~ []
                      , entTbl          .ind i .~ e
                      , objTbl          .ind i .~ o
                      , pausedEffectsTbl.ind i .~ []
                      , typeTbl         .ind i .~ ObjType ]


-----


putPla :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> RndmNamesTbl -> TeleLinkTbl -> PC -> Pla -> MudStack ()
putPla i e is c em m r t pc pla = tweaks [ activeEffectsTbl.ind i .~ []
                                         , coinsTbl        .ind i .~ c
                                         , entTbl          .ind i .~ e
                                         , eqTbl           .ind i .~ em
                                         , invTbl          .ind i .~ is
                                         , mobTbl          .ind i .~ m
                                         , pausedEffectsTbl.ind i .~ []
                                         , pcSingTbl       .at (e^.sing) ?~ i
                                         , pcTbl           .ind i .~ pc
                                         , plaTbl          .ind i .~ pla
                                         , rndmNamesMstrTbl.ind i .~ r
                                         , teleLinkMstrTbl .ind i .~ t
                                         , typeTbl         .ind i .~ PCType ]


-----


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = tweaks [ activeEffectsTbl.ind i .~ []
                        , coinsTbl        .ind i .~ c
                        , invTbl          .ind i .~ is
                        , pausedEffectsTbl.ind i .~ []
                        , rmTbl           .ind i .~ r
                        , typeTbl         .ind i .~ RmType ]


-----


putRmTeleName :: Id -> Text -> MudStack ()
putRmTeleName i tn = tweak $ rmTeleNameTbl.ind i .~ tn


-----


putVessel :: Id -> Ent -> Obj -> Maybe VesselCont -> MudStack ()
putVessel i e o c = tweaks [ activeEffectsTbl.ind i .~ []
                           , entTbl          .ind i .~ e
                           , objTbl          .ind i .~ o
                           , pausedEffectsTbl.ind i .~ []
                           , typeTbl         .ind i .~ VesselType
                           , vesselTbl       .ind i .~ mkVessel ]
  where
    mkVessel = let mouth = calcMaxMouthfuls o
                   c'    = second (min mouth) <$> c
               in Vessel mouth c'


-----


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = tweaks [ activeEffectsTbl.ind i .~ []
                        , entTbl          .ind i .~ e
                        , objTbl          .ind i .~ o
                        , pausedEffectsTbl.ind i .~ []
                        , typeTbl         .ind i .~ WpnType
                        , wpnTbl          .ind i .~ w ]


-----


putWritable :: Id -> Ent -> Obj -> Writable -> MudStack ()
putWritable i e o w = tweaks [ activeEffectsTbl.ind i .~ []
                             , entTbl          .ind i .~ e
                             , objTbl          .ind i .~ o
                             , pausedEffectsTbl.ind i .~ []
                             , typeTbl         .ind i .~ WritableType
                             , writableTbl     .ind i .~ w ]
