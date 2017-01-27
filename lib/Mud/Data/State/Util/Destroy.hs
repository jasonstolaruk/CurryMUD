{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Destroy where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Data.State.Util.Misc
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((%~), (.~))
import Data.List (delete)
import qualified Data.IntMap.Strict as IM (map)


destroy :: Inv -> MudStack ()
destroy is = sequence_ [ stopBiodegraders, mapM_ stopEffects is, tweak . destroyHelper $ is ]
  where
    stopBiodegraders = getState >>= \ms -> let f = maybeVoid throwDeath . (`getObjBiodegAsync` ms)
                                           in mapM_ f . filter (`hasObjId` ms) $ is


destroyHelper :: Inv -> MudState -> MudState -- The caller is responsible for stopping the biodegrader and effects.
destroyHelper = flip . foldr $ helper
  where
    helper i ms = case getType i ms of
      ArmType        -> upd ms [ destroyEnt, destroyObj, destroyArm,   rest ]
      ClothType      -> upd ms [ destroyEnt, destroyObj, destroyCloth, rest ]
      ConType        -> upd ms [ destroyCont
                               , destroyEnt
                               , destroyObj
                               , destroyInv
                               , destroyCoins
                               , destroyCloth
                               , destroyCon
                               , rest ]
      CorpseType     -> upd ms [ destroyCont
                               , destroyEnt
                               , destroyObj
                               , destroyInv
                               , destroyCoins
                               , destroyCloth
                               , destroyCon
                               , destroyCorpse
                               , rest ]
      FoodType       -> upd ms [ destroyEnt, destroyObj, destoryFood,       rest ]
      HolySymbolType -> upd ms [ destroyEnt, destroyObj, destroyHolySymbol, rest ]
      ObjType        -> upd ms [ destroyEnt, destroyObj,                    rest ]
      VesselType     -> upd ms [ destroyEnt, destroyObj, destroyVessel,     rest ]
      WpnType        -> upd ms [ destroyEnt, destroyObj, destroyWpn,        rest ]
      WritableType   -> upd ms [ destroyEnt, destroyObj, destroyWritable,   rest ]
      _              -> ms
      where
        destroyArm        = armTbl       .at i .~ Nothing
        destroyCloth      = clothTbl     .at i .~ Nothing
        destroyCoins      = coinsTbl     .at i .~ Nothing
        destroyCon        = conTbl       .at i .~ Nothing
        destroyCorpse     = corpseTbl    .at i .~ Nothing
        destroyEnt        = flip upd [ entTbl.at i .~ Nothing, pausedEffectsTbl.at i .~ Nothing ]
        destoryFood       = foodTbl      .at i .~ Nothing
        destroyHolySymbol = holySymbolTbl.at i .~ Nothing
        destroyInv        = invTbl       .at i .~ Nothing
        destroyObj        = objTbl       .at i .~ Nothing
        destroyType       = typeTbl      .at i .~ Nothing
        destroyVessel     = vesselTbl    .at i .~ Nothing
        destroyWpn        = wpnTbl       .at i .~ Nothing
        destroyWritable   = writableTbl  .at i .~ Nothing
        destroyCont ms'   = foldr helper ms' . getInv i $ ms'
        rest              = flip upd [ destroyType, invTbl %~ IM.map (i `delete`) ]
