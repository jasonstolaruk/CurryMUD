{-# LANGUAGE TupleSections #-}

module Mud.Data.State.Util.Destroy where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Hierarchy
import           Mud.Data.State.Util.Misc
import           Mud.Threads.Effect
import           Mud.Threads.Misc
import           Mud.Util.Operators

import           Control.Lens (at, to)
import           Control.Lens.Operators ((.~), (%~), (^.))
import           Control.Monad (forM_, mapM_, when)
import qualified Data.IntMap.Strict as IM (map)
import           Data.List (delete)
import           GHC.Stack (HasCallStack)


type DoOrDon'tStopDecomposers = Bool


destroy :: HasCallStack => Inv -> MudStack ()
destroy = destroyer True


destroyDisintegratedCorpse :: HasCallStack => Id -> MudStack ()
destroyDisintegratedCorpse = destroyer False . pure


destroyer :: HasCallStack => DoOrDon'tStopDecomposers -> Inv -> MudStack ()
destroyer b is = let helper ms = (ms, ) . pure $ do mapM_ (ms |&|) [ stopBiodegraders
                                                                   , stopCorpseDecomposers
                                                                   , stopLightTimers ]
                                                    ((>>) <$> mapM_ stopEffects <*> tweak . destroyHelper) is
                 in modifyStateSeq helper
  where
    stopBiodegraders      ms = forM_ (filter (`hasObjId` ms) is) $ maybeThrowDeath . (`getObjBiodegAsync` ms)
    stopCorpseDecomposers ms = when b . forM_ is $ \i -> ms^.corpseDecompAsyncTbl.at i.to maybeThrowDeath
    stopLightTimers       ms =          forM_ is $ \i -> ms^.lightAsyncTbl       .at i.to maybeThrowDeath


destroyHelper :: HasCallStack => Inv -> MudState -> MudState -- The caller is responsible for stopping the biodegrader, corpse decomposer, light timer, and effects.
destroyHelper = flip . foldr $ helper
  where
    helper i ms = case getType i ms of
      ArmType        -> upd ms [ destroyEnt, destroyObj, destroyArm,   rest ]
      ClothType      -> upd ms [ destroyEnt, destroyObj, destroyCloth, rest ]
      ConType        -> upd ms [ destroyContents
                               , destroyEnt
                               , destroyObj
                               , destroyInv
                               , destroyCoins
                               , destroyCloth
                               , destroyCon
                               , rest ]
      CorpseType     -> upd ms [ destroyContents
                               , destroyEnt
                               , destroyObj
                               , destroyInv
                               , destroyCoins
                               , destroyCloth
                               , destroyCon
                               , destroyCorpse
                               , rest ]
      FoodType       -> upd ms [ destroyEnt, destroyObj, destoryFood,                      rest ]
      HolySymbolType -> upd ms [ destroyEnt, destroyObj, destroyHolySymbol,                rest ]
      LightType      -> upd ms [ destroyEnt, destroyObj, destroyLight,                     rest ]
      ObjType        -> upd ms [ destroyEnt, destroyObj,                                   rest ]
      VesselType     -> upd ms [ destroyEnt, destroyObj, destroyVessel, destroyVesselHoly, rest ]
      WpnType        -> upd ms [ destroyEnt, destroyObj, destroyWpn,                       rest ]
      WritableType   -> upd ms [ destroyEnt, destroyObj, destroyWritable,                  rest ]
      -----
      NpcType        -> ms
      PlaType        -> ms
      RmType         -> ms
      where
        destroyArm          = armTbl       .at i .~ Nothing
        destroyCloth        = clothTbl     .at i .~ Nothing
        destroyCoins        = coinsTbl     .at i .~ Nothing
        destroyCon          = conTbl       .at i .~ Nothing
        destroyCorpse       = corpseTbl    .at i .~ Nothing
        destroyEnt          = flip upd [ durationalEffectTbl.at i .~ Nothing
                                       , entTbl             .at i .~ Nothing
                                       , pausedEffectTbl    .at i .~ Nothing ]
        destoryFood         = foodTbl      .at i .~ Nothing
        destroyHolySymbol   = holySymbolTbl.at i .~ Nothing
        destroyInv          = invTbl       .at i .~ Nothing
        destroyLight        = lightTbl     .at i .~ Nothing
        destroyObj          = objTbl       .at i .~ Nothing
        destroyType         = typeTbl      .at i .~ Nothing
        destroyVessel       = vesselTbl    .at i .~ Nothing
        destroyVesselHoly   | getVesselIsHoly i ms = destroyHolySymbol
                            | otherwise            = id
        destroyWpn          = wpnTbl       .at i .~ Nothing
        destroyWritable     = writableTbl  .at i .~ Nothing
        destroyContents ms' = foldr helper ms' . getInv i $ ms'
        rest                = flip upd [ destroyType, invTbl %~ IM.map (i `delete`) ]
