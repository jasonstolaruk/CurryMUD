{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Destroy where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Data.State.Util.Misc
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Util.Misc

import Control.Lens (at, to)
import Control.Lens.Operators ((.~), (%~), (^.))
import Control.Monad (forM_)
import Data.List (delete)
import GHC.Stack (HasCallStack)
import qualified Data.IntMap.Strict as IM (map)


destroy :: HasCallStack => Inv -> MudStack ()
destroy is = do ((>>) <$> stopBiodegraders <*> stopCorpseDecomposers) =<< getState
                mapM_ stopEffects is
                tweak . destroyHelper $ is
  where
    stopBiodegraders      ms = forM_ (filter (`hasObjId` ms) is) $ maybeVoid throwDeath . (`getObjBiodegAsync` ms)
    stopCorpseDecomposers ms = forM_ is $ \i -> ms^.corpseDecompAsyncTbl.at i.to (maybeVoid throwDeath)


destroyHelper :: HasCallStack => Inv -> MudState -> MudState -- The caller is responsible for stopping the biodegrader, corpse decomposer, and effects.
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
      FoodType       -> upd ms [ destroyEnt, destroyObj, destoryFood,       rest ]
      HolySymbolType -> upd ms [ destroyEnt, destroyObj, destroyHolySymbol, rest ]
      ObjType        -> upd ms [ destroyEnt, destroyObj,                    rest ]
      VesselType     -> upd ms [ destroyEnt, destroyObj, destroyVessel,     rest ]
      WpnType        -> upd ms [ destroyEnt, destroyObj, destroyWpn,        rest ]
      WritableType   -> upd ms [ destroyEnt, destroyObj, destroyWritable,   rest ]
      -----
      NpcType        -> ms
      PCType         -> ms
      RmType         -> ms
      where
        destroyArm          = armTbl       .at i .~ Nothing
        destroyCloth        = clothTbl     .at i .~ Nothing
        destroyCoins        = coinsTbl     .at i .~ Nothing
        destroyCon          = conTbl       .at i .~ Nothing
        destroyCorpse       = corpseTbl    .at i .~ Nothing
        destroyEnt          = flip upd [ entTbl.at i .~ Nothing, pausedEffectTbl.at i .~ Nothing ]
        destoryFood         = foodTbl      .at i .~ Nothing
        destroyHolySymbol   = holySymbolTbl.at i .~ Nothing
        destroyInv          = invTbl       .at i .~ Nothing
        destroyObj          = objTbl       .at i .~ Nothing
        destroyType         = typeTbl      .at i .~ Nothing
        destroyVessel       = vesselTbl    .at i .~ Nothing
        destroyWpn          = wpnTbl       .at i .~ Nothing
        destroyWritable     = writableTbl  .at i .~ Nothing
        destroyContents ms' = foldr helper ms' . getInv i $ ms'
        rest                = flip upd [ destroyType, invTbl %~ IM.map (i `delete`) ]
