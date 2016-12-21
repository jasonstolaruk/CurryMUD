{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Destroy where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((%~), (&), (.~))
import Data.List (delete)
import qualified Data.IntMap.Lazy as IM (map)


destroy :: Inv -> MudStack ()
destroy is = sequence_ [ stopBiodegraders, tweak . destroyHelper $ is ]
  where
    stopBiodegraders = getState >>= \ms -> let f = maybeVoid throwDeath . (`getObjBiodegAsync` ms)
                                           in mapM_ f . filter (`hasObjId` ms) $ is


destroyHelper :: Inv -> MudState -> MudState
destroyHelper = flip . foldr $ helper
  where
    helper i ms = case getType i ms of
      ArmType      -> ms & destroyEnt & destroyObj & destroyArm   & rest
      ClothType    -> ms & destroyEnt & destroyObj & destroyCloth & rest
      ConType      -> destroyCont & destroyEnt
                                  & destroyObj
                                  & destroyInv
                                  & destroyCoins
                                  & destroyCloth
                                  & destroyCon
                                  & rest
      CorpseType   -> destroyCont & destroyEnt
                                  & destroyObj
                                  & destroyInv
                                  & destroyCoins
                                  & destroyCloth
                                  & destroyCon
                                  & destroyCorpse
                                  & rest
      FoodType     -> ms & destroyEnt & destroyObj & destoryFood     & rest
      ObjType      -> ms & destroyEnt & destroyObj                   & rest
      VesselType   -> ms & destroyEnt & destroyObj & destroyVessel   & rest
      WpnType      -> ms & destroyEnt & destroyObj & destroyWpn      & rest
      WritableType -> ms & destroyEnt & destroyObj & destroyWritable & rest
      _            -> ms
      where
        destroyArm      = armTbl     .at i .~ Nothing
        destroyCloth    = clothTbl   .at i .~ Nothing
        destroyCoins    = coinsTbl   .at i .~ Nothing
        destroyCon      = conTbl     .at i .~ Nothing
        destroyCorpse   = corpseTbl  .at i .~ Nothing
        destroyEnt      = entTbl     .at i .~ Nothing
        destoryFood     = foodTbl    .at i .~ Nothing
        destroyInv      = invTbl     .at i .~ Nothing
        destroyObj      = objTbl     .at i .~ Nothing
        destroyType     = typeTbl    .at i .~ Nothing
        destroyVessel   = vesselTbl  .at i .~ Nothing
        destroyWpn      = wpnTbl     .at i .~ Nothing
        destroyWritable = writableTbl.at i .~ Nothing
        destroyCont     = foldr helper ms . getInv i $ ms
        rest ms'        = ms' & destroyType & invTbl %~ IM.map (i `delete`)
