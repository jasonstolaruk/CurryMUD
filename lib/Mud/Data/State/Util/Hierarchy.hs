{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Hierarchy where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get


entTypes :: [Type]
entTypes = objTypes ++ mobTypes


objTypes :: [Type]
objTypes = [ ArmType, ClothType, ConType, FoodType, ObjType, VesselType, WpnType, WritableType ]


invCoinsTypes :: [Type]
invCoinsTypes = [ ConType, RmType ] ++ mobTypes


mobTypes :: [Type]
mobTypes = [ NpcType, PCType ]


-----


hasEnt :: Type -> Bool
hasEnt = (`elem` entTypes)


hasObj :: Type -> Bool
hasObj = (`elem` objTypes)


hasInvCoins :: Type -> Bool
hasInvCoins = (`elem` invCoinsTypes)


hasMob :: Type -> Bool
hasMob = (`elem` mobTypes)


-----


hasEntId :: Id -> MudState -> Bool
hasEntId = hasHelper entTypes


hasHelper :: [Type] -> Id -> MudState -> Bool
hasHelper ts i ms = getType i ms `elem` ts


hasObjId :: Id -> MudState -> Bool
hasObjId = hasHelper objTypes


hasInvCoinsId :: Id -> MudState -> Bool
hasInvCoinsId = hasHelper invCoinsTypes


hasMobId :: Id -> MudState -> Bool
hasMobId = hasHelper mobTypes
