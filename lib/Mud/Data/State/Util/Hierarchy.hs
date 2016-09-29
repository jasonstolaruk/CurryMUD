{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Hierarchy where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get


conTypes :: [Type]
conTypes = [ ConType, CorpseType ]


entTypes :: [Type]
entTypes = objTypes ++ mobTypes


objTypes :: [Type]
objTypes = [ ArmType, ClothType, FoodType, ObjType, VesselType, WpnType, WritableType ] ++ conTypes


invCoinsTypes :: [Type]
invCoinsTypes = RmType : conTypes ++ mobTypes


mobTypes :: [Type]
mobTypes = [ NpcType, PCType ]


-----


hasCon :: Type -> Bool
hasCon = (`elem` conTypes)


hasEnt :: Type -> Bool
hasEnt = (`elem` entTypes)


hasObj :: Type -> Bool
hasObj = (`elem` objTypes)


hasInvCoins :: Type -> Bool
hasInvCoins = (`elem` invCoinsTypes)


hasMob :: Type -> Bool
hasMob = (`elem` mobTypes)


-----


hasConId :: Id -> MudState -> Bool
hasConId = hasHelper conTypes


hasHelper :: [Type] -> Id -> MudState -> Bool
hasHelper ts i ms = getType i ms `elem` ts


hasEntId :: Id -> MudState -> Bool
hasEntId = hasHelper entTypes


hasObjId :: Id -> MudState -> Bool
hasObjId = hasHelper objTypes


hasInvCoinsId :: Id -> MudState -> Bool
hasInvCoinsId = hasHelper invCoinsTypes


hasMobId :: Id -> MudState -> Bool
hasMobId = hasHelper mobTypes
