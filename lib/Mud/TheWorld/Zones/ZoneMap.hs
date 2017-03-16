{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.ZoneMap (getZoneForRmId) where

import Mud.Data.State.MudData
import Mud.TheWorld.Zones.AdminZoneIds
import Mud.TheWorld.Zones.DalbenIds
import Mud.TheWorld.Zones.TutorialIds

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM (IntMap, fromList, lookup)


type Zone = Text


getZoneForRmId :: Id -> Zone
getZoneForRmId = fromMaybe "unknown" . (`IM.lookup` zoneMap)


zoneMap :: IM.IntMap Zone
zoneMap = IM.fromList . concat $ [ adminZone
                                 , dalbenZone
                                 , tutorialZone ]


-----


adminZone :: [(Id, Zone)]
adminZone = zip rmIds . repeat $ "Admin zone"
  where
    rmIds = [ iAccessoriesCloset
            , iArmCloset
            , iAtrium
            , iAttic
            , iBasement
            , iCentral
            , iClone
            , iClothCloset
            , iCoinsCloset
            , iConCloset
            , iCottage
            , iEmpty
            , iHallwayEast
            , iHallwayWest
            , iInside
            , iLounge
            , iLoungeEntrance
            , iMobCloset
            , iNecropolis
            , iNoEnv
            , iObjCloset
            , iOutside
            , iPantry
            , iShop
            , iSpecial
            , iTrashDump
            , iTutEntrance
            , iVoid
            , iWeightRm
            , iWpnCloset ]


-----


dalbenZone :: [(Id, Zone)]
dalbenZone = zip rmIds . repeat $ "Dalben"
  where
    rmIds = pure iDalbenWelcome


-----


tutorialZone :: [(Id, Zone)]
tutorialZone = zip rmIds . repeat $ "Tutorial"
  where
    rmIds = pure iTutWelcome
