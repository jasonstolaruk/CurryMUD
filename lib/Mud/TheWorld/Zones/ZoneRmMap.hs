{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.ZoneRmMap (getZoneNameForRmId) where

import Mud.Data.State.MudData
import Mud.TheWorld.Zones.AdminZoneIds
import Mud.TheWorld.Zones.TutorialIds

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (IntMap, fromList, lookup)


type ZoneName = Text


getZoneNameForRmId :: Id -> ZoneName
getZoneNameForRmId = fromMaybe "unknown" . (`IM.lookup` zoneRmMap)


zoneRmMap :: IM.IntMap ZoneName
zoneRmMap = IM.fromList . concat $ [ adminZone
                                   , tutorialZone ]


adminZone :: [(Id, ZoneName)]
adminZone = zip rmIds . repeat $ "Admin zone"
  where
    rmIds = [ iAccessoriesCloset
            , iArmCloset
            , iAtrium
            , iAttic
            , iBasement
            , iCentral
            , iClothCloset
            , iConCloset
            , iCoinsCloset
            , iEmpty
            , iHallwayEast
            , iHallwayWest
            , iLounge
            , iLoungeEntrance
            , iMobCloset
            , iObjCloset
            , iTutEntrance
            , iVoid
            , iWeightRm
            , iWpnCloset ]


tutorialZone :: [(Id, ZoneName)]
tutorialZone = zip rmIds . repeat $ "Tutorial"
  where
    rmIds = pure iTutWelcome
