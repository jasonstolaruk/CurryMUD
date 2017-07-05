{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.ZoneMap (getZoneForRmId) where

import           Mud.Data.State.MudData
import           Mud.TheWorld.Zones.AdminZoneIds
import           Mud.TheWorld.Zones.LoplenkoIds
import           Mud.TheWorld.Zones.TutorialIds
import           Mud.TheWorld.Zones.WarehouseIds

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.IntMap.Strict as IM (IntMap, fromList, lookup)


type Zone = Text


getZoneForRmId :: Id -> Zone
getZoneForRmId = fromMaybe "unknown" . (`IM.lookup` zoneMap)


zoneMap :: IM.IntMap Zone
zoneMap = IM.fromList . concat $ [ adminZone
                                 , loplenkoZone
                                 , tutorialZone
                                 , warehouseZone ]


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


loplenkoZone :: [(Id, Zone)]
loplenkoZone = zip rmIds . repeat $ "Lop'len-ko"
  where
    rmIds = [ iLoplenkoWelcome
            , iLibrary ]


-----


tutorialZone :: [(Id, Zone)]
tutorialZone = zip rmIds . repeat $ "Tutorial"
  where
    rmIds = pure iTutWelcome


-----


warehouseZone :: [(Id, Zone)]
warehouseZone = zip rmIds . repeat $ "Warehouse"
  where
    rmIds = [ iBonusKit
            , iCommonKit
            , iDwarfKit
            , iElfKit
            , iFelinoidKit
            , iHobbitKit
            , iHumanKit
            , iLagomorphKit
            , iNymphKit
            , iTunnel1
            , iTunnel2
            , iVulpenoidKit
            , iWarehouseWelcome
            , iWpnsRm ]
