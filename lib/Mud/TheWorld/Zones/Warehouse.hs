{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.Warehouse (createWarehouse) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Put
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.TheWorld.Foods
import           Mud.TheWorld.Liqs
import           Mud.TheWorld.Zones.WarehouseIds
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Weights

import           Data.Bits (zeroBits)
import           Data.Text (Text)
import qualified Data.Map.Strict as M (empty)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.Warehouse"


-- ==================================================
-- Zone definition:


createWarehouse :: MudStack ()
createWarehouse = do
  logNotice "createWarehouse" "creating the warehouse."

  -----

  putRm iWarehouseWelcome
      []
      mempty
      (mkRm (RmTemplate "Welcome to the warehouse"
          "This is the warehouse. Items to be cloned are stored here.\n\
          \There's just one rule: you can look, but don't touch!"
          Nothing
          Nothing
          zeroBits
          [ StdLink South iDwarfKit 0 ]
          (0, 0, 0)
          InsideEnv
          (Just "Welcome")
          M.empty [] []))

  -----

  putRm iDwarfKit
      []
      mempty
      (mkRm (RmTemplate "Dwarf kit"
          "This room holds items unique to the dwarf kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iWarehouseWelcome 0
          , StdLink South iElfKit           0 ]
          (0, -1, 0)
          InsideEnv
          (Just "Dwarf")
          M.empty [] []))

  -----

  putRm iElfKit
      []
      mempty
      (mkRm (RmTemplate "Elf kit"
          "This room holds items unique to the elf kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iDwarfKit    0
          , StdLink South iFelinoidKit 0 ]
          (0, -2, 0)
          InsideEnv
          (Just "Elf")
          M.empty [] []))

  -----

  putRm iFelinoidKit
      []
      mempty
      (mkRm (RmTemplate "Felinoid kit"
          "This room holds items unique to the felinoid kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iElfKit    0
          , StdLink South iHobbitKit 0 ]
          (0, -3, 0)
          InsideEnv
          (Just "Felinoid")
          M.empty [] []))

  -----

  putRm iHobbitKit
      []
      mempty
      (mkRm (RmTemplate "Hobbit kit"
          "This room holds items unique to the hobbit kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iFelinoidKit 0
          , StdLink South iHumanKit    0 ]
          (0, -4, 0)
          InsideEnv
          (Just "Hobbit")
          M.empty [] []))

  -----

  putRm iHumanKit
      []
      mempty
      (mkRm (RmTemplate "Human kit"
          "This room holds items unique to the human kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iHobbitKit    0
          , StdLink South iLagomorphKit 0 ]
          (0, -5, 0)
          InsideEnv
          (Just "Human")
          M.empty [] []))

  -----

  putRm iLagomorphKit
      []
      mempty
      (mkRm (RmTemplate "Lagomorph kit"
          "This room holds items unique to the lagomorph kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iHumanKit 0
          , StdLink South iNymphKit 0 ]
          (0, -6, 0)
          InsideEnv
          (Just "Lagomorph")
          M.empty [] []))

  -----

  putRm iNymphKit
      []
      mempty
      (mkRm (RmTemplate "Nymph kit"
          "This rooms holds items unique to the nymph kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iLagomorphKit 0
          , StdLink South iVulpenoidKit 0 ]
          (0, -7, 0)
          InsideEnv
          (Just "Nymph")
          M.empty [] []))

  -----

  putRm iVulpenoidKit
      []
      mempty
      (mkRm (RmTemplate "Vulpenoid kit"
          "This room holds items unique to the vulpenoid kit."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iNymphKit  0
          , StdLink South iCommonKit 0 ]
          (0, -8, -0)
          InsideEnv
          (Just "Vulpenoid")
          M.empty [] []))

  -----

  putRm iCommonKit
      [ iBread, iSack, iSandals, iWaterskin ]
      mempty
      (mkRm (RmTemplate "Common kit"
          "This room holds items found in all kits."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iVulpenoidKit 0 ]
          (0, -9, -0)
          InsideEnv
          (Just "Common")
          M.empty [] []))

  putFood iBread
          (mkEnt iBread breadEntTemplate)
          (mkObj breadObjTemplate)
          breadFood

  putCon iSack
      (Ent iSack
          (Just "sack")
          "sack" ""
          "The durable sack is made from a coarse, woven fabric."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate sackWeight sackVol Nothing $ zeroBits)
      []
      mempty
      Nothing
      (Con False sackCap zeroBits)

  putArm iSandals
      (Ent iSandals
          (Just "sandals")
          "pair of simple leather sandals" "pairs of simple leather sandals"
          "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the \
          \soles of your feet."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate sandalsWeight sandalsVol Nothing $ zeroBits)
      (Arm Feet 1)

  putVessel iWaterskin
      (Ent iWaterskin
          (Just "waterskin")
          "waterskin" ""
          "The handy waterskin, crafted from the bladder of a bovine animal, is an indispensable piece of equipment \
          \when it comes to travel and, often, everyday life."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate waterskinWeight waterskinVol Nothing $ zeroBits)
      (Just (waterLiq, maxBound))
      Nothing

  -----

  putRmTeleName iWarehouseWelcome "warehouse"
