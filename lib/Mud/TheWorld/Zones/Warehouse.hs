{-# LANGUAGE TupleSections, OverloadedStrings #-}

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
import           Mud.Util.Misc
import           Mud.Util.Text

import           Control.Monad (forM_)
import           Data.Bits (zeroBits)
import qualified Data.Map.Strict as M (empty)
import           Data.Monoid ((<>))
import           Data.Text (Text)


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
      [ iDwarfApple1, iDwarfApple2 ]
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

  forM_ [ iDwarfApple1, iDwarfApple2 ] $ \i -> putFood i
      (mkEnt i appleEntTemplate)
      (mkObj appleObjTemplate)
      appleFood

  -----

  putRm iElfKit
      [ iElfBanana1, iElfBanana2 ]
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

  forM_ [ iElfBanana1, iElfBanana2 ] $ \i -> putFood i
      (mkEnt i bananaEntTemplate)
      (mkObj bananaObjTemplate)
      bananaFood

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
      [ iHobbitApple, iHobbitBanana, iHobbitOrange ]
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

  putFood iHobbitApple
      (mkEnt iHobbitApple appleEntTemplate)
      (mkObj appleObjTemplate)
      appleFood

  putFood iHobbitBanana
      (mkEnt iHobbitBanana bananaEntTemplate)
      (mkObj bananaObjTemplate)
      bananaFood

  putFood iHobbitOrange
      (mkEnt iHobbitOrange orangeEntTemplate)
      (mkObj orangeObjTemplate)
      orangeFood

  -----

  putRm iHumanKit
      [ iHumanApple, iHumanBanana, iHumanOrange ]
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

  putFood iHumanApple
      (mkEnt iHumanApple appleEntTemplate)
      (mkObj appleObjTemplate)
      appleFood

  putFood iHumanBanana
      (mkEnt iHumanBanana bananaEntTemplate)
      (mkObj bananaObjTemplate)
      bananaFood

  putFood iHumanOrange
      (mkEnt iHumanOrange orangeEntTemplate)
      (mkObj orangeObjTemplate)
      orangeFood

  -----

  putRm iLagomorphKit
      [ iLagomorphOrange1, iLagomorphOrange2 ]
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

  forM_ [ iLagomorphOrange1, iLagomorphOrange2 ] $ \i -> putFood i
      (mkEnt i orangeEntTemplate)
      (mkObj orangeObjTemplate)
      orangeFood

  -----

  putRm iNymphKit
      [ iGorhna1..iGorhna1 + 49 ]
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

  forM_ [iGorhna1..iGorhna1 + 49] $ \i -> putFood i
      (mkEnt i gorhnaEntTemplate)
      (mkObj gorhnaObjTemplate)
      gorhnaFood

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
          [ StdLink North iVulpenoidKit 0
          , StdLink South iBonusKit     0 ]
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
          (Just "The sack smells like burlap. It's a bit reminiscent of the smell of a barn or a farmyard.")
          zeroBits)
      (let taste = thrice prd "Munching on the sack, you experience firsthand the earthy taste of burlap. You begin \
                              \to suspect that the taste could linger in your mouth for some time"
       in mkObj . ObjTemplate sackWeight sackVol (Just taste) $ zeroBits)
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
          (Just "The sandals smell like leather, and not much else.")
          zeroBits)
      (let taste = "You chew on the sandals. The leather tastes a bit salty. Probably from foot sweat."
       in mkObj . ObjTemplate sandalsWeight sandalsVol (Just taste) $ zeroBits)
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

  let bonusPotionIds = [ iPotInstantHp, iPotInstantFp, iPotInstantSt ]
      bonusRingIds   = [ iNoseRing, iAmethystRing, iAquamarineRing, iEmeraldRing, iGarnetRing ]
  putRm iBonusKit
      (bonusPotionIds ++ bonusRingIds)
      mempty
      (mkRm (RmTemplate "Bonus kit"
          "This room holds bonus items."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iCommonKit 0 ]
          (0, -10, -0)
          InsideEnv
          (Just "Bonus")
          M.empty [] []))

  let flaskConts = (++ repeat Nothing) . map (Just . (, maxBound)) $ [ potInstantHpLiq, potInstantFpLiq, potInstantStLiq ]
  forM_ (zip bonusPotionIds flaskConts) $ \(i, mc) ->
      putVessel i
          (Ent i
              (Just "flask")
              "large potion flask" ""
              "This glass flask complete with cork stopper is the ideal vessel for potion storage and transportation."
              Nothing
              zeroBits)
          (mkObj . ObjTemplate potionFlaskLrgWeight potionFlaskLrgVol Nothing $ zeroBits)
          mc
          Nothing

  putCloth iNoseRing
      (Ent iNoseRing
          (Just "nose")
          "nose ring" ""
          "It's a plain copper stud intended to be worn on the nose."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate noseWeight noseVol Nothing $ zeroBits)
      NoseRing

  let ringTuples = [ (iAmethystRing,   "amethyst"  )
                   , (iAquamarineRing, "aquamarine")
                   , (iEmeraldRing,    "emerald"   )
                   , (iGarnetRing,     "garnet"    ) ]
  forM_ ringTuples $ \(i, t) ->
      putCloth i
          (Ent i
              (Just "ring")
              (t <> " ring") ""
              ("It's a simple copper band prominently featuring a beautiful " <> t <> " stone.")
              Nothing
              zeroBits)
          (mkObj . ObjTemplate ringWeight ringVol Nothing $ zeroBits)
          Ring

  -----

  putRmTeleName iWarehouseWelcome "warehouse"
