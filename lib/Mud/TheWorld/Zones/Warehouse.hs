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
      [ iDwarfApple1, iDwarfApple2, iAxeSml ]
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

  putWpn iAxeSml
      (Ent iAxeSml
          (Just "axe")
          "small axe" ""
          "The axe has a bronze head fashioned into a sharp, curved blade. The handle is ash. It's a sturdy, reliable \
          \weapon."
          (Just "The head of the axe smells like metal. The handle doesn't smell like much at all.")
          zeroBits)
      (let taste = "You lick the head of the axe. It tastes metallic."
       in mkObj . ObjTemplate axeSmlWeight axeSmlVol (Just taste) $ zeroBits)
      (Wpn OneHanded 1 10)

  -----

  putRm iElfKit
      [ iElfBanana1, iElfBanana2, iQuarterstaff ]
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

  putWpn iQuarterstaff
      (Ent iQuarterstaff
          (Just "staff")
          "quarterstaff" "quarterstaves"
          "The quarterstaff is a balanced, wooden pole, about 5 feet long and wielded with two hands."
          (Just "The polished wood of the quarterstaff doesn't have a detectable smell.")
          zeroBits)
      (let taste = "You lick the end of the quarterstaff. If anything, it might taste a little grimy."
       in mkObj . ObjTemplate quarterstaffWeight quarterstaffVol (Just taste) $ zeroBits)
      (Wpn TwoHanded 1 10)

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
      [ iHobbitApple, iHobbitBanana, iHobbitOrange, iShortsword ]
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

  putWpn iShortsword
      (Ent iShortsword
          (Just "sword")
          "shortsword" ""
          "The shortsword is a straightforward, cut-and-thrust sword and a trusty weapon. It's about 20 inches long."
          swordSmell
          zeroBits)
      (mkObj . ObjTemplate shortswordWeight shortswordVol swordTaste $ zeroBits)
      (Wpn OneHanded 1 10)

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
      ([ iGorhna1..iGorhna1 + 49 ] ++ [ iSpear ])
      mempty
      (mkRm (RmTemplate "Nymph kit"
          "This room holds items unique to the nymph kit."
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

  putWpn iSpear
      (Ent iSpear
          (Just "spear")
          "spear" ""
          "The wooden spear has a straight, double-edged, and pointed blade at its tip. The deadly weapon is about 4 \
          \feet long."
          (Just "You keep your nose clear of the blade and sniff the handle instead. There is no detectable smell.")
          zeroBits)
      (let taste = "You don't dare taste the blade: you'd certainly cut up your mouth and tongue! You decide to lick \
                   \the polished, wooden handle instead. Sadly, it doesn't taste like much at all."
       in mkObj . ObjTemplate spearWeight spearVol (Just taste) $ zeroBits)
      (Wpn OneHanded 1 10)

  -----

  putRm iVulpenoidKit
      [ iBroadsword ]
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

  putWpn iBroadsword
      (Ent iBroadsword
          (Just "sword")
          "broadsword" ""
          "The blade of the broadsword is straight, double-edged, and pointed. It's about 3.5 feet long including the \
          \handle. Although there's nothing extraordinary about the sword, it's a decent, solid weapon."
          swordSmell
          zeroBits)
      (mkObj . ObjTemplate broadswordWeight broadswordVol swordTaste $ zeroBits)
      (Wpn OneHanded 1 10)

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


swordSmell :: Maybe Text
swordSmell = Just "The blade of the sword smells like metal."


swordTaste :: Maybe Text
swordTaste = Just "You lick the blade of the sword, taking care not to cut your tongue. It tastes metallic."
