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
import qualified Data.Text as T


-- TODO: Review your descriptions.


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
          [ StdLink East iTunnel1 0 ]
          (0, 0, 0)
          InsideEnv
          (Just "Welcome")
          M.empty [] []))

  -----

  putRm iTunnel1
      []
      mempty
      (mkRm (RmTemplate "Tunnel"
          tunnelDesc
          Nothing
          Nothing
          zeroBits
          [ StdLink East  iTunnel2          0
          , StdLink West  iWarehouseWelcome 0
          , StdLink South iArmRm            0 ]
          (1, 0, 0)
          InsideEnv
          Nothing
          M.empty [] []))

  -----

  putRm iArmRm
      [ iKnitCap, iLeatherHelm, iLeatherSandals, iLeatherBoots ]
      mempty
      (mkRm (RmTemplate "Armor room"
          "This room holds armor."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iTunnel1 0
          , StdLink South iClothRm 0 ]
          (1, -1, 0)
          InsideEnv
          (Just "Armor")
          M.empty [] []))

  putArm iKnitCap
      (Ent iKnitCap
          (Just "cap")
          "knit cap" ""
          "It's a simple knit cap, designed to keep your head warm in cold weather."
          (Just "There is a faint scent of yarn.")
          zeroBits)
      (let taste = "It pretty much just tastes like yarn."
       in mkObj . ObjTemplate knitCapWeight knitCapVol (Just taste) $ zeroBits)
      (Arm Head 1)

  putArm iLeatherHelm
      (Ent iLeatherHelm
          (Just "helmet")
          "leather helmet" ""
          "This soft leather helmet covers the skull, providing moderate protection."
          (mkLeatherSmell "helmet smells")
          zeroBits)
      (mkObj . ObjTemplate helmLeatherWeight helmLeatherVol (mkLeatherTaste "helmet" "head") $ zeroBits)
      (Arm Head 1)

  putArm iLeatherSandals
      (Ent iLeatherSandals
          (Just "sandals")
          "pair of simple leather sandals" "pairs of simple leather sandals"
          "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the \
          \soles of your feet."
          (mkLeatherSmell "sandals smell")
          zeroBits)
      (mkObj . ObjTemplate sandalsWeight sandalsVol (mkLeatherTaste "sandals" "foot") $ zeroBits)
      (Arm Feet 1)

  putArm iLeatherBoots
      (Ent iLeatherBoots
          (Just "boots")
          "pair of leather boots" "pairs of leather boots"
          "These rugged, sturdy boots make excellent footwear for traveling across a variety of terrain."
          (mkLeatherSmell "boots smell")
          zeroBits)
      (mkObj . ObjTemplate bootsWeight bootsVol (mkLeatherTaste "boots" "foot") $ zeroBits)
      (Arm Feet 1)

  -----

  putRm iClothRm
      [ iChemise, iTunic, iApron, iTabard, iGreyCoat, iFrockCoat, iBreeches, iTrousers ]
      mempty
      (mkRm (RmTemplate "Clothing room"
          "This room holds clothing."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iArmRm         0
          , StdLink South iConRm         0
          , StdLink Down  iAccessoriesRm 0 ]
          (1, -2, 0)
          InsideEnv
          (Just "Clothing")
          M.empty [] []))

  putCloth iChemise
      (Ent iChemise
          (Just "chemise")
          "fine white chemise" ""
          "This voluminous frock, worn on the upper body, is fashioned out of thin, smooth linen. It hangs just below \
          \the waist while its loose-cut, wide sleeves are elbow length."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate shirtWeight shirtVol Nothing $ zeroBits)
      Shirt

  putCloth iTunic
      (Ent iTunic
          (Just "tunic")
          "cobalt blue wool tunic" ""
          "This heavy wool tunic is waist length and short-sleeved. Decorative white embroidery along the neck, \
          \sleeves, and waist adds an eye-catching touch."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate tunicHeavyWeight tunicHeavyVol Nothing $ zeroBits)
      Shirt

  putCloth iApron
      (Ent iApron
          (Just "apron")
          "heavy brown apron" ""
          "This sturdy padded utility apron provides adequate protection while its wearer labors and toils."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate apronHeavyWeight apronHeavyVol Nothing $ zeroBits)
      Smock

  putCloth iTabard
      (Ent iTabard
          (Just "tabard")
          "sleeveless blue tabard" ""
          "This sleeveless overgarment is open at both sides and extends down to the thigh. Dyed a deep shade of blue, \
          \a contrasting bright orange trim adds a distinct accent along the hems. There is a short collar around the \
          \neck complete with a small decorative yellow bowtie."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate tabardWeight tabardVol Nothing $ zeroBits)
      Smock
  putCloth iGreyCoat
      (Ent iGreyCoat
          (Just "coat")
          "mouse-grey coat" ""
          "Sure to keep its wearer warm in all but the coldest of weather, this heavy, long-sleeved coat reaches the \
          \knees, and features a tall collar followed by ten large silver buttons along its length."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate coatHeavyWeight coatHeavyVol Nothing $ zeroBits)
      Coat

  putCloth iFrockCoat
      (Ent iFrockCoat
          (Just "coat")
          "woman's red frock coat" ""
          "This fashionable long-sleeved coat is made of soft, bright-red fabric decorated with a fine, rich floral \
          \brocade. Six black buttons from the collar down the chest, when fastened, make this a particularly \
          \figure-flattering garment."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate coatWeight coatVol Nothing $ zeroBits)
      Coat

  putCloth iBreeches
      (Ent iBreeches
          (Just "breeches")
          "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches"
          "These thin, tight-fitting breeches extend just past the knees, where short drawstrings allow them to be \
          \neatly secured to the legs."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate trousersWeight trousersVol Nothing $ zeroBits)
      Trousers

  putCloth iTrousers
      (Ent iTrousers
          (Just "trousers")
          "pair of baggy beige trousers" "pairs of baggy beige trousers"
          "These wool trousers are loose-fitting so as to grant uninhibited movement. A rugged hemp drawstring allows \
          \them to be snugly tightened at the waist."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate trousersBaggyWeight trousersBaggyVol Nothing $ zeroBits)
      Trousers

  -----

  putRm iAccessoriesRm
      [ iAmethystRing, iAquamarineRing, iEmeraldRing, iGarnetRing, iNoseRing
      , iAzureEar, iCrimsonEar, iSeaGreenEar, iOnyxEar
      , iBronzeNeck, iSilverNeck, iGoldNeck, iPlatinumNeck
      , iCharmBracelet, iBangleBracelet, iBeadedBracelet, iPearlBracelet ]
      mempty
      (mkRm (RmTemplate "Accessories room"
          "This room holds accessories."
          Nothing
          Nothing
          zeroBits
          [ StdLink Up iClothRm 0 ]
          (1, -2, -1)
          InsideEnv
          (Just "Accessories")
          M.empty [] []))

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

  putCloth iNoseRing
      (Ent iNoseRing
          (Just "nose")
          "nose ring" ""
          "It's a plain copper stud intended to be worn on the nose."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate noseWeight noseVol Nothing $ zeroBits)
      NoseRing

  let earTuples = [ (iAzureEar,    "azure"    )
                  , (iCrimsonEar,  "crimson"  )
                  , (iSeaGreenEar, "sea green")
                  , (iOnyxEar,     "onyx"     ) ]

  forM_ earTuples $ \(i, t) ->
      putCloth i
          (Ent i
              (Just "earring")
              (t <> " earring") ""
              "It's a small, but tasteful, nondescript hoop."
              Nothing
              zeroBits)
          (mkObj . ObjTemplate earWeight earVol Nothing $ zeroBits)
          Earring

  let neckTuples = [ (iBronzeNeck,   "bronze"  )
                   , (iSilverNeck,   "silver"  )
                   , (iGoldNeck,     "gold"    )
                   , (iPlatinumNeck, "platinum") ]

  forM_ neckTuples $ \(i, t) ->
      putCloth i
          (Ent i
              (Just "necklace")
              (t <> " necklace") ""
              ("It's a simple " <> t <> " chain.")
              Nothing
              zeroBits)
          (mkObj . ObjTemplate neckWeight neckVol Nothing $ zeroBits)
          Necklace

  let charmBraceletDesc  = "The bracelet is adorned with a variety of quaint charms in the shape of musical \
                           \instruments, fashioned out of pewter."
      bangleBraceletDesc = "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and \
                           \about half an inch wide."
      beadedBraceletDesc = "This classic bracelet consist of small, spherical wooden beads, alternating black and \
                           \white in color."
      pearlBraceletDesc  = "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory."

  let braceletTuples = [ (iCharmBracelet,  "charm",         charmBraceletDesc,  10)
                       , (iBangleBracelet, "wooden bangle", bangleBraceletDesc, 1 )
                       , (iBeadedBracelet, "beaded",        beadedBraceletDesc, 2 )
                       , (iPearlBracelet,  "pearl",         pearlBraceletDesc,  4 ) ]

  forM_ braceletTuples $ \(i, t, d, w) ->
      putCloth i
          (Ent i
              (Just "bracelet")
              (t <> " bracelet") ""
              d
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w braceletVol Nothing $ zeroBits)
          Bracelet

  -----

  putRm iConRm
      [ iSack ]
      mempty
      (mkRm (RmTemplate "Containers room"
          "This room holds containers."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iClothRm 0
          , StdLink South iFoodRm  0 ]
          (1, -3, 0)
          InsideEnv
          (Just "Containers")
          M.empty [] []))

  putCon iSack
      (Ent iSack
          (Just "sack")
          "sack" ""
          "The durable sack is made from a coarse, woven fabric."
          (Just "The sack smells like burlap. It's a bit reminiscent of the smell of a barn or a farmyard.")
          zeroBits)
      (let taste = thrice prd "Munching on the sack, you experience firsthand the earthy taste of burlap. You begin to \
                              \suspect that the taste could linger in your mouth for some time"
       in mkObj . ObjTemplate sackWeight sackVol (Just taste) $ zeroBits)
      []
      mempty
      Nothing
      (Con False sackCap zeroBits)

  -----

  let breadIds  = [ iBread1 ..iBread1  + 49 ]
      appleIds  = [ iApple1 ..iApple1  + 49 ]
      bananaIds = [ iBanana1..iBanana1 + 49 ]
      orangeIds = [ iOrange1..iOrange1 + 49 ]
      gorhnaIds = [ iGorhna1..iGorhna1 + 49 ]

  putRm iFoodRm
      (concat [ breadIds, appleIds, bananaIds, orangeIds, gorhnaIds ])
      mempty
      (mkRm (RmTemplate "Food room"
          "This room holds food."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iConRm 0
          , StdLink South iNpcRm 0 ]
          (1, -4, 0)
          InsideEnv
          (Just "Food")
          M.empty [] []))

  forM_ breadIds $ \i -> putFood i
      (mkEnt i breadEntTemplate)
      (mkObj breadObjTemplate)
      breadFood

  forM_ appleIds $ \i -> putFood i
      (mkEnt i appleEntTemplate)
      (mkObj appleObjTemplate)
      appleFood

  forM_ bananaIds $ \i -> putFood i
      (mkEnt i bananaEntTemplate)
      (mkObj bananaObjTemplate)
      bananaFood

  forM_ orangeIds $ \i -> putFood i
      (mkEnt i orangeEntTemplate)
      (mkObj orangeObjTemplate)
      orangeFood

  forM_ gorhnaIds $ \i -> putFood i
      (mkEnt i gorhnaEntTemplate)
      (mkObj gorhnaObjTemplate)
      gorhnaFood

  -----

  putRm iNpcRm
      []
      mempty
      (mkRm (RmTemplate "NPC room"
          "This room holds NPCs."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iFoodRm 0
          , StdLink South iObjRm  0 ]
          (1, -5, 0)
          InsideEnv
          (Just "NPCs")
          M.empty [] []))

  -----

  putRm iObjRm
      []
      mempty
      (mkRm (RmTemplate "Objects room"
          "This room holds objects."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iNpcRm    0
          , StdLink South iVesselRm 0 ]
          (1, -6, 0)
          InsideEnv
          (Just "Objects")
          M.empty [] []))

  -----

  putRm iVesselRm
      [ iWaterskin ]
      mempty
      (mkRm (RmTemplate "Vessels room"
          "This room holds vessels."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iObjRm    0
          , StdLink South iWpnRm    0
          , StdLink Down  iPotionRm 0 ]
          (1, -7, 0)
          InsideEnv
          (Just "Vessels")
          M.empty [] []))

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

  let potionIds = [ iPotInstantHp, iPotInstantFp, iPotInstantSt ]

  putRm iPotionRm
      potionIds
      mempty
      (mkRm (RmTemplate "Potions room"
          "This room holds vessels containing potions."
          Nothing
          Nothing
          zeroBits
          [ StdLink Up iVesselRm 0 ]
          (1, -7, -1)
          InsideEnv
          (Just "Potions")
          M.empty [] []))

  let flaskConts = (++ repeat Nothing) . map (Just . (, maxBound)) $ [ potInstantHpLiq, potInstantFpLiq, potInstantStLiq ]

  forM_ (zip potionIds flaskConts) $ \(i, mc) ->
      putVessel i
          (Ent i
              (Just "flask")
              "potion flask" ""
              "This glass flask complete with cork stopper is the ideal vessel for potion storage and transportation."
              Nothing
              zeroBits)
          (mkObj . ObjTemplate potionFlaskWeight potionFlaskVol Nothing $ zeroBits)
          mc
          Nothing

  -----

  putRm iWpnRm
      [ iAxeSml, iBroadsword, iMace, iQuarterstaff, iShortsword, iSpear ]
      mempty
      (mkRm (RmTemplate "Weapons room"
          "This room holds weapons."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iVesselRm   0
          , StdLink South iWritableRm 0 ]
          (1, -8, 0)
          InsideEnv
          (Just "Weapons")
          M.empty [] []))

  putWpn iAxeSml
      (Ent iAxeSml
          (Just "axe")
          "small axe" ""
          "The axe has a bronze head fashioned into a sharp, curved blade. The handle is ash. It's a sturdy, reliable \
          \weapon."
          (mkWpnSmell "axe")
          zeroBits)
      (mkObj . ObjTemplate axeSmlWeight axeSmlVol (mkWpnTaste "axe") $ zeroBits)
      (Wpn OneHanded 1 10)

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

  putWpn iMace
      (Ent iMace
          (Just "mace")
          "mace" ""
          "The mace is essentially a war club with a heavy, round head of iron. You could really hurt someone with \
          \this thing."
          (mkWpnSmell "mace")
          zeroBits)
      (mkObj . ObjTemplate maceWeight maceVol (mkWpnTaste "mace") $ zeroBits)
      (Wpn OneHanded 1 10)

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

  putWpn iShortsword
      (Ent iShortsword
          (Just "sword")
          "shortsword" ""
          "The shortsword is a straightforward, cut-and-thrust sword and a trusty weapon. It's about 20 inches long."
          swordSmell
          zeroBits)
      (mkObj . ObjTemplate shortswordWeight shortswordVol swordTaste $ zeroBits)
      (Wpn OneHanded 1 10)

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

  putRm iWritableRm
      []
      mempty
      (mkRm (RmTemplate "Writables room"
          "This room holds writables."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iWpnRm 0 ]
          (1, -9, 0)
          InsideEnv
          (Just "Writables")
          M.empty [] []))

  -----

  putRm iTunnel2
      []
      mempty
      (mkRm (RmTemplate "Tunnel"
          tunnelDesc
          Nothing
          Nothing
          zeroBits
          [ StdLink West iTunnel1 0
          , StdLink East iTunnel3 0 ]
          (2, 0, 0)
          InsideEnv
          Nothing
          M.empty [] []))

  -----

  putRm iTunnel3 -- Continue building south of here as needed.
      []
      mempty
      (mkRm (RmTemplate "Tunnel"
          tunnelDesc
          Nothing
          Nothing
          zeroBits
          [ StdLink West iTunnel2 0 ]
          (3, 0, 0)
          InsideEnv
          Nothing
          M.empty [] []))

  -----

  putRmTeleName iWarehouseWelcome "warehouse"


mkLeatherSmell :: Text -> Maybe Text
mkLeatherSmell t = Just $ "The " <> t <> " like leather, and not much else."


mkLeatherTaste :: Text -> Text -> Maybe Text
mkLeatherTaste a b = Just . T.concat $ [ "You chew on the "
                                       , a
                                       , ". The leather tastes a bit salty. Probably from "
                                       , b
                                       , " sweat." ]


mkWpnSmell :: Text -> Maybe Text
mkWpnSmell t = Just $ "The head of the " <> t <> " smells like metal. The handle doesn't smell like much at all."


mkWpnTaste :: Text -> Maybe Text
mkWpnTaste t = Just $ "You lick the head of the " <> t <> ". It tastes metallic."


swordSmell :: Maybe Text
swordSmell = Just "The blade of the sword smells like metal."


swordTaste :: Maybe Text
swordTaste = Just "You lick the blade of the sword, taking care not to cut your tongue. It tastes metallic."


tunnelDesc :: Text
tunnelDesc = "This tunnel provides access to the different areas of the warehouse."
