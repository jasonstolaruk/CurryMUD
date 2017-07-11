{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Mud.TheWorld.Zones.Warehouse (createWarehouse) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Put
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.TheWorld.Foods
import           Mud.TheWorld.Liqs
import           Mud.TheWorld.Zones.WarehouseIds
import           Mud.TopLvlDefs.Seconds
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Weights
import           Mud.Util.Misc
import           Mud.Util.Text

import           Control.Monad (forM_)
import           Data.Bits (zeroBits)
import qualified Data.Map.Strict as M (empty, fromList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T


-- TODO: Review your descriptions.
-- TODO: Some things lack smell and taste.


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
          [ StdLink South iArmRm 0 ]
          (0, 0, 0)
          InsideEnv
          (Just "Welcome")
          M.empty [] []))

  -----

  putRm iArmRm
      [ iBootsLeather
      , iBootsThigh
      , iCapKnit
      , iHelmLeather
      , iSandalsLeather ]
      mempty
      (mkRm (RmTemplate "Armor room"
          "This room holds armor."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iWarehouseWelcome 0
          , StdLink South iClothRm          0 ]
          (0, -1, 0)
          InsideEnv
          (Just "Armor")
          M.empty [] []))

  putArm iBootsLeather
      (Ent iBootsLeather
          (Just "boots")
          "pair of leather boots" "pairs of leather boots"
          "These rugged, sturdy boots make excellent footwear for traveling across a variety of terrain."
          (mkLeatherSmell "boots smell")
          zeroBits)
      (mkObj . ObjTemplate bootsWeight bootsVol (mkLeatherTaste "boots" "foot") $ zeroBits)
      (Arm Feet 1)

  putArm iBootsThigh
      (Ent iBootsThigh
          (Just "boots")
          "pair of jet-black traveler's boots" "pair of jet-black traveler's boots"
          "These well-crafted, thigh-high boots are rugged and durable."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate bootsWeight bootsVol Nothing $ zeroBits)
      (Arm Feet 1)

  putArm iCapKnit
      (Ent iCapKnit
          (Just "cap")
          "knit cap" ""
          "It's a simple knit cap, designed to keep your head warm in cold weather."
          (Just "There is a faint scent of yarn.")
          zeroBits)
      (let taste = "It pretty much just tastes like yarn."
       in mkObj . ObjTemplate knitCapWeight knitCapVol (Just taste) $ zeroBits)
      (Arm Head 1)

  putArm iHelmLeather
      (Ent iHelmLeather
          (Just "helmet")
          "leather helmet" ""
          "This soft leather helmet covers the skull, providing moderate protection."
          (mkLeatherSmell "helmet smells")
          zeroBits)
      (mkObj . ObjTemplate helmLeatherWeight helmLeatherVol (mkLeatherTaste "helmet" "head") $ zeroBits)
      (Arm Head 1)

  putArm iSandalsLeather
      (Ent iSandalsLeather
          (Just "sandals")
          "pair of simple leather sandals" "pairs of simple leather sandals"
          "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the \
          \soles of your feet."
          (mkLeatherSmell "sandals smell")
          zeroBits)
      (mkObj . ObjTemplate sandalsWeight sandalsVol (mkLeatherTaste "sandals" "foot") $ zeroBits)
      (Arm Feet 1)

  -----

  putRm iClothRm
      [ iApronBrown
      , iApronLeather
      , iBreeches
      , iChemise
      , iCoatFrock
      , iCoatGrey
      , iOveralls
      , iShirtPeasant
      , iTabard
      , iTrousers
      , iTunic ]
      mempty
      (mkRm (RmTemplate "Clothing room"
          "This room holds clothing."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iArmRm         0
          , StdLink South iConRm         0
          , StdLink Down  iAccessoriesRm 0 ]
          (0, -2, 0)
          InsideEnv
          (Just "Clothing")
          M.empty [] []))

  putCloth iApronBrown
      (Ent iApronBrown
          (Just "apron")
          "heavy brown apron" ""
          "This sturdy padded utility apron provides adequate protection while its wearer labors and toils."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate apronHeavyWeight apronHeavyVol Nothing $ zeroBits)
      Smock

  putCloth iApronLeather
      (Ent iApronLeather
          (Just "apron")
          "leather apron" ""
          "This heavy apron, though bulky, is a must for those who undertake dirty and dangerous chores."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate apronHeavyWeight apronHeavyVol Nothing $ zeroBits)
      Smock

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

  putCloth iCoatFrock
      (Ent iCoatFrock
          (Just "coat")
          "woman's red frock coat" ""
          "This fashionable long-sleeved coat is made of soft, bright-red fabric decorated with a fine, rich floral \
          \brocade. Six black buttons from the collar down the chest, when fastened, make this a particularly \
          \figure-flattering garment."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate coatWeight coatVol Nothing $ zeroBits)
      Coat

  putCloth iCoatGrey
      (Ent iCoatGrey
          (Just "coat")
          "mouse-grey coat" ""
          "Sure to keep its wearer warm in all but the coldest of weather, this heavy, long-sleeved coat reaches the \
          \knees, and features a tall collar followed by ten large silver buttons along its length."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate coatHeavyWeight coatHeavyVol Nothing $ zeroBits)
      Coat

  putCloth iOveralls
      (Ent iOveralls
          (Just "overalls")
          "pair of many-pocketed brown overalls" "pairs of many-pocketed brown overalls"
          "These durable overalls are adorned with a multitude of little pockets."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate overallsWeight overallsVol Nothing $ zeroBits)
      Trousers

  putCloth iShirtPeasant
      (Ent iShirtPeasant
          (Just "shirt")
          "white peasant's shirt" ""
          "This shirt, favored by skilled laborers and lowly bumpkins alike, represents the epitome of function over \
          \fashion."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate shirtWeight shirtVol Nothing $ zeroBits)
      Shirt

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

  -----

  putRm iAccessoriesRm
      [ iBraceletBangle, iBraceletBeaded, iBraceletCharm, iBraceletPearl
      , iEarAzure, iEarCrimson, iEarOnyx, iEarSeaGreen
      , iNeckBronze, iNeckGold, iNeckPlatinum, iNeckSilver
      , iNoseRing
      , iRingAmethyst, iRingAquamarine, iRingEmerald, iRingGarnet ]
      mempty
      (mkRm (RmTemplate "Accessories room"
          "This room holds accessories."
          Nothing
          Nothing
          zeroBits
          [ StdLink Up iClothRm 0 ]
          (0, -2, -1)
          InsideEnv
          (Just "Accessories")
          M.empty [] []))

  let charmBraceletDesc  = "The bracelet is adorned with a variety of quaint charms in the shape of musical \
                           \instruments, fashioned out of pewter."
      bangleBraceletDesc = "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and \
                           \about half an inch wide."
      beadedBraceletDesc = "This classic bracelet consist of small, spherical wooden beads, alternating black and \
                           \white in color."
      pearlBraceletDesc  = "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory."

  let braceletTuples = [ (iBraceletBangle, "wooden bangle", bangleBraceletDesc, 1 )
                       , (iBraceletBeaded, "beaded",        beadedBraceletDesc, 2 )
                       , (iBraceletCharm,  "charm",         charmBraceletDesc,  10)
                       , (iBraceletPearl,  "pearl",         pearlBraceletDesc,  4 ) ]

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

  let earTuples = [ (iEarAzure,    "azure"    )
                  , (iEarCrimson,  "crimson"  )
                  , (iEarOnyx,     "onyx"     )
                  , (iEarSeaGreen, "sea green") ]

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

  let neckTuples = [ (iNeckBronze,   "bronze"  )
                   , (iNeckGold,     "gold"    )
                   , (iNeckPlatinum, "platinum")
                   , (iNeckSilver,   "silver"  ) ]

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

  putCloth iNoseRing
      (Ent iNoseRing
          (Just "nose")
          "nose ring" ""
          "It's a plain copper stud intended to be worn on the nose."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate noseWeight noseVol Nothing $ zeroBits)
      NoseRing

  let ringTuples = [ (iRingAmethyst,   "amethyst"  )
                   , (iRingAquamarine, "aquamarine")
                   , (iRingEmerald,    "emerald"   )
                   , (iRingGarnet,     "garnet"    ) ]

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

  putRm iConRm
      [ iBackSml, iBack, iBackLrg
      , iSackSml, iSack, iSackLrg ]
      mempty
      (mkRm (RmTemplate "Containers room"
          "This room holds containers."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iClothRm 0
          , StdLink South iFoodRm  0 ]
          (0, -3, 0)
          InsideEnv
          (Just "Containers")
          M.empty [] []))

  let backTuples = [ (iBackSml, "small ", backSmlWeight, backSmlVol, backSmlCap)
                   , (iBack,    "",       backWeight,    backVol,    backCap   )
                   , (iBackLrg, "large ", backLrgWeight, backLrgVol, backLrgCap) ]

  forM_ backTuples $ \(i, t, w, v, c) ->
      putCon i
          (Ent i
              (Just "back")
              (t <> "backpack") ""
              "The sturdy backpack is made of leather."
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w v Nothing $ zeroBits)
          []
          mempty
          (Just Backpack)
          (Con True c zeroBits)

  let sackTuples = [ (iSackSml, "small ", sackSmlWeight, sackSmlVol, sackSmlCap)
                   , (iSack,    "",       sackWeight,    sackVol,    sackCap   )
                   , (iSackLrg, "large ", sackLrgWeight, sackLrgVol, sackLrgCap) ]

  forM_ sackTuples $ \(i, t, w, v, c) ->
      putCon i
          (Ent i
              (Just "sack")
              (t <> "sack") ""
              "The durable sack is made from a coarse, woven fabric."
              (Just "The sack smells like burlap. It's a bit reminiscent of the smell of a barn or a farmyard.")
              zeroBits)
          (let taste = thrice prd "Munching on the sack, you experience firsthand the earthy taste of burlap. You begin \
                                  \to suspect the taste could linger in your mouth for some time"
           in mkObj . ObjTemplate w v (Just taste) $ zeroBits)
          []
          mempty
          Nothing
          (Con False c zeroBits)

  -----

  let appleIds  = [ iApple1 ..iApple1  + 49 ]
      bananaIds = [ iBanana1..iBanana1 + 49 ]
      breadIds  = [ iBread1 ..iBread1  + 49 ]
      gorhnaIds = [ iGorhna1..iGorhna1 + 49 ]
      orangeIds = [ iOrange1..iOrange1 + 49 ]

  putRm iFoodRm
      (concat [ appleIds, bananaIds, breadIds, gorhnaIds, orangeIds ])
      mempty
      (mkRm (RmTemplate "Food room"
          "This room holds food."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iConRm 0
          , StdLink South iNpcRm 0 ]
          (0, -4, 0)
          InsideEnv
          (Just "Food")
          M.empty [] []))

  forM_ appleIds $ \i -> putFood i
      (mkEnt i appleEntTemplate)
      (mkObj appleObjTemplate)
      appleFood

  forM_ bananaIds $ \i -> putFood i
      (mkEnt i bananaEntTemplate)
      (mkObj bananaObjTemplate)
      bananaFood

  forM_ breadIds $ \i -> putFood i
      (mkEnt i breadEntTemplate)
      (mkObj breadObjTemplate)
      breadFood

  forM_ gorhnaIds $ \i -> putFood i
      (mkEnt i gorhnaEntTemplate)
      (mkObj gorhnaObjTemplate)
      gorhnaFood

  forM_ orangeIds $ \i -> putFood i
      (mkEnt i orangeEntTemplate)
      (mkObj orangeObjTemplate)
      orangeFood

  -----

  putRm iNpcRm
      [ iPidge, iSkeleton1, iSkeleton2, iSkeleton3 ]
      mempty
      (mkRm (RmTemplate "NPC room"
          "This room holds NPCs."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iFoodRm 0
          , StdLink South iObjRm  0 ]
          (0, -5, 0)
          InsideEnv
          (Just "NPCs")
          M.empty [] []))

  putNpc iPidge
      (Ent iPidge
          (Just "pidge")
          "Pidge" ""
          "Pidge is a female hobbit with walnut-colored skin and large, brown eyes. She wears her silver-white hair in \
          \shoulder-length pigtails. Her small, round face is positively adorable."
          Nothing
          zeroBits)
      []
      mempty
      (M.fromList [ (ShirtS,    iShirtPeasant)
                  , (SmockS,    iApronLeather)
                  , (TrousersS, iOveralls    )
                  , (FeetS,     iBootsThigh  ) ])
      (mkMob (MobTemplate Female
          50 50 50 50 50
          100 100 100 100
          0 0
          RHand
          [ HobbitLang ]
          iNpcRm
          (Just MedMinus)
          (calcCorpseWeight Hobbit) (calcCorpseVol Hobbit) (calcCorpseCapacity Hobbit)
          (calcCorpseDecompSecs Hobbit)
          dfltParty))

  let skeletonCorpseWeight = round $ fromIntegral (calcCorpseWeight Human) * (0.15 :: Double)
  forM_ [ iSkeleton1, iSkeleton2, iSkeleton3 ] $ \i ->
      putNpc i
          (Ent i
              (Just "skeleton")
              "undead skeleton" ""
              "This mindless, bipedal skeleton has been animated and tasked with doing its master's bidding."
              Nothing
              zeroBits)
          []
          mempty
          M.empty
          (mkMob (MobTemplate NoSex
              50 50 50 50 50
              10 10 10 10
              10 0
              RHand
              []
              iNpcRm
              (Just MedMinus)
              skeletonCorpseWeight (calcCorpseVol Human) (calcCorpseCapacity Human)
              fiveMinsInSecs
              dfltParty))

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
          (0, -6, 0)
          InsideEnv
          (Just "Objects")
          M.empty [] []))

  -----

  putRm iVesselRm
      [ iBottleSml, iBottle, iBottleLrg
      , iJarSml, iJar, iJarLrg
      , iJugSml, iJug, iJugLrg
      , iPotionFlask, iPotionFlaskLrg
      , iWaterskin, iWaterskinWithWater, iWaterskinLrg ]
      mempty
      (mkRm (RmTemplate "Vessels room"
          "This room holds vessels."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iObjRm    0
          , StdLink South iWpnRm    0
          , StdLink Down  iPotionRm 0 ]
          (0, -7, 0)
          InsideEnv
          (Just "Vessels")
          M.empty [] []))

  let mkBottleDesc a b =
          T.concat [ "This "
                   , a
                   , "earthenware bottle is designed to be as portable and practical as possible. A glaze of "
                   , b
                   , " hues gives the vessel a glossy finish and makes it impermeable." ]

      bottelTuples = [ (iBottleSml, "small ", ("small, ", "light brown"),  bottleSmlWeight, bottleSmlVol)
                     , (iBottle,    "",       ("",        "mixed azure"),  bottleWeight,    bottleVol   )
                     , (iBottleLrg, "large ", ("large, ", "rusty orange"), bottleLrgWeight, bottleLrgVol) ]

  forM_ bottelTuples $ \(i, t, d, w, v) ->
      putVessel i
          (Ent i
              (Just "bottle")
              (t <> "bottle") ""
              (uncurry mkBottleDesc d)
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w v Nothing $ zeroBits)
          Nothing
          Nothing

  putVessel iJarSml
      (Ent iJarSml
          (Just "jar")
          "small jar" ""
          "This versatile, small glass jar comes affixed with an airtight lid."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate jarSmlWeight jarSmlVol Nothing $ zeroBits)
      Nothing
      Nothing

  putVessel iJar
      (Ent iJar
          (Just "jar")
          "jar" ""
          "This versatile glass jar comes affixed with an airtight lid."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate jarWeight jarVol Nothing $ zeroBits)
      Nothing
      Nothing

  putVessel iJarLrg
      (Ent iJarLrg
          (Just "jar")
          "large jar" ""
          "This versatile, large glass jar comes affixed with an airtight lid."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate jarLrgWeight jarLrgVol Nothing $ zeroBits)
      Nothing
      Nothing

  let jugTuples = [ (iJugSml, "small ", jugSmlWeight, jugSmlVol)
                  , (iJug,     "",      jugWeight,    jugVol   )
                  , (iJugLrg, "large ", jugLrgWeight, jugLrgVol) ]

  forM_ jugTuples $ \(i, t, w, v) ->
      putVessel i
          (Ent i
              (Just "jug")
              (t <> "jug") ""
              "While capable of containing a large amount of liquid, this corked, ceramic jug is rather cumbersome."
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w v Nothing $ zeroBits)
          Nothing
          Nothing

  mkWaterskin iWaterskin          Nothing

  mkWaterskin iWaterskinWithWater (Just (waterLiq, maxBound))

  putVessel iWaterskinLrg
      (Ent iWaterskinLrg
          (Just "waterskin")
          "large waterskin" ""
          (waterskinDesc <> " This waterskin is particularly large, making it suitable for long journeys.")
          Nothing
          zeroBits)
      (mkObj . ObjTemplate waterskinLrgWeight waterskinLrgVol Nothing $ zeroBits)
      Nothing
      Nothing

  -----

  let potionIds = [ iPotInstantFp, iPotInstantHp, iPotInstantSt ]

  putRm iPotionRm
      potionIds
      mempty
      (mkRm (RmTemplate "Potions room"
          "This room holds vessels containing potions."
          Nothing
          Nothing
          zeroBits
          [ StdLink Up iVesselRm 0 ]
          (0, -7, -1)
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
      [ iAxeSml
      , iMace
      , iSpear
      , iStaffQuarter
      , iSwordBroad
      , iSwordShort ]
      mempty
      (mkRm (RmTemplate "Weapons room"
          "This room holds weapons."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iVesselRm   0
          , StdLink South iWritableRm 0 ]
          (0, -8, 0)
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

  putWpn iStaffQuarter
      (Ent iStaffQuarter
          (Just "staff")
          "quarterstaff" "quarterstaves"
          "The quarterstaff is a balanced, wooden pole, about 5 feet long and wielded with two hands."
          (Just "The polished wood of the quarterstaff doesn't have a detectable smell.")
          zeroBits)
      (let taste = "You lick the end of the quarterstaff. If anything, it might taste a little grimy."
       in mkObj . ObjTemplate quarterstaffWeight quarterstaffVol (Just taste) $ zeroBits)
      (Wpn TwoHanded 1 10)

  putWpn iSwordBroad
      (Ent iSwordBroad
          (Just "sword")
          "broadsword" ""
          "The blade of the broadsword is straight, double-edged, and pointed. It's about 3.5 feet long including the \
          \handle. Although there's nothing extraordinary about the sword, it's a decent, solid weapon."
          swordSmell
          zeroBits)
      (mkObj . ObjTemplate broadswordWeight broadswordVol swordTaste $ zeroBits)
      (Wpn OneHanded 1 10)

  putWpn iSwordShort
      (Ent iSwordShort
          (Just "sword")
          "shortsword" ""
          "The shortsword is a straightforward, cut-and-thrust sword and a trusty weapon. It's about 20 inches long."
          swordSmell
          zeroBits)
      (mkObj . ObjTemplate shortswordWeight shortswordVol swordTaste $ zeroBits)
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
          (0, -9, 0)
          InsideEnv
          (Just "Writables")
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


mkWaterskin :: Id -> Maybe VesselCont -> MudStack ()
mkWaterskin i l = putVessel i
    (Ent i
        (Just "waterskin")
        "waterskin" ""
        waterskinDesc
        Nothing
        zeroBits)
    (mkObj . ObjTemplate waterskinWeight waterskinVol Nothing $ zeroBits)
    l
    Nothing


mkWpnTaste :: Text -> Maybe Text
mkWpnTaste t = Just $ "You lick the head of the " <> t <> ". It tastes metallic."


swordSmell :: Maybe Text
swordSmell = Just "The blade of the sword smells like metal."


swordTaste :: Maybe Text
swordTaste = Just "You lick the blade of the sword, taking care not to cut your tongue. It tastes slightly metallic."


waterskinDesc :: Text
waterskinDesc = "The handy waterskin, crafted from the bladder of a bovine animal, is an indispensable piece of \
                \equipment when it comes to travel and, often, everyday life."
