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
import           Mud.TopLvlDefs.Misc
import           Mud.TopLvlDefs.Seconds
import           Mud.TopLvlDefs.Vals
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Wear
import           Mud.TopLvlDefs.Weights
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Text

import           Control.Arrow (second)
import           Control.Monad (forM_)
import           Data.Bits (zeroBits)
import qualified Data.Map.Strict as M (empty, fromList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T


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
      (Coins (1000, 1000, 1000))
      (mkRm (RmTemplate "Welcome to the warehouse"
          "This is the warehouse. Items to be cloned are stored here.\n\
          \There's just one rule: you can look, but don't touch!"
          Nothing
          Nothing
          zeroBits
          [ StdLink South iArmRm 0 ]
          (0, 0, 0)
          OutsideEnv
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
      (let ot = ObjTemplate bootsWeight
                            bootsVol
                            (mkLeatherTasteSalty "boots" "foot")
                            bootsLeatherVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Arm Feet 1)

  putArm iBootsThigh
      (Ent iBootsThigh
          (Just "boots")
          "pair of jet-black, thigh-high boots" "pairs of jet-black, thigh-high boots"
          "These well-crafted, thigh-high boots are rugged and durable."
          Nothing
          zeroBits)
      (let ot = ObjTemplate bootsWeight
                            bootsVol
                            Nothing
                            bootsThighVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Arm Feet 1)

  putArm iCapKnit
      (Ent iCapKnit
          (Just "cap")
          "knit cap" ""
          "Constructed out of yellow and blue yarn, this simple knit cap is designed to keep your head warm in cold \
          \weather."
          (Just "There is a faint scent of yarn.")
          zeroBits)
      (let ot = ObjTemplate knitCapWeight
                            knitCapVol
                            (Just "It pretty much just tastes like yarn.")
                            capKnitVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Arm Head 1)

  putArm iHelmLeather
      (Ent iHelmLeather
          (Just "helmet")
          "leather helmet" ""
          "This soft leather helmet covers the skull and is secured with a strap across the chin."
          (mkLeatherSmell "helmet smells")
          zeroBits)
      (let ot = ObjTemplate helmLeatherWeight
                            helmLeatherVol
                            (mkLeatherTasteSalty "helmet" "head")
                            helmLeatherVal
                            helmWear
                            zeroBits
       in mkObj ot)
      (Arm Head 1)

  putArm iSandalsLeather
      (Ent iSandalsLeather
          (Just "sandals")
          "pair of simple leather sandals" "pairs of simple leather sandals"
          "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the \
          \soles of your feet."
          (mkLeatherSmell "sandals smell")
          zeroBits)
      (let ot = ObjTemplate sandalsWeight
                            sandalsVol
                            (mkLeatherTasteSalty "sandals" "foot")
                            sandalsLeatherVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Arm Feet 1)

  -----

  putRm iClothRm
      [ iApronHeavy
      , iApronLeather
      , iBreeches
      , iChemise
      , iCoatFrock
      , iCoatGrey
      , iOveralls
      , iShirtPeasant
      , iTabard
      , iTrousers
      , iTunicHeavy ]
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

  putCloth iApronHeavy
      (Ent iApronHeavy
          (Just "apron")
          "heavy brown apron" ""
          "This padded utility apron provides adequate protection while its wearer labors and toils."
          (Just "The apron smells like the layered cloth from which its padding is constructed.")
          zeroBits)
      (let ot = ObjTemplate apronWeight
                            apronVol
                            (Just "You put the apron in your mouth. You wisely conclude that it tastes like cloth.")
                            apronHeavyVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Smock

  putCloth iApronLeather
      (Ent iApronLeather
          (Just "apron")
          "leather apron" ""
          "This heavy apron, though bulky, is a must for those who undertake dirty and dangerous chores."
          (mkLeatherSmell "apron smells")
          zeroBits)
      (let ot = ObjTemplate apronWeight
                            apronVol
                            (mkLeatherTaste "apron")
                            apronLeatherVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Smock

  putCloth iBreeches
      (Ent iBreeches
          (Just "breeches")
          "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches"
          "These thin, tight-fitting breeches extend just past the knees, where short drawstrings allow them to be \
          \neatly secured to the legs."
          (mkFabricSmell False "breeches")
          zeroBits)
      (let ot = ObjTemplate trousersWeight
                            trousersVol
                            (mkFabricTaste False "breeches")
                            breechesVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Trousers

  putCloth iChemise
      (Ent iChemise
          (Just "chemise")
          "fine white chemise" ""
          "This voluminous frock, worn on the upper body, is fashioned out of thin, smooth linen. It hangs just below \
          \the waist. Its loose-cut, wide sleeves are elbow length."
          (mkFabricSmell True "chemise")
          zeroBits)
      (let ot = ObjTemplate shirtWeight
                            shirtVol
                            (mkFabricTaste True "chemise")
                            chemiseVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Shirt

  putCloth iCoatFrock
      (Ent iCoatFrock
          (Just "coat")
          "woman's red frock coat" ""
          "This fashionable long-sleeved coat is made of soft, bright-red fabric decorated with a fine, rich floral \
          \brocade. There are six black buttons from the collar down the chest. When fastened, this is a particularly \
          \figure-flattering garment."
          (mkFabricSmell True "coat")
          zeroBits)
      (let ot = ObjTemplate coatWeight
                            coatVol
                            (mkFabricTaste True "coat")
                            coatFrockVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Coat

  putCloth iCoatGrey
      (Ent iCoatGrey
          (Just "coat")
          "mouse-grey coat" ""
          "Sure to keep its wearer warm in all but the coldest of weather, this heavy, long-sleeved coat reaches the \
          \knees. A tall collar is followed by ten large silver buttons along its length."
          (mkFabricSmell True "coat")
          zeroBits)
      (let ot = ObjTemplate coatHeavyWeight
                            coatHeavyVol
                            (mkFabricTaste True "coat")
                            coatHeavyVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Coat

  putCloth iOveralls
      (Ent iOveralls
          (Just "overalls")
          "pair of many-pocketed brown overalls" "pairs of many-pocketed brown overalls"
          "These durable overalls are adorned with a multitude of little pockets."
          (mkFabricSmell False "overalls")
          zeroBits)
      (let ot = ObjTemplate overallsWeight
                            overallsVol
                            (mkFabricTaste False "overalls")
                            overallsVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Trousers

  putCloth iShirtPeasant
      (Ent iShirtPeasant
          (Just "shirt")
          "white peasant's shirt" ""
          "This shirt, favored by skilled laborers and lowly bumpkins alike, represents the epitome of function over \
          \fashion."
          (mkFabricSmell True "shirt")
          zeroBits)
      (let ot = ObjTemplate shirtWeight
                            shirtVol
                            (mkFabricTaste True "shirt")
                            shirtPeasantVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Shirt

  putCloth iTabard
      (Ent iTabard
          (Just "tabard")
          "sleeveless blue tabard" ""
          "This sleeveless overgarment is open at both sides and extends down to the thigh. Dyed a deep shade of blue, \
          \a contrasting bright orange trim adds a distinct accent along the hems. There is a short collar around the \
          \neck complete with a small yellow bowtie."
          (mkFabricSmell True "tabard")
          zeroBits)
      (let ot = ObjTemplate tabardWeight
                            tabardVol
                            (mkFabricTaste True "tabard")
                            tabardVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Smock

  putCloth iTrousers
      (Ent iTrousers
          (Just "trousers")
          "pair of baggy beige trousers" "pairs of baggy beige trousers"
          "These wool trousers are loose-fitting so as to grant uninhibited movement. A rugged hemp drawstring allows \
          \them to be snugly tightened at the waist."
          (mkFabricSmell False "trousers")
          zeroBits)
      (let ot = ObjTemplate trousersBaggyWeight
                            trousersBaggyVol
                            (mkFabricTaste False "trousers")
                            trousersVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Trousers

  putCloth iTunicHeavy
      (Ent iTunicHeavy
          (Just "tunic")
          "cobalt blue wool tunic" ""
          "This heavy wool tunic is waist-length and short-sleeved. Decorative white embroidery along the neck, \
          \sleeves, and waist adds an eye-catching touch."
          (mkFabricSmell True "tunic")
          zeroBits)
      (let ot = ObjTemplate tunicHeavyWeight
                            tunicHeavyVol
                            (mkFabricTaste True "tunic")
                            tunicHeavyVal
                            Nothing
                            zeroBits
       in mkObj ot)
      Shirt

  -----

  putRm iAccessoriesRm
      [ iBraceletBangle, iBraceletBeaded, iBraceletCharm, iBraceletPearl
      , iEarAzure, iEarCrimson, iEarOnyx, iEarSeaGreen
      , iNeckBronze, iNeckGold, iNeckSilver
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

  let charmBraceletDesc  = "The bracelet is adorned with a variety of quaint pewter charms in the shape of musical \
                           \instruments."
      bangleBraceletDesc = "The bangle bracelet is made of smooth, polished wood stained an earthy shade of brown. It's \
                           \about half an inch wide."
      beadedBraceletDesc = "This classic bracelet consists of small, spherical wooden beads alternating black and white \
                           \in color."
      pearlBraceletDesc  = "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory."

      braceletTuples = [ (iBraceletBangle, "wooden bangle", bangleBraceletDesc, 1,  braceletVal     )
                       , (iBraceletBeaded, "beaded",        beadedBraceletDesc, 2,  braceletVal     )
                       , (iBraceletCharm,  "charm",         charmBraceletDesc,  10, braceletCharmVal)
                       , (iBraceletPearl,  "pearl",         pearlBraceletDesc,  4,  braceletPearlVal) ]

  forM_ braceletTuples $ \(i, t, d, w, val) ->
      putCloth i
          (Ent i
              (Just "bracelet")
              (t <> " bracelet") ""
              d
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w braceletVol Nothing val Nothing $ zeroBits)
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
          (mkObj . ObjTemplate earWeight earVol Nothing earVal Nothing $ zeroBits)
          Earring

  let neckTuples = [ (iNeckBronze,   "bronze",   neckBronzeVal  )
                   , (iNeckGold,     "gold",     neckGoldVal    )
                   , (iNeckSilver,   "silver",   neckSilverVal  ) ]

  forM_ neckTuples $ \(i, t, val) ->
      putCloth i
          (Ent i
              (Just "necklace")
              (t <> " necklace") ""
              ("It's an artless " <> t <> " chain fashioned out of tiny rectangular links.")
              Nothing
              zeroBits)
          (mkObj . ObjTemplate neckWeight neckVol Nothing val Nothing $ zeroBits)
          Necklace

  putCloth iNoseRing
      (Ent iNoseRing
          (Just "nose")
          "nose ring" ""
          "It's a round copper stud intended to be worn on the nose."
          Nothing
          zeroBits)
      (mkObj . ObjTemplate noseWeight noseVol Nothing noseVal Nothing $ zeroBits)
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
              ("A simple copper band prominently features a beautiful " <> t <> " stone.")
              Nothing
              zeroBits)
          (mkObj . ObjTemplate ringWeight ringVol Nothing ringVal Nothing $ zeroBits)
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

  let backTuples = [ (iBackSml, "small ", backSmlWeight, backSmlVol, backSmlVal, backSmlCap)
                   , (iBack,    "",       backWeight,    backVol,    backVal,    backCap   )
                   , (iBackLrg, "large ", backLrgWeight, backLrgVol, backLrgVal, backLrgCap) ]

  forM_ backTuples $ \(i, t, w, v, val, c) ->
      putCon i
          (Ent i
              (Just "back")
              (t <> "backpack") ""
              "The sturdy backpack is made of leather. A wide flap, secured with two straps, closes over its sole \
              \compartment."
              (mkLeatherSmell "backpack smells")
              zeroBits)
          (let ot = ObjTemplate w
                                v
                                (mkLeatherTaste "backpack")
                                val
                                Nothing
                                zeroBits
           in mkObj ot)
          []
          mempty
          (Just Backpack)
          (Con True c zeroBits)

  let sackTuples = [ (iSackSml, "small ", sackSmlWeight, sackSmlVol, sackSmlVal, sackSmlCap)
                   , (iSack,    "",       sackWeight,    sackVol,    sackVal,    sackCap   )
                   , (iSackLrg, "large ", sackLrgWeight, sackLrgVol, sackLrgVal, sackLrgCap) ]

  forM_ sackTuples $ \(i, t, w, v, val, c) ->
      putCon i
          (Ent i
              (Just "sack")
              (t <> "sack") ""
              "The durable sack is made from a coarse woven fabric."
              (Just "The sack smells like burlap. It's a bit reminiscent of the smell of a barn or a farmyard.")
              zeroBits)
          (let ot = ObjTemplate w
                                v
                                (Just "Munching on the sack, you experience firsthand the earthy taste of burlap.")
                                val
                                Nothing
                                zeroBits
           in mkObj ot)
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
          [ StdLink North iConRm   0
          , StdLink South iLightRm 0 ]
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

  putRm iLightRm
      ([ iTorch1..iTorch1 + 9 ] ++ [ iLampSml, iLamp, iLampLrg ])
      mempty
      (mkRm (RmTemplate "Light source room"
          "This room holds light sources."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iFoodRm 0
          , StdLink South iNpcRm  0 ]
          (0, -5, 0)
          InsideEnv
          (Just "Light")
          M.empty [] []))

  forM_ [ iTorch1..iTorch1 + 9 ] $ \i -> putLight i
      (Ent i
          (Just "torch")
          "torch" "torches"
          "The torch is a bundle of rushes that provides illumination when set aflame. It's been dipped in pitch so as \
          \to encourage a long, steady burn."
          (Just "There is a subdued grassy smell emanating from the rushes, along with the scent of resin and tar.")
          zeroBits)
      (let ot = ObjTemplate torchWeight
                            torchVol
                            (Just "The rushes that comprise the torch have a grassy taste, while the pitch in which \
                                  \they have dipped has a sour, woody taste.")
                            torchVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Light Torch torchSecs False)

  putLight iLampSml
      (Ent iLampSml
          (Just "lamp")
          "small bronze lamp" ""
          "The portable oil lamp consists of a round covered vessel (containing the oil) from which a triangular spout \
          \extends. It may be comfortably carried by its curved metal handle."
          (mkLampSmell "metal")
          zeroBits)
      (let ot = ObjTemplate lampSmlWeight
                            lampSmlVol
                            (Just "You lick the oil lamp. It tastes of grimy metal.")
                            lampSmlVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Light (Lamp lampSmlSecs) lampSmlSecs False)

  putLight iLamp
      (Ent iLamp
          (Just "lamp")
          "ceramic oil lamp" ""
          "The reddish brown oil lamp is oblong and spherical in shape. A wick is threaded through the center of a cork \
          \at the top. There is a small round handle with which to carry it by."
          (mkLampSmell "ceramics")
          zeroBits)
      (let ot = ObjTemplate lampWeight
                            lampVol
                            lampCeramicTaste
                            lampVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Light (Lamp lampSecs) lampSecs False)

  putLight iLampLrg
      (Ent iLampLrg
          (Just "lamp")
          "large hanging lamp" ""
          "Three chains are attached to a large ceramic vessel in which the oil is contained. A wick is threaded \
          \through a round stopper at the top. Although the lamp is on the bulky side, it may be freely carried by its \
          \chains."
          (mkLampSmell "ceramics")
          zeroBits)
      (let ot = ObjTemplate lampLrgWeight
                            lampLrgVol
                            lampCeramicTaste
                            lampLrgVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Light (Lamp lampLrgSecs) lampLrgSecs False)

  -----

  putRm iNpcRm
      [ iPidge, iSkeleton ]
      mempty
      (mkRm (RmTemplate "NPC room"
          "This room holds NPCs."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iLightRm 0
          , StdLink South iObjRm   0 ]
          (0, -6, 0)
          InsideEnv
          (Just "NPCs")
          M.empty [] []))

  putNpc iPidge
      (Ent iPidge
          (Just "pidge")
          "Pidge" ""
          "Pidge is a female hobbit with walnut-colored skin and large brown eyes. She wears her silver-white hair in \
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

  putNpc iSkeleton
      (Ent iSkeleton
          (Just "skeleton")
          "undead skeleton" ""
          "This mindless bipedal skeleton has been animated and tasked with doing its master's bidding."
          (Just "The dry, fleshless bones of the skeleton are scentless.")
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
      [ iTinderbox ]
      mempty
      (mkRm (RmTemplate "Objects room"
          "This room holds objects."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iNpcRm    0
          , StdLink South iVesselRm 0 ]
          (0, -7, 0)
          InsideEnv
          (Just "Objects")
          M.empty [] []))

  putObj iTinderbox
      (Ent iTinderbox
          (Just "tinderbox")
          "tinderbox" "tinderboxes"
          "tinderbox desc" -- TODO: A "tinderbox" is a tinderbox with flint and steel strikers combined into a single object.
          (Just "You detect the lingering scent of burnt tinder.")
          zeroBits)
      (let ot = ObjTemplate tinderboxWeight
                            tinderboxVol
                            (Just "You lick the flint and steel strikers. The indistinct taste isn't anything to write \
                                  \home about.")
                            tinderboxVal
                            tinderboxWear
                            zeroBits
       in mkObj ot)

  -----

  putRm iVesselRm
      [ iBottleSml, iBottle, iBottleLrg, iBottleWithOil
      , iJarSml, iJar, iJarLrg
      , iJugSml, iJug, iJugLrg
      , iPotionFlask, iPotionFlaskLrg
      , iWaterskin, iWaterskinLrg, iWaterskinWithWater  ]
      mempty
      (mkRm (RmTemplate "Vessels room"
          "This room holds vessels."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iObjRm    0
          , StdLink South iWpnRm    0
          , StdLink Down  iPotionRm 0 ]
          (0, -8, 0)
          InsideEnv
          (Just "Vessels")
          M.empty [] []))

  let bottleTuples = [ (iBottleSml,     "small ", ("small ", "light brown" ), bottleSmlWeight, bottleSmlVol, bottleSmlVal)
                     , (iBottle,        "",       ("",       "mixed azure" ), bottleWeight,    bottleVol,    bottleVal   )
                     , (iBottleLrg,     "large ", ("large ", "rusty orange"), bottleLrgWeight, bottleLrgVol, bottleLrgVal)
                     , (iBottleWithOil, "",       ("",       "murky green" ), bottleWeight,    bottleVol,    bottleVal   ) ]

      mkBottleDesc a b =
          T.concat [ "This "
                   , a
                   , "earthenware bottle is designed to be as portable and practical as possible. A glaze of "
                   , b
                   , " hues gives the vessel a glossy finish and makes it impermeable." ]

  forM_ bottleTuples $ \(i, t, d, w, v, val) ->
      putVessel i
          (Ent i
              (Just "bottle")
              (t <> "bottle") ""
              (uncurry mkBottleDesc d)
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w v Nothing val Nothing $ zeroBits)
          (i == iBottleWithOil ? Just (oilLiq, maxBound) :? Nothing)
          Nothing

  let jarTuples = [ (iJarSml, "small ", "small and ", jarSmlWeight, jarSmlVol, jarSmlVal)
                  , (iJar,    "",       "",           jarWeight,    jarVol,    jarVal   )
                  , (iJarLrg, "large ", "large and ", jarLrgWeight, jarLrgVol, jarLrgVal) ]

  forM_ jarTuples $ \(i, t, d, w, v, val) ->
      putVessel i
          (Ent i
              (Just "jar")
              (t <> "jar") ""
              ("This " <> d <> "versatile glass jar comes affixed with an airtight lid.")
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w v Nothing val Nothing $ zeroBits)
          Nothing
          Nothing

  let jugTuples = [ (iJugSml, "small ", jugSmlWeight, jugSmlVol, jugSmlVal)
                  , (iJug,     "",      jugWeight,    jugVol,    jugVal   )
                  , (iJugLrg, "large ", jugLrgWeight, jugLrgVol, jugLrgVal) ]

  forM_ jugTuples $ \(i, t, w, v, val) ->
      putVessel i
          (Ent i
              (Just "jug")
              (t <> "jug") ""
              "While capable of containing a large amount of liquid, this corked ceramic jug is rather cumbersome."
              Nothing
              zeroBits)
          (mkObj . ObjTemplate w v Nothing val Nothing $ zeroBits)
          Nothing
          Nothing

  mkPotionFlask False iPotionFlask    Nothing
  mkPotionFlask True  iPotionFlaskLrg Nothing

  mkWaterskin iWaterskin          Nothing
  mkWaterskin iWaterskinWithWater (Just (waterLiq, maxBound))

  putVessel iWaterskinLrg
      (Ent iWaterskinLrg
          (Just "waterskin")
          "large waterskin" ""
          (waterskinDesc <> " This waterskin is particularly large, making it suitable for long journeys.")
          Nothing
          zeroBits)
      (mkObj . ObjTemplate waterskinLrgWeight waterskinLrgVol Nothing waterskinLrgVal Nothing $ zeroBits)
      Nothing
      Nothing

  -----

  let potionTuples = [ (iPotHp,              potHpLiq             )
                     , (iPotInstantHp,       potInstantHpLiq      )
                     , (iPotMp,              potMpLiq             )
                     , (iPotInstantMp,       potInstantMpLiq      )
                     , (iPotPp,              potPpLiq             )
                     , (iPotInstantPp,       potInstantPpLiq      )
                     , (iPotFp,              potFpLiq             )
                     , (iPotInstantFp,       potInstantFpLiq      )
                     , (iPotSt,              potStLiq             )
                     , (iPotInstantSt,       potInstantStLiq      )
                     , (iPotDx,              potDxLiq             )
                     , (iPotInstantDx,       potInstantDxLiq      )
                     , (iPotHt,              potHtLiq             )
                     , (iPotInstantHt,       potInstantHtLiq      )
                     , (iPotMa,              potMaLiq             )
                     , (iPotInstantMa,       potInstantMaLiq      )
                     , (iPotPs,              potPsLiq             )
                     , (iPotInstantPs,       potInstantPsLiq      )
                     , (iPotTinnitus,        potTinnitusLiq       )
                     , (iPotInstantTinnitus, potInstantTinnitusLiq) ]

  putRm iPotionRm
      (map fst potionTuples)
      mempty
      (mkRm (RmTemplate "Potions room"
          "This room holds vessels containing potions."
          Nothing
          Nothing
          zeroBits
          [ StdLink Up iVesselRm 0 ]
          (0, -8, -1)
          InsideEnv
          (Just "Potions")
          M.empty [] []))

  forM_ potionTuples $ uncurry (mkPotionFlask False) . second (Just . (, maxBound))

  -----

  putRm iWpnRm
      [ iAxeSml
      , iClub
      , iKnife
      , iMace
      , iSpear
      , iStaffQuarter
      , iSword
      , iSwordBroad
      , iSwordLong
      , iSwordShort ]
      mempty
      (mkRm (RmTemplate "Weapons room"
          "This room holds weapons."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iVesselRm   0
          , StdLink South iWritableRm 0 ]
          (0, -9, 0)
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
      (let ot = ObjTemplate axeSmlWeight
                            axeSmlVol
                            (mkWpnTaste "axe")
                            axeSmlVal
                            axeWear
                            zeroBits
       in mkObj ot)
      (Wpn OneHanded 1 10)

  putWpn iClub
      (Ent iClub
          (Just "club")
          "wooden club" ""
          "It's a crude wooden club, one of the simplest and oldest of weapons. This club is little more than a thick \
          \stick with a comfortable handle."
          (Just "The club smells like wood.")
          zeroBits)
      (let ot = ObjTemplate clubWeight
                            clubVol
                            (Just "You lick the club so as to discern its taste. It has a somewhat woody flavor.")
                            clubVal
                            clubWear
                            zeroBits
       in mkObj ot)
      (Wpn OneHanded 1 10)

  putWpn iKnife
      (Ent iKnife
          (Just "knife")
          "utility knife" "utility knives"
          "This small knife doesn't make much of a weapon, but it could be useful in a pinch."
          Nothing
          zeroBits)
      (let taste = "You don't dare taste the blade: you'd certainly cut up your mouth and tongue! You decide to lick \
                   \the handle instead. It tastes a little salty."
           ot    = ObjTemplate knifeWeight
                               knifeVol
                               (Just taste)
                               knifeVal
                               knifeWear
                               zeroBits
       in mkObj ot)
      (Wpn OneHanded 1 10)

  putWpn iMace
      (Ent iMace
          (Just "mace")
          "mace" ""
          "The mace is essentially a war club with a heavy round head of iron. You could really hurt someone with \
          \this thing."
          (mkWpnSmell "mace")
          zeroBits)
      (let ot = ObjTemplate maceWeight
                            maceVol
                            (mkWpnTaste "mace")
                            maceVal
                            maceWear
                            zeroBits
       in mkObj ot)
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
                   \the polished wooden handle instead. Sadly, it doesn't taste like much at all."
           ot    = ObjTemplate spearWeight
                               spearVol
                               (Just taste)
                               spearVal
                               spearWear
                               zeroBits
       in mkObj ot)
      (Wpn TwoHanded 1 10)

  putWpn iStaffQuarter
      (Ent iStaffQuarter
          (Just "staff")
          "quarterstaff" "quarterstaves"
          "The quarterstaff is a balanced wooden pole, about 5 feet in length. The correct technique requires a \
          \two-handed grip."
          (Just "The polished wood of the quarterstaff doesn't have a detectable smell.")
          zeroBits)
      (let ot = ObjTemplate staffQuarterWeight
                            staffQuarterVol
                            (Just "You lick the end of the quarterstaff. If anything, it might taste a little grimy.")
                            staffQuarterVal
                            staffQuarterWear
                            zeroBits
       in mkObj ot)
      (Wpn TwoHanded 1 10)

  putWpn iSword
      (Ent iSword
          (Just "sword")
          "sword" ""
          "The sword has a straight blade and a pointed tip. The hilt is metal with a generic round pommel at the \
          \top. It is a humble sword; there are no engravings or embellishments to speak of."
          swordSmell
          zeroBits)
      (let ot = ObjTemplate swordWeight
                            swordVol
                            swordTaste
                            swordVal
                            swordWear
                            zeroBits
       in mkObj ot)
      (Wpn OneHanded 1 10)

  putWpn iSwordBroad
      (Ent iSwordBroad
          (Just "sword")
          "broadsword" ""
          "The blade of the broadsword is straight, double-edged, and pointed. It's about 3.5 feet long including the \
          \hilt. Although there's nothing extraordinary about the sword, it's a decent, solid weapon."
          swordSmell
          zeroBits)
      (let ot = ObjTemplate swordBroadWeight
                            swordBroadVol
                            swordTaste
                            swordBroadVal
                            swordWear
                            zeroBits
       in mkObj ot)
      (Wpn OneHanded 1 10)

  putWpn iSwordLong
      (Ent iSwordLong
          (Just "sword")
          "longsword" ""
          "The longsword is a big weapon: not only is the straight blade quite long, but the hilt is lengthy enough to \
          \accommodate the required two hands. Despite its size, the sword is well balanced and not particularly heavy."
          swordSmell
          zeroBits)
      (let ot = ObjTemplate swordLongWeight
                            swordLongVol
                            swordTaste
                            swordLongVal
                            swordWear
                            zeroBits
       in mkObj ot)
      (Wpn TwoHanded 1 10)

  putWpn iSwordShort
      (Ent iSwordShort
          (Just "sword")
          "shortsword" ""
          "The shortsword is a straightforward cut-and-thrust blade and a trusty weapon. It's about 20 inches long."
          swordSmell
          zeroBits)
      (let ot = ObjTemplate swordShortWeight
                            swordShortVol
                            swordTaste
                            swordShortVal
                            swordWear
                            zeroBits
       in mkObj ot)
      (Wpn OneHanded 1 10)

  -----

  putRm iWritableRm
      [ iParchment1..iParchment1 + 9 ]
      mempty
      (mkRm (RmTemplate "Writables room"
          "This room holds writables."
          Nothing
          Nothing
          zeroBits
          [ StdLink North iWpnRm 0 ]
          (0, -10, 0)
          InsideEnv
          (Just "Writables")
          M.empty [] []))

  forM_ [ iParchment1..iParchment1 + 9 ] $ \i -> putWritable i
      (Ent i
          (Just "parchment")
          "piece of parchment" "pieces of parchment"
          "It's an everyday piece of parchment made from processed animal skin."
          Nothing -- Ancient parchment did not have a smell.
          zeroBits)
      (let ot = ObjTemplate parchmentWeight
                            parchmentVol
                            Nothing
                            parchmentVal
                            Nothing
                            zeroBits
       in mkObj ot)
      (Writable Nothing Nothing)

  -----

  putRmTeleName iWarehouseWelcome "warehouse"


-- ==================================================
-- Zone definition helper functions:


lampCeramicTaste :: Maybe Text
lampCeramicTaste = Just "You feel the coarse ceramic surface of the lamp with your tongue."


mkLampSmell :: Text -> Maybe Text
mkLampSmell t = Just $ "There is a slight smell of " <> t <> " and a lingering scent of oil."


type IsSing = Bool


mkFabricSmell :: IsSing -> Text -> Maybe Text
mkFabricSmell b t = Just . T.concat $ [ "The ", t, " smell", sOnTrue b, " like fabric." ]


mkFabricTaste :: IsSing -> Text -> Maybe Text
mkFabricTaste b t = Just . T.concat $ [ "You munch on the ", t, ". ", txt, " taste", sOnTrue b, " like fabric." ]
  where
    txt | b         = "It"
        | otherwise = "They"


mkLeatherSmell :: Text -> Maybe Text
mkLeatherSmell t = Just $ "The " <> t <> " like leather and not much else."


mkLeatherTaste :: Text -> Maybe Text
mkLeatherTaste t = Just $ "You chew on the " <> t <> ". It tastes like leather."


mkLeatherTasteSalty :: Text -> Text -> Maybe Text
mkLeatherTasteSalty a b = Just . T.concat $ [ "You chew on the "
                                            , a
                                            , ". The leather tastes a bit salty. Probably from "
                                            , b
                                            , " sweat." ]


mkPotionFlask :: Bool -> Id -> Maybe VesselCont -> MudStack ()
mkPotionFlask isLrg i mc = putVessel i
    (Ent i
        (Just "flask")
        (onTrue isLrg ("large " <>) "potion flask") ""
        ("This " <> t <> "glass flask complete with cork stopper is the ideal vessel for potion storage and \
                         \transportation.")
        Nothing
        zeroBits)
    (let ot = ObjTemplate (f potionFlaskLrgWeight potionFlaskWeight)
                          (f potionFlaskLrgVol    potionFlaskVol   )
                          Nothing
                          (f potionFlaskLrgVal    potionFlaskVal   )
                          Nothing
                          zeroBits
     in mkObj ot)
    mc
    Nothing
  where
    t     | isLrg     = ""
          | otherwise = "small "
    f a b | isLrg     = a
          | otherwise = b


mkWaterskin :: Id -> Maybe VesselCont -> MudStack ()
mkWaterskin i mc = putVessel i
    (Ent i
        (Just "waterskin")
        "waterskin" ""
        waterskinDesc
        Nothing
        zeroBits)
    (let ot = ObjTemplate waterskinWeight
                          waterskinVol
                          Nothing
                          waterskinVal
                          Nothing
                          zeroBits
     in mkObj ot)
    mc
    Nothing


mkWpnSmell :: Text -> Maybe Text
mkWpnSmell t = Just $ "The head of the " <> t <> " smells like metal. The handle doesn't smell like much at all."


mkWpnTaste :: Text -> Maybe Text
mkWpnTaste t = Just $ "You lick the head of the " <> t <> ". It tastes metallic."


swordSmell :: Maybe Text
swordSmell = Just "The blade of the sword smells like metal."


swordTaste :: Maybe Text
swordTaste = Just "You lick the blade of the sword, taking care not to cut your tongue. It tastes slightly metallic."


waterskinDesc :: Text
waterskinDesc = "The handy waterskin, crafted from the bladder of a bovine animal, is an indispensable piece of \
                \equipment when it comes to travel, and often to everyday life."
