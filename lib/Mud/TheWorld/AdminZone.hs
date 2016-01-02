{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.AdminZone (createAdminZone) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Put
import Mud.TheWorld.AdminZoneIds
import Mud.TheWorld.TutorialIds (iTutWelcome)
import qualified Mud.Misc.Logging as L (logNotice)

import Data.Bits (setBit, zeroBits)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Lazy as M (empty, fromList)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.AdminZone"


-- ==================================================


adminFlags :: Int
adminFlags = foldl' setBit zeroBits . map fromEnum $ [ IsAdmin
                                                     , IsNotFirstAdminMsg
                                                     , IsNotFirstLook
                                                     , IsNotFirstMobSay
                                                     , IsTunedAdmin
                                                     , IsTunedQuestion ]


-----


createAdminZone :: MudStack ()
createAdminZone = do
  logNotice "createAdminZone" "creating the admin zone."

  -- ==================================================
  -- Players:
  putPla iRoot
         (Ent iRoot
              Nothing
              "Root" ""
              "He is the root administrator."
              zeroBits)
         []
         mempty
         M.empty
         (Mob Male
              50 50 50 50 50
              100 100
              100 100
              100 100
              100 100
              0
              RHand
              iLoggedOut
              Nothing Nothing)
         M.empty
         (M.fromList [("Curry", True)])
         (PC Human ["Curry"] ["Curry"])
         (Pla "" Nothing
              (setBit adminFlags . fromEnum $ IsIncognito)
              80 24
              [] [] Nothing
              []
              (Just iLounge))
  putPla iCurry
         (Ent iCurry
              Nothing
              "Curry" ""
              "He is a CurryMUD administrator."
              zeroBits)
         []
         mempty
         M.empty
         (Mob Male
              50 50 50 50 50
              100 100
              100 100
              100 100
              100 100
              0
              RHand
              iLoggedOut
              Nothing Nothing)
         M.empty
         (M.fromList [("Root", True)])
         (PC Human ["Root"] ["Root"])
         (Pla "" Nothing
              adminFlags
              80 24
              [] [] Nothing
              []
              (Just iLounge))

  -- ==================================================
  -- Rooms:
  putRm iLoggedOut
        [iRoot]
        mempty
        (Rm "Logged out room"
            "PCs are placed here when their players log out."
            zeroBits
            [])
  putRm iWelcome
        []
        mempty
        (Rm "Welcome room"
            "Ad-hoc PCs created for new connections are placed here."
            zeroBits
            [])
  putRm iCentral
        []
        mempty
        (Rm "Central control room"
            "Welcome to the heart of the machine. Sprawled about this dome-shaped, white room is a cluster of \
            \electronic displays and control panels, used by the admins to monitor and supervise the daily operations \
            \of CurryMUD.\n\
            \A spiral staircase leads down."
            zeroBits
            [ StdLink Down iBasement ])
  putRm iBasement
        []
        mempty
        (Rm "The basement"
            "This dusty, unfinished basement smells of mold.\n\
            \Eight doors are positioned about the round, stucco wall at even intervals. A spiral staircase leads up. \
            \Next to the staircase lies an open manhole."
            zeroBits
            [ StdLink North     iWeightRm
            , StdLink Northeast iObjCloset
            , StdLink East      iClothCloset
            , StdLink Southeast iCoinsCloset
            , StdLink South     iConCloset
            , StdLink Southwest iWpnCloset
            , StdLink West      iArmCloset
            , StdLink Northwest iMobCloset
            , StdLink Up        iCentral
            , NonStdLink "manhole" iVoid "% climbs into the manhole." "% climbs out of the manhole." ])
  putRm iWeightRm
        [ i190Lb
        , i100Lb
        , i75Lb
        , i50Lb1
        , i50Lb2
        , i25Lb1
        , i25Lb2
        , i10Lb1
        , i10Lb2
        , i5Lb1
        , i5Lb2
        , i1Lb1
        , i1Lb2
        , i1Lb3
        , i1Lb4
        , i1Lb5 ]
        mempty
        (Rm "Weight closet"
            "This closet holds weights."
            zeroBits
            [ StdLink South iBasement ])
  putRm iObjCloset
        [ iKewpie1, iKewpie2 ]
        mempty
        (Rm "Object closet"
            "This closet holds objects."
            zeroBits
            [ StdLink Southwest iBasement ])
  putRm iClothCloset
        [ iChemise, iTunic, iApron, iTabard, iGreyCoat, iFrockCoat, iBreeches1, iBreeches2, iTrousers1, iTrousers2 ]
        mempty
        (Rm "Clothing closet"
            "This closet holds clothing."
            zeroBits
            [ StdLink West iBasement, StdLink Down iAccessoriesCloset ])
  putRm iAccessoriesCloset
        [ iEar1
        , iEar2
        , iEar3
        , iEar4
        , iEar5
        , iEar6
        , iEar7
        , iEar8
        , iNoseRing1
        , iNoseRing2
        , iNoseRing3
        , iNeck1
        , iNeck2
        , iNeck3
        , iNeck4
        , iBracelet1
        , iBracelet2
        , iBracelet3
        , iBracelet4
        , iBracelet5
        , iBracelet6
        , iBracelet7
        , iBracelet8
        , iRing1
        , iRing2
        , iRing3
        , iRing4
        , iRing5
        , iRing6
        , iRing7
        , iRing8
        , iRing9 ]
        mempty
        (Rm "Accessories closet"
            "This closet holds accessories."
            zeroBits
            [ StdLink Up iClothCloset ])
  putRm iCoinsCloset
        []
        (Coins (100, 100, 100))
        (Rm "Coin closet"
            "This closet holds coins."
            zeroBits
            [ StdLink Northwest iBasement ])
  putRm iConCloset
        [ iSack1, iSack2, iBackpack1, iBackpack2 ]
        mempty
        (Rm "Container closet"
            "This closet holds containers."
            zeroBits
            [ StdLink North iBasement ])
  putRm iWpnCloset
        [ iSword1, iSword2, iLongSword, iClub, iKnife1, iKnife2 ]
        mempty
        (Rm "Weapon closet"
            "This closet holds weapons."
            zeroBits
            [ StdLink Northeast iBasement ])
  putRm iArmCloset
        [ iCap, iHelm, iSandals1, iSandals2, iBoots ]
        mempty
        (Rm "Armor closet"
            "This closet holds armor."
            zeroBits
            [ StdLink East iBasement ])
  putRm iMobCloset
        [ iRockCavy1, iRockCavy2, iPidge, iSkeleton ]
        mempty
        (Rm "Mob closet"
            "This closet holds mobs."
            zeroBits
            [ StdLink Southeast iBasement ])
  putRm iVoid
        []
        mempty
        (Rm "The void"
            "You have stumbled into a vast, empty space. You are floating.\n\
            \An open manhole hovers above you. You see a colorful round shape some distance off to the north, while to \
            \the south a door floats innocuously."
            zeroBits
            [ StdLink North iTutEntrance
            , StdLink South iLoungeEntrance
            , NonStdLink "manhole" iBasement "% climbs into the manhole." "% climbs out of the manhole." ])
  putRm iTutEntrance
        []
        mempty
        (Rm "The portal"
            "Floating before you is a large round portal in which dazzling shapes and colors spin and dance. You feel \
            \a peculiar pulling sensation in your abdomen, as if the portal is attempting to draw you towards itself.\n\
            \Suspended above the portal is a wooden plaque reading, \"TUTORIAL THIS WAY.\""
            zeroBits
            [ StdLink South iVoid
            , NonStdLink "portal" iTutWelcome "% floats into the portal, and promptly disappears."
                                              "% arrives in the tutorial." ])
  putRm iLoungeEntrance
        []
        mempty
        (Rm "The floating door"
            "Floating before you is polished wooden door surrounded by featureless white trimming. Hanging from a nail \
            \affixed to the door is a small sign reading, \"Admin Lounge.\""
            zeroBits
            [ StdLink North iVoid
            , NonStdLink "lounge" iLounge "% enters the lounge." "% enters the lounge." ])
  putRm iLounge
        []
        mempty
        (Rm "The admin lounge"
            "Welcome, admin! Have a seat by the fire and relax for awhile."
            zeroBits
            [ NonStdLink "out" iLoungeEntrance "% exits the lounge." "% exits the lounge." ])

  -- ==================================================
  -- Room teleport names:
  putRmTeleName iCentral "central"
  putRmTeleName iLounge "lounge"

  -- ==================================================
  -- Objects:
  let kewpieDesc = "The kewpie doll is disgustingly cute."
  putObj iKewpie1
         (Ent iKewpie1
              (Just "doll")
              "kewpie doll" ""
              kewpieDesc
              zeroBits)
         (Obj 25 1)
  putObj iKewpie2
         (Ent iKewpie2
              (Just "doll")
              "kewpie doll" ""
              kewpieDesc
              zeroBits)
         (Obj 25 1)
  let weightDesc = "It's a heavy slab of metal."
  putObj i190Lb
         (Ent i190Lb
              (Just "weight")
              "190 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 19000 1)
  putObj i100Lb
         (Ent i100Lb
              (Just "weight")
              "100 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 10000 1)
  putObj i75Lb
         (Ent i75Lb
              (Just "weight")
              "75 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 7500 1)
  putObj i50Lb1
         (Ent i50Lb1
              (Just "weight")
              "50 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 5000 1)
  putObj i50Lb2
         (Ent i50Lb2
              (Just "weight")
              "50 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 5000 1)
  putObj i25Lb1
         (Ent i25Lb1
              (Just "weight")
              "25 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 2500 1)
  putObj i25Lb2
         (Ent i25Lb2
              (Just "weight")
              "25 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 2500 1)
  putObj i10Lb1
         (Ent i10Lb1
              (Just "weight")
              "10 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 1000 1)
  putObj i10Lb2
         (Ent i10Lb2
              (Just "weight")
              "10 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 1000 1)
  putObj i5Lb1
         (Ent i5Lb1
              (Just "weight")
              "5 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 500 1)
  putObj i5Lb2
         (Ent i5Lb2
              (Just "weight")
              "5 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 500 1)
  putObj i1Lb1
         (Ent i1Lb1
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 100 1)
  putObj i1Lb2
         (Ent i1Lb2
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 100 1)
  putObj i1Lb3
         (Ent i1Lb3
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 100 1)
  putObj i1Lb4
         (Ent i1Lb4
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 100 1)
  putObj i1Lb5
         (Ent i1Lb5
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              zeroBits)
         (Obj 100 1)

  -- ==================================================
  -- Clothing:
  let earringDesc = "It's a small, but tasteful, nondescript hoop."
  putCloth iEar1
           (Ent iEar1
                (Just "earring")
                "azure earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar2
           (Ent iEar2
                (Just "earring")
                "crimson earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar3
           (Ent iEar3
                (Just "earring")
                "sea green earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar4
           (Ent iEar4
                (Just "earring")
                "onyx earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar5
           (Ent iEar5
                (Just "earring")
                "azure earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar6
           (Ent iEar6
                (Just "earring")
                "crimson earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar7
           (Ent iEar7
                (Just "earring")
                "sea green earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  putCloth iEar8
           (Ent iEar8
                (Just "earring")
                "onyx earring" ""
                earringDesc
                zeroBits)
           (Obj 2 1)
           Earring
  let noseRingDesc = "It's a plain copper stud, intended to be worn on the nose."
  putCloth iNoseRing1
           (Ent iNoseRing1
                (Just "nose")
                "nose ring" ""
                noseRingDesc
                zeroBits)
           (Obj 2 1)
           NoseRing
  putCloth iNoseRing2
           (Ent iNoseRing2
                (Just "nose")
                "nose ring" ""
                noseRingDesc
                zeroBits)
           (Obj 2 1)
           NoseRing
  putCloth iNoseRing3
           (Ent iNoseRing3
                (Just "nose")
                "nose ring" ""
                noseRingDesc
                zeroBits)
           (Obj 2 1)
           NoseRing
  let mkNecklaceDesc x = "It's a simple " <> x <> " chain."
  putCloth iNeck1
           (Ent iNeck1
                (Just "necklace")
                "bronze necklace" ""
                (mkNecklaceDesc "bronze")
                zeroBits)
           (Obj 4 1)
           Necklace
  putCloth iNeck2
           (Ent iNeck2
                (Just "necklace")
                "silver necklace" ""
                (mkNecklaceDesc "silver")
                zeroBits)
           (Obj 4 1)
           Necklace
  putCloth iNeck3
           (Ent iNeck3
                (Just "necklace")
                "gold necklace" ""
                (mkNecklaceDesc "gold")
                zeroBits)
           (Obj 4 1)
           Necklace
  putCloth iNeck4
           (Ent iNeck4
                (Just "necklace")
                "platinum necklace" ""
                (mkNecklaceDesc "platinum")
                zeroBits)
           (Obj 4 1)
           Necklace
  let charmBraceletDesc = "The bracelet is adorned with a variety of quaint charms in the shape of musical \
                          \instruments, fashioned out of pewter."
  putCloth iBracelet1
           (Ent iBracelet1
                (Just "bracelet")
                "charm bracelet" ""
                charmBraceletDesc
                zeroBits)
           (Obj 10 1)
           Bracelet
  let bangleBraceletDesc = "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and \
                           \about half an inch wide."
  putCloth iBracelet2
           (Ent iBracelet2
                (Just "bracelet")
                "wooden bangle bracelet" ""
                bangleBraceletDesc
                zeroBits)
           (Obj 1 1)
           Bracelet
  let beadedBraceletDesc = "This classic bracelet consist of small, spherical wooden beads, alternating black and \
                           \white in color."
  putCloth iBracelet3
           (Ent iBracelet3
                (Just "bracelet")
                "beaded bracelet" ""
                beadedBraceletDesc
                zeroBits)
           (Obj 2 1)
           Bracelet
  let pearlBraceletDesc = "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory."
  putCloth iBracelet4
           (Ent iBracelet4
                (Just "bracelet")
                "pearl bracelet" ""
                pearlBraceletDesc
                zeroBits)
           (Obj 4 1)
           Bracelet
  putCloth iBracelet5
           (Ent iBracelet5
                (Just "bracelet")
                "charm bracelet" ""
                charmBraceletDesc
                zeroBits)
           (Obj 10 1)
           Bracelet
  putCloth iBracelet6
           (Ent iBracelet6
                (Just "bracelet")
                "wooden bangle bracelet" ""
                bangleBraceletDesc
                zeroBits)
           (Obj 1 1)
           Bracelet
  putCloth iBracelet7
           (Ent iBracelet7
                (Just "bracelet")
                "beaded bracelet" ""
                beadedBraceletDesc
                zeroBits)
           (Obj 2 1)
           Bracelet
  putCloth iBracelet8
           (Ent iBracelet8
                (Just "bracelet")
                "pearl bracelet" ""
                pearlBraceletDesc
                zeroBits)
           (Obj 4 1)
           Bracelet
  let mkRingDesc x = "It's a simple copper band prominently featuring a beautiful " <> x <> " stone."
  putCloth iRing1
           (Ent iRing1
                (Just "ring")
                "garnet ring" ""
                (mkRingDesc "garnet")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing2
           (Ent iRing2
                (Just "ring")
                "amethyst ring" ""
                (mkRingDesc "amethyst")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing3
           (Ent iRing3
                (Just "ring")
                "aquamarine ring" ""
                (mkRingDesc "aquamarine")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing4
           (Ent iRing4
                (Just "ring")
                "diamond ring" ""
                (mkRingDesc "diamond")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing5
           (Ent iRing5
                (Just "ring")
                "garnet ring" ""
                (mkRingDesc "garnet")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing6
           (Ent iRing6
                (Just "ring")
                "amethyst ring" ""
                (mkRingDesc "amethyst")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing7
           (Ent iRing7
                (Just "ring")
                "aquamarine ring" ""
                (mkRingDesc "aquamarine")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing8
           (Ent iRing8
                (Just "ring")
                "diamond ring" ""
                (mkRingDesc "diamond")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iRing9
           (Ent iRing9
                (Just "ring")
                "emerald ring" ""
                (mkRingDesc "emerald")
                zeroBits)
           (Obj 5 1)
           Ring
  putCloth iChemise
           (Ent iChemise
                (Just "chemise")
                "fine white chemise" ""
                "This voluminous frock, worn on the upper body, is fashioned out of thin, smooth linen. It hangs just \
                \below the waist, while its loose-cut, wide sleeves are elbow length."
                zeroBits)
           (Obj 100 1)
           Shirt
  putCloth iTunic
           (Ent iTunic
                (Just "tunic")
                "cobalt blue wool tunic" ""
                "This heavy wool tunic is waist length and short-sleeved. Decorative white embroidery along the neck, \
                \sleeves, and waist adds an eye-catching touch."
                zeroBits)
           (Obj 225 1)
           Shirt
  putCloth iApron
           (Ent iApron
                (Just "apron")
                "heavy brown apron" ""
                "This sturdy padded utility apron provides adequate protection while its wearer labors and toils."
                zeroBits)
           (Obj 225 1)
           Smock
  putCloth iTabard
           (Ent iTabard
                (Just "tabard")
                "sleeveless blue tabard" ""
                "This sleeveless overgarment is open at both sides and extends down to the thigh. Dyed a deep shade of \
                \blue, a contrasting bright orange trim adds a distinct accent along the hems. There is a short collar \
                \around the neck, complete with a small decorative yellow bowtie."
                zeroBits)
           (Obj 150 1)
           Smock
  putCloth iGreyCoat
           (Ent iGreyCoat
                (Just "coat")
                "mouse-grey coat" ""
                "Sure to keep its wearer warm in all but the coldest of weather, this heavy, long-sleeved coat reaches \
                \the knees, and features a tall collar followed by ten large silver buttons along its length."
                zeroBits)
           (Obj 1000 1)
           Coat
  putCloth iFrockCoat
           (Ent iFrockCoat
                (Just "coat")
                "woman's red frock coat" ""
                "This fashionable long-sleeved coat is made of soft, bright-red fabric decorated with a fine, rich \
                \floral brochade. Six black buttons from the collar down the chest, when fastened, make this a \
                \particularly figure-flattering garment."
                zeroBits)
           (Obj 150 1)
           Coat
  let breechesDesc = "These thin, tight-fitting breeches extend just past the knees, where short drawstrings allow \
                     \them to be neatly secured."
  putCloth iBreeches1
           (Ent iBreeches1
                (Just "breeches")
                "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches"
                breechesDesc
                zeroBits)
           (Obj 100 1)
           Trousers
  putCloth iBreeches2
           (Ent iBreeches2
                (Just "breeches")
                "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches"
                breechesDesc
                zeroBits)
           (Obj 100 1)
           Trousers
  let trousersDesc = "These wool trousers are loose-fitting so as to grant uninhibited movement. A rugged hemp \
                     \drawstring allows them to be snugly tightened at the waist."
  putCloth iTrousers1
           (Ent iTrousers1
                (Just "trousers")
                "pair of baggy beige trousers" "pairs of baggy beige trousers"
                trousersDesc
                zeroBits)
           (Obj 125 1)
           Trousers
  putCloth iTrousers2
           (Ent iTrousers2
                (Just "trousers")
                "pair of baggy beige trousers" "pairs of baggy beige trousers"
                trousersDesc
                zeroBits)
           (Obj 125 1)
           Trousers

  -- ==================================================
  -- Containers:
  let mkSackDesc x = "It's a typical cloth sack, perfect for holding your treasure. It's " <> x <> "."
  putCon iSack1
         (Ent iSack1
              (Just "sack")
              "cloth sack" ""
              (mkSackDesc "red")
              zeroBits)
         (Obj 100 1)
         []
         mempty
         Nothing
         (Con False 10)
  putCon iSack2
         (Ent iSack2
              (Just "sack")
              "cloth sack" ""
              (mkSackDesc "blue")
              zeroBits)
         (Obj 100 1)
         []
         mempty
         Nothing
         (Con False 10)
  let backpackDesc = "The sturdy backpack is made of leather."
  putCon iBackpack1
         (Ent iBackpack1
              (Just "back")
              "backpack" ""
              backpackDesc
              zeroBits)
         (Obj 500 1)
         []
         mempty
         (Just Backpack)
         (Con True 10)
  putCon iBackpack2
         (Ent iBackpack2
              (Just "back")
              "backpack" ""
              backpackDesc
              zeroBits)
         (Obj 500 1)
         []
         mempty
         (Just Backpack)
         (Con True 10)

  -- ==================================================
  -- Weapons:
  let swordDesc = "It's a sword; short but still sharp!"
  putWpn iSword1
         (Ent iSword1
              (Just "sword")
              "short sword" ""
              swordDesc
              zeroBits)
         (Obj 200 1)
         (Wpn OneHanded 1 10)
  putWpn iSword2
         (Ent iSword2
              (Just "sword")
              "short sword" ""
              swordDesc
              zeroBits)
         (Obj 200 1)
         (Wpn OneHanded 1 10)
  putWpn iLongSword
         (Ent iLongSword
              (Just "sword")
              "two-handed long sword" ""
              "With the right technique, this bulky sword could do a great deal of damage."
              zeroBits)
         (Obj 400 1)
         (Wpn TwoHanded 1 10)
  putWpn iClub
         (Ent iClub
              (Just "club")
              "wooden club" ""
              "It's a crude wooden club, the type a neanderthal might use to great effect."
              zeroBits)
         (Obj 300 1)
         (Wpn OneHanded 1 10)
  let knifeDesc = "This small knife could be useful in a pinch."
  putWpn iKnife1
         (Ent iKnife1
              (Just "knife")
              "utility knife" "utility knives"
              knifeDesc
              zeroBits)
         (Obj 50 1)
         (Wpn OneHanded 1 10)
  putWpn iKnife2
         (Ent iKnife2
              (Just "knife")
              "utility knife" "utility knives"
              knifeDesc
              zeroBits)
         (Obj 50 1)
         (Wpn OneHanded 1 10)

  -- ==================================================
  -- Armor:
  putArm iCap
         (Ent iCap
              (Just "cap")
              "knit cap" ""
              "It's a simple knit cap, designed to keep your head warm in cold weather."
              zeroBits)
         (Obj 10 1)
         (Arm Head 1)
  putArm iHelm
         (Ent iHelm
              (Just "helmet")
              "leather helmet" ""
              "The functional leather helmet provides a comfortable fit."
              zeroBits)
         (Obj 300 1)
         (Arm Head 1)
  let sandalsDesc = "These humble leather sandals offer little in the way of fashion; they will, however, adequately \
                    \protect the soles of your feet."
  putArm iSandals1
         (Ent iSandals1
              (Just "sandals")
              "pair of leather sandals" "pairs of leather sandals"
              sandalsDesc
              zeroBits)
         (Obj 50 1)
         (Arm Feet 1)
  putArm iSandals2
         (Ent iSandals2
              (Just "sandals")
              "pair of leather sandals" "pairs of leather sandals"
              sandalsDesc
              zeroBits)
         (Obj 50 1)
         (Arm Feet 1)
  putArm iBoots
         (Ent iBoots
              (Just "boots")
              "pair of leather boots" "pairs of leather boots"
              "These rugged, sturdy boots make excellent footwear for traveling across a variety of terrain."
              zeroBits)
         (Obj 300 1)
         (Arm Feet 1)

  -- ==================================================
  -- Mobs:
  let rockCavyDesc = "It looks like a slightly oversized guinea pig with soft, grey fur. You imagine that the rock \
                     \cavy would prefer dry, rocky areas (with low, scrubby vegetation), close to stony mountains and \
                     \hills."
  putNpc iRockCavy1
         (Ent iRockCavy1
              (Just "rock")
              "rock cavy" "rock cavies"
              rockCavyDesc
              zeroBits)
         []
         mempty
         M.empty
         (Mob Male
              50 50 50 50 50
              10 10
              10 10
              10 10
              10 10
              10
              NoHand
              iMobCloset
              Nothing Nothing)
  putNpc iRockCavy2
         (Ent iRockCavy2
              (Just "rock")
              "rock cavy" "rock cavies"
              rockCavyDesc
              zeroBits)
         []
         mempty
         M.empty
         (Mob Male
              50 50 50 50 50
              10 10
              10 10
              10 10
              10 10
              10
              NoHand
              iMobCloset
              Nothing Nothing)
  putNpc iPidge
         (Ent iPidge
              (Just "pidge")
              "Pidge" ""
              "Pidge is a female dwarf with walnut-colored skin and large, brown eyes. She wears her silver-white hair \
              \in shoulder-length pigtails. Her small, round face is positively adorable."
              zeroBits)
         []
         mempty
         (M.fromList [ (ShirtS,    iPeasant'sShirt )
                     , (SmockS,    iLeatherApron   )
                     , (TrousersS, iOveralls       )
                     , (FeetS,     iTraveler'sBoots) ])
         (Mob Female
              50 50 50 50 50
              10  100 -- TODO: Poor Pidge...
              100 100
              100 100
              100 100
              0
              RHand
              iMobCloset
              Nothing Nothing)
  putCloth iPeasant'sShirt
           (Ent iPeasant'sShirt
                (Just "shirt")
                "white peasant's shirt" ""
                "This shirt, favored by skilled laborers and lowly bumpkins alike, represents the epitome of function \
                \over fashion."
                zeroBits)
           (Obj 100 1)
           Shirt
  putCloth iOveralls
           (Ent iOveralls
                (Just "overalls")
                "pair of many-pocketed brown overalls" "pairs of many-pocketed brown overalls"
                "These durable overalls are adorned with a multitude of little pockets."
                zeroBits)
           (Obj 225 1)
           Trousers
  putCloth iLeatherApron
           (Ent iLeatherApron
                (Just "apron")
                "leather apron" ""
                "This heavy apron, though bulky, is a must for those who undertake dirty and dangerous chores."
                zeroBits)
           (Obj 325 1)
           Smock
  putArm iTraveler'sBoots
         (Ent iTraveler'sBoots (Just "boots")
              "pair of jet-black traveler's boots" "pair of jet-black traveler's boots"
              "These well-crafted, thigh-high boots are rugged and durable."
              zeroBits)
         (Obj 300 1)
         (Arm Feet 1)
  putNpc iSkeleton
         (Ent iSkeleton
              (Just "skeleton")
              "undead skeleton" ""
              "This mindless, bipedal skeleton has been animated and tasked with doing its master's bidding."
              zeroBits)
         []
         mempty
         M.empty
         (Mob NoSex
              50 50 50 50 50
              10 10
              10 10
              10 10
              10 10
              10
              RHand
              iMobCloset
              Nothing Nothing)
