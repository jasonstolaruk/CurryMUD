{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.TheWorld.TheWorld ( initMudData
                             , initWorld ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Put
import Mud.Misc.Logging hiding (logNotice)
import Mud.TheWorld.Ids
import Mud.TopLvlDefs.FilePaths
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent.STM.TMVar (newTMVarIO)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Lens.Setter (ASetter)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bits (setBit, zeroBits)
import Data.IORef (newIORef)
import Data.List (delete, foldl', sort)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import System.Clock (Clock(..), getTime)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Random.MWC (createSystemRandom)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.IntMap.Lazy as IM (empty, foldrWithKey, toList, map)
import qualified Data.Map.Lazy as M (empty, fromList)
import qualified Data.Text as T


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.TheWorld"


-- ==================================================


initMudData :: ShouldLog -> IO MudData
initMudData shouldLog = do
    (errorLogService, noticeLogService) <- initLogging shouldLog
    genIO   <- createSystemRandom
    msIORef <- newIORef MudState { _armTbl        = IM.empty
                                 , _clothTbl      = IM.empty
                                 , _coinsTbl      = IM.empty
                                 , _conTbl        = IM.empty
                                 , _entTbl        = IM.empty
                                 , _eqTbl         = IM.empty
                                 , _hostTbl       =  M.empty
                                 , _invTbl        = IM.empty
                                 , _mobTbl        = IM.empty
                                 , _msgQueueTbl   = IM.empty
                                 , _objTbl        = IM.empty
                                 , _pcTbl         = IM.empty
                                 , _plaLogTbl     = IM.empty
                                 , _plaTbl        = IM.empty
                                 , _rmTbl         = IM.empty
                                 , _rmTeleNameTbl = IM.empty
                                 , _talkAsyncTbl  =  M.empty
                                 , _threadTbl     =  M.empty
                                 , _typeTbl       = IM.empty
                                 , _wpnTbl        = IM.empty }
    ls    <- Locks <$> newTMVarIO Done
                   <*> newTMVarIO Done
                   <*> newTMVarIO Done
                   <*> newTMVarIO Done
                   <*> newTMVarIO Done
                   <*> newTMVarIO Done
                   <*> newTMVarIO Done
    start <- getTime Monotonic
    return MudData { _errorLog       = errorLogService
                   , _gen            = genIO
                   , _locks          = ls
                   , _mudStateIORef  = msIORef
                   , _noticeLog      = noticeLogService
                   , _startTime      = start }


initWorld :: MudStack Bool
initWorld = dropIrrelevantFilenames . sort <$> (liftIO . getDirectoryContents $ persistDir) >>= \cont ->
    ()# cont ? (createWorld >> return True) :? (loadWorld . last $ cont)


createWorld :: MudStack ()
createWorld = do
    logNotice "createWorld" "creating the world."

    putPla iRoot (Ent iRoot Nothing "Root" "" "This is the root admin." zeroBits) [] mempty M.empty (Mob Male 50 50 50 50 10 10 0 RHand) (PC iLoggedOut Human [] []) (Pla "" Nothing adminFlags 80 24 Nothing [] [] [] (Just iLounge))
    putPla iJason (Ent iJason Nothing "Jason" "" "Jason is the creator of CurryMUD." zeroBits) [] mempty M.empty (Mob Male 50 50 50 50 10 10 0 LHand) (PC iLoggedOut Human [] []) (Pla "" Nothing adminFlags 80 24 Nothing [] [] [] (Just iLounge))

    putRm iLoggedOut (pure iRoot) mempty (Rm "Logged out room" "PCs are placed here when their players log out." zeroBits [])
    putRm iWelcome [] mempty (Rm "Welcome room" "Ad-hoc PCs created for new connections are placed here." zeroBits [])
    putRm iCentral [] mempty (Rm "Central control room" "Welcome to the heart of the machine." zeroBits [ StdLink Northeast iObjCloset, StdLink East iClothCloset, StdLink Southeast iCoinsCloset, StdLink South iConCloset, StdLink Southwest iWpnCloset, StdLink West iArmCloset, StdLink Northwest iMobCloset, StdLink Up iWeightRm, StdLink Down iVoid ])
    putRm iObjCloset [ iKewpie1, iKewpie2 ] mempty (Rm "Object closet" "This closet holds objects." zeroBits [ StdLink Southwest iCentral ])
    putRm iClothCloset [ iChemise, iTunic, iApron, iTabard, iGreyCoat, iFrockCoat, iBreeches1, iBreeches2, iTrousers1, iTrousers2 ] mempty (Rm "Clothing closet" "This closet holds clothing." zeroBits [ StdLink West iCentral, StdLink Down iAccessoriesCloset ])
    putRm iAccessoriesCloset [ iEar1, iEar2, iEar3, iEar4, iEar5, iEar6, iEar7, iEar8, iNoseRing1, iNoseRing2, iNoseRing3, iNeck1, iNeck2, iNeck3, iNeck4, iBracelet1, iBracelet2, iBracelet3, iBracelet4, iBracelet5, iBracelet6, iBracelet7, iBracelet8, iRing1, iRing2, iRing3, iRing4, iRing5, iRing6, iRing7, iRing8, iRing9 ] mempty (Rm "Accessories closet" "This closet holds accessories." zeroBits [ StdLink Up iClothCloset ])
    putRm iCoinsCloset [] (Coins (100, 100, 100)) (Rm "Coin closet" "This closet holds coins." zeroBits [ StdLink Northwest iCentral ])
    putRm iConCloset [ iSack1, iSack2, iBackpack1, iBackpack2 ] mempty (Rm "Container closet" "This closet holds containers." zeroBits [ StdLink North iCentral ])
    putRm iWpnCloset [ iSword1, iSword2, iLongSword, iClub, iKnife1, iKnife2 ] mempty (Rm "Weapon closet" "This closet holds weapons." zeroBits [ StdLink Northeast iCentral ])
    putRm iArmCloset [ iCap, iHelm, iSandals1, iSandals2, iBoots ] mempty (Rm "Armor closet" "This closet holds armor." zeroBits [ StdLink East iCentral ])
    putRm iMobCloset [ iRockCavy1, iRockCavy2, iPidge ] mempty (Rm "Mob closet" "This closet holds mobs." zeroBits [ StdLink Southeast iCentral ])
    putRm iVoid [] mempty (Rm "The void" "You have stumbled into an empty space. The world dissolves into nothingness. You are floating." zeroBits [ StdLink Up iCentral, NonStdLink "lounge" iLounge "% enters the lounge." "% enters the lounge." ])
    putRm iLounge [] mempty (Rm "The admin lounge" "Welcome, admin! Have a seat by the fire and relax for awhile." zeroBits [ NonStdLink "out" iVoid "% exits the lounge." "% exits the lounge." ])
    putRm iWeightRm [ i100Lb, i75Lb, i50Lb1, i50Lb2, i25Lb1, i25Lb2, i10Lb1, i10Lb2, i5Lb1, i5Lb2, i1Lb1, i1Lb2, i1Lb3, i1Lb4, i1Lb5 ] mempty (Rm "The weight room" "This room contains weights." zeroBits [ StdLink Down iCentral ])

    putRmTeleName iCentral "central"
    putRmTeleName iLounge "lounge"

    putObj iKewpie1 (Ent iKewpie1 (Just "doll") "kewpie doll" "" "The kewpie doll is disgustingly cute." zeroBits) (Obj 25 1)
    putObj iKewpie2 (Ent iKewpie2 (Just "doll") "kewpie doll" "" "The kewpie doll is disgustingly cute." zeroBits) (Obj 25 1)
    putObj i100Lb (Ent i100Lb (Just "weight") "100 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 10000 1)
    putObj i75Lb (Ent i75Lb (Just "weight") "75 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 7500 1)
    putObj i50Lb1 (Ent i50Lb1 (Just "weight") "50 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 5000 1)
    putObj i50Lb2 (Ent i50Lb2 (Just "weight") "50 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 5000 1)
    putObj i25Lb1 (Ent i25Lb1 (Just "weight") "25 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 2500 1)
    putObj i25Lb2 (Ent i25Lb2 (Just "weight") "25 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 2500 1)
    putObj i10Lb1 (Ent i10Lb1 (Just "weight") "10 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 1000 1)
    putObj i10Lb2 (Ent i10Lb2 (Just "weight") "10 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 1000 1)
    putObj i5Lb1 (Ent i5Lb1 (Just "weight") "5 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 500 1)
    putObj i5Lb2 (Ent i5Lb2 (Just "weight") "5 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 500 1)
    putObj i1Lb1 (Ent i1Lb1 (Just "weight") "1 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 100 1)
    putObj i1Lb2 (Ent i1Lb2 (Just "weight") "1 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 100 1)
    putObj i1Lb3 (Ent i1Lb3 (Just "weight") "1 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 100 1)
    putObj i1Lb4 (Ent i1Lb4 (Just "weight") "1 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 100 1)
    putObj i1Lb5 (Ent i1Lb5 (Just "weight") "1 lb weight" "" "It's a heavy slab of metal." zeroBits) (Obj 100 1)

    putCloth iEar1 (Ent iEar1 (Just "earring") "azure earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar2 (Ent iEar2 (Just "earring") "crimson earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar3 (Ent iEar3 (Just "earring") "sea green earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar4 (Ent iEar4 (Just "earring") "onyx earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar5 (Ent iEar5 (Just "earring") "azure earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar6 (Ent iEar6 (Just "earring") "crimson earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar7 (Ent iEar7 (Just "earring") "sea green earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iEar8 (Ent iEar8 (Just "earring") "onyx earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 2 1) Earring
    putCloth iNoseRing1 (Ent iNoseRing1 (Just "nose") "nose ring" "" "It's a plain copper stud, intended to be worn on the nose." zeroBits) (Obj 2 1) NoseRing
    putCloth iNoseRing2 (Ent iNoseRing2 (Just "nose") "nose ring" "" "It's a plain copper stud, intended to be worn on the nose." zeroBits) (Obj 2 1) NoseRing
    putCloth iNoseRing3 (Ent iNoseRing3 (Just "nose") "nose ring" "" "It's a plain copper stud, intended to be worn on the nose." zeroBits) (Obj 2 1) NoseRing
    putCloth iNeck1 (Ent iNeck1 (Just "necklace") "bronze necklace" "" "It's a simple bronze chain." zeroBits) (Obj 4 1) Necklace
    putCloth iNeck2 (Ent iNeck2 (Just "necklace") "silver necklace" "" "It's a simple silver chain." zeroBits) (Obj 4 1) Necklace
    putCloth iNeck3 (Ent iNeck3 (Just "necklace") "gold necklace" "" "It's a simple gold chain." zeroBits) (Obj 4 1) Necklace
    putCloth iNeck4 (Ent iNeck4 (Just "necklace") "platinum necklace" "" "It's a simple platinum chain." zeroBits) (Obj 4 1) Necklace
    putCloth iBracelet1 (Ent iBracelet1 (Just "bracelet") "charm bracelet" "" "The bracelet is adorned with a variety of quaint charms in the shape of musical instruments, fashioned out of pewter." zeroBits) (Obj 10 1) Bracelet
    putCloth iBracelet2 (Ent iBracelet2 (Just "bracelet") "wooden bangle bracelet" "" "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and about half an inch wide." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet3 (Ent iBracelet3 (Just "bracelet") "beaded bracelet" "" "This classic bracelet consist of small, spherical wooden beads, alternating black and white in color." zeroBits) (Obj 2 1) Bracelet
    putCloth iBracelet4 (Ent iBracelet4 (Just "bracelet") "pearl bracelet" "" "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory." zeroBits) (Obj 4 1) Bracelet
    putCloth iBracelet5 (Ent iBracelet5 (Just "bracelet") "charm bracelet" "" "The bracelet is adorned with a variety of quaint charms in the shape of musical instruments, fashioned out of pewter." zeroBits) (Obj 10 1) Bracelet
    putCloth iBracelet6 (Ent iBracelet6 (Just "bracelet") "wooden bangle bracelet" "" "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and about half an inch wide." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet7 (Ent iBracelet7 (Just "bracelet") "beaded bracelet" "" "This classic bracelet consist of small, spherical wooden beads, alternating black and white in color." zeroBits) (Obj 2 1) Bracelet
    putCloth iBracelet8 (Ent iBracelet8 (Just "bracelet") "pearl bracelet" "" "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory." zeroBits) (Obj 4 1) Bracelet
    putCloth iRing1 (Ent iRing1 (Just "ring") "garnet ring" "" "It's a simple copper band prominently featuring a beautiful garnet stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing2 (Ent iRing2 (Just "ring") "amethyst ring" "" "It's a simple copper band prominently featuring a beautiful amethyst stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing3 (Ent iRing3 (Just "ring") "aquamarine ring" "" "It's a simple copper band prominently featuring a beautiful aquamarine stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing4 (Ent iRing4 (Just "ring") "diamond ring" "" "It's a simple copper band prominently featuring a beautiful diamond stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing5 (Ent iRing5 (Just "ring") "garnet ring" "" "It's a simple copper band prominently featuring a beautiful garnet stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing6 (Ent iRing6 (Just "ring") "amethyst ring" "" "It's a simple copper band prominently featuring a beautiful amethyst stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing7 (Ent iRing7 (Just "ring") "aquamarine ring" "" "It's a simple copper band prominently featuring a beautiful aquamarine stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing8 (Ent iRing8 (Just "ring") "diamond ring" "" "It's a simple copper band prominently featuring a beautiful diamond stone." zeroBits) (Obj 5 1) Ring
    putCloth iRing9 (Ent iRing9 (Just "ring") "emerald ring" "" "It's a simple copper band prominently featuring a beautiful emerald stone." zeroBits) (Obj 5 1) Ring
    putCloth iChemise (Ent iChemise (Just "chemise") "fine white chemise" "" "This voluminous frock, worn on the upper body, is fashioned out of thin, smooth linen. It hangs just below the waist, while its loose-cut, wide sleeves are elbow length." zeroBits) (Obj 100 1) Shirt
    putCloth iTunic (Ent iTunic (Just "tunic") "cobalt blue wool tunic" "" "This heavy wool tunic is waist length and short-sleeved. Decorative white embroidery along the neck, sleeves, and waist adds an eye-catching touch." zeroBits) (Obj 225 1) Shirt
    putCloth iApron (Ent iApron (Just "apron") "heavy brown apron" "" "This sturdy padded utility apron provides adequate protection while its wearer labors and toils." zeroBits) (Obj 225 1) Smock
    putCloth iTabard (Ent iTabard (Just "tabard") "sleeveless blue tabard" "" "This sleeveless overgarment is open at both sides and extends down to the thigh. Dyed a deep shade of blue, a contrasting bright orange trim adds a distinct accent along the hems. There is a short collar around the neck, complete with a small decorative yellow bowtie." zeroBits) (Obj 150 1) Smock
    putCloth iGreyCoat (Ent iGreyCoat (Just "coat") "mouse-grey coat" "" "Sure to keep its wearer warm in all but the coldest of weather, this heavy, long-sleeved coat reaches the knees, and features a tall collar followed by ten large silver buttons along its length." zeroBits) (Obj 1000 1) Coat
    putCloth iFrockCoat (Ent iFrockCoat (Just "coat") "woman's red frock coat" "" "This fashionable long-sleeved coat is made of soft, bright-red fabric decorated with a fine, rich floral brochade. Six black buttons from the collar down the chest, when fastened, make this a particularly figure-flattering garment." zeroBits) (Obj 150 1) Coat
    putCloth iBreeches1 (Ent iBreeches1 (Just "breeches") "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches" "These thin, tight-fitting breeches extend just past the knees, where short drawstrings allow them to be neatly secured." zeroBits) (Obj 100 1) Trousers
    putCloth iBreeches2 (Ent iBreeches2 (Just "breeches") "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches" "These thin, tight-fitting breeches extend just past the knees, where short drawstrings allow them to be neatly secured." zeroBits) (Obj 100 1) Trousers
    putCloth iTrousers1 (Ent iTrousers1 (Just "trousers") "pair of baggy beige trousers" "pairs of baggy beige trousers" "These wool trousers are loose-fitting so as to grant uninhibited movement. A rugged hemp drawstring allows them to be snugly tightened at the waist." zeroBits) (Obj 125 1) Trousers
    putCloth iTrousers2 (Ent iTrousers2 (Just "trousers") "pair of baggy beige trousers" "pairs of baggy beige trousers" "These wool trousers are loose-fitting so as to grant uninhibited movement. A rugged hemp drawstring allows them to be snugly tightened at the waist." zeroBits) (Obj 125 1) Trousers

    putCon iSack1 (Ent iSack1 (Just "sack") "cloth sack" "" "It's a typical cloth sack, perfect for holding your treasure. It's red." zeroBits) (Obj 100 1) [] mempty Nothing (Con False 10)
    putCon iSack2 (Ent iSack2 (Just "sack") "cloth sack" "" "It's a typical cloth sack, perfect for holding your treasure. It's blue." zeroBits) (Obj 100 1) [] mempty Nothing (Con False 10)
    putCon iBackpack1 (Ent iBackpack1 (Just "back") "backpack" "" "The sturdy backpack is made of leather." zeroBits) (Obj 500 1) [] mempty (Just Backpack) (Con True 10)
    putCon iBackpack2 (Ent iBackpack2 (Just "back") "backpack" "" "The sturdy backpack is made of leather." zeroBits) (Obj 500 1) [] mempty (Just Backpack) (Con True 10)

    putWpn iSword1 (Ent iSword1 (Just "sword") "short sword" "" "It's a sword; short but still sharp!" zeroBits) (Obj 200 1) (Wpn OneHanded 1 10)
    putWpn iSword2 (Ent iSword2 (Just "sword") "short sword" "" "It's a sword; short but still sharp!" zeroBits) (Obj 200 1) (Wpn OneHanded 1 10)
    putWpn iLongSword (Ent iLongSword (Just "sword") "two-handed long sword" "" "With the right technique, this bulky sword could do a great deal of damage." zeroBits) (Obj 400 1) (Wpn TwoHanded 1 10)
    putWpn iClub (Ent iClub (Just "club") "wooden club" "" "It's a crude wooden club, the type a neanderthal might use to great effect." zeroBits) (Obj 300 1) (Wpn OneHanded 1 10)
    putWpn iKnife1 (Ent iKnife1 (Just "knife") "utility knife" "utility knives" "This small knife could be useful in a pinch." zeroBits) (Obj 50 1) (Wpn OneHanded 1 10)
    putWpn iKnife2 (Ent iKnife2 (Just "knife") "utility knife" "utility knives" "This small knife could be useful in a pinch." zeroBits) (Obj 50 1) (Wpn OneHanded 1 10)

    putArm iCap (Ent iCap (Just "cap") "knit cap" "" "It's a simple knit cap, designed to keep your head warm in cold weather." zeroBits) (Obj 10 1) (Arm Head 1)
    putArm iHelm (Ent iHelm (Just "helmet") "leather helmet" "" "The functional leather helmet provides a comfortable fit." zeroBits) (Obj 300 1) (Arm Head 1)
    putArm iSandals1 (Ent iSandals1 (Just "sandals") "pair of leather sandals" "pairs of leather sandals" "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the soles of your feet." zeroBits) (Obj 50 1) (Arm Feet 1)
    putArm iSandals2 (Ent iSandals2 (Just "sandals") "pair of leather sandals" "pairs of leather sandals" "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the soles of your feet." zeroBits) (Obj 50 1) (Arm Feet 1)
    putArm iBoots (Ent iBoots (Just "boots") "pair of leather boots" "pairs of leather boots" "These rugged, sturdy boots make excellent footwear for traveling across a variety of terrain." zeroBits) (Obj 300 1) (Arm Feet 1)

    putMob iRockCavy1 (Ent iRockCavy1 (Just "rock") "rock cavy" "rock cavies" "It looks like a slightly oversized guinea pig with soft, grey fur. You imagine that the rock cavy would prefer dry, rocky areas (with low, scrubby vegetation), close to stony mountains and hills." zeroBits) [] mempty M.empty (Mob Male 25 75 10 25 10 10 10 NoHand)
    putMob iRockCavy2 (Ent iRockCavy2 (Just "rock") "rock cavy" "rock cavies" "It looks like a slightly oversized guinea pig with soft, grey fur. You imagine that the rock cavy would prefer dry, rocky areas (with low, scrubby vegetation), close to stony mountains and hills." zeroBits) [] mempty M.empty (Mob Female 25 75 10 25 10 10 10 NoHand)
    putMob iPidge (Ent iPidge (Just "pidge") "Pidge" "" "Pidge is a female dwarf with walnut-colored skin and large, brown eyes. She wears her silver-white hair in shoulder-length pigtails. Her small, round face is positively adorable." zeroBits) [] mempty (M.fromList [ (ShirtS, iPeasant'sShirt), (SmockS, iLeatherApron), (TrousersS, iOveralls), (FeetS, iTraveler'sBoots) ]) (Mob Female 50 50 50 50 10 10 10 RHand)
    putCloth iPeasant'sShirt (Ent iPeasant'sShirt (Just "shirt") "white peasant's shirt" "" "This shirt, favored by skilled laborers and lowly bumpkins alike, represents the epitome of function over fashion." zeroBits) (Obj 100 1) Shirt
    putCloth iOveralls (Ent iOveralls (Just "overalls") "pair of many-pocketed brown overalls" "pairs of many-pocketed brown overalls" "These durable overalls are adorned with a multitude of little pockets." zeroBits) (Obj 225 1) Trousers
    putCloth iLeatherApron (Ent iLeatherApron (Just "apron") "leather apron" "" "This heavy apron, though bulky, is a must for those who undertake in dirty and dangerous chores." zeroBits) (Obj 325 1) Smock
    putArm iTraveler'sBoots (Ent iTraveler'sBoots (Just "boots") "pair of jet-black traveler's boots" "pair of jet-black traveler's boots" "These well-crafted, thigh-high boots are rugged and durable." zeroBits) (Obj 300 1) (Arm Feet 1)


adminFlags :: Int
adminFlags = foldl' setBit zeroBits . map fromEnum $ [ IsAdmin, IsNotFirstAdminMsg, IsNotFirstLook, IsNotFirstMobSay ]


loadWorld :: FilePath -> MudStack Bool
loadWorld dir@((persistDir </>) -> path) = do
    logNotice "loadWorld" $ "loading the world from the " <> (dblQuote . T.pack $ dir) <> " directory."
    loadEqTblRes <- loadEqTbl path
    ((loadEqTblRes :) -> res) <- mapM (path |&|) [ loadTbl armTblFile        armTbl
                                                 , loadTbl clothTblFile      clothTbl
                                                 , loadTbl coinsTblFile      coinsTbl
                                                 , loadTbl conTblFile        conTbl
                                                 , loadTbl entTblFile        entTbl
                                                 , loadTbl hostTblFile       hostTbl
                                                 , loadTbl invTblFile        invTbl
                                                 , loadTbl mobTblFile        mobTbl
                                                 , loadTbl objTblFile        objTbl
                                                 , loadTbl pcTblFile         pcTbl
                                                 , loadTbl plaTblFile        plaTbl
                                                 , loadTbl rmTblFile         rmTbl
                                                 , loadTbl rmTeleNameTblFile rmTeleNameTbl
                                                 , loadTbl typeTblFile       typeTbl
                                                 , loadTbl wpnTblFile        wpnTbl ]
    modifyState $ \ms -> (foldr removeAdHoc ms . getInv iWelcome $ ms, ())
    movePCs
    return . and $ res


loadEqTbl :: FilePath -> MudStack Bool
loadEqTbl ((</> eqTblFile) -> absolute) = do
    json <- liftIO . B.readFile $ absolute
    case eitherDecode json of
      Left err -> sorry absolute err
      Right (IM.map (M.fromList . map swap . IM.toList) -> tbl) -> modifyState ((, ()) . (eqTbl .~ tbl)) >> return True


sorry :: FilePath -> String -> MudStack Bool
sorry absolute (T.pack -> err) =
    (logError . T.concat $ [ "error parsing ", dblQuote . T.pack $ absolute, ": ", err, "." ]) >> return False


loadTbl :: (FromJSON b) => FilePath -> ASetter MudState MudState a b -> FilePath -> MudStack Bool
loadTbl tblFile lens path = let absolute = path </> tblFile in
    eitherDecode <$> (liftIO . B.readFile $ absolute) >>= \case
      Left  err -> sorry absolute err
      Right tbl -> modifyState ((, ()) . (lens .~ tbl)) >> return True


movePCs :: MudStack ()
movePCs = modifyState $ \ms ->
    let idsWithRmIds       = let pairs = IM.foldrWithKey (\i pc -> ((i, pc^.rmId) :)) [] $ ms^.pcTbl
                             in filter ((/= iLoggedOut) . snd) pairs
        helper (i, ri) ms' = ms' & invTbl.ind ri         %~ (i `delete`)
                                 & invTbl.ind iLoggedOut %~ (i :)
                                 & pcTbl .ind i.rmId     .~ iLoggedOut
                                 & plaTbl.ind i.lastRmId .~ Just ri
    in (foldr helper ms idsWithRmIds, ())
