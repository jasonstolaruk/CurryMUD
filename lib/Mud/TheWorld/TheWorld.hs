{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.TheWorld ( initMudState
                             , initWorld ) where

import Mud.Data.State.State
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Put
import Mud.Data.State.Util.STM
import Mud.TheWorld.Ids
import qualified Mud.Logging as L (logNotice)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM.TMVar (newTMVarIO)
import Control.Lens.Operators ((&), (.~), (^.))
import Data.Bits (zeroBits)
import Data.Monoid (mempty)
import Formatting ((%), sformat)
import Formatting.Formatters (stext)
import System.Clock (Clock(..), getTime)
import qualified Data.IntMap.Lazy as IM (empty, map)
import qualified Data.Map.Lazy as M (empty)
import qualified Data.Text as T


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.TheWorld"


-- ==================================================


initMudState :: IO MudState
initMudState = do
    let dictionaries = Dicts Nothing Nothing
    start <- getTime Monotonic
    (mqtTMVar, pltTMVar, ptTMVar, tatTMVar, ttTMVar, wsTMVar) <- (,,,,,) <$> newTMVarIO IM.empty
                                                                         <*> newTMVarIO IM.empty
                                                                         <*> newTMVarIO IM.empty
                                                                         <*> newTMVarIO M.empty
                                                                         <*> newTMVarIO M.empty
                                                                         <*> newTMVarIO ws
    return MudState { _worldStateTMVar = wsTMVar
                    , _nonWorldState   = NonWorldState { _dicts             = dictionaries
                                                       , _errorLog          = Nothing
                                                       , _msgQueueTblTMVar  = mqtTMVar
                                                       , _noticeLog         = Nothing
                                                       , _plaLogTblTMVar    = pltTMVar
                                                       , _plaTblTMVar       = ptTMVar
                                                       , _startTime         = start
                                                       , _talkAsyncTblTMVar = tatTMVar
                                                       , _threadTblTMVar    = ttTMVar } }
  where
    ws = WorldState IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty


initWorld :: MudStack ()
initWorld = createWorld >> sortAllInvs


createWorld :: MudStack ()
createWorld = do
    logNotice "createWorld" "creating the world."

    putRm iWelcome [] mempty (Rm "Welcome room" "Ad-hoc PCs created for new connections are placed here." zeroBits [ StdLink Down iCentral ])
    putRm iCentral [] mempty (Rm "Central control room" "Welcome to the heart of the machine." zeroBits [ StdLink Northeast iObjCloset, StdLink East iClothCloset, StdLink Southeast iCoinsCloset, StdLink South iConCloset, StdLink Southwest iWpnCloset, StdLink West iArmCloset, StdLink Northwest iMobCloset, StdLink Up iWelcome, StdLink Down iVoid ])
    putRm iObjCloset [ iKewpie1, iKewpie2 ] mempty (Rm "Object closet" "This closet holds objects." zeroBits [ StdLink Southwest iCentral ])
    putRm iClothCloset [ iEar1, iEar2, iEar3, iEar4, iEar5, iEar6, iEar7, iEar8, iNoseRing1, iNoseRing2, iNoseRing3, iNeck1, iNeck2, iNeck3, iNeck4, iBracelet1, iBracelet2, iBracelet3, iBracelet4, iBracelet5, iBracelet6, iBracelet7, iBracelet8, iRing1, iRing2, iRing3, iRing4, iRing5, iRing6, iRing7, iRing8, iRing9 ] mempty (Rm "Clothing closet" "This closet holds clothing." zeroBits [ StdLink West iCentral ])
    putRm iCoinsCloset [] (Coins (100, 100, 100)) (Rm "Coin closet" "This closet holds coins." zeroBits [ StdLink Northwest iCentral ])
    putRm iConCloset [ iBag1, iBag2, iBackpack1, iBackpack2 ] mempty (Rm "Container closet" "This closet holds containers." zeroBits [ StdLink North iCentral ])
    putRm iWpnCloset [ iSword1, iSword2, iLongSword, iClub, iKnife1, iKnife2 ] mempty (Rm "Weapon closet" "This closet holds weapons." zeroBits [ StdLink Northeast iCentral ])
    putRm iArmCloset [ iCap, iHelm, iSandals1, iSandals2, iBoots ] mempty (Rm "Armor closet" "This closet holds armor." zeroBits [ StdLink East iCentral ])
    putRm iMobCloset [iRockCavy] mempty (Rm "Mob closet" "This closet holds mobs." zeroBits [ StdLink Southeast iCentral ])
    putRm iVoid [] mempty (Rm "The void" "You have stumbled into an empty space. The world dissolves into nothingness. You are floating." zeroBits [ StdLink Up iCentral, NonStdLink "lounge" iLounge (sformat $ stext % " enters the lounge.") (sformat $ stext % " enters the lounge.") ])
    putRm iLounge [] mempty (Rm "The admin lounge" "Welcome, admin! Have a seat by the fire and relax for awhile." zeroBits [ NonStdLink "out" iVoid (sformat $ stext % " exits the hut.") (sformat $ stext % " exits the hut.") ])

    putObj iKewpie1 (Ent iKewpie1 (Just "doll") "kewpie doll" "" "The kewpie doll is disgustingly cute." zeroBits) (Obj 1 1)
    putObj iKewpie2 (Ent iKewpie2 (Just "doll") "kewpie doll" "" "The kewpie doll is disgustingly cute." zeroBits) (Obj 1 1)

    putCloth iEar1 (Ent iEar1 (Just "earring") "azure earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar2 (Ent iEar2 (Just "earring") "crimson earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar3 (Ent iEar3 (Just "earring") "sea green earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar4 (Ent iEar4 (Just "earring") "onyx earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar5 (Ent iEar5 (Just "earring") "azure earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar6 (Ent iEar6 (Just "earring") "crimson earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar7 (Ent iEar7 (Just "earring") "sea green earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iEar8 (Ent iEar8 (Just "earring") "onyx earring" "" "It's a small, but tasteful, nondescript hoop." zeroBits) (Obj 1 1) Earring
    putCloth iNoseRing1 (Ent iNoseRing1 (Just "nose") "nose ring" "" "It's a plain copper stud, intended to be worn on the nose." zeroBits) (Obj 1 1) NoseRing
    putCloth iNoseRing2 (Ent iNoseRing2 (Just "nose") "nose ring" "" "It's a plain copper stud, intended to be worn on the nose." zeroBits) (Obj 1 1) NoseRing
    putCloth iNoseRing3 (Ent iNoseRing3 (Just "nose") "nose ring" "" "It's a plain copper stud, intended to be worn on the nose." zeroBits) (Obj 1 1) NoseRing
    putCloth iNeck1 (Ent iNeck1 (Just "necklace") "bronze necklace" "" "It's a simple bronze chain." zeroBits) (Obj 1 1) Necklace
    putCloth iNeck2 (Ent iNeck2 (Just "necklace") "silver necklace" "" "It's a simple silver chain." zeroBits) (Obj 1 1) Necklace
    putCloth iNeck3 (Ent iNeck3 (Just "necklace") "gold necklace" "" "It's a simple gold chain." zeroBits) (Obj 1 1) Necklace
    putCloth iNeck4 (Ent iNeck4 (Just "necklace") "platinum necklace" "" "It's a simple platinum chain." zeroBits) (Obj 1 1) Necklace
    putCloth iBracelet1 (Ent iBracelet1 (Just "bracelet") "charm bracelet" "" "The bracelet is adorned with a variety of quaint charms in the shape of musical instruments, fashioned out of pewter." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet2 (Ent iBracelet2 (Just "bracelet") "wooden bangle bracelet" "" "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and about half an inch wide." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet3 (Ent iBracelet3 (Just "bracelet") "beaded bracelet" "" "This classic bracelet consist of small, spherical wooden beads, alternating black and white in color." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet4 (Ent iBracelet4 (Just "bracelet") "pearl bracelet" "" "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet5 (Ent iBracelet5 (Just "bracelet") "charm bracelet" "" "The bracelet is adorned with a variety of quaint charms in the shape of musical instruments, fashioned out of pewter." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet6 (Ent iBracelet6 (Just "bracelet") "wooden bangle bracelet" "" "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and about half an inch wide." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet7 (Ent iBracelet7 (Just "bracelet") "beaded bracelet" "" "This classic bracelet consist of small, spherical wooden beads, alternating black and white in color." zeroBits) (Obj 1 1) Bracelet
    putCloth iBracelet8 (Ent iBracelet8 (Just "bracelet") "pearl bracelet" "" "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory." zeroBits) (Obj 1 1) Bracelet
    putCloth iRing1 (Ent iRing1 (Just "ring") "garnet ring" "" "It's a simple copper band prominently featuring a beautiful garnet stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing2 (Ent iRing2 (Just "ring") "amethyst ring" "" "It's a simple copper band prominently featuring a beautiful amethyst stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing3 (Ent iRing3 (Just "ring") "aquamarine ring" "" "It's a simple copper band prominently featuring a beautiful aquamarine stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing4 (Ent iRing4 (Just "ring") "diamond ring" "" "It's a simple copper band prominently featuring a beautiful diamond stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing5 (Ent iRing5 (Just "ring") "garnet ring" "" "It's a simple copper band prominently featuring a beautiful garnet stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing6 (Ent iRing6 (Just "ring") "amethyst ring" "" "It's a simple copper band prominently featuring a beautiful amethyst stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing7 (Ent iRing7 (Just "ring") "aquamarine ring" "" "It's a simple copper band prominently featuring a beautiful aquamarine stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing8 (Ent iRing8 (Just "ring") "diamond ring" "" "It's a simple copper band prominently featuring a beautiful diamond stone." zeroBits) (Obj 1 1) Ring
    putCloth iRing9 (Ent iRing9 (Just "ring") "emerald ring" "" "It's a simple copper band prominently featuring a beautiful emerald stone." zeroBits) (Obj 1 1) Ring

    putCon iBag1 (Ent iBag1 (Just "sack") "cloth sack" "" "It's a typical cloth sack, perfect for holding your treasure. It's red." zeroBits) (Obj 1 1) [] mempty Nothing (Con 10 False)
    putCon iBag2 (Ent iBag2 (Just "sack") "cloth sack" "" "It's a typical cloth sack, perfect for holding your treasure. It's blue." zeroBits) (Obj 1 1) [] mempty Nothing (Con 10 False)
    putCon iBackpack1 (Ent iBackpack1 (Just "back") "backpack" "" "The sturdy backpack is made of leather." zeroBits) (Obj 1 1) [] mempty (Just Backpack) (Con 10 True)
    putCon iBackpack2 (Ent iBackpack2 (Just "back") "backpack" "" "The sturdy backpack is made of leather." zeroBits) (Obj 1 1) [] mempty (Just Backpack) (Con 10 True)

    putWpn iSword1 (Ent iSword1 (Just "sword") "short sword" "" "It's a sword; short but still sharp!" zeroBits) (Obj 1 1) (Wpn OneHanded 1 10)
    putWpn iSword2 (Ent iSword2 (Just "sword") "short sword" "" "It's a sword; short but still sharp!" zeroBits) (Obj 1 1) (Wpn OneHanded 1 10)
    putWpn iLongSword (Ent iLongSword (Just "sword") "two-handed long sword" "" "With the right technique, this bulky sword could do a great deal of damage." zeroBits) (Obj 1 1) (Wpn TwoHanded 1 10)
    putWpn iClub (Ent iClub (Just "club") "wooden club" "" "It's a crude wooden club, the type a neanderthal might use to great effect." zeroBits) (Obj 1 1) (Wpn OneHanded 1 10)
    putWpn iKnife1 (Ent iKnife1 (Just "knife") "pocket knife" "pocket knives" "This small utility knife could be useful in a pinch." zeroBits) (Obj 1 1) (Wpn OneHanded 1 10)
    putWpn iKnife2 (Ent iKnife2 (Just "knife") "pocket knife" "pocket knives" "This small utility knife could be useful in a pinch." zeroBits) (Obj 1 1) (Wpn OneHanded 1 10)

    putArm iCap (Ent iCap (Just "cap") "knit cap" "" "It's a simple knit cap, designed to keep your head warm in cold weather." zeroBits) (Obj 1 1) (Arm Head 1)
    putArm iHelm (Ent iHelm (Just "helmet") "leather helmet" "" "The functional leather helmet provides a comfortable fit." zeroBits) (Obj 1 1) (Arm Head 1)
    putArm iSandals1 (Ent iSandals1 (Just "sandals") "pair of leather sandals" "pairs of leather sandals" "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the soles of your feet." zeroBits) (Obj 1 1) (Arm Feet 1)
    putArm iSandals2 (Ent iSandals2 (Just "sandals") "pair of leather sandals" "pairs of leather sandals" "These humble leather sandals offer little in the way of fashion; they will, however, adequately protect the soles of your feet." zeroBits) (Obj 1 1) (Arm Feet 1)
    putArm iBoots (Ent iBoots (Just "boots") "pair of leather boots" "pairs of leather boots" "These rugged, sturdy boots make excellent footwear for traveling across a variety of terrain." zeroBits) (Obj 1 1) (Arm Feet 1)

    putMob iRockCavy (Ent iRockCavy (Just "rock") "rock cavy" "rock cavies" "It looks like a slightly oversized guinea pig. You imagine that the rock cavy would prefer dry, rocky areas (with low, scrubby vegetation), close to stony mountains and hills." zeroBits) [] mempty M.empty (Mob Male 10 10 10 10 10 10 10 NoHand)


sortAllInvs :: MudStack ()
sortAllInvs = do
    logNotice "sortAllInvs" "sorting all inventories."
    modifyWS $ \ws ->
        ws & invTbl .~ IM.map (sortInv ws) (ws^.invTbl)
