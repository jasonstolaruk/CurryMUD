{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Mud.StateDataTypes where

import Mud.StateInIORefT

import Control.Lens (lens, Lens', makeLenses)
import GHC.IO.Handle (Handle)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Map.Lazy as M (Map)
import qualified Data.Text as T
import System.Log.Handler.Simple (GenericHandler)

-----

class HasNameDesc a where
  name, desc :: Lens' a T.Text

instance HasNameDesc Ent where
  name = lens _entName (\e v -> e { _entName = v })
  desc = lens _entDesc (\e v -> e { _entDesc = v })

instance HasNameDesc Rm where
  name = lens _rmName (\e v -> e { _rmName = v })
  desc = lens _rmDesc (\e v -> e { _rmDesc = v })

-----

class HasFlags a where
  flags :: Lens' a Int

instance HasFlags Ent where
  flags = lens _entFlags (\e v -> e { _entFlags = v })

instance HasFlags Rm where
  flags = lens _rmFlags (\e v -> e { _rmFlags = v })

-----

type Id = Int

type Sing = T.Text
type Plur = T.Text

data Ent = Ent { _entId    :: !Id
               , _entName  :: !T.Text
               , _sing     :: !Sing
               , _plur     :: !Plur
               , _entDesc  :: !T.Text
               , _entFlags :: !Int } deriving (Eq, Show)

-----

-- Has an entity.

data Obj = Obj { _weight :: !Int
               , _vol    :: !Int } deriving (Eq, Show)

-----

-- Has an object (and an entity).

data Cloth = EarC
           | NoseC
           | NeckC
           | WristC
           | FingerC
           | UpBodyC
           | LowBodyC
           | FullBodyC
           | BackC
           | FeetC deriving (Eq, Show)

-----

type Inv = [Id]

-----

data Coins = Coins { _cp :: !Int
                   , _sp :: !Int
                   , _gp :: !Int } deriving (Eq, Show)

type CoinNameAmt = (T.Text, Int) -- TODO: Move this?

-----

-- Has an object (and an entity) and an inventory and coins.

type Cap = Int

newtype Con = Con Cap deriving (Eq, Show)

-----

-- Has an object (and an entity).

data Wpn = Wpn { _wpnSub :: !WpnSub
               , _minDmg :: !Int
               , _maxDmg :: !Int } deriving (Eq, Show)

data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Show)

-----

-- Has an object (and an entity).

type AC = Int

data Arm = Arm { _armSub :: !ArmSub
               , _ac     :: !AC } deriving (Eq, Show)

data ArmSub = HeadA
            | UpBodyA
            | LowBodyA
            | FullBodyA deriving (Eq, Show)

-----

type EqMap = M.Map Slot Id

data Slot = HeadS
          | REar1S | REar2S
          | LEar1S | LEar2S
          | Nose1S | Nose2S
          | Neck1S | Neck2S | Neck3S
          | RWrist1S | RWrist2S | RWrist3S
          | LWrist1S | LWrist2S | LWrist3S
          | RIndexFS | RMidFS | RRingFS | RPinkyFS
          | LIndexFS | LMidFS | LRingFS | LPinkyFS
          | RHandS
          | LHandS
          | BothHandsS
          | UpBodyCS
          | LowBodyCS
          | FullBodyCS
          | UpBodyAS
          | LowBodyAS
          | FullBodyAS
          | BackS
          | FeetS deriving (Eq, Ord, Enum)

-----

-- Has an entity and an inventory and coins and equipment.

data Mob = Mob { _gender            :: !Gender
               , _st, _dx, _iq, _ht :: !Int
               , _hp, _fp           :: !Int
               , _xp                :: !Int
               , _hand              :: !Hand } deriving (Eq, Show)

data Gender = Male
            | Female
            | NoGender deriving (Eq, Show)

data Hand = RHand
          | LHand
          | NoHand deriving (Eq, Show)

-----

-- Has a mob (and an entity and an inventory and coins and equipment).

data PC = PC { _rmId :: !Id
             , _race :: !Race } deriving (Eq, Show)

data Race = Human
          | Elf
          | Dwarf
          | Halfling
          | Nymph
          | Felinoid
          | Vulpenoid
          | Lagomorph deriving (Eq, Show)

----

-- Has an inventory and coins.

data Rm = Rm { _rmName  :: !T.Text
             , _rmDesc  :: !T.Text
             , _rmFlags :: !Int
             , _rmLinks :: ![RmLink] } deriving (Eq, Show)

type LinkName = T.Text

data RmLink = RmLink { _linkName :: !LinkName
                     , _destId   :: !Id } deriving (Eq, Show)

-----

data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | MobType
          | PCType
          | RmType deriving (Eq, Show)

-----

-- TODO: Move into a "Pla" data type.
data Hist = Hist { _cmds     :: ![T.Text]
                 , _overflow :: !T.Text } deriving (Eq, Show)

-----

data LogHandles = LogHandles { _errorHandle  :: Maybe (GenericHandler Handle)
                             , _noticeHandle :: Maybe (GenericHandler Handle) }

-----

type EntTbl   = IM.IntMap Ent
type ObjTbl   = IM.IntMap Obj
type ClothTbl = IM.IntMap Cloth
type InvTbl   = IM.IntMap Inv
type CoinsTbl = IM.IntMap Coins
type ConTbl   = IM.IntMap Con
type WpnTbl   = IM.IntMap Wpn
type ArmTbl   = IM.IntMap Arm
type EqTable  = IM.IntMap EqMap
type MobTbl   = IM.IntMap Mob
type RmTbl    = IM.IntMap Rm
type TypeTbl  = IM.IntMap Type

data MudState = MudState { _entTbl     :: EntTbl
                         , _objTbl     :: ObjTbl
                         , _clothTbl   :: ClothTbl
                         , _invTbl     :: InvTbl
                         , _coinsTbl   :: CoinsTbl
                         , _conTbl     :: ConTbl
                         , _wpnTbl     :: WpnTbl
                         , _armTbl     :: ArmTbl
                         , _eqTbl      :: EqTable
                         , _mobTbl     :: MobTbl
                         , _pc         :: PC
                         , _rmTbl      :: RmTbl
                         , _typeTbl    :: TypeTbl
                         , _hist       :: Hist
                         , _logHandles :: LogHandles }


-----

type MudStack = StateInIORefT MudState IO

-----

makeLenses ''Ent
makeLenses ''Obj
makeLenses ''Coins
makeLenses ''Wpn
makeLenses ''Arm
makeLenses ''Mob
makeLenses ''PC
makeLenses ''Rm
makeLenses ''RmLink
makeLenses ''Hist
makeLenses ''LogHandles
makeLenses ''MudState
