{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Mud.StateDataTypes where

import Mud.StateInIORefT

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Lens (lens, Lens', makeLenses)
import Data.Monoid (mappend, mempty, Monoid)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Map.Lazy as M (Map)
import qualified Data.Text as T


-- ==================================================
-- Typeclasses and instances:


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


-- ==================================================
-- The MUD state wrapper:


-- TODO: Put elements of the state in STM.

data MudState = MudState { _worldState  :: WorldState
                         , _logServices :: LogServices }

data WorldState = WorldState { _entTbl      :: EntTbl
                             , _objTbl      :: ObjTbl
                             , _clothTbl    :: ClothTbl
                             , _invTbl      :: InvTbl
                             , _coinsTbl    :: CoinsTbl
                             , _conTbl      :: ConTbl
                             , _wpnTbl      :: WpnTbl
                             , _armTbl      :: ArmTbl
                             , _eqTbl       :: EqTable
                             , _mobTbl      :: MobTbl
                             , _pc          :: PC
                             , _rmTbl       :: RmTbl
                             , _typeTbl     :: TypeTbl }

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


-- ==================================================
-- The monad transformer stack:


type MudStack = StateInIORefT MudState IO


-- ==================================================
-- Entity:


type Id = Int

type Sing = T.Text
type Plur = T.Text

data Ent = Ent { _entId    :: !Id
               , _entName  :: !T.Text
               , _sing     :: !Sing
               , _plur     :: !Plur
               , _entDesc  :: !T.Text
               , _entFlags :: !Int } deriving (Eq, Show)


-- ==================================================
-- Object:
-- Has an entity.


data Obj = Obj { _weight :: !Int
               , _vol    :: !Int } deriving (Eq, Show)


-- ==================================================
-- Clothing:
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


-- ==================================================
-- Inventory:


type Inv = [Id]


-- ==================================================
-- Coins:


type Cop = Int
type Sil = Int
type Gol = Int

newtype Coins = Coins (Cop, Sil, Gol) deriving (Eq, Show)

instance Monoid Coins where
  mempty = Coins (0, 0, 0)
  Coins (cop, sil, gol) `mappend` Coins (cop', sil', gol') = Coins (cop + cop', sil + sil', gol + gol')


-- ==================================================
-- Container:
-- Has an object (and an entity) and an inventory and coins.


type Cap = Int

newtype Con = Con Cap deriving (Eq, Show)

type ConName = T.Text


-- ==================================================
-- Weapon:
-- Has an object (and an entity).


data Wpn = Wpn { _wpnSub :: !WpnSub
               , _minDmg :: !Int
               , _maxDmg :: !Int } deriving (Eq, Show)

data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Show)


-- ==================================================
-- Armor:
-- Has an object (and an entity).


type AC = Int

data Arm = Arm { _armSub :: !ArmSub
               , _ac     :: !AC } deriving (Eq, Show)

data ArmSub = HeadA
            | UpBodyA
            | LowBodyA
            | FullBodyA deriving (Eq, Show)


-- ==================================================
-- Equipment:


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


-- ==================================================
-- Mobile:
-- Has an entity and an inventory and coins and equipment.


data Mob = Mob { _gender            :: !Gender
               , _st, _dx, _iq, _ht :: !Int
               , _hp, _fp           :: !Int
               , _xp                :: !Int
               , _hand              :: !Hand } deriving (Eq, Show)

data Gender = Male
            | Female
            | NoGender deriving (Eq, Show)

data Hand   = RHand
            | LHand
            | NoHand deriving (Eq, Show)


-- ==================================================
-- Player character:
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


-- ==================================================
-- Room:
-- Has an inventory and coins.


data Rm = Rm { _rmName  :: !T.Text
             , _rmDesc  :: !T.Text
             , _rmFlags :: !Int
             , _rmLinks :: ![RmLink] } deriving (Eq, Show)

type LinkName = T.Text

data RmLink = RmLink { _linkName :: !LinkName
                     , _destId   :: !Id } deriving (Eq, Show)


-- ==================================================
-- Types of world elements:


data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | MobType
          | PCType
          | RmType deriving (Eq, Show)


-- ==================================================
-- Log queues:


data LogCmd      = Stop | Msg String

type LogAsync    = Async ()

type LogQueue    = TBQueue LogCmd

type LogService  = (LogAsync, LogQueue)

data LogServices = LogServices { _noticeLog :: Maybe LogService
                               , _errorLog  :: Maybe LogService }


-- ==================================================
-- Template Haskell for creating lenses:


makeLenses ''MudState

makeLenses ''WorldState
makeLenses ''Ent
makeLenses ''Obj
makeLenses ''Wpn
makeLenses ''Arm
makeLenses ''Mob
makeLenses ''PC
makeLenses ''Rm
makeLenses ''RmLink

makeLenses ''LogServices
