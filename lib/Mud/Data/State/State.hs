{-# LANGUAGE OverloadedStrings, TemplateHaskell, ViewPatterns #-}

module Mud.Data.State.State where -- TODO: Rename?

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue

import Control.Arrow ((***), first)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens (makeLenses)
import Control.Monad.Reader (ReaderT)
import Data.Monoid (Monoid, mappend, mempty)
import Network (HostName)
import System.Clock (TimeSpec)
import System.Random (Random, random, randomR)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Map.Lazy as M (Map)
import qualified Data.Text as T


type MudStack = ReaderT MudData IO


-- ==================================================


data MudData = MudData { _armTblTVar       :: TVar ArmTbl
                       , _clothTblTVar     :: TVar ClothTbl
                       , _coinsTblTVar     :: TVar CoinsTbl
                       , _conTblTVar       :: TVar ConTbl
                       , _entTblTVar       :: TVar EntTbl
                       , _eqTblTVar        :: TVar EqTbl
                       , _errorLog         :: LogService
                       , _invTblTVar       :: TVar InvTbl
                       , _mobTblTVar       :: TVar MobTbl
                       , _msgQueueTblTVar  :: TVar MsgQueueTbl
                       , _noticeLog        :: LogService
                       , _objTblTVar       :: TVar ObjTbl
                       , _pcTblTVar        :: TVar PCTbl
                       , _plaLogTblTVar    :: TVar PlaLogTbl
                       , _plaTblTVar       :: TVar PlaTbl
                       , _rmTblTVar        :: TVar RmTbl
                       , _startTime        :: TimeSpec
                       , _talkAsyncTblTVar :: TVar TalkAsyncTbl
                       , _threadTblTVar    :: TVar ThreadTbl
                       , _typeTblTVar      :: TVar TypeTbl
                       , _wpnTblTVar       :: TVar WpnTbl }


type ArmTbl       = IM.IntMap Arm
type ClothTbl     = IM.IntMap Cloth
type CoinsTbl     = IM.IntMap Coins
type ConTbl       = IM.IntMap Con
type EntTbl       = IM.IntMap Ent
type EqTbl        = IM.IntMap EqMap
type InvTbl       = IM.IntMap Inv
type MobTbl       = IM.IntMap Mob
type MsgQueueTbl  = IM.IntMap MsgQueue
type ObjTbl       = IM.IntMap Obj
type PCTbl        = IM.IntMap PC
type PlaLogTbl    = IM.IntMap LogService
type PlaTbl       = IM.IntMap Pla
type RmTbl        = IM.IntMap Rm
type TalkAsyncTbl = M.Map ThreadId (Async ())
type ThreadTbl    = M.Map ThreadId ThreadType
type TypeTbl      = IM.IntMap Type
type WpnTbl       = IM.IntMap Wpn


-- ==================================================


-- Has an object (and an entity).
data Arm = Arm { _armSub   :: ArmSub
               , _armClass :: AC } deriving (Eq, Show)


data ArmSub = Head -- cap, helmet, etc.
            | Torso
            | Arms -- bracers, etc.
            | Hands -- gloves, mittens
            | LowerBody -- leggings, etc.
            | Feet -- any footwear
            | Shield
            deriving (Eq, Show)


type AC = Int


-- ==================================================


-- Has an object (and an entity).
data Cloth = Earring
           | NoseRing
           | Necklace
           | Bracelet
           | Ring
           | Shirt
           | Smock
           | Coat
           | Trousers
           | Skirt
           | Dress
           | FullBody
           | Backpack
           | Cloak deriving (Enum, Eq, Show)


-- ==================================================


newtype Coins = Coins (Cop, Sil, Gol) deriving (Eq, Show)


type Cop = Int


type Sil = Int


type Gol = Int


instance Monoid Coins where
  mempty = Coins (0, 0, 0)
  Coins (cop, sil, gol) `mappend` Coins (cop', sil', gol') = Coins (cop + cop', sil + sil', gol + gol')


-- ============================================================


-- Has an object (and an entity) and an inventory and coins.
data Con = Con { _isCloth :: Bool
               , _cap     :: Cap } deriving (Eq, Show)


type Cap = Int


type ConName = T.Text


-- ==================================================


data Ent = Ent { _entId    :: Id
               , _entName  :: Maybe T.Text
               , _sing     :: Sing
               , _plur     :: Plur
               , _entDesc  :: T.Text
               , _entFlags :: Int } deriving (Eq, Show)


type Id = Int


type Sing = T.Text


type Plur = T.Text


data EntFlags = EntFlagsTODO deriving Enum


-- ==================================================


type EqMap = M.Map Slot Id


data Slot =
            HeadS                                   -- armor
          | EarringR1S | EarringR2S                 -- clothing
          | EarringL1S | EarringL2S                 -- clothing
          | NoseRing1S | NoseRing2S                 -- clothing
          | Necklace1S | Necklace2S | Necklace3S    -- clothing
          | ShirtS                                  -- clothing
          | DressS                                  -- clothing
          | TorsoS                                  -- armor
          | SmockS                                  -- clothing
          | ArmsS                                   -- armor
          | CoatS                                   -- clothing
          | BraceletR1S | BraceletR2S | BraceletR3S -- clothing
          | BraceletL1S | BraceletL2S | BraceletL3S -- clothing
          | RingRIS | RingRMS | RingRRS | RingRPS   -- clothing
          | RingLIS | RingLMS | RingLRS | RingLPS   -- clothing
          | HandsS                                  -- armor
          | RHandS                                  -- weapon/shield
          | LHandS                                  -- weapon/shield
          | BothHandsS                              -- weapon
          | TrousersS                               -- clothing
          | SkirtS                                  -- clothing
          | LowerBodyS                              -- armor
          | CloakS                                  -- clothing
          | FullBodyS                               -- clothing
          | FeetS                                   -- armor
          | BackpackS                               -- container/clothing
          deriving (Enum, Eq, Ord)


-- ==================================================


type Inv = [Id]


-- ==================================================


type LogService = (LogAsync, LogQueue)


type LogAsync = Async ()


type LogQueue = TQueue LogCmd


data LogCmd = LogMsg T.Text
            | RotateLog
            | StopLog
            | Throw


-- ==================================================


-- Has an entity and an inventory and coins and equipment.
data Mob = Mob { _sex               :: Sex
               , _st, _dx, _iq, _ht :: Int
               , _hp, _fp           :: Int
               , _xp                :: Int
               , _hand              :: Hand } deriving (Eq, Show)


data Sex = Male
         | Female
         | NoSex deriving (Eq, Show)


data Hand = RHand
          | LHand
          | NoHand deriving (Eq, Show)


-- ==================================================


-- Has an entity.
data Obj = Obj { _weight :: Int
               , _vol    :: Int } deriving (Eq, Show)


-- ==================================================


-- Has a mob (and an entity and an inventory and coins and equipment).
data PC = PC { _rmId       :: Id
             , _race       :: Race
             , _introduced :: [Sing]
             , _linked     :: [Sing] } deriving (Eq, Show)


data Race = Dwarf
          | Elf
          | Felinoid
          | Halfling
          | Human
          | Lagomorph
          | Nymph
          | Vulpenoid deriving (Bounded, Enum, Eq, Show)


instance Random Race where
  randomR (fromEnum *** fromEnum -> intPair) = first toEnum . randomR intPair
  random                                     = randomR (minBound, maxBound)


-- ==================================================


data Pla = Pla { _hostName  :: HostName
               , _plaFlags  :: Int
               , _columns   :: Int
               , _pageLines :: Int
               , _interp    :: Maybe Interp
               , _peepers   :: Inv
               , _peeping   :: Inv }


data PlaFlags = IsAdmin
              | IsNotFirstAdminTell
              | IsNotFirstLook
              | IsNotFirstMobSay deriving Enum


type Interp  = (CmdName -> ActionParams -> MudStack ())


type CmdName = T.Text


-- ======================================================================


-- Has an inventory and coins.
data Rm = Rm { _rmName  :: T.Text
             , _rmDesc  :: T.Text
             , _rmFlags :: Int
             , _rmLinks :: [RmLink] } deriving Eq


data RmFlags = RmFlagsTODO deriving Enum


data RmLink = StdLink    { _linkDir      :: LinkDir
                         , _stdDestId    :: Id }
            | NonStdLink { _linkName     :: LinkName
                         , _nonStdDestId :: Id
                         , _originMsg    :: T.Text -> T.Text
                         , _destMsg      :: T.Text -> T.Text }


data LinkDir = North
             | Northeast
             | East
             | Southeast
             | South
             | Southwest
             | West
             | Northwest
             | Up
             | Down deriving (Eq, Show)


type LinkName = T.Text


instance Eq RmLink where
  (StdLink    dir i      ) == (StdLink    dir' i'        ) | dir   == dir'
                                                           , i     == i'     = True
  (NonStdLink ln  i om dm) == (NonStdLink ln'  i' om' dm') | ln    == ln'
                                                           , i     == i'
                                                           , om "" == om' ""
                                                           , dm "" == dm' "" = True
  _                        == _                            = False


-- ==================================================


data ThreadType = Error
                | InacTimer Id
                | Listen
                | Notice
                | PlaLog    Id
                | Receive   Id
                | Server    Id
                | Talk      Id
                | ThreadTblPurger deriving (Eq, Ord, Show)


-- ==================================================


data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | MobType
          | PCType
          | RmType deriving (Eq, Show)


-- ==================================================


-- Has an object (and an entity).
data Wpn = Wpn { _wpnSub :: WpnSub
               , _minDmg :: Int
               , _maxDmg :: Int } deriving (Eq, Show)


data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Show)


-- ==================================================


makeLenses ''Arm
makeLenses ''Con
makeLenses ''Ent
makeLenses ''Mob
makeLenses ''MudData
makeLenses ''Obj
makeLenses ''PC
makeLenses ''Pla
makeLenses ''Rm
makeLenses ''RmLink
makeLenses ''Wpn
