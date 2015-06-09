{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Mud.Data.State.MudData where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue

import Control.Applicative ((<$>), (<*>), empty, pure)
import Control.Arrow ((***), first)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Lens (makeLenses)
import Control.Monad.Reader (ReaderT)
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), Value(..), object)
import Data.Aeson.Types (Parser)
import Data.IORef (IORef)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network (HostName)
import System.Clock (TimeSpec)
import System.Random (Random, random, randomR)
import System.Random.MWC (GenIO)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Map.Lazy as M (Map)
import qualified Data.Set as S (Set)
import qualified Data.Text as T


type MudStack = ReaderT MudData IO


-- ==================================================


data MudData = MudData { _errorLog       :: Maybe LogService
                       , _gen            :: GenIO
                       , _mudStateIORef  :: IORef MudState
                       , _noticeLog      :: Maybe LogService
                       , _persisterTMVar :: TMVar PersisterDone
                       , _startTime      :: TimeSpec }


data MudState = MudState { _armTbl        :: ArmTbl
                         , _clothTbl      :: ClothTbl
                         , _coinsTbl      :: CoinsTbl
                         , _conTbl        :: ConTbl
                         , _entTbl        :: EntTbl
                         , _eqTbl         :: EqTbl
                         , _hostNameTbl   :: HostNameTbl
                         , _invTbl        :: InvTbl
                         , _mobTbl        :: MobTbl
                         , _msgQueueTbl   :: MsgQueueTbl
                         , _objTbl        :: ObjTbl
                         , _pcTbl         :: PCTbl
                         , _plaLogTbl     :: PlaLogTbl
                         , _plaTbl        :: PlaTbl
                         , _rmTbl         :: RmTbl
                         , _rmTeleNameTbl :: RmTeleNameTbl
                         , _talkAsyncTbl  :: TalkAsyncTbl
                         , _threadTbl     :: ThreadTbl
                         , _typeTbl       :: TypeTbl
                         , _wpnTbl        :: WpnTbl }


type ArmTbl        = IM.IntMap Arm
type ClothTbl      = IM.IntMap Cloth
type CoinsTbl      = IM.IntMap Coins
type ConTbl        = IM.IntMap Con
type EntTbl        = IM.IntMap Ent
type EqTbl         = IM.IntMap EqMap
type HostNameTbl   = M.Map Sing (S.Set HostRecord)
type InvTbl        = IM.IntMap Inv
type MobTbl        = IM.IntMap Mob
type MsgQueueTbl   = IM.IntMap MsgQueue
type ObjTbl        = IM.IntMap Obj
type PCTbl         = IM.IntMap PC
type PlaLogTbl     = IM.IntMap LogService
type PlaTbl        = IM.IntMap Pla
type RmTbl         = IM.IntMap Rm
type RmTeleNameTbl = IM.IntMap T.Text
type TalkAsyncTbl  = M.Map ThreadId (Async ())
type ThreadTbl     = M.Map ThreadId ThreadType
type TypeTbl       = IM.IntMap Type
type WpnTbl        = IM.IntMap Wpn


-- ==================================================


-- Has an object (and an entity).
data Arm = Arm { _armSub   :: ArmSub
               , _armClass :: AC } deriving (Eq, Generic, Show)


data ArmSub = Head
            | Torso
            | Arms
            | Hands
            | LowerBody
            | Feet
            | Shield deriving (Eq, Generic, Show)


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
           | Cloak deriving (Enum, Eq, Generic, Show)


-- ==================================================


newtype Coins = Coins (Cop, Sil, Gol) deriving (Eq, Generic, Show)


type Cop = Int


type Sil = Int


type Gol = Int


instance Monoid Coins where
  mempty = Coins (0, 0, 0)
  Coins (cop, sil, gol) `mappend` Coins (cop', sil', gol') = do
      let res = ( cop + cop'
                , sil + sil'
                , gol + gol')
      Coins res


-- ============================================================


-- Has an object (and an entity) and an inventory and coins.
data Con = Con { _isCloth :: Bool
               , _cap     :: Cap } deriving (Eq, Generic, Show)


type Cap = Int


type ConName = T.Text


-- ==================================================


data Ent = Ent { _entId    :: Id
               , _entName  :: Maybe T.Text
               , _sing     :: Sing
               , _plur     :: Plur
               , _entDesc  :: T.Text
               , _entFlags :: Int } deriving (Eq, Generic, Show)


type Id = Int


type Sing = T.Text


type Plur = T.Text


data EntFlags = IsInvis deriving Enum


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
          deriving (Enum, Eq, Generic, Ord)


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
               , _hand              :: Hand } deriving (Eq, Generic, Show)


data Sex = Male
         | Female
         | NoSex deriving (Eq, Generic, Show)


data Hand = RHand
          | LHand
          | NoHand deriving (Eq, Generic, Show)


-- ==================================================


data HostRecord = HostRecord { _hostName     :: HostName
                             , _noOfLogins   :: Int
                             , _hrsConnected :: Int
                             , _lastLogout   :: UTCTime } deriving (Eq, Generic, Show)


-- ==================================================


data PersisterDone = PersisterDone


-- ==================================================


-- Has an entity.
data Obj = Obj { _weight :: Int
               , _vol    :: Int } deriving (Eq, Generic, Show)


-- ==================================================


-- Has a mob (and an entity and an inventory and coins and equipment).
data PC = PC { _rmId       :: Id
             , _race       :: Race
             , _introduced :: [Sing]
             , _linked     :: [Sing] } deriving (Eq, Generic, Show)


data Race = Dwarf
          | Elf
          | Felinoid
          | Halfling
          | Human
          | Lagomorph
          | Nymph
          | Vulpenoid deriving (Bounded, Enum, Eq, Generic, Show)


instance Random Race where
  randomR (fromEnum *** fromEnum -> intPair) = first toEnum . randomR intPair
  random                                     = randomR (minBound, maxBound)


-- ==================================================


data Pla = Pla { _currHostName :: HostName
               , _connectTime  :: Maybe UTCTime
               , _plaFlags     :: Int
               , _columns      :: Int
               , _pageLines    :: Int
               , _interp       :: Maybe Interp
               , _peepers      :: Inv
               , _peeping      :: Inv
               , _retainedMsgs :: [T.Text]
               , _lastRmId     :: Maybe Id }


data PlaFlags = IsAdmin
              | IsIncognito
              | IsNotFirstAdminTell
              | IsNotFirstLook
              | IsNotFirstMobSay
              | IsSeeingInvis deriving Enum


type Interp  = CmdName -> ActionParams -> MudStack ()


type CmdName = T.Text


instance FromJSON Pla where parseJSON = jsonToPla
instance ToJSON   Pla where toJSON    = plaToJSON


plaToJSON :: Pla -> Value
plaToJSON Pla { .. } = object [ "_currHostName" .= _currHostName
                              , "_connectTime"  .= _connectTime
                              , "_plaFlags"     .= _plaFlags
                              , "_columns"      .= _columns
                              , "_pageLines"    .= _pageLines
                              , "_retainedMsgs" .= _retainedMsgs
                              , "_lastRmId"     .= _lastRmId ]


jsonToPla :: Value -> Parser Pla
jsonToPla (Object o) = Pla <$> o .: "_currHostName"
                           <*> o .: "_connectTime"
                           <*> o .: "_plaFlags"
                           <*> o .: "_columns"
                           <*> o .: "_pageLines"
                           <*> pure Nothing
                           <*> pure []
                           <*> pure []
                           <*> o .: "_retainedMsgs"
                           <*> o .: "_lastRmId"
jsonToPla _          = empty


-- ======================================================================


-- Has an inventory and coins.
data Rm = Rm { _rmName   :: T.Text
             , _rmDesc   :: T.Text
             , _rmFlags  :: Int
             , _rmLinks  :: [RmLink] } deriving (Eq, Generic)


data RmFlags = RmFlagsTODO deriving Enum


data RmLink = StdLink    { _linkDir      :: LinkDir
                         , _stdDestId    :: Id }
            | NonStdLink { _linkName     :: LinkName
                         , _nonStdDestId :: Id
                         , _originMsg    :: T.Text
                         , _destMsg      :: T.Text } deriving (Eq, Generic)


data LinkDir = North
             | Northeast
             | East
             | Southeast
             | South
             | Southwest
             | West
             | Northwest
             | Up
             | Down deriving (Eq, Generic, Show)


type LinkName = T.Text


-- ==================================================


data ThreadType = Error
                | InacTimer Id
                | Listen
                | Notice
                | PlaLog    Id
                | Receive   Id
                | Server    Id
                | Talk      Id
                | ThreadTblPurger
                | WorldPersister deriving (Eq, Ord, Show)


-- ==================================================


data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | MobType
          | PCType
          | RmType deriving (Eq, Generic, Show)


-- ==================================================


-- Has an object (and an entity).
data Wpn = Wpn { _wpnSub :: WpnSub
               , _minDmg :: Int
               , _maxDmg :: Int } deriving (Eq, Generic, Show)


data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Generic, Show)


-- ==================================================


instance FromJSON Arm
instance FromJSON ArmSub
instance FromJSON Cloth
instance FromJSON Coins
instance FromJSON Con
instance FromJSON Ent
instance FromJSON Hand
instance FromJSON HostRecord
instance FromJSON LinkDir
instance FromJSON Mob
instance FromJSON Obj
instance FromJSON PC
instance FromJSON Race
instance FromJSON Rm
instance FromJSON RmLink
instance FromJSON Sex
instance FromJSON Slot
instance FromJSON Type
instance FromJSON Wpn
instance FromJSON WpnSub
instance ToJSON   Arm
instance ToJSON   ArmSub
instance ToJSON   Cloth
instance ToJSON   Coins
instance ToJSON   Con
instance ToJSON   Ent
instance ToJSON   Hand
instance ToJSON   HostRecord
instance ToJSON   LinkDir
instance ToJSON   Mob
instance ToJSON   Obj
instance ToJSON   PC
instance ToJSON   Race
instance ToJSON   Rm
instance ToJSON   RmLink
instance ToJSON   Sex
instance ToJSON   Slot
instance ToJSON   Type
instance ToJSON   Wpn
instance ToJSON   WpnSub


-- ==================================================


makeLenses ''Arm
makeLenses ''Con
makeLenses ''Ent
makeLenses ''Mob
makeLenses ''MudData
makeLenses ''MudState
makeLenses ''Obj
makeLenses ''PC
makeLenses ''Pla
makeLenses ''Rm
makeLenses ''RmLink
makeLenses ''Wpn
