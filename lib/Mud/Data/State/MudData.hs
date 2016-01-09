{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Mud.Data.State.MudData where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue

import Control.Applicative (empty)
import Control.Arrow ((***), first)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Lens (makeLenses)
import Control.Monad.Reader (ReaderT)
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), Value(..), defaultOptions, genericToJSON, object)
import Data.Aeson.Types (Parser)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network (HostName)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Map.Lazy as M (Map)
import System.Clock (TimeSpec)
import System.Random (Random, random, randomR)
import System.Random.MWC (GenIO)


type MudStack = ReaderT MudData IO


-- ==================================================


data MudData = MudData { _errorLog      :: Maybe LogService
                       , _gen           :: GenIO
                       , _locks         :: Locks
                       , _mudStateIORef :: IORef MudState
                       , _noticeLog     :: Maybe LogService
                       , _startTime     :: TimeSpec }


data MudState = MudState { _armTbl           :: ArmTbl
                         , _chanTbl          :: ChanTbl
                         , _clothTbl         :: ClothTbl
                         , _coinsTbl         :: CoinsTbl
                         , _conTbl           :: ConTbl
                         , _entTbl           :: EntTbl
                         , _eqTbl            :: EqTbl
                         , _hostTbl          :: HostTbl
                         , _invTbl           :: InvTbl
                         , _mobTbl           :: MobTbl
                         , _msgQueueTbl      :: MsgQueueTbl
                         , _npcTbl           :: NpcTbl
                         , _objTbl           :: ObjTbl
                         , _pcTbl            :: PCTbl
                         , _plaLogTbl        :: PlaLogTbl
                         , _plaTbl           :: PlaTbl
                         , _rmTbl            :: RmTbl
                         , _rmTeleNameTbl    :: RmTeleNameTbl
                         , _rndmNamesMstrTbl :: RndmNamesMstrTbl
                         , _talkAsyncTbl     :: TalkAsyncTbl
                         , _teleLinkMstrTbl  :: TeleLinkMstrTbl
                         , _threadTbl        :: ThreadTbl
                         , _typeTbl          :: TypeTbl
                         , _wpnTbl           :: WpnTbl }


type ArmTbl           = IM.IntMap Arm
type ChanTbl          = IM.IntMap Chan
type ClothTbl         = IM.IntMap Cloth
type CoinsTbl         = IM.IntMap Coins
type ConTbl           = IM.IntMap Con
type EntTbl           = IM.IntMap Ent
type EqTbl            = IM.IntMap EqMap
type HostTbl          = M.Map Sing HostMap
type InvTbl           = IM.IntMap Inv
type MobTbl           = IM.IntMap Mob
type MsgQueueTbl      = IM.IntMap MsgQueue
type NpcTbl           = IM.IntMap Npc
type ObjTbl           = IM.IntMap Obj
type PCTbl            = IM.IntMap PC
type PlaLogTbl        = IM.IntMap LogService
type PlaTbl           = IM.IntMap Pla
type RmTbl            = IM.IntMap Rm
type RmTeleNameTbl    = IM.IntMap Text
type RndmNamesMstrTbl = IM.IntMap RndmNamesTbl
type TalkAsyncTbl     = M.Map ThreadId TalkAsync
type TeleLinkMstrTbl  = IM.IntMap TeleLinkTbl
type ThreadTbl        = M.Map ThreadId ThreadType
type TypeTbl          = IM.IntMap Type
type WpnTbl           = IM.IntMap Wpn


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


data Chan = Chan { _chanId      :: Int
                 , _chanName    :: ChanName
                 , _chanConnTbl :: ChanConnTbl
                 , _wiretappers :: [Sing] } deriving (Eq, Generic, Show)


type ChanName = Text


type ChanConnTbl = M.Map Sing IsTuned


type IsTuned = Bool


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
data Con = Con { _isCloth  :: Bool
               , _capacity :: Vol } deriving (Eq, Generic, Show)


type Vol = Int


type ConName = Text


-- ==================================================


data Ent = Ent { _entId    :: Id
               , _entName  :: Maybe Text
               , _sing     :: Sing
               , _plur     :: Plur
               , _entDesc  :: Text
               , _entFlags :: Int } deriving (Eq, Generic, Show)


type Id = Int


type Sing = Text


type Plur = Text


data EntFlags = IsInvis deriving Enum


-- ==================================================


type EqMap = M.Map Slot Id


data Slot = HeadS                                   -- armor
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


type HostMap = M.Map HostName HostRecord


data HostRecord = HostRecord { _noOfLogouts   :: Int
                             , _secsConnected :: Integer
                             , _lastLogout    :: UTCTime } deriving (Eq, Generic, Show)


-- ==================================================


type Inv = [Id]


-- ==================================================


data Locks = Locks { _loggingExLock :: Lock
                   , _persistLock   :: Lock }


type Lock = TMVar Done


data Done = Done


-- ==================================================


type LogService = (LogAsync, LogQueue)


type LogAsync = Async ()


type LogQueue = TQueue LogCmd


data LogCmd = LogMsg Text
            | RotateLog
            | StopLog
            | Throw


-- ==================================================


-- Has an entity and an inventory and coins and equipment.
data Mob = Mob { _sex                    :: Sex
               , _st, _dx, _ht, _ma, _ps :: Int
               , _curHp, _maxHp          :: Int
               , _curMp, _maxMp          :: Int
               , _curPp, _maxPp          :: Int
               , _curFp, _maxFp          :: Int
               , _exp                    :: Exp
               , _hand                   :: Hand
               , _rmId                   :: Id
               , _regenAsync             :: Maybe RegenAsync
               , _interp                 :: Maybe Interp }


data Sex = Male
         | Female
         | NoSex deriving (Eq, Generic, Show)


type Exp = Int


data Hand = RHand
          | LHand
          | NoHand deriving (Eq, Generic, Show)


type RegenAsync = Async ()


instance FromJSON Mob where parseJSON = jsonToMob
instance ToJSON   Mob where toJSON    = mobToJSON


mobToJSON :: Mob -> Value
mobToJSON Mob { .. } = object [ "_sex"   .= _sex
                              , "_st"    .= _st
                              , "_dx"    .= _dx
                              , "_ht"    .= _ht
                              , "_ma"    .= _ma
                              , "_ps"    .= _ps
                              , "_curHp" .= _curHp
                              , "_maxHp" .= _maxHp
                              , "_curMp" .= _curMp
                              , "_maxMp" .= _maxMp
                              , "_curPp" .= _curPp
                              , "_maxPp" .= _maxPp
                              , "_curFp" .= _curFp
                              , "_maxFp" .= _maxFp
                              , "_exp"   .= _exp
                              , "_hand"  .= _hand
                              , "_rmId"  .= _rmId ]


jsonToMob :: Value -> Parser Mob
jsonToMob (Object o) = Mob <$> o .: "_sex"
                           <*> o .: "_st"
                           <*> o .: "_dx"
                           <*> o .: "_ht"
                           <*> o .: "_ma"
                           <*> o .: "_ps"
                           <*> o .: "_curHp"
                           <*> o .: "_maxHp"
                           <*> o .: "_curMp"
                           <*> o .: "_maxMp"
                           <*> o .: "_curPp"
                           <*> o .: "_maxPp"
                           <*> o .: "_curFp"
                           <*> o .: "_maxFp"
                           <*> o .: "_exp"
                           <*> o .: "_hand"
                           <*> o .: "_rmId"
                           <*> pure Nothing
                           <*> pure Nothing
jsonToMob _          = empty


-- ==================================================


-- Has a mob (and an entity and an inventory and coins and equipment).
data Npc = Npc { _npcMsgQueue    :: NpcMsgQueue
               , _npcServerAsync :: NpcServerAsync
               , _possessor      :: Maybe Id }


type NpcServerAsync = Async ()


-- ==================================================


-- Has an entity.
data Obj = Obj { _weight :: Weight
               , _vol    :: Vol } deriving (Eq, Generic, Show)


type Weight = Int


-- ==================================================


-- Has a mob (and an entity and an inventory and coins and equipment).
data PC = PC { _race       :: Race
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


-- Has a PC (and a mob and an entity and an inventory and coins and equipment) and a random names table and a telepathic link table.
data Pla = Pla { _currHostName :: HostName
               , _connectTime  :: Maybe UTCTime
               , _plaFlags     :: Int
               , _columns      :: Int
               , _pageLines    :: Int
               , _peepers      :: Inv
               , _peeping      :: Inv
               , _possessing   :: Maybe Id
               , _retainedMsgs :: [Text]
               , _lastRmId     :: Maybe Id }


data PlaFlags = IsAdmin
              | IsIncognito
              | IsNotFirstAdminMsg
              | IsNotFirstLook
              | IsNotFirstMobSay
              | IsTunedAdmin
              | IsTunedQuestion deriving Enum


type Interp  = CmdName -> ActionParams -> MudStack ()


type CmdName = Text


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
                           <*> pure []
                           <*> pure []
                           <*> pure Nothing
                           <*> o .: "_retainedMsgs"
                           <*> o .: "_lastRmId"
jsonToPla _          = empty


-- ======================================================================


type RndmNamesTbl = M.Map Sing Sing


-- ======================================================================


-- Has an inventory and coins.
data Rm = Rm { _rmName  :: Text
             , _rmDesc  :: Text
             , _rmFlags :: Int
             , _rmLinks :: [RmLink] } deriving (Eq, Generic)


data RmFlags = RmFlagsTODO deriving Enum


data RmLink = StdLink    { _linkDir      :: LinkDir
                         , _stdDestId    :: Id }
            | NonStdLink { _linkName     :: LinkName
                         , _nonStdDestId :: Id
                         , _originMsg    :: Text
                         , _destMsg      :: Text } deriving (Eq, Generic)


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


type LinkName = Text


-- ==================================================


type TalkAsync = Async ()


-- ==================================================


type TeleLinkTbl = M.Map Sing IsTuned


-- ==================================================


data ThreadType = DbTblPurger
                | Error
                | InacTimer   Id
                | Listen
                | Notice
                | NpcServer   Id
                | PlaLog      Id
                | Receive     Id
                | RegenChild  Id
                | RegenParent Id
                | Server      Id
                | Talk        Id
                | ThreadTblPurger
                | WorldPersister deriving (Eq, Ord, Show)


-- ==================================================


data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | NpcType
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
instance FromJSON Chan
instance FromJSON Cloth
instance FromJSON Coins
instance FromJSON Con
instance FromJSON Ent
instance FromJSON Hand
instance FromJSON HostRecord
instance FromJSON LinkDir
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
  -- TODO: Temporary fix for aeson issue. https://github.com/bos/aeson/issues/290
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   ArmSub
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Chan
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Cloth
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Coins
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Con
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Ent
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Hand
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   HostRecord
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   LinkDir
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Obj
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   PC
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Race
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Rm
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   RmLink
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Sex
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Slot
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Type
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   Wpn
  where
    toJSON = genericToJSON defaultOptions
instance ToJSON   WpnSub
  where
    toJSON = genericToJSON defaultOptions


-- ==================================================


makeLenses ''Arm
makeLenses ''Chan
makeLenses ''Con
makeLenses ''Ent
makeLenses ''HostRecord
makeLenses ''Locks
makeLenses ''Mob
makeLenses ''MudData
makeLenses ''MudState
makeLenses ''Npc
makeLenses ''Obj
makeLenses ''PC
makeLenses ''Pla
makeLenses ''Rm
makeLenses ''RmLink
makeLenses ''Wpn
