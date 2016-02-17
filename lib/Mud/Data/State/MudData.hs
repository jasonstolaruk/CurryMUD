{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Mud.Data.State.MudData where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.TopLvlDefs.Misc

import Control.Applicative (empty)
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
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network (HostName)
import qualified Data.IntMap.Lazy as IM (IntMap)
import qualified Data.Map.Lazy as M (Map)
import qualified Data.Vector.Unboxed as V (Vector)
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


data MudState = MudState { _activeEffectsTbl :: ActiveEffectsTbl
                         , _armTbl           :: ArmTbl
                         , _chanTbl          :: ChanTbl
                         , _clothTbl         :: ClothTbl
                         , _coinsTbl         :: CoinsTbl
                         , _conTbl           :: ConTbl
                         , _effectFunTbl     :: EffectFunTbl
                         , _entTbl           :: EntTbl
                         , _eqTbl            :: EqTbl
                         , _funTbl           :: FunTbl
                         , _hookFunTbl       :: HookFunTbl
                         , _hostTbl          :: HostTbl
                         , _invTbl           :: InvTbl
                         , _mobTbl           :: MobTbl
                         , _msgQueueTbl      :: MsgQueueTbl
                         , _npcTbl           :: NpcTbl
                         , _objTbl           :: ObjTbl
                         , _pausedEffectsTbl :: PausedEffectsTbl
                         , _pcTbl            :: PCTbl
                         , _plaLogTbl        :: PlaLogTbl
                         , _plaTbl           :: PlaTbl
                         , _rmActionFunTbl   :: RmActionFunTbl
                         , _rmTbl            :: RmTbl
                         , _rmTeleNameTbl    :: RmTeleNameTbl
                         , _rndmNamesMstrTbl :: RndmNamesMstrTbl
                         , _talkAsyncTbl     :: TalkAsyncTbl
                         , _teleLinkMstrTbl  :: TeleLinkMstrTbl
                         , _threadTbl        :: ThreadTbl
                         , _typeTbl          :: TypeTbl
                         , _vesselTbl        :: VesselTbl
                         , _wpnTbl           :: WpnTbl
                         , _writableTbl      :: WritableTbl
                         , _opList           :: [Operation] }


type ActiveEffectsTbl = IM.IntMap [ActiveEffect]
type ArmTbl           = IM.IntMap Arm
type ChanTbl          = IM.IntMap Chan
type ClothTbl         = IM.IntMap Cloth
type CoinsTbl         = IM.IntMap Coins
type ConTbl           = IM.IntMap Con
type EffectFunTbl     = M.Map FunName EffectFun
type EntTbl           = IM.IntMap Ent
type EqTbl            = IM.IntMap EqMap
type FunTbl           = M.Map FunName Fun
type HookFunTbl       = M.Map HookName HookFun
type HostTbl          = M.Map Sing HostMap
type InvTbl           = IM.IntMap Inv
type MobTbl           = IM.IntMap Mob
type MsgQueueTbl      = IM.IntMap MsgQueue
type NpcTbl           = IM.IntMap Npc
type ObjTbl           = IM.IntMap Obj
type Operation        = MudStack ()
type PausedEffectsTbl = IM.IntMap [PausedEffect]
type PCTbl            = IM.IntMap PC
type PlaLogTbl        = IM.IntMap LogService
type PlaTbl           = IM.IntMap Pla
type RmActionFunTbl   = M.Map FunName RmActionFun
type RmTbl            = IM.IntMap Rm
type RmTeleNameTbl    = IM.IntMap Text
type RndmNamesMstrTbl = IM.IntMap RndmNamesTbl
type TalkAsyncTbl     = M.Map ThreadId TalkAsync
type TeleLinkMstrTbl  = IM.IntMap TeleLinkTbl
type ThreadTbl        = M.Map ThreadId ThreadType
type TypeTbl          = IM.IntMap Type
type VesselTbl        = IM.IntMap Vessel
type WpnTbl           = IM.IntMap Wpn
type WritableTbl      = IM.IntMap Writable


-- ==================================================


data Action = Action { actionFun    :: ActionFun
                     , shouldPrompt :: Bool }


type ActionFun = ActionParams -> MudStack ()


-- ==================================================


data ActiveEffect = ActiveEffect { _effect        :: Effect
                                 , _effectService :: EffectService }


data Effect = EffectArm   ArmEffect
            | EffectEnt   EntEffect
            | EffectMob   MobEffect
            | EffectRm    RmEffect
            | EffectOther FunName deriving (Eq, Generic, Show)


data ArmEffect = ArmEffectAC AC deriving (Eq, Generic, Show)


data EntEffect = EntEffectFlags Int deriving (Eq, Generic, Show)


data MobEffect = MobEffectAttrib Attrib Int
               | MobEffectAC AC deriving (Eq, Generic, Show)


data Attrib = St | Dx | Ht | Ma | Ps deriving (Eq, Generic, Show)


data RmEffect = RmEffectFlags Int deriving (Eq, Generic, Show)


type EffectService = (EffectAsync, EffectQueue)


type EffectAsync = Async ()


type EffectQueue = TQueue EffectCmd


data EffectCmd = PauseEffect (TMVar Seconds)
               | StopEffect


type EffectFun = Id -> Seconds -> MudStack ()


-- ==================================================


-- Has an object (and an entity and effects).
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


-- Has an object (and an entity and effects).
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


-- Has an object (and an entity and effects) and an inventory and coins.
data Con = Con { _isCloth  :: Bool
               , _capacity :: Vol } deriving (Eq, Generic, Show)


type Vol = Int


type ConName = Text


-- ==================================================


-- Has effects.
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


type FunName = Text


type Fun = MudStack ()


-- ==================================================


type HostMap = M.Map HostName HostRecord


data HostRecord = HostRecord { _noOfLogouts   :: Int
                             , _secsConnected :: Integer
                             , _lastLogout    :: UTCTime } deriving (Eq, Generic, Show)


-- ==================================================


data InstaEffect = InstaEffectEnt EntInstaEffect
                 | InstaEffectMob MobInstaEffect
                 | InstaEffectRm  RmInstaEffect deriving (Eq, Generic, Show)


data EntInstaEffect = EntInstaEffectFlags Int deriving (Eq, Generic, Show)


data MobInstaEffect = MobInstaEffectPts PtsType Int deriving (Eq, Generic, Show)


data PtsType = CurHp | CurMp | CurPp | CurFp deriving (Eq, Generic, Show)


data RmInstaEffect = RmInstaEffectFlags Int deriving (Eq, Generic, Show)


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


-- Has an entity (and effects) and an inventory and coins and equipment.
data Mob = Mob { _sex                    :: Sex
               , _st, _dx, _ht, _ma, _ps :: Int
               , _curHp, _maxHp          :: Int
               , _curMp, _maxMp          :: Int
               , _curPp, _maxPp          :: Int
               , _curFp, _maxFp          :: Int
               , _exp                    :: Exp
               , _hand                   :: Hand
               , _knownLangs             :: [Lang]
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


data Lang = CommonLang
          | DwarfLang
          | ElfLang
          | FelinoidLang
          | HalflingLang
          | HumanLang
          | LagomorphLang
          | NymphLang
          | VulpenoidLang deriving (Bounded, Enum, Eq, Generic, Show)


type RegenAsync = Async ()


instance FromJSON Mob where parseJSON = jsonToMob
instance ToJSON   Mob where toJSON    = mobToJSON


mobToJSON :: Mob -> Value
mobToJSON Mob { .. } = object [ "_sex"        .= _sex
                              , "_st"         .= _st
                              , "_dx"         .= _dx
                              , "_ht"         .= _ht
                              , "_ma"         .= _ma
                              , "_ps"         .= _ps
                              , "_curHp"      .= _curHp
                              , "_maxHp"      .= _maxHp
                              , "_curMp"      .= _curMp
                              , "_maxMp"      .= _maxMp
                              , "_curPp"      .= _curPp
                              , "_maxPp"      .= _maxPp
                              , "_curFp"      .= _curFp
                              , "_maxFp"      .= _maxFp
                              , "_exp"        .= _exp
                              , "_hand"       .= _hand
                              , "_knownLangs" .= _knownLangs
                              , "_rmId"       .= _rmId ]


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
                           <*> o .: "_knownLangs"
                           <*> o .: "_rmId"
                           <*> pure Nothing
                           <*> pure Nothing
jsonToMob _          = empty


-- ==================================================


-- Has a mob (and an entity and effects and an inventory and coins and equipment).
data Npc = Npc { _npcMsgQueue    :: NpcMsgQueue
               , _npcServerAsync :: NpcServerAsync
               , _possessor      :: Maybe Id }


type NpcServerAsync = Async ()


-- ==================================================


-- Has an entity (and effects).
data Obj = Obj { _weight           :: Weight
               , _vol              :: Vol
               , _objFlags         :: Int
               , _biodegraderAsync :: Maybe BiodegraderAsync }


type Weight = Int


data ObjFlags = IsBiodegradable deriving Enum


type BiodegraderAsync = Async ()


instance FromJSON Obj where parseJSON = jsonToObj
instance ToJSON   Obj where toJSON    = objToJSON


objToJSON :: Obj -> Value
objToJSON Obj { .. } = object [ "_weight"   .= _weight
                              , "_vol"      .= _vol
                              , "_objFlags" .= _objFlags ]


jsonToObj :: Value -> Parser Obj
jsonToObj (Object o) = Obj <$> o .: "_weight"
                           <*> o .: "_vol"
                           <*> o .: "_objFlags"
                           <*> pure Nothing
jsonToObj _          = empty


-- ==================================================


data PausedEffect = PausedEffect { _pausedEffect  :: Effect
                                 , _timeRemaining :: Seconds } deriving (Eq, Generic, Show)


-- ==================================================


-- Has a mob (and an entity and effects and an inventory and coins and equipment).
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


-- Has a PC (and a mob and an entity and effects and an inventory and coins and equipment) and a random names table and a telepathic link table.
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


-- Has effects and an inventory and coins.
data Rm = Rm { _rmName      :: Text
             , _rmDesc      :: Text
             , _rmFlags     :: Int
             , _rmLinks     :: [RmLink]
             , _hookMap     :: HookMap
             , _rmActions   :: [RmAction]
             , _rmFunNames  :: [FunName]
             , _rmFunAsyncs :: [RmFunAsync] } deriving (Eq, Generic)


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


type HookMap = M.Map CmdName [Hook]


data Hook = Hook { hookName :: HookName
                 , triggers :: [Text] } deriving (Eq, Generic, Show)


type HookName = Text


type HookFun = Id -> Hook -> V.Vector Int -> (Args, GenericIntermediateRes) -> (Args, GenericIntermediateRes)


type Args = [Text]


type GenericIntermediateRes = (MudState,  [Text], [Broadcast], [Text])


type GenericRes             = (MudState, ([Text], [Broadcast], [Text]))


type Broadcast = (Text, Inv)


data RmAction = RmAction { rmActionCmdName :: CmdName
                         , rmActionFunName :: FunName } deriving (Eq, Generic, Show)


type RmFunAsync = Async ()


instance FromJSON Rm where parseJSON = jsonToRm
instance ToJSON   Rm where toJSON    = rmToJSON


rmToJSON :: Rm -> Value
rmToJSON Rm { .. } = object [ "_rmName"     .= _rmName
                            , "_rmDesc"     .= _rmDesc
                            , "_rmFlags"    .= _rmFlags
                            , "_rmLinks"    .= _rmLinks
                            , "_hookMap"    .= _hookMap
                            , "_rmActions"  .= _rmActions
                            , "_rmFunNames" .= _rmFunNames ]


jsonToRm :: Value -> Parser Rm
jsonToRm (Object o) = Rm <$> o .: "_rmName"
                         <*> o .: "_rmDesc"
                         <*> o .: "_rmFlags"
                         <*> o .: "_rmLinks"
                         <*> o .: "_hookMap"
                         <*> o .: "_rmActions"
                         <*> o .: "_rmFunNames"
                         <*> pure []
jsonToRm _          = empty


-- ==================================================


type RmActionFun = ActionFun


-- ==================================================


type RndmNamesTbl = M.Map Sing Sing


-- ======================================================================


type TalkAsync = Async ()


-- ==================================================


type TeleLinkTbl = M.Map Sing IsTuned


-- ==================================================


data ThreadType = Biodegrader    Id
                | DbTblPurger
                | EffectListener Id
                | EffectThread   Id
                | EffectTimer    Id
                | Error
                | InacTimer      Id
                | Listen
                | Notice
                | NpcServer      Id
                | OpListMonitor
                | PlaLog         Id
                | Receive        Id
                | RegenChild     Id
                | RegenParent    Id
                | RmFun          Id
                | Server         Id
                | Talk           Id
                | ThreadTblPurger
                | TrashDumpPurger
                | WorldPersister deriving (Eq, Ord, Show)


-- ==================================================


data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | NpcType
          | PCType
          | RmType
          | VesselType
          | WritableType deriving (Eq, Generic, Show)


-- ==================================================


-- Has an object (and an entity and effects).
data Vessel = Vessel { _maxQuaffs :: Quaffs -- obj vol / quaff vol
                     , _contents  :: Maybe Contents } deriving (Eq, Generic, Show)


type Quaffs = Int


type Contents = (Liquid, Quaffs)


data Liquid = Water
            | Potion Effect Seconds
            | InstaPotion InstaEffect deriving (Eq, Generic, Show)


-- ==================================================


-- Has an object (and an entity and effects).
data Wpn = Wpn { _wpnSub :: WpnSub
               , _minDmg :: Int
               , _maxDmg :: Int } deriving (Eq, Generic, Show)


data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Generic, Show)


-- ==================================================


-- Has an object (and an entity and effects).
data Writable = Writable { _message :: Maybe (Text, Lang)
                         , _recip   :: Maybe Sing {- for magically scribed msgs -} } deriving (Eq, Generic, Show)


-- ==================================================


instance FromJSON Arm
instance FromJSON ArmEffect
instance FromJSON ArmSub
instance FromJSON Attrib
instance FromJSON Chan
instance FromJSON Cloth
instance FromJSON Coins
instance FromJSON Con
instance FromJSON Effect
instance FromJSON Ent
instance FromJSON EntEffect
instance FromJSON EntInstaEffect
instance FromJSON Hand
instance FromJSON Hook
instance FromJSON HostRecord
instance FromJSON InstaEffect
instance FromJSON Lang
instance FromJSON LinkDir
instance FromJSON Liquid
instance FromJSON MobEffect
instance FromJSON MobInstaEffect
instance FromJSON PausedEffect
instance FromJSON PC
instance FromJSON PtsType
instance FromJSON Race
instance FromJSON RmAction
instance FromJSON RmEffect
instance FromJSON RmInstaEffect
instance FromJSON RmLink
instance FromJSON Sex
instance FromJSON Slot
instance FromJSON Type
instance FromJSON Vessel
instance FromJSON Wpn
instance FromJSON WpnSub
instance FromJSON Writable
instance ToJSON   Arm
instance ToJSON   ArmEffect
instance ToJSON   ArmSub
instance ToJSON   Attrib
instance ToJSON   Chan
instance ToJSON   Cloth
instance ToJSON   Coins
instance ToJSON   Con
instance ToJSON   Effect
instance ToJSON   Ent
instance ToJSON   EntEffect
instance ToJSON   EntInstaEffect
instance ToJSON   Hand
instance ToJSON   Hook
instance ToJSON   HostRecord
instance ToJSON   InstaEffect
instance ToJSON   Lang
instance ToJSON   LinkDir
instance ToJSON   Liquid
instance ToJSON   MobEffect
instance ToJSON   MobInstaEffect
instance ToJSON   PausedEffect
instance ToJSON   PC
instance ToJSON   PtsType
instance ToJSON   Race
instance ToJSON   RmAction
instance ToJSON   RmEffect
instance ToJSON   RmInstaEffect
instance ToJSON   RmLink
instance ToJSON   Sex
instance ToJSON   Slot
instance ToJSON   Type
instance ToJSON   Vessel
instance ToJSON   Wpn
instance ToJSON   WpnSub
instance ToJSON   Writable


-- ==================================================


makeLenses ''ActiveEffect
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
makeLenses ''PausedEffect
makeLenses ''PC
makeLenses ''Pla
makeLenses ''Rm
makeLenses ''RmLink
makeLenses ''Vessel
makeLenses ''Wpn
makeLenses ''Writable
