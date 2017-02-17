{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, TemplateHaskell, ViewPatterns #-}

module Mud.Data.State.MudData where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.TopLvlDefs.Misc

import Control.Applicative (empty)
import Control.Arrow (first)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM.TMQueue (TMQueue)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Lens (both, makeLenses)
import Control.Lens.Operators ((%~), (&))
import Control.Monad.Reader (ReaderT)
import Data.Aeson ((.:), (.=), FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..), Value(..), genericParseJSON, genericToJSON, object)
import Data.Aeson.Types (Options, Parser, defaultOptions, fieldLabelModifier)
import Data.Int (Int16)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network (HostName)
import qualified Data.IntMap.Strict as IM (IntMap)
import qualified Data.Map.Strict as M (Map, empty)
import qualified Data.Vector.Unboxed as V (Vector)
import System.Clock (TimeSpec)
import System.Random (Random, random, randomR)
import System.Random.MWC (GenIO)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


type MudStack = ReaderT MudData IO


data MudData = MudData { _errorLog      :: Maybe LogService
                       , _noticeLog     :: Maybe LogService
                       , _gen           :: GenIO
                       , _locks         :: Locks
                       , _startTime     :: TimeSpec
                       , _mudStateIORef :: IORef MudState }


data MudState = MudState { _activeEffectsTbl       :: ActiveEffectsTbl
                         , _armTbl                 :: ArmTbl
                         , _chanTbl                :: ChanTbl
                         , _clothTbl               :: ClothTbl
                         , _coinsTbl               :: CoinsTbl
                         , _conTbl                 :: ConTbl
                         , _corpseDecompAsyncTbl   :: CorpseDecompAsyncTbl
                         , _corpseTbl              :: CorpseTbl
                         , _distinctFoodTbl        :: DistinctFoodTbl
                         , _distinctLiqTbl         :: DistinctLiqTbl
                         , _effectFunTbl           :: EffectFunTbl
                         , _entTbl                 :: EntTbl
                         , _eqTbl                  :: EqTbl
                         , _feelingFunTbl          :: FeelingFunTbl
                         , _foodTbl                :: FoodTbl
                         , _funTbl                 :: FunTbl
                         , _holySymbolTbl          :: HolySymbolTbl
                         , _hookFunTbl             :: HookFunTbl
                         , _hostTbl                :: HostTbl
                         , _instaEffectFunTbl      :: InstaEffectFunTbl
                         , _invTbl                 :: InvTbl
                         , _mobTbl                 :: MobTbl
                         , _msgQueueTbl            :: MsgQueueTbl
                         , _npcTbl                 :: NpcTbl
                         , _objTbl                 :: ObjTbl
                         , _pausedCorpseDecompsTbl :: PausedCorpseDecompsTbl
                         , _pausedEffectsTbl       :: PausedEffectsTbl
                         , _pcSingTbl              :: PCSingTbl
                         , _pcTbl                  :: PCTbl
                         , _pickPtsTbl             :: PickPtsTbl
                         , _plaLogTbl              :: PlaLogTbl
                         , _plaTbl                 :: PlaTbl
                         , _rmActionFunTbl         :: RmActionFunTbl
                         , _rmTbl                  :: RmTbl
                         , _rmTeleNameTbl          :: RmTeleNameTbl
                         , _rndmNamesMstrTbl       :: RndmNamesMstrTbl
                         , _talkAsyncTbl           :: TalkAsyncTbl
                         , _teleLinkMstrTbl        :: TeleLinkMstrTbl
                         , _threadTbl              :: ThreadTbl
                         , _typeTbl                :: TypeTbl
                         , _vesselTbl              :: VesselTbl
                         , _wpnTbl                 :: WpnTbl
                         , _writableTbl            :: WritableTbl }


type ActiveEffectsTbl       = IM.IntMap [ActiveEffect]
type ArmTbl                 = IM.IntMap Arm
type ChanTbl                = IM.IntMap Chan
type ClothTbl               = IM.IntMap Cloth
type CoinsTbl               = IM.IntMap Coins
type ConTbl                 = IM.IntMap Con
type CorpseDecompAsyncTbl   = IM.IntMap CorpseDecompAsync
type CorpseTbl              = IM.IntMap Corpse
type DistinctFoodTbl        = IM.IntMap DistinctFood
type DistinctLiqTbl         = IM.IntMap DistinctLiq
type EffectFunTbl           = M.Map FunName EffectFun
type EntTbl                 = IM.IntMap Ent
type EqTbl                  = IM.IntMap EqMap
type FeelingFunTbl          = M.Map FeelingTag FeelingFun
type FoodTbl                = IM.IntMap Food
type FunTbl                 = M.Map FunName Fun
type HolySymbolTbl          = IM.IntMap HolySymbol
type HookFunTbl             = M.Map HookName HookFun
type HostTbl                = M.Map Sing HostMap
type InstaEffectFunTbl      = M.Map FunName InstaEffectFun
type InvTbl                 = IM.IntMap Inv
type MobTbl                 = IM.IntMap Mob
type MsgQueueTbl            = IM.IntMap MsgQueue
type NpcTbl                 = IM.IntMap Npc
type ObjTbl                 = IM.IntMap Obj
type PausedCorpseDecompsTbl = IM.IntMap SecondsPair
type PausedEffectsTbl       = IM.IntMap [PausedEffect]
type PCSingTbl              = M.Map Sing Id
type PCTbl                  = IM.IntMap PC
type PickPtsTbl             = IM.IntMap Int
type PlaLogTbl              = IM.IntMap LogService
type PlaTbl                 = IM.IntMap Pla
type RmActionFunTbl         = M.Map FunName RmActionFun
type RmTbl                  = IM.IntMap Rm
type RmTeleNameTbl          = IM.IntMap Text
type RndmNamesMstrTbl       = IM.IntMap RndmNamesTbl
type TalkAsyncTbl           = M.Map ThreadId TalkAsync
type TeleLinkMstrTbl        = IM.IntMap TeleLinkTbl
type ThreadTbl              = M.Map ThreadId ThreadType
type TypeTbl                = IM.IntMap Type
type VesselTbl              = IM.IntMap Vessel
type WpnTbl                 = IM.IntMap Wpn
type WritableTbl            = IM.IntMap Writable


-- ==================================================


data Action = Action { actionFun          :: ActionFun
                     , actionShouldPrompt :: Bool }


type ActionFun = ActionParams -> MudStack ()


-- ==================================================


data ActiveEffect = ActiveEffect { _effect        :: Effect
                                 , _effectService :: EffectService }


{-
Effects that have a duration.
These effects have a timer that may be paused (and later restarted) or stopped (cancelled).
There may be a feeling associated with the effect. The feeling will be started and stopped with the effect.
-}
data Effect = Effect { _effectSub     :: EffectSub
                     , _effectVal     :: Maybe EffectVal
                     , _effectDur     :: Seconds
                     , _effectFeeling :: Maybe EffectFeeling } deriving (Eq, Generic, Show)


data EffectSub = ArmEffectAC
               | EntEffectFlags
               | MobEffectAttrib Attrib
               | MobEffectAC
               | RmEffectFlags
               | EffectOther FunName -- "EffectOther" has an "effect function" that is run every second.
               deriving (Eq, Generic, Show)


data Attrib = St | Dx | Ht | Ma | Ps deriving (Bounded, Enum, Eq, Generic, Show)


data EffectVal = DefiniteVal Int
               | RangeVal    Range deriving (Eq, Generic, Show)


type Range = (Int, Int)


{-
A feeling is distinguished by its tag.
TODO
-}
data EffectFeeling = EffectFeeling { efTag :: FeelingTag
                                   , efDur :: Seconds } deriving (Eq, Generic, Show)


type EffectService = (EffectAsync, EffectQueue)


type EffectAsync = Async ()


type EffectQueue = TQueue EffectCmd


data EffectCmd = PauseEffect  (TMVar Seconds)
               | QueryRemTime (TMVar Seconds)
               | StopEffect


type EffectFun = Id -> Seconds -> MudStack ()


-- ==================================================


-- Has an object.
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


data Chan = Chan { _chanId          :: Int
                 , _chanName        :: ChanName
                 , _chanConnTbl     :: ChanConnTbl
                 , _chanWiretappers :: [Sing] } deriving (Eq, Generic, Show)


type ChanName = Text


type ChanConnTbl = M.Map Sing IsTuned


type IsTuned = Bool


-- ==================================================


-- Has an object.
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
                , gol + gol' )
      Coins res


-- ==================================================


-- Has an object and an inventory and coins.
data Con = Con { _conIsCloth  :: Bool
               , _conCapacity :: Vol
               , _conFlags    :: Flags } deriving (Eq, Generic, Show)


type Vol = Int -- 100 "Vol" = 1 cubic in


data ConFlags = ConFlagsTODO deriving Enum


type Flags = Int16


type ConName = Text


-- ==================================================


-- Has a container.
data Corpse = PCCorpse  { _pcCorpseSing  :: Sing
                        , _pcCorpseDesc  :: Text
                        , _pcCorpseSex   :: Sex
                        , _pcCorpseRace  :: Race }
            | NpcCorpse { _npcCorpseDesc :: Text } deriving (Eq, Generic, Show)


-- ==================================================


type CorpseDecompAsync = Async ()


-- ==================================================


data EdibleEffects = EdibleEffects { _digestEffects  :: Maybe DigestEffects
                                   , _consumpEffects :: Maybe ConsumpEffects }


type DigestEffects = EffectList


newtype EffectList = EffectList { unEffectList :: [Either InstaEffect Effect] }


data ConsumpEffects = ConsumpEffects { _consumpAmt        :: Mouthfuls
                                     , _consumpInterval   :: Seconds
                                     , _consumpEffectList :: EffectList }


type Mouthfuls = Int


-- ==================================================


-- Has effects.
data Ent = Ent { _entId    :: Id
               , _entName  :: Maybe Text
               , _sing     :: Sing
               , _plur     :: Plur
               , _entDesc  :: Text
               , _entSmell :: Maybe Text
               , _entFlags :: Flags } deriving (Eq, Generic, Show)


type Id = Int


type Sing = Text


type Plur = Text


data EntFlags = EntFlagsTODO deriving Enum


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


-- Has an object.
data Food = Food { _foodId           :: DistinctFoodId
                 , _foodEatDesc      :: Text
                 , _foodRemMouthfuls :: Mouthfuls } deriving (Eq, Generic, Show)


newtype DistinctFoodId = DistinctFoodId Id deriving (Eq, Generic, Ord, Show)


data DistinctFood = DistinctFood { _foodMouthfuls     :: Mouthfuls
                                 , _foodEdibleEffects :: EdibleEffects }


-- ==================================================


type Fun = MudStack ()


type Funs = [Fun]


type FunName = Text


-- ==================================================


-- Has an object.
newtype HolySymbol = HolySymbol { unHolySymbol :: GodName } deriving (Generic)


data GodName = Aule
             | Caila
             | Celoriel
             | Dellio
             | Drogo
             | Iminye
             | Itulvatar
             | Murgorhd
             | Rha'yk
             | Rumialys deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)


-- ==================================================


type HostMap = M.Map HostName HostRecord


data HostRecord = HostRecord { _noOfLogouts   :: Int
                             , _secsConnected :: Integer
                             , _lastLogout    :: UTCTime } deriving (Eq, Generic, Show)


-- ==================================================


{-
Effects that are instantaneous.
There may be a feeling associated with the effect. The feeling will be started with the effect.
-}
data InstaEffect = InstaEffect { _instaEffectSub     :: InstaEffectSub
                               , _instaEffectVal     :: Maybe EffectVal
                               , _instaEffectFeeling :: Maybe EffectFeeling } deriving (Eq, Generic, Show)


data InstaEffectSub = EntInstaEffectFlags
                    | MobInstaEffectPts PtsType
                    | RmInstaEffectFlags
                    | InstaEffectOther FunName deriving (Eq, Generic, Show)


data PtsType = CurHp | CurMp | CurPp | CurFp deriving (Eq, Generic, Show)


type InstaEffectFun = Id -> MudStack ()


-- ==================================================


type Inv = [Id]


-- ==================================================


data Liq = Liq { _liqId        :: DistinctLiqId
               , _liqNoun      :: Noun
               , _liqSmellDesc :: Text
               , _liqTasteDesc :: Text
               , _liqDrinkDesc :: Text } deriving (Eq, Generic, Show)


newtype DistinctLiqId = DistinctLiqId Id  deriving (Eq, Generic, Ord, Show)


data DistinctLiq = DistinctLiq { _liqEdibleEffects :: EdibleEffects }


data Noun = DoArticle    Text
          | Don'tArticle Text deriving (Eq, Generic, Show)


-- ==================================================


data Locks = Locks { _dbLock        :: Lock
                   , _loggingExLock :: Lock
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
               , _lvl                    :: Lvl
               , _hand                   :: Hand
               , _knownLangs             :: [Lang]
               , _rmId                   :: Id
               , _lastRmId               :: Id
               , _mobRmDesc              :: MobRmDesc
               , _tempDesc               :: TempDesc
               , _mobSize                :: Maybe MobSize -- Used to calculate NPC stomach size.
               , _corpseWeight           :: Weight
               , _corpseVol              :: Vol
               , _corpseCapacity         :: Vol
               , _corpseDecompSecs       :: Seconds
               , _party                  :: Party
               , _stomach                :: [StomachCont]
               , _digesterAsync          :: Maybe StomachAsync
               , _feelingMap             :: FeelingMap
               , _actMap                 :: ActMap
               , _nowEating              :: Maybe NowEating
               , _nowDrinking            :: Maybe NowDrinking
               , _regenQueue             :: Maybe RegenQueue
               , _interp                 :: Maybe Interp }


data Sex = Male
         | Female
         | NoSex deriving (Eq, Generic, Ord, Show)


data StomachCont = StomachCont { _distinctId             :: Either DistinctLiqId DistinctFoodId
                               , _consumpTime            :: UTCTime
                               , _hasCausedConsumpEffect :: Bool } deriving (Eq, Generic, Show)


type StomachAsync = Async ()


type Exp = Int


type Lvl = Int


type LvlExp = (Lvl, Exp)


data Hand = RHand
          | LHand
          | NoHand deriving (Eq, Generic, Show)


data Lang = CommonLang
          | DwarfLang
          | ElfLang
          | FelinoidLang
          | HobbitLang
          | HumanLang
          | LagomorphLang
          | NymphLang
          | VulpenoidLang deriving (Bounded, Enum, Eq, Generic, Ord, Show)


type MobRmDesc = Maybe Text


type TempDesc = Maybe Text


data MobSize = SmlMinus -- A rodent.
             | SmlPlus
             | MedMinus -- A biped (humanoid).
             | MedPlus
             | LrgMinus
             | LrgPlus deriving (Eq, Generic, Show)


data Party = Party { _following :: Maybe Id
                   , _followers :: Inv
                   , _myGroup   :: Inv
                   , _memberOf  :: Maybe Id } deriving (Generic)


type ActMap = M.Map ActType ActAsync


data ActType = Attacking
             | Drinking
             | Eating
             | Sacrificing deriving (Bounded, Enum, Eq, Ord)


type ActAsync = Async ()


type FeelingMap = M.Map FeelingTag Feeling


type FeelingTag = Text


data Feeling = Feeling { feelingVal        :: FeelingVal
                       , feelingDur        :: Seconds
                       , feelingTimerQueue :: TimerQueue
                       , feelingAsync      :: FeelingAsync }


data FeelingVal = NoVal | IntVal Int


type FeelingFun = FeelingVal -> Text


type TimerQueue = TMQueue TimerMsg


data TimerMsg = ResetTimer


type FeelingAsync = Async ()


type NowEating = Sing


type NowDrinking = (Liq, Sing)


type RegenQueue = TQueue RegenCmd


data RegenCmd = StopRegen


instance FromJSON Mob where parseJSON = jsonToMob
instance ToJSON   Mob where toJSON    = mobToJSON


mobToJSON :: Mob -> Value
mobToJSON Mob { .. } = object [ "sex"              .= _sex
                              , "st"               .= _st
                              , "dx"               .= _dx
                              , "ht"               .= _ht
                              , "ma"               .= _ma
                              , "ps"               .= _ps
                              , "curHp"            .= _curHp
                              , "maxHp"            .= _maxHp
                              , "curMp"            .= _curMp
                              , "maxMp"            .= _maxMp
                              , "curPp"            .= _curPp
                              , "maxPp"            .= _maxPp
                              , "curFp"            .= _curFp
                              , "maxFp"            .= _maxFp
                              , "exp"              .= _exp
                              , "lvl"              .= _lvl
                              , "hand"             .= _hand
                              , "knownLangs"       .= _knownLangs
                              , "rmId"             .= _rmId
                              , "lastRmId"         .= _lastRmId
                              , "mobRmDesc"        .= _mobRmDesc
                              , "tempDesc"         .= _tempDesc
                              , "mobSize"          .= _mobSize
                              , "corpseWeight"     .= _corpseWeight
                              , "corpseVol"        .= _corpseVol
                              , "corpseCapacity"   .= _corpseCapacity
                              , "corpseDecompSecs" .= _corpseDecompSecs
                              , "party"            .= _party
                              , "stomach"          .= _stomach ]


jsonToMob :: Value -> Parser Mob
jsonToMob (Object o) = Mob <$> o .: "sex"
                           <*> o .: "st"
                           <*> o .: "dx"
                           <*> o .: "ht"
                           <*> o .: "ma"
                           <*> o .: "ps"
                           <*> o .: "curHp"
                           <*> o .: "maxHp"
                           <*> o .: "curMp"
                           <*> o .: "maxMp"
                           <*> o .: "curPp"
                           <*> o .: "maxPp"
                           <*> o .: "curFp"
                           <*> o .: "maxFp"
                           <*> o .: "exp"
                           <*> o .: "lvl"
                           <*> o .: "hand"
                           <*> o .: "knownLangs"
                           <*> o .: "rmId"
                           <*> o .: "lastRmId"
                           <*> o .: "mobRmDesc"
                           <*> o .: "tempDesc"
                           <*> o .: "mobSize"
                           <*> o .: "corpseWeight"
                           <*> o .: "corpseVol"
                           <*> o .: "corpseCapacity"
                           <*> o .: "corpseDecompSecs"
                           <*> o .: "party"
                           <*> o .: "stomach"
                           <*> pure Nothing
                           <*> pure M.empty
                           <*> pure M.empty
                           <*> pure Nothing
                           <*> pure Nothing
                           <*> pure Nothing
                           <*> pure Nothing
jsonToMob _          = empty


dfltParty :: Party
dfltParty = Party Nothing [] [] Nothing


-- ==================================================


-- Has a mob.
data Npc = Npc { _npcMsgQueue    :: NpcMsgQueue
               , _npcServerAsync :: NpcServerAsync
               , _npcPossessor   :: Maybe Id }


type NpcServerAsync = Async ()


-- ==================================================


-- Has an entity.
data Obj = Obj { _objWeight      :: Weight
               , _objVol         :: Vol
               , _objTaste       :: Maybe Text
               , _objFlags       :: Flags
               , _objBiodegAsync :: Maybe BiodegAsync }


type Weight = Int -- 100 "Weight" = 1 lb


data ObjFlags = IsBiodegradable
              | IsHumming deriving Enum


type BiodegAsync = Async ()


instance FromJSON Obj where parseJSON = jsonToObj
instance ToJSON   Obj where toJSON    = objToJSON


objToJSON :: Obj -> Value
objToJSON Obj { .. } = object [ "objWeight" .= _objWeight
                              , "objVol"    .= _objVol
                              , "objTaste"  .= _objTaste
                              , "objFlags"  .= _objFlags ]


jsonToObj :: Value -> Parser Obj
jsonToObj (Object o) = Obj <$> o .: "objWeight"
                           <*> o .: "objVol"
                           <*> o .: "objTaste"
                           <*> o .: "objFlags"
                           <*> pure Nothing
jsonToObj _          = empty


-- ==================================================


type SecondsPair = (RemainingSeconds, TotalSeconds)


type RemainingSeconds = Seconds


type TotalSeconds = Seconds


-- ==================================================


newtype PausedEffect = PausedEffect Effect deriving (Eq, Generic, Show)


-- ==================================================


-- Has a mob.
data PC = PC { _race          :: Race
             , _introduced    :: [Sing]
             , _linked        :: [Sing]
             , _skillPts      :: SkillPts
             , _sacrificesTbl :: SacrificesTbl } deriving (Eq, Generic, Show)


data Race = Dwarf
          | Elf
          | Felinoid
          | Hobbit
          | Human
          | Lagomorph
          | Nymph
          | Vulpenoid deriving (Bounded, Enum, Eq, Generic, Ord, Show)


instance Random Race where
  randomR pair = first toEnum . randomR (pair & both %~ fromEnum)
  random       = randomR (minBound, maxBound)


type SkillPts = Int


type SacrificesTbl = M.Map GodName Int


-- ==================================================


-- Has a PC and random names and telepathic links.
data Pla = Pla { _currHostName   :: HostName
               , _connectTime    :: Maybe UTCTime
               , _loginTime      :: Maybe UTCTime
               , _disconnectTime :: Maybe UTCTime
               , _plaFlags       :: Flags
               , _columns        :: Int
               , _pageLines      :: Int
               , _peepers        :: Inv
               , _peeping        :: Inv
               , _possessing     :: Maybe Id
               , _retainedMsgs   :: [Text]
               , _logoutRmId     :: Maybe Id
               , _bonusTime      :: Maybe UTCTime
               , _spiritAsync    :: Maybe SpiritAsync } deriving Eq


data PlaFlags = HasRazzled
              | IsAdmin
              | IsGmcp
              | IsIncognito
              | IsNotFirstAdminMsg
              | IsNotFirstMobSay
              | IsNotFirstSpiritCmdNotFound
              | IsShowingFp
              | IsShowingHp
              | IsShowingMp
              | IsShowingPp
              | IsSpirit
              | IsTunedAdmin
              | IsTunedQuestion deriving Enum


type SpiritAsync = Async ()


type Interp = CmdName -> ActionParams -> MudStack ()


type CmdName = Text


instance FromJSON Pla where parseJSON = jsonToPla
instance ToJSON   Pla where toJSON    = plaToJSON


plaToJSON :: Pla -> Value
plaToJSON Pla { .. } = object [ "currHostName"   .= _currHostName
                              , "connectTime"    .= _connectTime
                              , "loginTime"      .= _loginTime
                              , "disconnectTime" .= _disconnectTime
                              , "plaFlags"       .= _plaFlags
                              , "columns"        .= _columns
                              , "pageLines"      .= _pageLines
                              , "retainedMsgs"   .= _retainedMsgs
                              , "logoutRmId"     .= _logoutRmId
                              , "bonusTime"      .= _bonusTime ]


jsonToPla :: Value -> Parser Pla
jsonToPla (Object o) = Pla <$> o .: "currHostName"
                           <*> o .: "connectTime"
                           <*> o .: "loginTime"
                           <*> o .: "disconnectTime"
                           <*> o .: "plaFlags"
                           <*> o .: "columns"
                           <*> o .: "pageLines"
                           <*> pure []
                           <*> pure []
                           <*> pure Nothing
                           <*> o .: "retainedMsgs"
                           <*> o .: "logoutRmId"
                           <*> o .: "bonusTime"
                           <*> pure Nothing
jsonToPla _          = empty


-- ==================================================


-- Has effects and an inventory and coins.
data Rm = Rm { _rmName      :: Text
             , _rmDesc      :: Text
             , _rmListen    :: Maybe Text
             , _rmSmell     :: Maybe Text
             , _rmFlags     :: Flags
             , _rmLinks     :: [RmLink]
             , _rmCoords    :: RmCoords
             , _rmEnv       :: RmEnv
             , _rmLabel     :: RmLabel
             , _rmHookMap   :: HookMap
             , _rmActions   :: [RmAction]
             , _rmFunNames  :: [FunName]
             , _rmFunAsyncs :: [RmFunAsync] } deriving (Eq, Generic)


data RmFlags = RmFlagsTODO deriving Enum


data RmLink = StdLink    { _slDir        :: LinkDir
                         , _slDestId     :: Id
                         , _slCost       :: MoveCost }
            | NonStdLink { _nslName      :: LinkName
                         , _nslDestId    :: Id
                         , _nslCost      :: MoveCost
                         , _nslOriginMsg :: Text
                         , _nslDestMsg   :: Text } deriving (Eq, Generic)


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


type MoveCost = Int


type LinkName = Text


type RmCoords = (XCoord, YCoord, ZCoord)


type XCoord = Int


type YCoord = Int


type ZCoord = Int


data RmEnv = InsideEnv
           | OutsideEnv
           | ShopEnv
           | SpecialEnv
           | NoEnv deriving (Eq, Generic, Show)


type RmLabel = Maybe Text


type HookMap = M.Map CmdName [Hook]


data Hook = Hook { hookName     :: HookName
                 , hookTriggers :: [Text] } deriving (Eq, Generic, Show)


type HookName = Text


type HookFun = Id -> Hook -> V.Vector Int -> HookFunRes -> HookFunRes


type HookFunRes = (Args, GenericIntermediateRes, Funs)


type Args = [Text]


type GenericIntermediateRes = (MudState,  [Text], [Broadcast], [Text])


type GenericRes             = (MudState, ([Text], [Broadcast], [Text]))


type GenericResWithHooks    = (MudState, ([Text], [Broadcast], [Text], Funs))


type Broadcast = (Text, Inv)


data RmAction = RmAction { rmActionCmdName :: CmdName
                         , rmActionFunName :: FunName } deriving (Eq, Generic, Show)


type RmFunAsync = Async ()


instance FromJSON Rm where parseJSON = jsonToRm
instance ToJSON   Rm where toJSON    = rmToJSON


rmToJSON :: Rm -> Value
rmToJSON Rm { .. } = object [ "rmName"     .= _rmName
                            , "rmDesc"     .= _rmDesc
                            , "rmListen"   .= _rmListen
                            , "rmSmell"    .= _rmSmell
                            , "rmFlags"    .= _rmFlags
                            , "rmLinks"    .= _rmLinks
                            , "rmCoords"   .= _rmCoords
                            , "rmEnv"      .= _rmEnv
                            , "rmLabel"    .= _rmLabel
                            , "rmHookMap"  .= _rmHookMap
                            , "rmActions"  .= _rmActions
                            , "rmFunNames" .= _rmFunNames ]


jsonToRm :: Value -> Parser Rm
jsonToRm (Object o) = Rm <$> o .: "rmName"
                         <*> o .: "rmDesc"
                         <*> o .: "rmListen"
                         <*> o .: "rmSmell"
                         <*> o .: "rmFlags"
                         <*> o .: "rmLinks"
                         <*> o .: "rmCoords"
                         <*> o .: "rmEnv"
                         <*> o .: "rmLabel"
                         <*> o .: "rmHookMap"
                         <*> o .: "rmActions"
                         <*> o .: "rmFunNames"
                         <*> pure []
jsonToRm _          = empty


-- ==================================================


type RmActionFun = ActionFun


-- ==================================================


type RndmNamesTbl = M.Map Sing Sing


-- ==================================================


type TalkAsync = Async ()


-- ==================================================


type TeleLinkTbl = M.Map Sing IsTuned


-- ==================================================


data ThreadType = Biodegrader       Id
                | CorpseDecomposer  Id
                | DbTblPurger
                | Digester          Id
                | DrinkingThread    Id
                | EatingThread      Id
                | EffectListener    Id
                | EffectThread      Id
                | EffectTimer       Id
                | Error
                | FeelingTimer      Id
                | InacTimer         Id
                | Listen
                | Notice
                | NpcServer         Id
                | PlaLog            Id
                | Receive           Id
                | RegenChild        Id
                | RegenParent       Id
                | RmFun             Id
                | SacrificingThread Id
                | Server            Id
                | SpiritTimer       Id
                | Talk              Id
                | ThreadTblPurger
                | TrashDumpPurger
                | WorldPersister deriving (Eq, Ord, Show)


-- ==================================================


data Type = ArmType
          | ClothType
          | ConType
          | CorpseType
          | FoodType
          | HolySymbolType
          | NpcType
          | ObjType
          | PCType
          | RmType
          | VesselType
          | WpnType
          | WritableType deriving (Eq, Generic, Show)


-- ==================================================


-- Has an object.
data Vessel = Vessel { _vesselMaxMouthfuls :: Mouthfuls -- obj vol / mouthful vol
                     , _vesselCont         :: Maybe VesselCont } deriving (Eq, Generic, Show)


type VesselCont = (Liq, Mouthfuls)


-- ==================================================


-- Has an object.
data Wpn = Wpn { _wpnSub    :: WpnSub
               , _wpnMinDmg :: Int
               , _wpnMaxDmg :: Int } deriving (Eq, Generic, Show)


data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Generic, Show)


-- ==================================================


-- Has an object.
data Writable = Writable { _writMessage :: Maybe (Text, Lang)
                         , _writRecip   :: Maybe Sing {- for magically scribed msgs -} } deriving (Eq, Generic, Show)


-- ==================================================


instance FromJSON Arm            where parseJSON = genericParseJSON dropUnderscore
instance FromJSON ArmSub
instance FromJSON Attrib
instance FromJSON Chan           where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Cloth
instance FromJSON Coins
instance FromJSON Con            where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Corpse         where parseJSON = genericParseJSON dropUnderscore
instance FromJSON DistinctFoodId where parseJSON = genericParseJSON dropUnderscore
instance FromJSON DistinctLiqId  where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Effect         where parseJSON = genericParseJSON dropUnderscore
instance FromJSON EffectFeeling
instance FromJSON EffectSub
instance FromJSON EffectVal
instance FromJSON Ent            where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Food           where parseJSON = genericParseJSON dropUnderscore
instance FromJSON GodName
instance FromJSON Hand
instance FromJSON HolySymbol
instance FromJSON Hook
instance FromJSON HostRecord     where parseJSON = genericParseJSON dropUnderscore
instance FromJSON InstaEffect    where parseJSON = genericParseJSON dropUnderscore
instance FromJSON InstaEffectSub
instance FromJSON Lang
instance FromJSON LinkDir
instance FromJSON Liq            where parseJSON = genericParseJSON dropUnderscore
instance FromJSON MobSize        where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Noun
instance FromJSON Party          where parseJSON = genericParseJSON dropUnderscore
instance FromJSON PausedEffect   where parseJSON = genericParseJSON dropUnderscore
instance FromJSON PC             where parseJSON = genericParseJSON dropUnderscore
instance FromJSON PtsType
instance FromJSON Race
instance FromJSON RmAction
instance FromJSON RmEnv          where parseJSON = genericParseJSON dropUnderscore
instance FromJSON RmLink         where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Sex
instance FromJSON Slot
instance FromJSON StomachCont    where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Type
instance FromJSON Vessel         where parseJSON = genericParseJSON dropUnderscore
instance FromJSON Wpn            where parseJSON = genericParseJSON dropUnderscore
instance FromJSON WpnSub
instance FromJSON Writable       where parseJSON = genericParseJSON dropUnderscore
instance ToJSON Arm              where toJSON    = genericToJSON    dropUnderscore
instance ToJSON ArmSub
instance ToJSON Attrib
instance ToJSON Chan             where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Cloth
instance ToJSON Coins
instance ToJSON Con              where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Corpse           where toJSON    = genericToJSON    dropUnderscore
instance ToJSON DistinctFoodId   where toJSON    = genericToJSON    dropUnderscore
instance ToJSON DistinctLiqId    where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Effect           where toJSON    = genericToJSON    dropUnderscore
instance ToJSON EffectFeeling
instance ToJSON EffectSub
instance ToJSON EffectVal
instance ToJSON Ent              where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Food             where toJSON    = genericToJSON    dropUnderscore
instance ToJSON GodName
instance ToJSON Hand
instance ToJSON HolySymbol
instance ToJSON Hook
instance ToJSON HostRecord       where toJSON    = genericToJSON    dropUnderscore
instance ToJSON InstaEffect      where toJSON    = genericToJSON    dropUnderscore
instance ToJSON InstaEffectSub
instance ToJSON Lang
instance ToJSON LinkDir
instance ToJSON Liq              where toJSON    = genericToJSON    dropUnderscore
instance ToJSON MobSize          where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Noun
instance ToJSON Party            where toJSON    = genericToJSON    dropUnderscore
instance ToJSON PausedEffect     where toJSON    = genericToJSON    dropUnderscore
instance ToJSON PC               where toJSON    = genericToJSON    dropUnderscore
instance ToJSON PtsType
instance ToJSON Race
instance ToJSON RmAction
instance ToJSON RmEnv            where toJSON    = genericToJSON    dropUnderscore
instance ToJSON RmLink           where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Sex
instance ToJSON Slot
instance ToJSON StomachCont      where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Type
instance ToJSON Vessel           where toJSON    = genericToJSON    dropUnderscore
instance ToJSON Wpn              where toJSON    = genericToJSON    dropUnderscore
instance ToJSON WpnSub
instance ToJSON Writable         where toJSON    = genericToJSON    dropUnderscore

instance FromJSONKey GodName
instance ToJSONKey   GodName


dropUnderscore :: Options
dropUnderscore = defaultOptions { fieldLabelModifier = tail }


-- ==================================================


makeLenses ''ActiveEffect
makeLenses ''Arm
makeLenses ''Chan
makeLenses ''Con
makeLenses ''ConsumpEffects
makeLenses ''Corpse
makeLenses ''DistinctFood
makeLenses ''DistinctLiq
makeLenses ''EdibleEffects
makeLenses ''Effect
makeLenses ''Ent
makeLenses ''Food
makeLenses ''HostRecord
makeLenses ''InstaEffect
makeLenses ''Liq
makeLenses ''Locks
makeLenses ''Mob
makeLenses ''MudData
makeLenses ''MudState
makeLenses ''Npc
makeLenses ''Obj
makeLenses ''Party
makeLenses ''PausedEffect
makeLenses ''PC
makeLenses ''Pla
makeLenses ''Rm
makeLenses ''RmLink
makeLenses ''StomachCont
makeLenses ''Vessel
makeLenses ''Wpn
makeLenses ''Writable
