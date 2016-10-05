{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, ParallelListComp, RebindableSyntax, RecordWildCards, ViewPatterns #-}

module Mud.Data.Misc ( Action(..)
                     , ActionFun
                     , Amount
                     , AOrThe(..)
                     , Args
                     , BanRecord(..)
                     , ChanContext(..)
                     , ClassifiedBcast(..)
                     , Cmd(..)
                     , CmdDesc
                     , CmdFullName
                     , CmdPriorityAbbrevTxt
                     , Cols
                     , deserialize
                     , Desig(..)
                     , DrinkBundle(..)
                     , EmoteWord(..)
                     , EmptyNoneSome(..)
                     , EquipInvLookCmd(..)
                     , ExpCmd(..)
                     , ExpCmdFun
                     , ExpCmdName
                     , ExpCmdType(..)
                     , fromRol
                     , GenericRes
                     , getEntFlag
                     , GetEntsCoinsRes(..)
                     , getObjFlag
                     , GetOrDrop(..)
                     , getPlaFlag
                     , getRmFlag
                     , Help(..)
                     , HelpName
                     , IdSingTypeDesig(..)
                     , Index
                     , InInvEqRm(..)
                     , LastArgIsTargetBindings(..)
                     , LoggedInOrOut(..)
                     , NewCharBundle(..)
                     , pp
                     , Pretty
                     , PutOrRem(..)
                     , RightOrLeft(..)
                     , Serializable
                     , serialize
                     , setEntFlag
                     , setObjFlag
                     , SetOp(..)
                     , setPlaFlag
                     , setRmFlag
                     , ShouldCap(..)
                     , ShouldLog(..)
                     , ShouldQuote(..)
                     , SingleTarget(..)
                     , ToOrFromThePeeped(..)
                     , Verb(..)
                     , WhichLog(..) ) where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Misc.Database
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (Getting, Setting, both)
import Control.Lens.Operators ((%~), (&), (^.))
import Data.Bits (clearBit, setBit, testBit)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Prelude hiding ((>>), pi)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.Misc"


-- ==================================================
-- Original typeclasses and instances:


class FromRol a where
  fromRol :: RightOrLeft -> a


instance FromRol Slot where
  fromRol RI = RingRIS
  fromRol RM = RingRMS
  fromRol RR = RingRRS
  fromRol RP = RingRPS
  fromRol LI = RingLIS
  fromRol LM = RingLMS
  fromRol LR = RingLRS
  fromRol LP = RingLPS
  fromRol s  = patternMatchFail "fromRol" . showText $ s


-----


class BanRecord a where
  recTimestamp :: a -> Text
  recTarget    :: a -> Text
  recIsBanned  :: a -> Bool
  recReason    :: a -> Text


instance BanRecord BanHostRec where
  recTimestamp = dbTimestamp
  recTarget    = dbHost
  recIsBanned  = dbIsBanned
  recReason    = dbReason


instance BanRecord BanPCRec where
  recTimestamp = dbTimestamp
  recTarget    = dbName
  recIsBanned  = dbIsBanned
  recReason    = dbReason


-----


class HasFlags a where
  flagGetter :: Getting Int a Int

  flagSetter :: Setting (->) a a Int Int

  getFlag :: (Enum e) => e -> a -> Bool
  getFlag (fromEnum -> flagBitNum) a = (a^.flagGetter) `testBit` flagBitNum

  setFlag :: (Enum e) => e -> Bool -> a -> a
  setFlag (fromEnum -> flagBitNum) b = flagSetter %~ (🍬 flagBitNum)
    where
      (🍬) = b ? setBit :? clearBit


instance HasFlags Ent where
  flagGetter = entFlags
  flagSetter = entFlags


getEntFlag :: EntFlags -> Ent -> Bool
getEntFlag = getFlag


setEntFlag :: EntFlags -> Bool -> Ent -> Ent
setEntFlag = setFlag


instance HasFlags Obj where
  flagGetter = objFlags
  flagSetter = objFlags


getObjFlag :: ObjFlags -> Obj -> Bool
getObjFlag = getFlag


setObjFlag :: ObjFlags -> Bool -> Obj -> Obj
setObjFlag = setFlag


instance HasFlags Rm where
  flagGetter = rmFlags
  flagSetter = rmFlags


getRmFlag :: RmFlags -> Rm -> Bool
getRmFlag = getFlag


setRmFlag :: RmFlags -> Bool -> Rm -> Rm
setRmFlag = setFlag


instance HasFlags Pla where
  flagGetter = plaFlags
  flagSetter = plaFlags


getPlaFlag :: PlaFlags -> Pla -> Bool
getPlaFlag = getFlag


setPlaFlag :: PlaFlags -> Bool -> Pla -> Pla
setPlaFlag = setFlag


-----


class Pretty a where
  pp :: a -> Text


instance Pretty ActiveEffect where
  pp (ActiveEffect e _) = pp e


instance Pretty ActType where
  pp Attacking = "attacking"
  pp Drinking  = "drinking"
  pp Eating    = "eating"
  pp Moving    = "moving"


instance Pretty AlertExecRec where
  pp AlertExecRec { .. } = slashes [ dbTimestamp
                                   , dbName
                                   , dbCmdName
                                   , dbTarget
                                   , dbArgs ]


instance Pretty AlertMsgRec where
  pp AlertMsgRec { .. } = slashes [ dbTimestamp
                                  , dbName
                                  , dbCmdName
                                  , dbTrigger
                                  , dbMsg ]


instance Pretty AOrThe where
  pp A   = "a"
  pp The = "the"


instance Pretty ArmSub where
  pp LowerBody = "lower body"
  pp x         = uncapitalize . showText $ x


instance Pretty Attrib where
  pp St = "ST"
  pp Dx = "DX"
  pp Ht = "HT"
  pp Ma = "MA"
  pp Ps = "PS"


instance Pretty BanHostRec where
  pp BanHostRec { .. } = slashes [ dbTimestamp
                                 , dbHost
                                 , dbIsBanned ? "banned" :? "unbanned"
                                 , dbReason ]


instance Pretty BanPCRec where
  pp BanPCRec { .. } = slashes [ dbTimestamp
                               , dbName
                               , dbIsBanned ? "banned" :? "unbanned"
                               , dbReason ]


instance Pretty BugRec where
  pp BugRec { .. } = slashes [ dbTimestamp
                             , dbName
                             , dbLoc
                             , dbDesc ]


instance Pretty ChanContext where
  pp ChanContext { .. } = someCmdName <> maybe "" spcL someChanName


instance Pretty Cloth where
  pp Backpack = "backpack"
  pp Bracelet = "bracelet"
  pp Cloak    = "cloak"
  pp Coat     = "coat"
  pp Dress    = "dress"
  pp Earring  = "earring"
  pp FullBody = "robes"
  pp Necklace = "necklace"
  pp NoseRing = "nose ring"
  pp Ring     = "ring"
  pp Shirt    = "shirt"
  pp Skirt    = "skirt"
  pp Smock    = "smock"
  pp Trousers = "trousers"


instance Pretty DiscoverRec where
  pp DiscoverRec { .. } = slashes [ dbTimestamp
                                  , dbHost
                                  , dbMsg ]


instance Pretty Effect where
  pp (Effect effSub effVal secs effFeeling) = T.concat [ bracketQuote "durational"
                                                       , " "
                                                       , pp effSub
                                                       , " by "
                                                       , effectValHelper effVal
                                                       , " "
                                                       , mkSecsTxt secs
                                                       , effectFeelingHelper effFeeling ]


mkSecsTxt :: Seconds -> Text
mkSecsTxt secs = parensQuote $ commaEvery3 (showText secs) <> " secs"


effectValHelper :: Maybe EffectVal -> Text
effectValHelper = maybe (parensQuote "no value") pp


effectFeelingHelper :: Maybe EffectFeeling -> Text
effectFeelingHelper = maybe "" (spcL . pp)


instance Pretty EffectFeeling where
  pp (EffectFeeling tag dur) = bracketQuote $ tag |<>| mkSecsTxt dur


instance Pretty EffectSub where
  pp ArmEffectAC              = "armor AC"
  pp EntEffectFlags           = "ent flags"
  pp (MobEffectAttrib attrib) = "mob "   <> pp attrib
  pp MobEffectAC              = "mob AC"
  pp RmEffectFlags            = "room flags"
  pp (EffectOther fn)         = "other " <> parensQuote fn


instance Pretty EffectVal where
  pp (DefiniteVal x     ) = showText x
  pp (RangeVal    (x, y)) = showText x <> T.cons '-' (showText y)


instance Pretty Feeling where
  pp (Feeling fv dur _ _) = pp fv |<>| mkSecsTxt dur


instance Pretty FeelingVal where
  pp NoVal      = "NoVal"
  pp (IntVal x) = "IntVal " <> showText x


instance Pretty Hand where
  pp RHand  = "right-handed"
  pp LHand  = "left-handed"
  pp NoHand = "not handed"


instance Pretty InstaEffect where
  pp (InstaEffect effSub effVal effFeeling) = T.concat [ bracketQuote "instantaneous"
                                                       , " "
                                                       , pp effSub
                                                       , " by "
                                                       , effectValHelper effVal
                                                       , effectFeelingHelper effFeeling ]


instance Pretty InstaEffectSub where
  pp EntInstaEffectFlags         = "ent flags"
  pp (MobInstaEffectPts ptsType) = "mob "   <> pp ptsType
  pp RmInstaEffectFlags          = "room flags"
  pp (InstaEffectOther fn)       = "other " <> parensQuote fn


instance Pretty Lang where
  pp CommonLang    = "common"
  pp DwarfLang     = "dwarvish"
  pp ElfLang       = "elvish"
  pp FelinoidLang  = "felinoidean"
  pp HobbitLang    = "hobbitish"
  pp HumanLang     = "hominal"
  pp LagomorphLang = "lagomorphean"
  pp NymphLang     = "naelyni"
  pp VulpenoidLang = "vulpenoidean"


instance Pretty LinkDir where
  pp = uncapitalize . showText


instance Pretty LoggedInOrOut where
  pp LoggedIn  = "logged in"
  pp LoggedOut = "logged out"


instance Pretty MobSize where
  pp SmlMinus = "Sml Minus"
  pp SmlPlus  = "Sml Plus"
  pp MedMinus = "Med Minus"
  pp MedPlus  = "Med Plus"
  pp LrgMinus = "Lrg Minus"
  pp LrgPlus  = "Lrg Plus"


instance Pretty PausedEffect where
  pp (PausedEffect e) = pp e


instance Pretty ProfRec where
  pp ProfRec { .. } = spaces [ dbTimestamp, dbHost, dbProfanity ]


instance Pretty PtsType where
  pp CurHp = "cur HP"
  pp CurMp = "cur MP"
  pp CurPp = "cur PP"
  pp CurFp = "cur FP"


instance Pretty SetOp where
  pp Assign    = "="
  pp AddAssign = "+="
  pp SubAssign = "-="


instance Pretty StomachCont where
  pp (StomachCont (Left  dli) t b) = ppStomachContHelper (showText dli) t b
  pp (StomachCont (Right dfi) t b) = ppStomachContHelper (showText dfi) t b


ppStomachContHelper :: Text -> UTCTime -> Bool -> Text
ppStomachContHelper txt t b = slashes [ txt, T.pack . formatTime defaultTimeLocale "%F %T" $ t, showText b ]


instance Pretty Race where
  pp Dwarf     = "dwarf"
  pp Elf       = "elf"
  pp Felinoid  = "felinoid"
  pp Hobbit    = "hobbit"
  pp Human     = "human"
  pp Lagomorph = "lagomorph"
  pp Nymph     = "nymph"
  pp Vulpenoid = "vulpenoid"


instance Pretty RightOrLeft where
  pp R   = "right"
  pp L   = "left"
  pp rol = pp (fromRol rol :: Slot)


instance Pretty Sex where
  pp Male   = "male"
  pp Female = "female"
  pp NoSex  = none


instance Pretty Slot where
  -- Clothing slots:
  pp EarringR1S  = "right ear"
  pp EarringR2S  = "right ear"
  pp EarringL1S  = "left ear"
  pp EarringL2S  = "left ear"
  pp NoseRing1S  = "nose"
  pp NoseRing2S  = "nose"
  pp Necklace1S  = "neck"
  pp Necklace2S  = "neck"
  pp Necklace3S  = "neck"
  pp BraceletR1S = "right wrist"
  pp BraceletR2S = "right wrist"
  pp BraceletR3S = "right wrist"
  pp BraceletL1S = "left wrist"
  pp BraceletL2S = "left wrist"
  pp BraceletL3S = "left wrist"
  pp RingRIS     = "right index finger"
  pp RingRMS     = "right middle finger"
  pp RingRRS     = "right ring finger"
  pp RingRPS     = "right pinky finger"
  pp RingLIS     = "left index finger"
  pp RingLMS     = "left middle finger"
  pp RingLRS     = "left ring finger"
  pp RingLPS     = "left pinky finger"
  pp ShirtS      = "shirt"
  pp SmockS      = "smock"
  pp CoatS       = "coat"
  pp TrousersS   = "trousers"
  pp SkirtS      = "skirt"
  pp DressS      = "dress"
  pp FullBodyS   = "about body"
  pp BackpackS   = "backpack"
  pp CloakS      = "cloak"
  -- Armor slots:
  pp HeadS       = "head"
  pp TorsoS      = "torso"
  pp ArmsS       = "arms"
  pp HandsS      = "hands"
  pp LowerBodyS  = "lower body"
  pp FeetS       = "feet"
  -- Weapon/shield slots:
  pp RHandS      = "right hand"
  pp LHandS      = "left hand"
  pp BothHandsS  = "both hands"


instance Pretty Type where
  pp ArmType      = "armor"
  pp ClothType    = "clothing"
  pp ConType      = "container"
  pp CorpseType   = "corpse"
  pp FoodType     = "food"
  pp NpcType      = "NPC"
  pp ObjType      = "object"
  pp PCType       = "PC"
  pp RmType       = "room"
  pp VesselType   = "vessel"
  pp WpnType      = "weapon"
  pp WritableType = "writable"


instance Pretty TypoRec where
  pp TypoRec { .. } = slashes [ dbTimestamp
                              , dbName
                              , dbLoc
                              , dbDesc ]


instance Pretty WhichLog where
  pp BugLog  = "bug"
  pp TypoLog = "typo"


instance Pretty WpnSub where
  pp OneHanded = "one-handed"
  pp TwoHanded = "two-handed"


-----


class Serializable a where
  serialize   :: a -> Text
  deserialize :: Text -> a


instance Serializable Desig where
  serialize StdDesig { .. }
    | fields <- [ serMaybeText desigEntSing, showText desigShouldCap, desigEntName, showText desigId, showText desigIds ]
    = quoteWith sdd . T.intercalate dd $ fields
    where
      serMaybeText Nothing    = ""
      serMaybeText (Just txt) = txt
      (sdd, dd)               = (stdDesigDelimiter, desigDelimiter) & both %~ T.singleton
  serialize NonStdDesig { .. } = quoteWith nsdd $ do
      dEntSing
      dd
      dDesc
    where
      (>>)       = (<>)
      (nsdd, dd) = (nonStdDesigDelimiter, desigDelimiter) & both %~ T.singleton
  serialize (CorpseDesig i) = quoteWith cdd . showText $ i
    where
      cdd = T.singleton corpseDesigDelimiter
  deserialize a@(headTail -> (c, T.init -> t))
    | c == stdDesigDelimiter
    , [ es, sc, en, i, is ] <- T.splitOn dd t
    = StdDesig { desigEntSing   = deserMaybeText es
               , desigShouldCap = read . T.unpack $ sc
               , desigEntName   = en
               , desigId        = read . T.unpack $ i
               , desigIds       = read . T.unpack $ is }
    | c == nonStdDesigDelimiter
    , [ es, nsd ] <- T.splitOn dd t
    = NonStdDesig { dEntSing = es, dDesc = nsd }
    | c == corpseDesigDelimiter
    = CorpseDesig . read . T.unpack $ t
    | otherwise = patternMatchFail "deserialize" . showText $ a
    where
      deserMaybeText ""  = Nothing
      deserMaybeText txt = Just txt
      dd                 = T.singleton desigDelimiter


-- ==================================================
-- Data types:


type Punc = Text


data EmoteWord = ForNonTargets Text
               | ForTarget     Punc Id
               | ForTargetPoss Punc Id deriving (Eq, Show)


-----


type ExpCmdName = Text


type ToSelf             = Text
type ToOthers           = Text
type ToSelfWithTarget   = Text
type ToTarget           = Text
type ToOthersWithTarget = Text


data ExpCmdType = NoTarget  ToSelf ToOthers
                | HasTarget                 ToSelfWithTarget ToTarget ToOthersWithTarget
                | Versatile ToSelf ToOthers ToSelfWithTarget ToTarget ToOthersWithTarget deriving (Eq, Ord, Show)


type ExpCmdFun = Id -> MsgQueue -> Cols -> ExpCmdName -> (Text, [Broadcast], MobRmDesc, Text) -> MudStack ()


data ExpCmd = ExpCmd { expCmdName :: ExpCmdName
                     , expCmdType :: ExpCmdType
                     , expCmdFun  :: Maybe ExpCmdFun
                     , expDesc    :: MobRmDesc }


instance Eq ExpCmd where
  (==) = (==) `on` expCmdName


instance Ord ExpCmd where
  compare = compare `on` expCmdName


-----


data AOrThe = A | The


-----


data ChanContext = ChanContext { someCmdName      :: Text
                               , someChanName     :: Maybe ChanName
                               , revealAdminNames :: Bool }


-----


data ClassifiedBcast = TargetBcast    Broadcast
                     | NonTargetBcast Broadcast deriving Eq


instance Ord ClassifiedBcast where
  TargetBcast    _ `compare` NonTargetBcast _ = LT
  NonTargetBcast _ `compare` TargetBcast    _ = GT
  _うんこ           `compare` _糞              = EQ


-----


type CmdPriorityAbbrevTxt = Text
type CmdFullName          = Text
type CmdDesc              = Text


data Cmd = Cmd { cmdName           :: CmdName
               , cmdPriorityAbbrev :: Maybe CmdPriorityAbbrevTxt
               , cmdFullName       :: CmdFullName
               , cmdAction         :: Action
               , cmdDesc           :: CmdDesc }


instance Eq Cmd where
  (==) (Cmd cn1 cpa1 cfn1 _ cd1)
       (Cmd cn2 cpa2 cfn2 _ cd2) = and [ c1 == c2 | c1 <- [ cn1, fromMaybe "" cpa1, cfn1, cd1 ]
                                                  | c2 <- [ cn2, fromMaybe "" cpa2, cfn2, cd2 ] ]


instance Ord Cmd where
  compare = compare `on` cmdName


-----


data Desig = StdDesig    { desigEntSing   :: Maybe Text
                         , desigShouldCap :: ShouldCap
                         , desigEntName   :: Text
                         , desigId        :: Id
                         , desigIds       :: Inv }
           | NonStdDesig { dEntSing       :: Text
                         , dDesc          :: Text }
           | CorpseDesig Id deriving (Eq, Show)


data ShouldCap = DoCap | Don'tCap deriving (Eq, Read, Show)


-----


data DrinkBundle = DrinkBundle { drinkerId       :: Id
                               , drinkerMq       :: MsgQueue
                               , drinkerCols     :: Cols
                               , drinkVesselId   :: Maybe Id -- "Nothing" for hooks.
                               , drinkVesselSing :: Sing
                               , drinkLiq        :: Liq
                               , drinkAmt        :: Mouthfuls }


-----


data EquipInvLookCmd = EquipCmd | InvCmd | LookCmd deriving Eq


instance Show EquipInvLookCmd where
  show EquipCmd = "equipment"
  show InvCmd   = "inventory"
  show LookCmd  = "look"


-----


type Amount = Int
type Index  = Int


data GetEntsCoinsRes = Mult    { amount          :: Amount
                               , nameSearchedFor :: Text
                               , entsRes         :: Maybe [Ent]
                               , coinsRes        :: Maybe (EmptyNoneSome Coins) }
                     | Indexed { index           :: Index
                               , nameSearchedFor :: Text
                               , entRes          :: Either Plur Ent }
                     | Sorry   { nameSearchedFor :: Text }
                     | SorryIndexedCoins deriving Show


data EmptyNoneSome a = Empty
                     | NoneOf a
                     | SomeOf a deriving (Eq, Show)


-----


data GetOrDrop = Get | Drop


-----


type HelpName = Text


data Help = Help { helpName     :: HelpName
                 , helpFilePath :: FilePath
                 , isCmdHelp    :: Bool
                 , isAdminHelp  :: Bool } deriving (Eq, Ord)


-----


data IdSingTypeDesig = IdSingTypeDesig { theId    :: Id
                                       , theSing  :: Sing
                                       , theType  :: Type
                                       , theDesig :: Text }


-----


data InInvEqRm = InInv | InEq | InRm deriving Show


-----


data LastArgIsTargetBindings = LastArgIsTargetBindings { srcDesig    :: Desig
                                                       , srcInvCoins :: (Inv, Coins)
                                                       , rmInvCoins  :: (Inv, Coins)
                                                       , targetArg   :: Text
                                                       , otherArgs   :: Args }


-----


data LoggedInOrOut = LoggedIn | LoggedOut deriving Eq


-----


data NewCharBundle = NewCharBundle { ncbOldSing :: Sing
                                   , ncbSing    :: Sing
                                   , ncbPW      :: Text }


-----


data PutOrRem = Put | Rem deriving (Eq, Show)


-----


data RightOrLeft = R
                 | L
                 | RI | RM | RR | RP
                 | LI | LM | LR | LP deriving (Read, Show)


-----


data SetOp = Assign | AddAssign | SubAssign


-----


data ShouldQuote = DoQuote | Don'tQuote deriving Eq


-----


data ShouldLog = DoLog | Don'tLog deriving Show


-----


data SingleTarget = SingleTarget { strippedTarget   :: Text
                                 , strippedTarget'  :: Text
                                 , sendFun          :: Text   -> MudStack ()
                                 , multiSendFun     :: [Text] -> MudStack ()
                                 , consLocPrefMsg   :: [Text] -> [Text]
                                 , consLocPrefBcast :: Id -> [Broadcast] -> [Broadcast] }


-----


data ToOrFromThePeeped = ToThePeeped | FromThePeeped


-----


data Verb = SndPer | ThrPer deriving Eq


-----


data WhichLog = BugLog | TypoLog deriving Show
