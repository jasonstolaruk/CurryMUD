{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ParallelListComp, RebindableSyntax, RecordWildCards, ViewPatterns #-}

module Mud.Data.Misc ( AOrThe(..)
                     , Action
                     , Amount
                     , Args
                     , BanRecord(..)
                     , Broadcast
                     , ChanContext(..)
                     , ClassifiedBroadcast(..)
                     , Cmd(..)
                     , CmdDesc
                     , CmdFullName
                     , CmdPriorityAbbrevTxt
                     , Cols
                     , EmoteWord(..)
                     , EmptyNoneSome(..)
                     , EquipInvLookCmd(..)
                     , ExpCmd(..)
                     , ExpCmdName
                     , ExpCmdType(..)
                     , GetEntsCoinsRes(..)
                     , GetOrDrop(..)
                     , Help(..)
                     , HelpName
                     , IdSingTypeDesig(..)
                     , InInvEqRm(..)
                     , Index
                     , LoggedInOrOut(..)
                     , PCDesig(..)
                     , PlsDie(..)
                     , Pretty
                     , PutOrRem(..)
                     , RightOrLeft(..)
                     , Serializable
                     , ShouldBracketQuote(..)
                     , ShouldCap(..)
                     , ShouldLog(..)
                     , SingleTarget(..)
                     , ToOrFromThePeeped(..)
                     , Verb(..)
                     , WhichLog(..)
                     , deserialize
                     , fromRol
                     , getEntFlag
                     , getPlaFlag
                     , getRmFlag
                     , pp
                     , serialize
                     , setEntFlag
                     , setPlaFlag
                     , setRmFlag ) where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.MudData
import Mud.Misc.Database
import Mud.TopLvlDefs.Chars
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Exception (Exception)
import Control.Lens (Getting, Setting, both)
import Control.Lens.Operators ((%~), (&), (^.))
import Data.Bits (clearBit, setBit, testBit)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Typeable (Typeable)
import Prelude hiding ((>>), pi)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
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
  fromRol s  = patternMatchFail "fromRol" [ showText s ]


-----


class BanRecord a where
  recTimestamp :: a -> T.Text
  recTarget    :: a -> T.Text
  recIsBanned  :: a -> Bool
  recReason    :: a -> T.Text


instance BanRecord BanHostRec where
  recTimestamp = banHostTimestamp
  recTarget    = banHostHost
  recIsBanned  = banHostIsBanned
  recReason    = banHostReason


instance BanRecord BanPlaRec where
  recTimestamp = banPlaTimestamp
  recTarget    = banPlaName
  recIsBanned  = banPlaIsBanned
  recReason    = banPlaReason


-----


class HasFlags a where
  flagGetter :: Getting Int a Int

  flagSetter :: Setting (->) a a Int Int

  getFlag :: (Enum e) => e -> a -> Bool
  getFlag (fromEnum -> flagBitNum) a = (a^.flagGetter) `testBit` flagBitNum

  setFlag :: (Enum e) => e -> Bool -> a -> a
  setFlag (fromEnum -> flagBitNum) b = flagSetter %~ (`f` flagBitNum)
    where
      f = b ? setBit :? clearBit


instance HasFlags Ent where
  flagGetter = entFlags
  flagSetter = entFlags


getEntFlag :: EntFlags -> Ent -> Bool
getEntFlag = getFlag


setEntFlag :: EntFlags -> Bool -> Ent -> Ent
setEntFlag = setFlag


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
  pp :: a -> T.Text


instance Pretty AOrThe where
  pp A   = "a"
  pp The = "the"


instance Pretty BanHostRec where
  pp (BanHostRec { .. }) = slashes [ banHostTimestamp
                                   , banHostHost
                                   , banHostIsBanned ? "banned" :? "unbanned"
                                   , banHostReason ]


instance Pretty BanPlaRec where
  pp (BanPlaRec { .. }) = slashes [ banPlaTimestamp
                                  , banPlaName
                                  , banPlaIsBanned ? "banned" :? "unbanned"
                                  , banPlaReason ]


instance Pretty BugRec where
  pp (BugRec { .. }) = slashes [ bugTimestamp
                               , bugName
                               , bugLoc
                               , bugDesc
                               , bugIsOpen ? "open" :? "closed" ]


instance Pretty ChanContext where
  pp (ChanContext { .. }) = someCmdName <> maybe "" (" " <>) someChanName


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


instance Pretty ProfRec where
  pp (ProfRec { .. }) = T.intercalate " " [ profTimestamp
                                          , profHost
                                          , profProfanity ]


instance Pretty Race where
  pp Dwarf     = "dwarf"
  pp Elf       = "elf"
  pp Felinoid  = "felinoid"
  pp Halfling  = "halfling"
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
  pp NoSex  = undefined


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


instance Pretty TypoRec where
  pp (TypoRec { .. }) = slashes [ typoTimestamp
                                , typoName
                                , typoLoc
                                , typoDesc
                                , typoIsOpen ? "open" :? "closed" ]


instance Pretty WhichLog where
  pp BugLog  = "bug"
  pp TypoLog = "typo"


-----


class Serializable a where
  serialize   :: a -> T.Text
  deserialize :: T.Text -> a


instance Serializable PCDesig where
  serialize StdDesig { .. }
    | fields <- [ serMaybeText stdPCEntSing, showText shouldCap, pcEntName, showText pcId, showText pcIds ]
    = quoteWith sdd . T.intercalate dd $ fields
    where
      serMaybeText Nothing    = ""
      serMaybeText (Just txt) = txt
      (sdd, dd)               = (stdDesigDelimiter, desigDelimiter) & both %~ T.singleton
  serialize NonStdDesig { .. } = quoteWith nsdd $ do
      nonStdPCEntSing
      dd
      nonStdDesc
    where
      (>>)       = (<>)
      (nsdd, dd) = (nonStdDesigDelimiter, desigDelimiter) & both %~ T.singleton
  deserialize a@(headTail -> (c, T.init -> t))
    | c == stdDesigDelimiter, [ pes, sc, pen, pi, pis ] <- T.splitOn dd t =
        StdDesig { stdPCEntSing = deserMaybeText pes
                 , shouldCap    = read . T.unpack $ sc
                 , pcEntName    = pen
                 , pcId         = read . T.unpack $ pi
                 , pcIds        = read . T.unpack $ pis }
    | c == nonStdDesigDelimiter, [ pes, nsd ] <- T.splitOn dd t =
        NonStdDesig { nonStdPCEntSing = pes, nonStdDesc = nsd }
    | otherwise = patternMatchFail "deserialize" [ showText a ]
    where
      deserMaybeText ""  = Nothing
      deserMaybeText txt = Just txt
      dd                 = T.singleton desigDelimiter


-- ==================================================
-- Data types:


type Punc = T.Text


data EmoteWord = ForNonTargets T.Text
               | ForTarget     Punc Id
               | ForTargetPoss Punc Id deriving (Eq, Show)


-----


type ExpCmdName = T.Text


type ToSelf             = T.Text
type ToOthers           = T.Text
type ToSelfWithTarget   = T.Text
type ToTarget           = T.Text
type ToOthersWithTarget = T.Text


data ExpCmdType = NoTarget  ToSelf ToOthers
                | HasTarget                 ToSelfWithTarget ToTarget ToOthersWithTarget
                | Versatile ToSelf ToOthers ToSelfWithTarget ToTarget ToOthersWithTarget deriving (Eq, Ord, Show)


data ExpCmd = ExpCmd ExpCmdName ExpCmdType deriving (Eq, Ord)


-----


data AOrThe = A | The


-----


data ChanContext = ChanContext { someCmdName      :: T.Text
                               , someChanName     :: Maybe ChanName
                               , revealAdminNames :: Bool }


-----


type Broadcast = (T.Text, Inv)


data ClassifiedBroadcast = TargetBroadcast    Broadcast
                         | NonTargetBroadcast Broadcast deriving Eq


instance Ord ClassifiedBroadcast where
  TargetBroadcast    _ `compare` NonTargetBroadcast _ = LT
  NonTargetBroadcast _ `compare` TargetBroadcast    _ = GT
  _うんこ `compare` _糞 = EQ


-----


type CmdPriorityAbbrevTxt = T.Text
type CmdFullName          = T.Text
type Action               = ActionParams -> MudStack ()
type CmdDesc              = T.Text


data Cmd = Cmd { cmdName           :: CmdName
               , cmdPriorityAbbrev :: Maybe CmdPriorityAbbrevTxt
               , cmdFullName       :: CmdFullName
               , action            :: Action
               , cmdDesc           :: CmdDesc }


instance Eq Cmd where
  (==) Cmd { cmdName = cn1, cmdPriorityAbbrev = cpa1, cmdDesc = cd1 }
       Cmd { cmdName = cn2, cmdPriorityAbbrev = cpa2, cmdDesc = cd2 } =
       and [ c1 == c2 | c1 <- [ cn1, fromMaybe "" cpa1, cd1 ]
                      | c2 <- [ cn2, fromMaybe "" cpa2, cd2 ] ]


instance Ord Cmd where
  Cmd { cmdName = cn1 } `compare` Cmd { cmdName = cn2 } = cn1 `compare` cn2


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
                               , nameSearchedFor :: T.Text
                               , entsRes         :: Maybe [Ent]
                               , coinsRes        :: Maybe (EmptyNoneSome Coins) }
                     | Indexed { index           :: Index
                               , nameSearchedFor :: T.Text
                               , entRes          :: Either Plur Ent }
                     | Sorry   { nameSearchedFor :: T.Text }
                     | SorryIndexedCoins deriving Show


data EmptyNoneSome a = Empty
                     | NoneOf a
                     | SomeOf a deriving (Eq, Show)


-----


data GetOrDrop = Get | Drop


-----


type HelpName = T.Text


data Help = Help { helpName     :: HelpName
                 , helpFilePath :: FilePath
                 , isCmdHelp    :: Bool
                 , isAdminHelp  :: Bool } deriving (Eq, Ord)


-----


data IdSingTypeDesig = IdSingTypeDesig { theId    :: Id
                                       , theSing  :: Sing
                                       , theType  :: Type
                                       , theDesig :: T.Text }


-----


data InInvEqRm = InInv | InEq | InRm deriving (Show)


-----


data LoggedInOrOut = LoggedIn | LoggedOut deriving Eq


instance Show LoggedInOrOut where
  show LoggedIn  = "logged in"
  show LoggedOut = "logged out"


-----


data PCDesig = StdDesig    { stdPCEntSing    :: Maybe T.Text
                           , shouldCap       :: ShouldCap
                           , pcEntName       :: T.Text
                           , pcId            :: Id
                           , pcIds           :: Inv }
             | NonStdDesig { nonStdPCEntSing :: T.Text
                           , nonStdDesc      :: T.Text } deriving (Eq, Show)


data ShouldCap = DoCap | Don'tCap deriving (Eq, Read, Show)


-----


data PlsDie = PlsDie deriving (Show, Typeable)


instance Exception PlsDie


-----


data PutOrRem = Put | Rem deriving (Eq, Show)


-----


data RightOrLeft = R
                 | L
                 | RI | RM | RR | RP
                 | LI | LM | LR | LP deriving (Read, Show)


-----


data ShouldBracketQuote = DoBracket | Don'tBracket


-----


data ShouldLog = DoLog | Don'tLog deriving (Show)


-----


data SingleTarget = SingleTarget { strippedTarget     :: T.Text
                                 , strippedTarget'    :: T.Text
                                 , sendFun            :: T.Text   -> MudStack ()
                                 , multiSendFun       :: [T.Text] -> MudStack ()
                                 , consSorry          :: [T.Text] -> [T.Text]
                                 , consSorryBroadcast :: Id -> [Broadcast] -> [Broadcast] }


-----


data ToOrFromThePeeped = ToThePeeped | FromThePeeped


-----


data Verb = SndPer | ThrPer deriving Eq


-----


data WhichLog = BugLog | TypoLog deriving Show
