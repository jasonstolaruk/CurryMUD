{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ParallelListComp, RebindableSyntax, RecordWildCards, ViewPatterns #-}

module Mud.Data.Misc ( AOrThe(..)
                     , Action
                     , ActionCmd(..)
                     , ActionCmdType(..)
                     , Amount
                     , Args
                     , Broadcast
                     , ClassifiedBroadcast(..)
                     , Cmd(..)
                     , CmdName
                     , Cols
                     , EmptyNoneSome(..)
                     , GetEntsCoinsRes(..)
                     , GetOrDrop(..)
                     , Help(..)
                     , HelpName
                     , Index
                     , InvType(..)
                     , PCDesig(..)
                     , PutOrRem(..)
                     , RightOrLeft(..)
                     , Serializable
                     , ShouldBracketQuote(..)
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
import Mud.Data.State.State
import Mud.TopLvlDefs.Chars
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (both, over)
import Control.Lens.Getter (Getting)
import Control.Lens.Operators ((^.))
import Control.Lens.Setter (Setting)
import Data.Bits (clearBit, setBit, testBit)
import Data.Monoid ((<>))
import Data.String (fromString)
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


class HasFlags a where
  flagGetter :: Getting Int a Int

  flagSetter :: Setting (->) a a Int Int

  getFlag :: (Enum e) => e -> a -> Bool
  getFlag (fromEnum -> flagBitNum) a = (a^.flagGetter) `testBit` flagBitNum

  setFlag :: (Enum e) => e -> Bool -> a -> a
  setFlag (fromEnum -> flagBitNum) b = over flagSetter (`f` flagBitNum)
    where
      f = if b then setBit else clearBit


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


instance Pretty WhichLog where
  pp BugLog  = "bug"
  pp TypoLog = "typo"


-----


class Serializable a where
  serialize   :: a -> T.Text
  deserialize :: T.Text -> a


instance Serializable PCDesig where
  serialize StdDesig { .. }
    | fields <- [ serMaybeText stdPCEntSing, showText isCap, pcEntName, showText pcId, showText pcIds ]
    = quoteWith sdd . T.intercalate dd $ fields
    where
      serMaybeText Nothing    = ""
      serMaybeText (Just txt) = txt
      (sdd, dd)               = over both T.singleton (stdDesigDelimiter, desigDelimiter)
  serialize NonStdDesig { .. } = quoteWith nsdd $ do
      nonStdPCEntSing
      dd
      nonStdDesc
    where
      (>>)       = (<>)
      (nsdd, dd) = over both T.singleton (nonStdDesigDelimiter, desigDelimiter)
  deserialize a@(headTail' -> (c, T.init -> t))
    | c == stdDesigDelimiter, [ pes, ic, pen, pi, pis ] <- T.splitOn dd t =
        StdDesig { stdPCEntSing = deserMaybeText pes
                 , isCap        = read . T.unpack $ ic
                 , pcEntName    = pen
                 , pcId         = read . T.unpack $ pi
                 , pcIds        = read . T.unpack $ pis }
    | [ pes, nsd ] <- T.splitOn dd t = NonStdDesig { nonStdPCEntSing = pes
                                                   , nonStdDesc      = nsd }
    | otherwise = patternMatchFail "deserialize" [ showText a ]
    where
      deserMaybeText ""  = Nothing
      deserMaybeText txt = Just txt
      dd                 = T.singleton desigDelimiter


-- ==================================================
-- Data types:


type ActionCmdName = T.Text


type ToSelf             = T.Text
type ToOthers           = T.Text
type ToSelfWithTarget   = T.Text
type ToTarget           = T.Text
type ToOthersWithTarget = T.Text


data ActionCmdType = NoTarget  !ToSelf !ToOthers
                   | HasTarget                   !ToSelfWithTarget !ToTarget !ToOthersWithTarget
                   | Versatile !ToSelf !ToOthers !ToSelfWithTarget !ToTarget !ToOthersWithTarget
                   deriving (Eq, Ord, Show)


data ActionCmd = ActionCmd !ActionCmdName !ActionCmdType deriving (Eq, Ord)


-----


data AOrThe = A | The


-----


type Broadcast = (T.Text, Inv)


data ClassifiedBroadcast = TargetBroadcast    Broadcast
                         | NonTargetBroadcast Broadcast deriving Eq


instance Ord ClassifiedBroadcast where
  TargetBroadcast    _ `compare` NonTargetBroadcast _ = LT
  NonTargetBroadcast _ `compare` TargetBroadcast    _ = GT
  _                    `compare` _                    = EQ


-----


type Action = ActionParams -> MudStack () -- TODO: Change "Action" to "CmdFun" and "ActionParams" to "CmdFunParams".


data Cmd = Cmd { cmdName :: !CmdName
               , action  :: !Action
               , cmdDesc :: !T.Text }


instance Eq Cmd where
  Cmd { cmdName = cn1, cmdDesc = cd1 } == Cmd { cmdName = cn2, cmdDesc = cd2 } =
      and [ c1 == c2 | c1 <- [ cn1, cd1 ] | c2 <- [ cn2, cd2 ]]


instance Ord Cmd where
  Cmd { cmdName = cn1 } `compare` Cmd { cmdName = cn2 } = cn1 `compare` cn2


-----


type Amount = Int
type Index  = Int


data GetEntsCoinsRes = Mult    { amount          :: !Amount
                               , nameSearchedFor :: !T.Text
                               , entsRes         :: !(Maybe [Ent])
                               , coinsRes        :: !(Maybe (EmptyNoneSome Coins)) }
                     | Indexed { index           :: !Index
                               , nameSearchedFor :: !T.Text
                               , entRes          :: !(Either Plur Ent) }
                     | Sorry   { nameSearchedFor :: !T.Text }
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
                 , isAdminHelp  :: Bool }


-----

data InvType = PCInv | PCEq | RmInv deriving Eq


-----


data PCDesig = StdDesig    { stdPCEntSing    :: !(Maybe T.Text)
                           , isCap           :: !Bool
                           , pcEntName       :: !T.Text
                           , pcId            :: !Id
                           , pcIds           :: !Inv }
             | NonStdDesig { nonStdPCEntSing :: !T.Text
                           , nonStdDesc      :: !T.Text } deriving (Eq, Show)


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


data ToOrFromThePeeped = ToThePeeped | FromThePeeped


-----


data Verb = SndPer | ThrPer deriving Eq


-----


data WhichLog = BugLog | TypoLog deriving Show
