{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.MiscDataTypes ( AOrThe(..)
                         , Action
                         , Amount
                         , Broadcast
                         , ClassifiedBroadcast(..)
                         , Cmd(..)
                         , CmdName
                         , Cols
                         , EmptyNoneSome(..)
                         , FromRol
                         , GetEntsCoinsRes(..)
                         , GetOrDrop(..)
                         , IdMsgQueueCols
                         , Index
                         , InvType(..)
                         , NameSearchedFor
                         , PCDesig(..)
                         , Pretty
                         , PutOrRem(..)
                         , Rest
                         , RightOrLeft(..)
                         , Serializable
                         , deserialize
                         , fromRol
                         , pp
                         , serialize
                         , Verb(..) ) where

import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative (pure)
import Control.Lens (both, over)
import Data.Monoid ((<>))
import Prelude hiding (pi)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.MiscDataTypes"


-- ==================================================
-- Typeclasses and instances:


class Pretty a where
  pp :: a -> T.Text


instance Pretty Sex where
  pp Male   = "male"
  pp Female = "female"
  pp NoSex  = undefined


instance Pretty Race where
  pp Dwarf     = "dwarf"
  pp Elf       = "elf"
  pp Felinoid  = "felinoid"
  pp Halfling  = "halfling"
  pp Human     = "human"
  pp Lagomorph = "lagomorph"
  pp Nymph     = "nymph"
  pp Vulpenoid = "vulpenoid"


instance Pretty Slot where
  pp HeadS      = "head"
  pp REar1S     = "right ear"
  pp REar2S     = "right ear"
  pp LEar1S     = "left ear"
  pp LEar2S     = "left ear"
  pp Nose1S     = "nose"
  pp Nose2S     = "nose"
  pp Neck1S     = "neck"
  pp Neck2S     = "neck"
  pp Neck3S     = "neck"
  pp RWrist1S   = "right wrist"
  pp RWrist2S   = "right wrist"
  pp RWrist3S   = "right wrist"
  pp LWrist1S   = "left wrist"
  pp LWrist2S   = "left wrist"
  pp LWrist3S   = "left wrist"
  pp RIndexFS   = "right index finger"
  pp RMidFS     = "right middle finger"
  pp RRingFS    = "right ring finger"
  pp RPinkyFS   = "right pinky finger"
  pp LIndexFS   = "left index finger"
  pp LMidFS     = "left middle finger"
  pp LRingFS    = "left ring finger"
  pp LPinkyFS   = "left pinky finger"
  pp RHandS     = "right hand"
  pp LHandS     = "left hand"
  pp BothHandsS = "both hands"
  pp UpBodyCS   = "upper body"
  pp LowBodyCS  = "lower body"
  pp FullBodyCS = "full body"
  pp UpBodyAS   = "upper body"
  pp LowBodyAS  = "lower body"
  pp FullBodyAS = "full body"
  pp BackS      = "back"
  pp FeetS      = "feet"


instance Pretty AOrThe where
  pp A   = "a"
  pp The = "the"


instance Pretty RightOrLeft where
  pp R   = "right"
  pp L   = "left"
  pp rol = pp (fromRol rol :: Slot)


-----


class FromRol a where
  fromRol :: RightOrLeft -> a


instance FromRol Slot where
  fromRol RI = RIndexFS
  fromRol RM = RMidFS
  fromRol RR = RRingFS
  fromRol RP = RPinkyFS
  fromRol LI = LIndexFS
  fromRol LM = LMidFS
  fromRol LR = LRingFS
  fromRol LP = LPinkyFS
  fromRol s  = patternMatchFail "fromRol" [ showText s ]


-----


class Serializable a where
  serialize   :: a -> T.Text
  deserialize :: T.Text -> a


instance Serializable PCDesig where
  serialize (StdDesig pes ic pen pi pis) | fields <- [ serMaybeText pes, showText ic, pen, showText pi, showText pis ] =
      quoteWith d . T.intercalate d' $ fields
    where
      serMaybeText Nothing    = ""
      serMaybeText (Just txt) = txt
      (d, d')                 = over both (T.pack . pure) (stdDesigDelimiter, desigDelimiter)
  serialize (NonStdDesig pes nsd) = quoteWith d $ pes <> d' <> nsd
    where
      (d, d') = over both (T.pack . pure) (nonStdDesigDelimiter, desigDelimiter)
  deserialize a@(headTail -> (c, T.init -> t))
    | c == stdDesigDelimiter, [ pes, ic, pen, pi, pis ] <- T.splitOn d t =
        StdDesig { stdPCEntSing = deserMaybeText pes
                 , isCap        = read . T.unpack $ ic
                 , pcEntName    = pen
                 , pcId         = read . T.unpack $ pi
                 , pcIds        = read . T.unpack $ pis }
    | [ pes, nsd ] <- T.splitOn d t = NonStdDesig { nonStdPCEntSing = pes
                                                  , nonStdDesc      = nsd }
    | otherwise = patternMatchFail "deserialize" [ showText a ]
    where
      deserMaybeText ""  = Nothing
      deserMaybeText txt = Just txt
      d                  = T.pack [desigDelimiter]


-----


instance Ord ClassifiedBroadcast where
  TargetBroadcast    _ `compare` NonTargetBroadcast _ = LT
  NonTargetBroadcast _ `compare` TargetBroadcast    _ = GT
  _                    `compare` _                    = EQ


-- ==================================================
-- Data types:


type CmdName        = T.Text
type Action         = IdMsgQueueCols -> Rest -> MudStack ()
type IdMsgQueueCols = (Id, MsgQueue, Cols)
type Cols           = Int
type Rest           = [T.Text]


data Cmd = Cmd { cmdName :: !CmdName
               , action  :: !Action
               , cmdDesc :: !T.Text }


-----


type Amount          = Int
type Index           = Int
type NameSearchedFor = T.Text


data GetEntsCoinsRes = Mult    !Amount !NameSearchedFor !(Maybe [Ent]) !(Maybe (EmptyNoneSome Coins))
                     | Indexed !Index  !NameSearchedFor !(Either Plur Ent)
                     | Sorry           !NameSearchedFor
                     | SorryIndexedCoins deriving Show


data EmptyNoneSome a = Empty
                     | NoneOf a
                     | SomeOf a deriving (Eq, Show)


-----


data AOrThe = A | The


data Verb = SndPer | ThrPer


data GetOrDrop = Get | Drop


data PutOrRem = Put | Rem deriving (Eq, Show)


data RightOrLeft = R
                 | L
                 | RI | RM | RR | RP
                 | LI | LM | LR | LP deriving (Show, Read)


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


type Broadcast = (T.Text, Inv)


data ClassifiedBroadcast = TargetBroadcast    Broadcast
                         | NonTargetBroadcast Broadcast deriving Eq
