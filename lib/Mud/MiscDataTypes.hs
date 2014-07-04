{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.MiscDataTypes ( Action
                         , Amount
                         , BothGramNos
                         , Cmd(..)
                         , CmdName
                         , ConvRol
                         , EmptyNoneSome(..)
                         , FromId
                         , fromRol
                         , GetEntsCoinsRes(..)
                         , GetOrDrop(..)
                         , HelpTopic
                         , Index
                         , Input
                         , InvCoins
                         , InvType(..)
                         , NameSearchedFor
                         , pp
                         , Pretty
                         , PutOrRem(..)
                         , Rest
                         , ToId
                         , ReconciledCoins
                         , RightOrLeft(..)
                         , ShouldNewLine ) where

import Mud.StateDataTypes
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.MiscDataTypes"

-----

class Pretty a where
  -- Pretty print.
  pp :: a -> T.Text

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

instance Pretty RightOrLeft where
  pp R   = "right"
  pp L   = "left"
  pp rol = pp (fromRol rol :: Slot)

-----

class ConvRol a where
  fromRol :: RightOrLeft -> a

instance ConvRol Slot where
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

type CmdName = T.Text
type Rest    = [T.Text]

type Action = Rest -> MudStack ()

data Cmd = Cmd { cmdName :: !CmdName
               , action  :: !Action
               , cmdDesc :: !T.Text }

type Input = (CmdName, Rest)

-----

data InvType = PCInv | PCEq | RmInv deriving Eq

data GetOrDrop = Get | Drop

data PutOrRem = Put | Rem deriving Show

data RightOrLeft = R
                 | L
                 | RI | RM | RR | RP
                 | LI | LM | LR | LP deriving (Show, Read)

-----

data EmptyNoneSome a = Empty
                     | NoneOf a
                     | SomeOf a deriving (Eq, Show)

type ReconciledCoins = Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)

-----

type Amount          = Int
type Index           = Int
type NameSearchedFor = T.Text

data GetEntsCoinsRes = Mult    !Amount !NameSearchedFor !(Maybe [Ent]) !(Maybe (EmptyNoneSome Coins))
                     | Indexed !Index  !NameSearchedFor !(Either Plur Ent)
                     | Sorry           !NameSearchedFor
                     | SorryIndexedCoins deriving Show

-----

type FromId = Id
type ToId   = Id

-----

type InvCoins      = (Inv, Coins)

type BothGramNos   = (Sing, Plur)

type HelpTopic     = T.Text

type ShouldNewLine = Bool