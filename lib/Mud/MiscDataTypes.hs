{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Copyright 2014 Jason Stolaruk and Detroit Labs LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Mud.MiscDataTypes ( Action
                         , Amount
                         , Cmd(..)
                         , CmdName
                         , Cols
                         , ConvRol
                         , EmptyNoneSome(..)
                         , GetEntsCoinsRes(..)
                         , GetOrDrop(..)
                         , IdMsgQueueCols
                         , Index
                         , InvType(..)
                         , NameSearchedFor
                         , Pretty
                         , PutOrRem(..)
                         , Rest
                         , fromRol
                         , pp
                         , RightOrLeft(..) ) where

import Mud.StateDataTypes
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.MiscDataTypes"


-- ==================================================
-- Typeclasses and instances:


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


data GetOrDrop   = Get | Drop

data PutOrRem    = Put | Rem deriving Show

data RightOrLeft = R
                 | L
                 | RI | RM | RR | RP
                 | LI | LM | LR | LP deriving (Show, Read)

data InvType     = PCInv | PCEq | RmInv deriving Eq
