module Mud.Data.State.MsgQueue ( MsgQueue
                               , NpcMsgQueue
                               , NpcThreadMsg(..)
                               , ThreadMsg(..) ) where

import Mud.TopLvlDefs.Seconds

import Control.Concurrent.STM.TQueue (TQueue)
import Data.Text (Text)

type MsgQueue = TQueue ThreadMsg

data ThreadMsg = AsSelf Text
               | BlankLine
               | Dropped
               | FinishedEgress
               | FinishedSpirit
               | FromClient Text
               | FromServer Text
               | InacBoot
               | InacSecs Seconds
               | InacStop
               | MsgBoot Text
               | Peeped  Text
               | Prompt  Text
               | Quit
               | ShowHandle
               | ShutDown
               | SilentBoot
               | ToNpc Text

-----

type NpcMsgQueue = TQueue NpcThreadMsg

data NpcThreadMsg = ExternCmd MsgQueue Cols Text
                  | StopNpcServer

type Cols = Int
