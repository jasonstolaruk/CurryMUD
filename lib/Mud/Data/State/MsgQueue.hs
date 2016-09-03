{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.MsgQueue ( Msg(..)
                               , MsgQueue
                               , NpcMsg(..)
                               , NpcMsgQueue ) where

import Control.Concurrent.STM.TQueue (TQueue)
import Data.Text (Text)


type MsgQueue = TQueue Msg


data Msg = AsSelf     Text
         | BlankLine
         | Dropped
         | FromClient Text
         | FromServer Text
         | InacBoot
         | InacStop
         | MsgBoot    Text
         | Peeped     Text
         | Prompt     Text
         | Quit
         | ShowHandle
         | Shutdown
         | SilentBoot
         | ToNpc      Text


-----


type NpcMsgQueue = TQueue NpcMsg


data NpcMsg = ExternCmd MsgQueue Cols Text
            | StopNpcServer


type Cols = Int
