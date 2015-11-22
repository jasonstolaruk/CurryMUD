{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.MsgQueue ( Msg(..)
                               , MsgQueue
                               , NpcMsg(..)
                               , NpcMsgQueue ) where

import Control.Concurrent.STM.TQueue (TQueue)
import qualified Data.Text as T


type MsgQueue = TQueue Msg


data Msg = AsSelf     T.Text
         | Dropped
         | FromClient T.Text
         | FromServer T.Text
         | InacBoot
         | InacStop
         | MsgBoot    T.Text
         | Peeped     T.Text
         | Prompt     T.Text
         | Quit
         | Shutdown
         | SilentBoot


-----


type NpcMsgQueue = TQueue NpcMsg


data NpcMsg = ExternCmd MsgQueue Cols T.Text
            | StopNpcServer


type Cols = Int
