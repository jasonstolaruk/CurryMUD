module Mud.Data.State.MsgQueue where

import Control.Concurrent.STM.TQueue (TQueue)
import qualified Data.Text as T


type MsgQueue = TQueue Msg


data Msg = Dropped
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
