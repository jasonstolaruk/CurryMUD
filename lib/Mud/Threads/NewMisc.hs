{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Mud.Threads.NewMisc ( die
                           , plaThreadExHandler
                           , PlsDie(..)
                           , threadExHandler
                           , TimerMsg(..)
                           , TimerQueue ) where

import Mud.Cmds.Util.Misc
import Mud.Data.State.MudData
import Mud.Misc.Logging hiding (logExMsg, logNotice)
import qualified Mud.Misc.Logging as L (logExMsg, logNotice)

import Control.Concurrent.STM.TMQueue (TMQueue)
import Control.Exception (AsyncException(..), Exception, SomeException, fromException)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import qualified Data.Text as T


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.Misc"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Misc"


-- ==================================================


-- TODO: Reorder definitions.


data PlsDie = PlsDie deriving (Show, Typeable)


instance Exception PlsDie


type TimerQueue = TMQueue TimerMsg


data TimerMsg = ResetTimer


die :: T.Text -> PlsDie -> MudStack ()
die threadName = const . logNotice "die" $ "the " <> threadName <> " thread is dying."


plaThreadExHandler :: T.Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler threadName i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = threadExHandler threadName e


threadExHandler :: T.Text -> SomeException -> MudStack ()
threadExHandler threadName e = logExMsg "threadExHandler" ("on " <> threadName <> " thread") e >> throwToListenThread e
