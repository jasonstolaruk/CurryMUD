{-# LANGUAGE DeriveDataTypeable, LambdaCase, OverloadedStrings, TupleSections #-}

module Mud.Threads.Misc ( die
                        , plaThreadExHandler
                        , PlsDie(..)
                        , runAsync
                        , setThreadType
                        , stopTimerThread
                        , threadExHandler
                        , throwWait
                        , throwWaitRegen
                        , TimerMsg(..)
                        , TimerQueue ) where

import Mud.Cmds.Util.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Misc.Logging hiding (logExMsg, logNotice, logPla)
import Mud.Util.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logExMsg, logNotice, logPla)

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (Async, async, asyncThreadId, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue)
import Control.Exception (AsyncException(..), Exception, SomeException, fromException)
import Control.Exception.Lifted (throwTo)
import Control.Lens (at)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>), void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import qualified Data.Text as T


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.Misc"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Misc"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Misc"


-- ==================================================


data PlsDie = PlsDie deriving (Show, Typeable)


instance Exception PlsDie


-----


type TimerQueue = TMQueue TimerMsg


data TimerMsg = ResetTimer


-----


die :: T.Text -> Maybe Id -> PlsDie -> MudStack ()
die threadName = \case Nothing -> const . logNotice "die"   $ msg
                       Just i  -> const . logPla    "die" i $ msg
  where
    msg = "the " <> threadName <> " thread is dying."


-----


plaThreadExHandler :: T.Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler threadName i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = threadExHandler threadName e


threadExHandler :: T.Text -> SomeException -> MudStack ()
threadExHandler threadName e = logExMsg "threadExHandler" ("on " <> threadName <> " thread") e >> throwToListenThread e


-----


runAsync :: MudStack () -> MudStack (Async ())
runAsync f = onEnv $ liftIO . async . runReaderT f


-----


setThreadType :: ThreadType -> MudStack ()
setThreadType threadType = liftIO myThreadId >>= \ti -> modifyState (helper ti)
  where
    helper ti = (, ()) . (threadTbl.at ti ?~ threadType)


-----


stopTimerThread :: TimerQueue -> MudStack ()
stopTimerThread = liftIO . atomically . closeTMQueue


-----


throwWait :: Async () -> MudStack ()
throwWait a = throwTo (asyncThreadId a) PlsDie >> (liftIO . void . wait $ a)


throwWaitRegen :: Id -> MudStack ()
throwWaitRegen i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.plaTbl.ind i.regenAsync
                in (ms & plaTbl.ind i.regenAsync .~ Nothing, a)
