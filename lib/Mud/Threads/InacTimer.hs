{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.InacTimer ( stopInacTimer
                             , threadInacTimer ) where

import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (closeTMQueue, tryReadTMQueue)
import Control.Exception.Lifted (catch, finally)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Data.Text as T
import System.Time.Utils (renderSecs)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.InacTimer"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.InacTimer"


-- ==================================================


threadInacTimer :: HasCallStack => Id -> MsgQueue -> InacTimerQueue -> MudStack ()
threadInacTimer i mq q = let f = sequence_ [ setThreadType . InacTimer $ i
                                           , loop maxInacSecs 0 `catch` threadExHandler (Just i) "inactivity timer" ]
                         in f `finally` stopInacTimer q
  where
    loop timerDur secs = do
        liftIO . delaySecs $ 1
        q |&| liftIO . atomically . tryReadTMQueue >=> \case
          Just Nothing | secs >= timerDur         -> inacBoot secs
                       | otherwise                -> loop timerDur . succ $ secs
          Just (Just ResetInacTimer             ) -> loop timerDur 0
          Just (Just (SetInacTimerDur timerDur')) -> let msg = prd $ "setting timer duration to " <> showSecs timerDur'
                                                     in do logPla "threadInacTimer loop" i msg
                                                           loop timerDur' . succ $ secs
          Nothing                                 -> unit
    inacBoot (parensQuote . showSecs -> secs) = descSingId i <$> getState >>= \t -> do
        logPla    "threadInacTimer inacBoot" i . prd $ "booting due to inactivity " <> secs
        logNotice "threadInacTimer inacBoot" $ "booting " <> t <> " due to inactivity."
        writeMsg mq InacBoot
    showSecs = T.pack . renderSecs . fromIntegral


-----


stopInacTimer :: HasCallStack => InacTimerQueue -> MudStack ()
stopInacTimer = liftIO . atomically . closeTMQueue
