{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.InacTimer (threadInacTimer) where

import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (tryReadTMQueue)
import Control.Exception.Lifted (catch, finally)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Time.Utils (renderSecs)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.InacTimer"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.InacTimer"


-- ==================================================


threadInacTimer :: Id -> MsgQueue -> TimerQueue -> MudStack ()
threadInacTimer i mq tq = let f = sequence_ [ setThreadType . InacTimer $ i
                                            , loop maxInacSecs 0 `catch` threadExHandler (Just i) "inactivity timer" ]
                          in f `finally` stopTimer tq
  where
    loop maxSecs secs = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        tq |&| liftIO . atomically . tryReadTMQueue >=> \case
          Just Nothing | secs >= maxSecs    -> inacBoot secs
                       | otherwise          -> loop maxSecs . succ $ secs
          Just (Just ResetTimer           ) -> loop maxSecs 0
          Just (Just (SetMaxSecs maxSecs')) -> let msg = prd $ "setting max inactivity to " <> showSecs maxSecs'
                                               in do logPla "threadInacTimer loop" i msg
                                                     loop maxSecs' . succ $ secs
          Nothing                           -> unit
    inacBoot (parensQuote . showSecs -> secs) = getSing i <$> getState >>= \s -> do
        logPla "threadInacTimer inacBoot" i . prd $ "booting due to inactivity " <> secs
        let noticeMsg = T.concat [ "booting player ", showText i, " ", parensQuote s, " due to inactivity." ]
        logNotice "threadInacTimer inacBoot" noticeMsg
        writeMsg mq InacBoot
    showSecs = T.pack . renderSecs . fromIntegral
