{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.FeelingTimer ( startFeeling
                                , stopFeelings
                                , threadFeelingTimer ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logExMsg, logPla)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, poll)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (newTMQueueIO, tryReadTMQueue, writeTMQueue)
import Control.Exception (AsyncException(..), SomeException, fromException)
import Control.Exception.Lifted (catch, finally)
import Control.Lens.Operators ((%~))
import Control.Monad ((>=>), forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Lazy as M (delete, insert, lookup, toList)
import qualified Data.Text as T


default (Int)


-----


logExMsg :: Text -> Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.FeelingTimer"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.FeelingTimer"


-- ==================================================


startFeeling :: Id -> Maybe EffectFeeling -> FeelingVal -> MudStack ()
startFeeling _ Nothing                           _    = unit
startFeeling i (Just (EffectFeeling tag newDur)) newV = getState >>= \ms ->
    let fm = getFeelingMap i ms
    in case M.lookup tag fm of
      Nothing -> do { feel <- uncurry (Feeling newV newDur) <$> spawn
                    ; helper feel
                    ; logHelper . T.concat $ [ "started new feeling with tag ", dblQuote tag, ": ", pp feel, "." ] }
      Just (Feeling _ existDur existQ existA)
        | newDur > existDur -> do { liftIO . cancel $ existA
                                  ; feel <- uncurry (Feeling newV newDur) <$> spawn
                                  ; helper feel
                                  ; logHelper . T.concat $ [ "feeling "
                                                           , dblQuote tag
                                                           , " has been restarted with a longer duration: "
                                                           , pp feel
                                                           , "." ] }
        | otherwise         -> liftIO (poll existA) >>= \case
          Nothing -> do { liftIO . atomically . writeTMQueue existQ $ ResetTimer
                        ; let feel = Feeling newV existDur existQ existA
                        ; helper     feel
                        ; logRestart feel }
          _       -> do { feel <- uncurry (Feeling newV existDur) <$> spawn
                        ; helper     feel
                        ; logRestart feel }
  where
    spawn = do
        newQ <- liftIO newTMQueueIO
        newA <- runAsync . threadFeelingTimer i tag newDur $ newQ
        return (newQ, newA)
    helper feel     = tweak $ mobTbl.ind i.feelingMap %~ M.insert tag feel
    logHelper       = logPla "startFeeling" i
    logRestart feel = logHelper . T.concat $ [ "feeling "
                                             , dblQuote tag
                                             , " has been restarted: "
                                             , pp feel
                                             , "." ]


threadFeelingTimer :: Id -> FeelingTag -> Seconds -> TimerQueue -> MudStack ()
threadFeelingTimer i tag dur tq =
    sequence_ [ setThreadType . FeelingTimer $ i
              , loop 0 `catch` exHandler ] `finally` stopTimer tq
  where
    loop secs = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        tq |&| liftIO . atomically . tryReadTMQueue >=> \case
          Just Nothing | secs >= dur -> do { tweak $ mobTbl.ind i.feelingMap %~ (tag `M.delete`)
                                           ; logHelper $ mkName <> " has expired." }
                       | otherwise   -> loop . succ $ secs
          Just (Just ResetTimer)     -> do { logHelper $ mkName <> " is resetting."
                                           ; loop 0 }
          Nothing                    -> unit
    exHandler :: SomeException -> MudStack ()
    exHandler e = case fromException e of
      Just ThreadKilled  -> logHelper $ "killed " <> mkName <> "."
      _                  -> logExMsg  "threadFeelingTimer" ("exception caught on thread for " <> mkName) e
    mkName    = T.concat [ "feeling timer ", showText i, " ", dblQuote tag ]
    logHelper = logPla "threadFeelingTimer" i


-----


stopFeelings :: Id -> MudStack ()
stopFeelings i = getFeelingMap i <$> getState >>= \fm -> forM_ (M.toList fm) $ stopTimer . feelingTimerQueue . snd
