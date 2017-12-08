{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Effect ( massPauseEffects
                          , massRestartPausedEffects
                          , pauseEffects
                          , restartPausedEffects
                          , startEffect
                          , stopEffect
                          , stopEffects ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Hierarchy
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Random
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Threads.FeelingTimer
import           Mud.Threads.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent (myThreadId)
import           Control.Concurrent.Async (asyncThreadId, cancel, wait)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, takeTMVar)
import           Control.Concurrent.STM.TMQueue (closeTMQueue, isClosedTMQueue, newTMQueueIO, readTMQueue, writeTMQueue)
import           Control.Exception.Lifted (finally, handle)
import           Control.Lens (_1, view, views)
import           Control.Lens.Operators ((?~), (.~), (&), (%~), (^.), (<>~))
import           Control.Monad ((>=>), forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (newIORef, readIORef)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import qualified Data.IntMap.Strict as IM (keys, toList)
import qualified Data.Map.Strict as M (delete, lookup)
import qualified Data.Text as T

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Effect"

logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Effect"

-- ==================================================

startEffect :: HasCallStack => Id -> Effect -> MudStack ()
startEffect i e@(Effect effTag _ (Just (EffectRangedVal range)) _ _) =
  handle (threadStarterExHandler i "startEffect" effTag) $ rndmR range >>= \x ->
      startEffectHelper i $ e & effectVal ?~ EffectFixedVal x
startEffect i e = startEffectHelper i e

startEffectHelper :: HasCallStack => Id -> Effect -> MudStack ()
startEffectHelper i e = getDurEffects i <$> getState >>= \durEffs -> do
    flip maybeVoid (e^.effectTag) $ \tag -> case filter (views (effect.effectTag) (== Just tag)) durEffs of
      [] -> unit
      xs -> do logHelper . prd $ "stopping existing effect with tag " <> dblQuote tag
               forM_ xs ((>>) <$> stopEffect i <*> views (effectService._1) (liftIO . wait))
    logHelper . prd $ "starting effect: " <> pp e
    q <- liftIO newTMQueueIO
    a <- runAsync . threadEffect i e $ q
    views effectFeeling (maybeVoid (flip (startFeeling i) FeelingNoVal)) e
    tweak $ durationalEffectTbl.ind i <>~ pure (DurationalEffect e (a, q))
  where
    logHelper = logPla "startEffectHelper" i

-----

threadEffect :: HasCallStack => Id -> Effect -> EffectQueue -> MudStack ()
threadEffect i (Effect _ effSub _ secs _) q = handle (threadExHandler (Just i) "effect") $ do
    setThreadType . EffectThread $ i
    logHelper "has started."
    (ti, ior) <- liftIO ((,) <$> myThreadId <*> newIORef secs)
    let effectTimer = setThreadType (EffectTimer i) >> loop secs
          where
            loop x = liftIO (atomicWriteIORef' ior x) >> if isZero x
                       then liftIO . atomically . writeTMQueue q $ StopEffect
                       else let f = case effSub of EffectOther fn -> runEffectFun fn i x
                                                   _              -> unit
                            in sequence_ [ liftIO . delaySecs $ 1, f, loop . pred $ x ]
        queueListener timerAsync = setThreadType (EffectListener i) >> loop `finally` done
          where
            loop = q |&| liftIO . atomically . readTMQueue >=> \case
              Just (PauseEffect tmv) -> liftIO $ atomically . putTMVar tmv =<< readIORef ior
              Just StopEffect        -> unit
              Nothing                -> unit
            done = do tweak $ durationalEffectTbl.ind i %~ filter (views effectService ((/= ti) . asyncThreadId . fst))
                      liftIO $ atomically (closeTMQueue q) >> cancel timerAsync
    liftIO . wait =<< runAsync . queueListener =<< runAsync effectTimer
    logHelper "is finishing."
  where
    logHelper rest = logNotice "threadEffect" . T.concat $ [ "effect thread for ID ", showTxt i, " ", rest ]

-----

pauseEffects :: HasCallStack => Id -> MudStack () -- When a player logs out.
pauseEffects i = getDurEffects i <$> getState >>= \es ->
    unless (null es) $ do logNotice "pauseEffects" . prd $ "pausing effects for ID " <> showTxt i
                          pes <- catMaybes <$> mapM helper es
                          tweaks [ durationalEffectTbl.ind i .~  []
                                 , pausedEffectTbl    .ind i <>~ pes ]
  where
    helper (DurationalEffect e (_, q)) = liftIO newEmptyTMVarIO >>= \tmv ->
        mIf (liftIO . atomically . writeTMQueueHelper q $ tmv)
            (do secs <- liftIO . atomically . takeTMVar $ tmv
                return . Just . PausedEffect $ e & effectDur     .~ secs
                                                 & effectFeeling %~ fmap (\effFeel -> effFeel { efDur = secs }))
            (return Nothing)
    writeTMQueueHelper q tmv = mIf (isClosedTMQueue q)
                                   (return False)
                                   (writeTMQueue q (PauseEffect tmv) >> return True)

massPauseEffects :: HasCallStack => MudStack () -- At server shutdown, after everyone has been disconnected.
massPauseEffects = sequence_ [ logNotice "massPauseEffects" "mass pausing effects."
                             , mapM_ pauseEffects . views durationalEffectTbl IM.keys =<< getState ]

-----

restartPausedEffects :: HasCallStack => Id -> MudStack () -- When a player logs in.
restartPausedEffects i = do pes <- getPausedEffects i <$> getState
                            unless (null pes) . restartPausedHelper i $ pes

restartPausedHelper :: HasCallStack => Id -> [PausedEffect] -> MudStack ()
restartPausedHelper i pes = sequence_ [ forM_ pes $ \(PausedEffect e) -> startEffect i e
                                      , tweak $ pausedEffectTbl.ind i .~ [] ]

massRestartPausedEffects :: HasCallStack => MudStack () -- At server startup.
massRestartPausedEffects = getState >>= \ms -> do logNotice "massRestartPausedEffects" "mass restarting paused effects."
                                                  views pausedEffectTbl (mapM_ (helper ms) . IM.toList) ms
  where
    helper _  (_, [] )                           = unit
    helper ms (i, _  ) | getType i ms == PlaType = unit
    helper _  (i, pes)                           = restartPausedHelper i pes

-----

stopEffect :: HasCallStack => Id -> DurationalEffect -> MudStack ()
stopEffect i (DurationalEffect e (_, q)) = sequence_ [ liftIO . atomically . writeTMQueue q $ StopEffect, stopFeeling i e ]

stopFeeling :: HasCallStack => Id -> Effect -> MudStack ()
stopFeeling i (view effectFeeling -> feel) = getState >>= \ms ->
    let f = flip maybeVoid feel $ \(EffectFeeling tag _) ->
                flip maybeVoid (M.lookup tag . getFeelingMap i $ ms) $ \(Feeling _ _ a) ->
                    sequence_ [ liftIO . cancel $ a, tweak $ mobTbl.ind i.feelingMap %~ (tag `M.delete`) ]
    in when (hasMobId i ms) f

stopEffects :: HasCallStack => Id -> MudStack ()
stopEffects i = mapM_ (stopEffect i) =<< getDurEffects i <$> getState
