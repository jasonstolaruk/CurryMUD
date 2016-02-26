{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.Effect ( massPauseEffects
                          , massRestartPausedEffects
                          , pauseEffects
                          , restartPausedEffects
                          , startEffect ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Random
import Mud.Threads.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (asyncThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception.Lifted (finally, handle)
import Control.Lens (views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (?~))
import Control.Monad ((>=>), forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (keys, toList)
import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Effect"


-- ==================================================


startEffect :: Id -> Effect -> MudStack ()
startEffect i e@(Effect _ (Just (RangeVal range)) _) = rndmR range >>= \x -> startEffectHelper i (e & effectVal ?~ DefiniteVal x)
startEffect i e                                      = startEffectHelper i e


startEffectHelper :: Id -> Effect -> MudStack ()
startEffectHelper i e = do
    q <- liftIO newTQueueIO
    a <- runAsync . threadEffect i e $ q
    tweak $ activeEffectsTbl.ind i <>~ pure (ActiveEffect e (a, q))


threadEffect :: Id -> Effect -> EffectQueue -> MudStack ()
threadEffect i (Effect effSub _ secs) q = handle (threadExHandler tn) . onEnv $ \md -> do
    ti <- liftIO myThreadId
    let effectTimer ior = setThreadType (EffectTimer i) >> loop secs `finally` done
          where
            loop x = liftIO (atomicWriteIORef' ior x) >> if x == 0
              then unit
              else liftIO (threadDelay $ 1 * 10 ^ 6) >> f >> loop (pred x)
              where
                f = case effSub of EffectOther fn -> runEffectFun fn i x
                                   _              -> unit
        done = tweak $ activeEffectsTbl.ind i %~ filter (views effectService ((/= ti) . asyncThreadId . fst))
        queueListener ior = setThreadType (EffectListener i) >> loop
          where
            loop = q |&| liftIO . atomically . readTQueue >=> \case
              PauseEffect  tmv -> putTMVarHelper tmv
              QueryRemTime tmv -> putTMVarHelper tmv >> loop
              StopEffect       -> unit
            putTMVarHelper tmv = liftIO (atomically . putTMVar tmv =<< readIORef ior)
    setThreadType . EffectThread $ i
    logHelper "has started."
    ior <- liftIO . newIORef $ secs
    racer md (effectTimer ior) . queueListener $ ior
    logHelper "is finishing."
  where
    tn             = "effect " <> idTxt
    idTxt          = showText i
    logHelper rest = logNotice "threadEffect" . T.concat $ [ "effect thread for ID ", idTxt, " ", rest ]


pauseEffects :: Id -> MudStack () -- When a player logs out.
pauseEffects i = getState >>= \ms ->
    let aes = getActiveEffects i ms
    in unless (null aes) $ do
        logNotice "pauseEffects" $ "pausing effects for ID " <> showText i <> "."
        pes <- mapM helper aes
        tweaks [ activeEffectsTbl.ind i .~  []
               , pausedEffectsTbl.ind i <>~ pes ]
  where
    helper (ActiveEffect e (_, q)) = do
        tmv <- liftIO newEmptyTMVarIO
        liftIO . atomically . writeTQueue q . PauseEffect $ tmv
        secs <- liftIO . atomically . takeTMVar $ tmv
        return . PausedEffect $ e & dur .~ secs


massPauseEffects :: MudStack () -- At server shutdown, after everyone has been disconnected.
massPauseEffects = do
    logNotice "massPauseEffects" "mass pausing effects."
    mapM_ pauseEffects . views activeEffectsTbl IM.keys =<< getState


restartPausedEffects :: Id -> MudStack () -- When a player logs in.
restartPausedEffects i = do
    pes <- getPausedEffects i <$> getState
    unless (null pes) . restartPausedHelper i $ pes


restartPausedHelper :: Id -> [PausedEffect] -> MudStack ()
restartPausedHelper i pes = do
    forM_ pes $ \(PausedEffect e) -> startEffect i e
    tweak $ pausedEffectsTbl.ind i .~ []


massRestartPausedEffects :: MudStack () -- At server startup.
massRestartPausedEffects = getState >>= \ms -> do
    logNotice "massRestartPausedEffects" "mass restarting paused effects."
    mapM_ (helper ms) . views pausedEffectsTbl IM.toList $ ms
  where
    helper _  (_, [] )                          = unit
    helper ms (i, _  ) | getType i ms == PCType = unit
    helper _  (i, pes)                          = restartPausedHelper i pes
