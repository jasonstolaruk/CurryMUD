{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Mud.Threads.Regen ( runRegenAsync
                         , startNpcRegens
                         , stopNpcRegens
                         , stopRegen ) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Threads.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators

import           Control.Concurrent.Async (cancel)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import           Control.Exception.Lifted (catch)
import           Control.Lens (Getter, Lens')
import           Control.Lens.Operators ((?~), (.~), (&), (^.))
import           Control.Monad ((>=>), forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Regen"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Regen"


-- ==================================================


runRegenAsync :: HasCallStack => Id -> MudStack ()
runRegenAsync i = liftIO newTQueueIO >>= \tq -> do tweak $ mobTbl.ind i.regenQueue ?~ tq
                                                   onNewThread . threadRegen i $ tq


startNpcRegens :: HasCallStack => MudStack ()
startNpcRegens =
    sequence_ [ logNotice "startNpcRegens" "starting NPC regens.", mapM_ runRegenAsync . findNpcIds =<< getState ]


stopNpcRegens :: HasCallStack => MudStack ()
stopNpcRegens =
    sequence_ [ logNotice "stopNpcRegens"  "stopping NPC regens.", mapM_ stopRegen     . findNpcIds =<< getState ]


stopRegen :: HasCallStack => Id -> MudStack ()
stopRegen i = do logPla "stopRegen" i "stopping regen."
                 helper |&| modifyState >=> maybeVoid (liftIO . atomically . (`writeTQueue` StopRegen))
  where
    helper ms = let tq = ms^.mobTbl.ind i.regenQueue
                in (ms & mobTbl.ind i.regenQueue .~ Nothing, tq)


-----


threadRegen :: HasCallStack => Id -> RegenQueue -> MudStack ()
threadRegen i tq = helper `catch` threadStarterExHandler i fn Nothing
  where
    helper = let regens = [ regen curHp maxHp calcRegenHpAmt calcRegenHpDelay
                          , regen curMp maxMp calcRegenMpAmt calcRegenMpDelay
                          , regen curPp maxPp calcRegenPpAmt calcRegenPpDelay
                          , regen curFp maxFp calcRegenFpAmt calcRegenFpDelay ]
             in do setThreadType . RegenParent $ i
                   logPla fn i "regen started."
                   asyncs <- mapM runAsync regens
                   liftIO $ (void . atomically . readTQueue $ tq) >> mapM_ cancel asyncs
    regen :: HasCallStack => Lens' Mob Int
                          -> Getter Mob Int
                          -> (Id -> MudState -> Int)
                          -> (Id -> MudState -> Int)
                          -> MudStack ()
    regen curLens maxLens calcAmt calcDelay = setThreadType (RegenChild i) >> forever loop
      where
        loop  = delay >> modifyStateSeq f
        delay = liftIO . delaySecs . calcDelay i =<< getState
        f ms  = let (mob, amt) = (getMob  `fanUncurry` calcAmt) (i, ms)
                    (c, m)     = (curLens `fanView`    maxLens) mob
                    total      = c + amt
                    c'         = (total > m) ? m :? total
                    g          = mobTbl.ind i.curLens .~ c'
                    res        = (onTrue (c < m) g ms, [])
                in if isPla i ms
                  then isLoggedIn (getPla i ms) ? res :? (ms, pure . stopRegen $ i)
                  else res
    fn = "threadRegen"
