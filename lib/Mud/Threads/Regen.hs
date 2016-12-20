{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes, TupleSections #-}

module Mud.Threads.Regen ( runRegenAsync
                         , startNpcRegens
                         , stopNpcRegens
                         , stopRegen ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Lens (Getter, Lens')
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>), forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Regen"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Regen"


-- ==================================================


runRegenAsync :: Id -> MudStack ()
runRegenAsync i = liftIO newTQueueIO >>= \tq -> do
    tweak $ mobTbl.ind i.regenQueue ?~ tq
    onNewThread . threadRegen i $ tq


startNpcRegens :: MudStack ()
startNpcRegens =
    sequence_ [ logNotice "startNpcRegens" "starting NPC regens.", mapM_ runRegenAsync . findNpcIds =<< getState ]


stopNpcRegens :: MudStack ()
stopNpcRegens =
    sequence_ [ logNotice "stopNpcRegens"  "stopping NPC regens.", mapM_ stopRegen     . findNpcIds =<< getState ]


stopRegen :: Id -> MudStack ()
stopRegen i = do logPla "stopRegen" i "stopping regen."
                 helper |&| modifyState >=> maybeVoid (liftIO . atomically . (`writeTQueue` StopRegen))
  where
    helper ms = let tq = ms^.mobTbl.ind i.regenQueue
                in (ms & mobTbl.ind i.regenQueue .~ Nothing, tq)


-----


threadRegen :: Id -> RegenQueue -> MudStack ()
threadRegen i tq = let regens = [ regen curHp maxHp calcRegenHpAmt calcRegenHpDelay
                                , regen curMp maxMp calcRegenMpAmt calcRegenMpDelay
                                , regen curPp maxPp calcRegenPpAmt calcRegenPpDelay
                                , regen curFp maxFp calcRegenFpAmt calcRegenFpDelay ]
                   in do setThreadType . RegenParent $ i
                         logPla "threadRegen" i "regen started."
                         asyncs <- mapM runAsync regens
                         liftIO $ (void . atomically . readTQueue $ tq) >> mapM_ cancel asyncs
  where
    regen :: Lens' Mob Int -> Getter Mob Int -> (Id -> MudState -> Int) -> (Id -> MudState -> Int) -> MudStack ()
    regen curLens maxLens calcAmt calcDelay = setThreadType (RegenChild i) >> forever loop
      where
        loop  = delay >> modifyStateSeq f
        delay = getState >>= \ms -> liftIO . threadDelay $ calcDelay i ms * 10 ^ 6
        f ms  = let (mob, amt) = (getMob  `fanUncurry` calcAmt) (i, ms)
                    (c, m)     = (curLens `fanView`    maxLens) mob
                    total      = c + amt
                    c'         = (total > m) ? m :? total
                    g          = mobTbl.ind i.curLens .~ c'
                    res        = (onTrue (c < m) g ms, [])
                in if isPC i ms
                  then isLoggedIn (getPla i ms) ? res :? (ms, pure . stopRegen $ i)
                  else res
