{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes, TupleSections #-}

module Mud.Threads.Regen ( runRegenAsync
                         , startNpcRegens
                         , stopNpcRegens
                         , throwWaitRegen ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla, logNotice)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (handle)
import Control.Lens (Getter, Lens', view)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>), forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T


default (Int)


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Threads.Regen"


-----


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Regen"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Regen"


-- ==================================================


runRegenAsync :: Id -> MudStack ()
runRegenAsync i = runAsync (threadRegen i) >>= \a -> tweak $ mobTbl.ind i.regenAsync ?~ a


startNpcRegens :: MudStack ()
startNpcRegens =
    logNotice "startNpcRegens" "starting NPC regens." >> (mapM_ runRegenAsync  . findNpcIds =<< getState)


stopNpcRegens :: MudStack ()
stopNpcRegens =
    logNotice "stopNpcRegens"  "stopping NPC regens." >> (mapM_ throwWaitRegen . findNpcIds =<< getState)


throwWaitRegen :: Id -> MudStack ()
throwWaitRegen i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.mobTbl.ind i.regenAsync
                in (ms & mobTbl.ind i.regenAsync .~ Nothing, a)


-----


threadRegen :: Id -> MudStack ()
threadRegen i = onEnv $ \md -> do
    setThreadType . RegenParent $ i
    getType i <$> getState >>= \case
      NpcType -> handle dieSilently . spawnThreadTree $ md
      PCType  -> handle (die (Just i) "regen") $ logPla "threadRegen" i "regen started." >> spawnThreadTree md
      x       -> patternMatchFail "threadRegen" [ showText x ]
  where
    spawnThreadTree md = liftIO . void . concurrentTree . map (`runReaderT` md) $ [ h, m, p, f ]
      where
        h = regen curHp maxHp calcRegenHpAmt calcRegenHpDelay
        m = regen curMp maxMp calcRegenMpAmt calcRegenMpDelay
        p = regen curPp maxPp calcRegenPpAmt calcRegenPpDelay
        f = regen curFp maxFp calcRegenFpAmt calcRegenFpDelay
    regen :: Lens' Mob Int -> Getter Mob Int -> (Id -> MudState -> Int) -> (Id -> MudState -> Int) -> MudStack ()
    regen curLens maxLens calcAmt calcDelay = (setThreadType . RegenChild $ i) >> forever loop
      where
        loop = delay >> getState >>= \ms ->
            let mob    = getMob i ms
                (c, m) = (view curLens *** view maxLens) . dup $ mob
                amt    = calcAmt i ms
                total  = c + amt
                c'     = (total > m) ? m :? total
            in when (c < m) . tweak $ mobTbl.ind i.curLens .~ c'
          where
            delay = getState >>= \ms -> liftIO . threadDelay $ calcDelay i ms * 10 ^ 6
