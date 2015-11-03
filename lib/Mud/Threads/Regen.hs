{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}

module Mud.Threads.Regen ( runRegenAsync
                         , threadRegen ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (handle)
import Control.Lens (view)
import Control.Lens.Operators ((.~), (?~))
import Control.Monad (forever, void, when)
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


-- ==================================================


runRegenAsync :: Id -> MudStack ()
runRegenAsync i = runAsync (threadRegen i) >>= \a -> modifyState $ (, ()) . (mobTbl.ind i.regenAsync ?~ a)


-----


-- TODO: Start regen threads for NPC mobs at world creation.
threadRegen :: Id -> MudStack ()
threadRegen i = onEnv $ \md -> do
    setThreadType . RegenParent $ i
    getType i <$> getState >>= \case
      MobType -> handle dieSilently . spawnThreadTree $ md
      PCType  -> handle (die (Just i) "regen") $ logPla "threadRegen" i "regen started." >> spawnThreadTree md
      x       -> patternMatchFail "threadRegen" [ showText x ]
  where
    spawnThreadTree md = liftIO . void . concurrentTree . map (`runReaderT` md) $ [ h, m, p, f ]
      where
        h = regen curHp curHp maxHp calcRegenHpAmt calcRegenHpDelay
        m = regen curMp curMp maxMp calcRegenMpAmt calcRegenMpDelay
        p = regen curPp curPp maxPp calcRegenPpAmt calcRegenPpDelay
        f = regen curFp curFp maxFp calcRegenFpAmt calcRegenFpDelay
    regen getCur setCur getMax calcAmt calcDelay = (setThreadType . RegenChild $ i) >> forever loop
      where
        loop = delay >> getState >>= \ms ->
            let mob    = getMob i ms
                (c, m) = (view getCur *** view getMax) . dup $ mob
                amt    = calcAmt i ms
                total  = c + amt
                c'     = (total > m) ? m :? total
            in when (c < m) . modifyState $ (, ()) . (mobTbl.ind i.setCur .~ c')
          where
            delay = getState >>= \ms -> liftIO . threadDelay $ calcDelay i ms * 10 ^ 6
