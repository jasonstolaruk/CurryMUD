{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.SpiritTimer ( runSpiritTimerAsync
                               , throwWaitSpiritTimer ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logPla)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (handle)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)


default (Int)


-----


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.SpiritTimer"


-- ==================================================


runSpiritTimerAsync :: Id -> Seconds -> MudStack ()
runSpiritTimerAsync i secs = runAsync (threadSpiritTimer i secs) >>= \a -> tweak $ plaTbl.ind i.spiritAsync ?~ a


throwWaitSpiritTimer :: Id -> MudStack ()
throwWaitSpiritTimer i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.plaTbl.ind i.spiritAsync
                in (ms & plaTbl.ind i.spiritAsync .~ Nothing, a)


-----


threadSpiritTimer :: Id -> Seconds -> MudStack ()
threadSpiritTimer i secs = handle (threadExHandler (Just i) "spirit timer") $ do
    setThreadType . SpiritTimer $ i
    logPla "threadSpiritTimer" i "spirit timer started."
    handle (die (Just i) "spirit timer") . spiritTimer i $ secs


spiritTimer :: Id -> Seconds -> MudStack ()
spiritTimer i 0    = logPla "spiritTimer" i "spirit timer expired."
spiritTimer i secs | False = unit
                   | otherwise = next
  where
    next = (liftIO . threadDelay $ 1 * 10 ^ 6) >> spiritTimer i (pred secs)
