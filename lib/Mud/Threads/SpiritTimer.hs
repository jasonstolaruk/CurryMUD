{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.SpiritTimer ( runSpiritTimerAsync
                               , throwWaitSpiritTimer ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla, logPlaOut)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (finally, handle)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)


default (Int)


-----


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.SpiritTimer"


logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Threads.SpiritTimer"


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
    logPla "threadSpiritTimer" i . prd $ "spirit timer started " <> parensQuote (showText secs <> " seconds")
    pair@(mq, _) <- getMsgQueueColumns i <$> getState
    handle (die (Just i) "spirit timer") $ uncurry go pair `finally` writeMsg mq TheBeyond
  where
    go mq cols = when (secs > 0) $ do
        liftIO . threadDelay $ 2 * 10 ^ 6
        wrapSend mq cols . colorWith spiritMsgColor $ spiritDetachMsg
        spiritTimer i mq cols secs


spiritTimer :: Id -> MsgQueue -> Cols -> Seconds -> MudStack ()
spiritTimer i _  _    0 = logPla "spiritTimer" i "spirit timer expired."
spiritTimer i mq cols secs
  | secs == 75 = helper "You feel the uncanny pull of the beyond. Your time in this dimension is coming to an end."
  | secs == 45 = helper . thrice prd $ "You feel the uncanny pull of the beyond. You have VERY little time left in \
                                       \this dimension"
  | secs == 15 = helper . thrice prd $ "You are fading away"
  | otherwise  = next
  where
    helper msg = do
        wrapSend mq cols . colorWith spiritMsgColor $ msg
        logPlaOut "spiritTimer" i . pure $ msg
        next
    next = (liftIO . threadDelay $ 1 * 10 ^ 6) >> spiritTimer i mq cols (pred secs)
