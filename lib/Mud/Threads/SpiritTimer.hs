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
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla, logPlaOut)

import Control.Exception.Lifted (finally, handle)
import Control.Lens (views)
import Control.Lens.Operators ((?~), (.~))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.SpiritTimer"


logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Threads.SpiritTimer"


-- ==================================================


runSpiritTimerAsync :: HasCallStack => Id -> Seconds -> MudStack ()
runSpiritTimerAsync i secs = runAsync (threadSpiritTimer i secs) >>= \a -> tweak $ plaTbl.ind i.spiritAsync ?~ a


-- Note that "threadSpiritTimer" sets "spiritAsync" to "Nothing" when the timer finishes.
throwWaitSpiritTimer :: HasCallStack => Id -> MudStack ()
throwWaitSpiritTimer i = views (plaTbl.ind i.spiritAsync) (maybeVoid throwWait) =<< getState


-----


threadSpiritTimer :: HasCallStack => Id -> Seconds -> MudStack ()
threadSpiritTimer i secs = handle (threadExHandler (Just i) "spirit timer") $ do
    setThreadType . SpiritTimer $ i
    singId <- descSingId i <$> getState
    logPla "threadSpiritTimer" i . prd $ "spirit timer starting " <> parensQuote (commaShow secs <> " seconds")
    (mq, cols) <- getMsgQueueColumns i <$> getState
    let go     = when (secs > 0) $ do liftIO . delaySecs $ 2
                                      wrapSend mq cols . colorWith spiritMsgColor $ spiritDetachMsg
                                      spiritTimer i mq cols secs
        finish = do logPla "threadSpiritTimer finish" i "spirit timer finishing."
                    tweak $ plaTbl.ind i.spiritAsync .~ Nothing
                    writeMsg mq FinishedSpirit
    handle (die (Just i) $ "spirit timer for " <> singId) $ go `finally` finish


spiritTimer :: HasCallStack => Id -> MsgQueue -> Cols -> Seconds -> MudStack ()
spiritTimer i _  _    0 = logPla "spiritTimer" i "spirit timer expired."
spiritTimer i mq cols secs
  | secs == 75 = helper "You feel the uncanny pull of the beyond. Your time in this dimension is coming to an end."
  | secs == 45 = helper . thrice prd $ "You feel the uncanny pull of the beyond. You have VERY little time left in \
                                       \this dimension"
  | secs == 15 = helper . thrice prd $ "You are fading away"
  | otherwise  = next
  where
    helper msg = do logPlaOut "spiritTimer" i . pure $ msg
                    wrapSend mq cols . colorWith spiritMsgColor $ msg
                    next
    next       = sequence_ [ liftIO . delaySecs $ 1, spiritTimer i mq cols . pred $ secs ]
