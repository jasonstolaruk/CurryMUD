{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.CorpseDecomposer (startCorpseDecomp) where
-- TODO: pauseCorpseDecomps
-- TODO: restartCorpseDecomps

import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

-- import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (finally, handle)
import Control.Lens (at)
import Control.Lens.Operators ((.~))
-- import Control.Monad (forM_, unless, when)
-- import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
-- import qualified Data.IntMap.Lazy as IM (keys, toList)
-- import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.CorpseDecomposer"


-- ==================================================


startCorpseDecomp :: Id -> Seconds -> MudStack ()
startCorpseDecomp i secs = runAsync (threadCorpseDecomp i secs) >>= \a -> tweak $ corpseDecompAsyncTbl.ind i .~ a


threadCorpseDecomp :: Id -> Seconds -> MudStack ()
threadCorpseDecomp i secs = handle (threadExHandler (Just i) "corpse decomposer") $ getState >>= \ms -> do
    setThreadType . CorpseDecomposer $ i
    let msg = prd $ "starting corpse decomposer for " <> descSingId i ms |<>| parensQuote (showText secs <> " seconds")
    logNotice "startCorpseDecomp" msg
    let finish = do { logNotice "threadCorpseDecomp" $ "corpse decomposer for " <> descSingId i ms <> " is finishing."
                    ; tweak $ corpseDecompAsyncTbl.at i .~ Nothing }
    handle (die (Just i) "corpse decomposer") $ corpseDecomp i (dup secs) `finally` finish


corpseDecomp :: Id -> (Seconds, Seconds) -> MudStack ()
corpseDecomp _ _ = unit
