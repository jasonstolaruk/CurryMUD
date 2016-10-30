{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.SpiritTimer ( runSpiritTimerAsync
                               , throwWaitSpiritTimer ) where

-- import Mud.Data.Misc
import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
-- import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
-- import Mud.Util.Quoting
-- import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla)

import Control.Concurrent (threadDelay)
-- import Control.Exception (AsyncException(..), SomeException, fromException)
import Control.Exception.Lifted (handle)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>), forever)
import Control.Monad.IO.Class (liftIO)
-- import Data.Monoid ((<>))
import Data.Text (Text)
-- import qualified Data.Map.Lazy as M (delete, empty, insert, lookup, toList)
-- import qualified Data.Text as T


default (Int)


-----


{-
logExMsg :: Text -> Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.SpiritTimer"
-}


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.SpiritTimer"


-- ==================================================


runSpiritTimerAsync :: Id -> MudStack ()
runSpiritTimerAsync i = runAsync (threadSpiritTimer i) >>= \a -> tweak $ plaTbl.ind i.spiritAsync ?~ a


throwWaitSpiritTimer :: Id -> MudStack ()
throwWaitSpiritTimer i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.plaTbl.ind i.spiritAsync
                in (ms & plaTbl.ind i.spiritAsync .~ Nothing, a)


-----


threadSpiritTimer :: Id -> MudStack ()
threadSpiritTimer i = handle (threadExHandler (Just i) "spirit timer") $ do
    setThreadType . SpiritTimer $ i
    let loop = (liftIO . threadDelay $ 1 * 10 ^ 6) >> spiritTimer i
    handle (die (Just i) "spirit") $ logPla "threadSpiritTimer" i "digester started." >> forever loop


spiritTimer :: Id -> MudStack ()
spiritTimer _ = unit
