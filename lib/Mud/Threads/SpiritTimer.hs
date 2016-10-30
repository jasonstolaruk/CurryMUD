{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.SpiritTimer (runSpiritAsync) where

-- import Mud.Data.Misc
import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
-- import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
-- import Mud.Util.Operators
-- import Mud.Util.Quoting
-- import Mud.Util.Text
-- import qualified Mud.Misc.Logging as L (logExMsg, logPla)

-- import Control.Concurrent (threadDelay)
-- import Control.Exception (AsyncException(..), SomeException, fromException)
-- import Control.Exception.Lifted (catch, finally)
import Control.Lens.Operators ({-(%~), (.~),-} (?~))
-- import Control.Monad ((>=>), mapM_)
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Monoid ((<>))
-- import Data.Text (Text)
-- import qualified Data.Map.Lazy as M (delete, empty, insert, lookup, toList)
-- import qualified Data.Text as T


default (Int)


-----


{-
logExMsg :: Text -> Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.SpiritTimer"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.SpiritTimer"
-}


-- ==================================================


runSpiritAsync :: Id -> MudStack ()
runSpiritAsync i = runAsync (threadSpirit i) >>= \a -> tweak $ plaTbl.ind i.spiritAsync ?~ a


-----


threadSpirit :: Id -> MudStack ()
threadSpirit _ = unit
