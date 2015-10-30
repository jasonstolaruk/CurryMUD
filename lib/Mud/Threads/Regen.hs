{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Threads.Regen ( runRegenAsync
                         , threadRegen ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logPla)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Lens.Operators ((?~))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T


default (Int)


-----


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Regen"


-- ==================================================


runRegenAsync :: Id -> MudStack ()
runRegenAsync i = runAsync (threadRegen i) >>= \a -> modifyState $ (, ()) . (plaTbl.ind i.regenAsync ?~ a)


-----


threadRegen :: Id -> MudStack ()
threadRegen i = handle (threadExHandler threadName) $ do
    setThreadType . Regen $ i
    logPla "threadRegen" i "regen started."
    forever loop `catch` die "regen" (Just i)
  where
    threadName = "regen " <> (parensQuote . showText $ i)
    loop       = liftIO . threadDelay $ 5 * 10 ^ 6
