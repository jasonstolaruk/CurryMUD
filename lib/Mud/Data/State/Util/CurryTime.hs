{-# OPTIONS_GHC -fno-warn-type-defaults -Wno-redundant-constraints #-} -- TODO: Needed?
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-} -- TODO: Needed?

module Mud.Data.State.Util.CurryTime where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Arrow ((***), first)
import Control.Lens (_1, _2, _3, at, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (^.))
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete, sortBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Data.State.Util.CurryTime"


-- ==================================================
