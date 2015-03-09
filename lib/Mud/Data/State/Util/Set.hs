module Mud.Data.State.Util.Set where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Data.IntMap.Lazy ((!))


setInterp :: Id -> Maybe Interp -> MudStack ()
setInterp i mi = modifyState $ \ms -> let pt = ms^.plaTbl
                                          p  = pt ! i & interp .~ mi
                                      in (ms & plaTbl .~ (pt & at i ?~ p), ())


setLogService :: Id -> LogService -> MudStack ()
setLogService i ls = modifyState $ \ms -> let plt = ms^.plaLogTbl & at i ?~ ls in (ms & plaLogTbl .~ plt, ())
