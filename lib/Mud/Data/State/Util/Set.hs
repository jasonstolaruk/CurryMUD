{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Set where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (Async, asyncThreadId)
import Control.Lens (at)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Lazy ((!))


setInterp :: Id -> Maybe Interp -> MudStack ()
setInterp i mi = modifyState $ \ms -> let pt = ms^.plaTbl
                                          p  = pt ! i & interp .~ mi
                                      in (ms & plaTbl .~ (pt & at i ?~ p), ())


setLogService :: Id -> LogService -> MudStack ()
setLogService i ls = modifyState $ \ms -> let plt = ms^.plaLogTbl & at i ?~ ls in (ms & plaLogTbl .~ plt, ())


setTalkAsync :: Async () -> MudStack ()
setTalkAsync a@(asyncThreadId -> ti) = modifyState $ \ms -> let tat = ms^.talkAsyncTbl & at ti ?~ a
                                                            in (ms & talkAsyncTbl .~ tat, ())


setThreadType :: ThreadType -> MudStack ()
setThreadType threadType = liftIO myThreadId >>= \ti -> modifyState (helper ti)
  where
    helper ti ms = let tt = ms^.threadTbl & at ti ?~ threadType
                   in (ms & threadTbl .~ tt, ())
