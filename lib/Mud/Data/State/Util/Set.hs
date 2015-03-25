{-# LANGUAGE TupleSections, ViewPatterns #-}

module Mud.Data.State.Util.Set where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Util.Misc

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (Async, asyncThreadId)
import Control.Lens.Operators ((.~))
import Control.Monad.IO.Class (liftIO)


setInterp :: Id -> Maybe Interp -> MudStack ()
setInterp i mi = modifyState $ (, ()) . (plaTbl.ind i.interp .~ mi)


setLogService :: Id -> LogService -> MudStack ()
setLogService i ls = modifyState $ (, ()) . (plaLogTbl.ind i .~ ls)


setTalkAsync :: Async () -> MudStack ()
setTalkAsync a@(asyncThreadId -> ti) = modifyState $ (, ()) . (talkAsyncTbl.ind ti .~ a)


setThreadType :: ThreadType -> MudStack ()
setThreadType threadType = liftIO myThreadId >>= \ti -> modifyState (helper ti)
  where
    helper ti = (, ()) . (threadTbl.ind ti .~ threadType)
