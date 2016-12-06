{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.RmFuns ( startRmFuns
                          , stopRmFuns ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Arrow (second)
import Control.Lens (view, views)
import Control.Lens.Operators ((.~), (<>~), (^.))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (filter, toList)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.RmFuns"


-- ==================================================


runRmFunAsync :: Id -> Fun -> MudStack ()
runRmFunAsync i fun = runAsync fun >>= \a -> tweak $ rmTbl.ind i.rmFunAsyncs <>~ pure a


startRmFuns :: MudStack ()
startRmFuns = getState >>= \ms -> do logNotice "startRmFuns" "starting room functions."
                                     mapM_ (\(i, fns) -> mapM_ (runRmFunAsync i . (`getFun` ms)) fns) . helper $ ms
  where
    helper = views rmTbl (map (second (view rmFunNames)) . IM.toList . IM.filter (views rmFunNames (()!#)))


stopRmFuns :: MudStack ()
stopRmFuns = do
    logNotice "stopRmFuns"  "stopping room functions."
    mapM_ (uncurry throwWaitRmFuns) . views rmTbl (IM.toList . IM.filter (views rmFunAsyncs (()!#))) =<< getState


throwWaitRmFuns :: Id -> Rm -> MudStack ()
throwWaitRmFuns i r = do
    mapM_ throwWait $ r^.rmFunAsyncs
    tweak $ rmTbl.ind i.rmFunAsyncs .~ []
