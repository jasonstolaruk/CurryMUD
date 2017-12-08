{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.RmFuns ( startRmFuns
                          , stopRmFuns ) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Threads.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators

import           Control.Arrow (second)
import           Control.Lens (view, views)
import           Control.Lens.Operators ((.~), (<>~))
import qualified Data.IntMap.Strict as IM (filter, toList)
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.RmFuns"

-- ==================================================

runRmFunAsync :: HasCallStack => Id -> Fun -> MudStack () -- Room functions should have their own exception handlers.
runRmFunAsync i f = runAsync f >>= \a -> tweak $ rmTbl.ind i.rmFunAsyncs <>~ pure a

startRmFuns :: HasCallStack => MudStack ()
startRmFuns = getState >>= \ms -> do logNotice "startRmFuns" "starting room functions."
                                     mapM_ (\(i, fns) -> mapM_ (runRmFunAsync i . (`getFun` ms)) fns) . helper $ ms
  where
    helper = views rmTbl (map (second (view rmFunNames)) . IM.toList . IM.filter (views rmFunNames (()!#)))

stopRmFuns :: HasCallStack => MudStack ()
stopRmFuns = do
    logNotice "stopRmFuns" "stopping room functions."
    mapM_ (uncurry throwWaitRmFuns) . views rmTbl (IM.toList . IM.filter (views rmFunAsyncs (()!#))) =<< getState

throwWaitRmFuns :: HasCallStack => Id -> Rm -> MudStack ()
throwWaitRmFuns i r = views rmFunAsyncs (mapM_ throwDeathWait) r >> tweak (rmTbl.ind i.rmFunAsyncs .~ [])
