{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Threads.Biodegrader ( runBiodegAsync
                               , startBiodegraders
                               , stopBiodegraders ) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Destroy
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Threads.Effect
import           Mud.Threads.Misc
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Exception.Lifted (catch, handle)
import           Control.Lens.Operators ((?~), (.~), (&), (^.))
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool (bool)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Biodegrader"


-- ==================================================


runBiodegAsync :: HasCallStack => Id -> MudStack ()
runBiodegAsync i = runAsync (threadBiodegrader i) >>= \a -> tweak $ objTbl.ind i.objBiodegAsync ?~ a


startBiodegraders :: HasCallStack => MudStack ()
startBiodegraders = do logNotice "startBiodegraders" "starting biodegraders."
                       mapM_ runBiodegAsync . findBiodegradableIds =<< getState


stopBiodegraders :: HasCallStack => MudStack ()
stopBiodegraders = do logNotice "stopBiodegraders"  "stopping biodegraders."
                      mapM_ throwWaitBiodegrader . findBiodegradableIds =<< getState


throwWaitBiodegrader :: HasCallStack => Id -> MudStack ()
throwWaitBiodegrader i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.objTbl.ind i.objBiodegAsync
                in (ms & objTbl.ind i.objBiodegAsync .~ Nothing, a)


-----


threadBiodegrader :: HasCallStack => Id -> MudStack ()
threadBiodegrader i = handle (threadExHandler (Just i) "biodegrader") $ descSingId i <$> getState >>= \singId -> do
    setThreadType . Biodegrader $ i
    logNotice "threadBiodegrader" . prd $ "biodegrader started for " <> singId
    loop 0 Nothing `catch` die Nothing ("biodegrader " <> parensQuote singId)
  where
    loop secs lastMaybeInvId = modifyStateSeq $ \ms ->
        let newMaybeInvId = findInvContaining i ms
            mkRes         = (ms, ) . (delay :) . pure
        in case newMaybeInvId of
          Nothing -> mkRes . loop 0 $ Nothing
          Just invId
            | newMaybeInvId == lastMaybeInvId -> if secs < biodegSecs
              then mkRes . loop (secs + biodegDelay) $ lastMaybeInvId
              else let pcsInRm = filter (`isPla` ms) . getInv invId $ ms
                       helper  = (ms, [ stopEffects i
                                      , tweak . destroyHelper . pure $ i
                                      , logNotice "threadBiodegrader" $ descSingId i ms <> " has biodegraded." ])
                   in bool helper (mkRes . loop secs $ lastMaybeInvId) $ ()!# pcsInRm
            | otherwise -> mkRes . uncurry loop $ case getType invId ms of RmType -> (biodegDelay, newMaybeInvId)
                                                                           _      -> (0,           Nothing      )
    delay = liftIO . delaySecs $ biodegDelay
