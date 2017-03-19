{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Mud.Threads.Biodegrader ( runBiodegAsync
                               , startBiodegraders
                               , stopBiodegraders ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Destroy
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Exception.Lifted (catch, handle)
import Control.Lens.Operators ((?~), (.~), (&), (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Text (Text)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Biodegrader"


-- ==================================================


runBiodegAsync :: Id -> MudStack ()
runBiodegAsync i = runAsync (threadBiodegrader i) >>= \a -> tweak $ objTbl.ind i.objBiodegAsync ?~ a


startBiodegraders :: MudStack ()
startBiodegraders = do logNotice "startBiodegraders" "starting biodegraders."
                       mapM_ runBiodegAsync . findBiodegradableIds =<< getState


stopBiodegraders :: MudStack ()
stopBiodegraders = do logNotice "stopBiodegraders"  "stopping biodegraders."
                      mapM_ throwWaitBiodegrader . findBiodegradableIds =<< getState


throwWaitBiodegrader :: Id -> MudStack ()
throwWaitBiodegrader i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.objTbl.ind i.objBiodegAsync
                in (ms & objTbl.ind i.objBiodegAsync .~ Nothing, a)


-----


threadBiodegrader :: Id -> MudStack ()
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
                       helper  = ( destroyHelper (pure i) ms
                                 , [ stopEffects i
                                   , logNotice "threadBiodegrader" $ descSingId i ms <> " has biodegraded." ] )
                   in bool helper (mkRes . loop secs $ lastMaybeInvId) $ ()!# pcsInRm
            | otherwise -> mkRes . uncurry loop $ case getType invId ms of RmType -> (biodegDelay, newMaybeInvId)
                                                                           _      -> (0,           Nothing      )
    delay = liftIO . delaySecs $ biodegDelay
