{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.Digester ( runDigesterAsync
                            , startNpcDigesters
                            , stopNpcDigesters
                            , throwWaitDigester ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Effect
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Random
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Threads.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Text

import           Control.Exception.Lifted (handle)
import           Control.Lens (views)
import           Control.Lens.Operators ((?~), (.~), (&), (%~), (^.))
import           Control.Monad ((>=>), forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (delete)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Digester"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Digester"


-- ==================================================


runDigesterAsync :: HasCallStack => Id -> MudStack ()
runDigesterAsync i = runAsync (threadDigester i) >>= \a -> tweak $ mobTbl.ind i.digesterAsync ?~ a


startNpcDigesters :: HasCallStack => MudStack ()
startNpcDigesters =
    sequence_ [ logNotice "startNpcDigesters" "starting NPC digesters.", mapM_ runDigesterAsync  . findNpcIds =<< getState ]


stopNpcDigesters :: HasCallStack => MudStack ()
stopNpcDigesters =
    sequence_ [ logNotice "stopNpcDigesters"  "stopping NPC digesters.", mapM_ throwWaitDigester . findNpcIds =<< getState ]


throwWaitDigester :: HasCallStack => Id -> MudStack ()
throwWaitDigester i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.mobTbl.ind i.digesterAsync
                in (ms & mobTbl.ind i.digesterAsync .~ Nothing, a)


-----


threadDigester :: HasCallStack => Id -> MudStack ()
threadDigester i = handle (threadExHandler (Just i) "digester") $ getState >>= \ms -> do
    setThreadType . Digester $ i
    let delay  | isPla i ms = calcDigesterDelay . getRace i $ ms
               | otherwise  = calcDigesterDelay Human
        loop                = sequence_ [ liftIO . delaySecs $ delay, digest i ]
        singId              = descSingId i ms
    handle (die (Just i) $ "digester for " <> singId) $ logPla "threadDigester" i "digester started." >> forever loop


digest :: HasCallStack => Id -> MudStack ()
digest i = getState >>= \ms -> case getStomach i ms of []  -> unit
                                                       scs -> helper ms scs
  where
    helper ms scs = rndmElem scs >>= \sc -> do
        logPla "digest" i . prd $ "digesting " <> pp sc
        case sc^.distinctId of Left  (DistinctLiqId  x) -> f liqEdibleEffects  . getDistinctLiq  $ x
                               Right (DistinctFoodId x) -> f foodEdibleEffects . getDistinctFood $ x
        tweak $ mobTbl.ind i.stomach %~ (sc `delete`)
      where
        f lens g = views (lens.digestEffects) (maybeVoid (procEffectList i)) . g $ ms
