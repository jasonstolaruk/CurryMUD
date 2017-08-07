{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.LightTimer (threadLightTimer) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Hierarchy
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import qualified Mud.Misc.Logging as L (logPla)
import           Mud.Threads.Misc
import           Mud.Util.Misc

import           Control.Exception.Lifted (catch, finally)
import           Control.Lens (at)
import           Control.Lens.Operators ((%~), (.~))
import           Control.Monad.IO.Class (liftIO)
import           Data.List (delete)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack (HasCallStack)


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.LightTimer"


-- ==================================================


threadLightTimer :: HasCallStack => Id -> MudStack ()
threadLightTimer i = helper `catch` threadExHandler (Just i) "light timer"
  where
    helper = descSingId i <$> getState >>= \singId ->
        let f = sequence_ [ setThreadType . LightTimer $ i, loop `catch` die Nothing ("light timer for " <> singId) ]
        in f `finally` cleanUp
    loop = getState >>= \ms -> let secs = getLightSecs i ms in if secs > 0
      then do liftIO . delaySecs $ 1
              tweak $ lightTbl.ind i.lightSecs %~ pred
              loop
      else let (locId, s) = (getLocation `fanUncurry` getSing) (i, ms)
               d          = mkStdDesig locId ms DoCap
           in if hasMobId locId ms
             then let (mq, cols) = getMsgQueueColumns locId ms
                      toSelf     = "Your " <> s <> " goes out."
                      bs         = pure ( T.concat [ serialize d, "'s ", s, " goes out." ]
                                        , locId `delete` desigIds d )
                      logMsg     = "The " <> s <> " goes out."
                  in do wrapSend mq cols toSelf
                        bcastIfNotIncogNl locId bs
                        logPla "threadLightTimer loop" locId logMsg
             else bcastNl . pure $ ("The " <> s <> " goes out.", desigIds d)
    cleanUp = tweaks [ lightTbl.ind i.lightIsLit .~ False, lightAsyncTbl.at i .~ Nothing ]
