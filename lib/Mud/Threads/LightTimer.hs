{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.LightTimer ( restartLightTimers
                              , startLightTimer
                              , stopLightTimers
                              , threadLightTimer ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Hierarchy
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import qualified Mud.Misc.Logging as L (logPla)
import           Mud.Threads.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Exception.Lifted (catch, finally, handle)
import           Control.Lens (at, views)
import           Control.Lens.Operators ((%~), (.~))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (delete)
import qualified Data.Map.Strict as M (elems, filterWithKey)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack (HasCallStack)


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.LightTimer"


-- ==================================================


startLightTimer :: HasCallStack => Id -> MudStack () -- The caller is responsible for setting "lightIsLit" to "True" when applicable.
startLightTimer i = runAsync (threadLightTimer i) >>= \a -> tweak $ lightAsyncTbl.ind i .~ a


-----


threadLightTimer :: HasCallStack => Id -> MudStack ()
threadLightTimer i = helper `catch` threadExHandler (Just i) "light timer"
  where
    helper = descSingId i <$> getState >>= \singId ->
        let f = sequence_ [ setThreadType . LightTimer $ i, loop ]
        in handle (die Nothing ("light timer for " <> singId)) $ f `finally` cleanUp
    loop = getState >>= \ms -> let secs = getLightSecs i ms in if secs > 0
      then do liftIO . delaySecs $ 1
              tweak $ lightTbl.ind i.lightSecs %~ pred
              loop
      else let (locId, s) = (getLocation `fanUncurry` getSing) (i, ms)
               d          = mkStdDesig locId ms DoCap
           in if hasMobId locId ms
             then let (mq, cols) = getMsgQueueColumns locId ms
                      toSelf     = T.concat [ "Your ", s, mkAux "in your inventory", " goes out." ]
                      mkAux txt  = isInInv |?| (spcL . parensQuote $ txt)
                      isInInv    = i `elem` getInv locId ms
                      bs         = pure ( T.concat [ serialize d, "'s ", s, " goes out." ]
                                        , locId `delete` desigIds d )
                      logMsg     = T.concat [ "The light timer for the ", s, mkAux "in inventory", " is expiring." ]
                  in do logPla "threadLightTimer loop" locId logMsg -- TODO: Test.
                        wrapSend mq cols toSelf
                        unless isInInv . bcastIfNotIncogNl locId $ bs
             else bcastNl . pure $ ("The " <> s <> " goes out.", desigIds d) -- TODO: Test.
    cleanUp = tweaks [ lightTbl.ind i.lightIsLit .~ False, lightAsyncTbl.at i .~ Nothing ]


 -----


stopLightTimers :: HasCallStack => Id -> MudStack () -- When a player logs out. The caller is responsible for setting "lightIsLit" to "False" when applicable.
stopLightTimers i = getState >>= \ms -> let is        = getMob'sLights i ms -- TODO: Test.
                                            f lightId = views (lightAsyncTbl.at lightId) (maybeVoid throwDeath) ms
                                        in logPla "stopLightTimers" i "stopping light timers." >> mapM_ f is


getMob'sLights :: HasCallStack => Id -> MudState -> Inv
getMob'sLights i ms = lightsInEq ++ lightsInInv
  where
    lightsInEq  = let f k v | k `elem` [ RHandS, LHandS ] = getType v ms == LightType | otherwise = False
                  in M.elems . M.filterWithKey f . getEqMap i $ ms
    lightsInInv = filter ((== LightType) . (`getType` ms)) . getInv i $ ms


-----


restartLightTimers :: HasCallStack => Id -> MudStack () -- When a player logs in.
restartLightTimers i = getState >>= \ms -> -- TODO: Test.
    let f lightId | ((&&) <$> uncurry getLightIsLit <*> (> 0) . uncurry getLightSecs) (lightId, ms)
                  = startLightTimer lightId
                  | otherwise = unit
    in logPla "restartLightTimers" i "restarting light timers." >> mapM_ f (getMob'sLights i ms)
