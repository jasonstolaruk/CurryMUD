{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

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
-- import           Mud.TopLvlDefs.Seconds
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Exception.Lifted (catch, finally, handle)
import           Control.Lens (at, views)
import           Control.Lens.Operators ((%~), (.~))
import           Control.Monad (unless, when)
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
    loop = getState >>= \ms ->
        let secs       = getLightSecs i ms
            (locId, s) = (getLocation `fanUncurry` getSing) (i, ms)
            (locIsMob, (mq, cols), locInv) = ((,,) <$> uncurry hasMobId
                                                   <*> uncurry getMsgQueueColumns
                                                   <*> uncurry getInv) (locId, ms)
            ioHelper     = wrapSend mq cols
            bcastHelper  = unless isInMobInv . bcastIfNotIncogNl locId
            isInMobInv   = i `elem` locInv
            mkBs t       = pure (t, locId `delete` desigIds d)
            d            = mkStdDesig locId ms DoCap
            mobIdsInRm   = findMobIds ms locInv
            leadTxt      | isInMobInv = the' s
                         | otherwise  = "Your " <> s
            inInvTxt     = mkInInvTxt "in your inventory"
            mkInInvTxt t = isInMobInv |?| (spcL . parensQuote $ t)
            notify           | secs == 10 = notifyHelper " is about to go out."                              True
                             | secs == 15 = notifyHelper " only has a few minutes of light left."            False
                             | secs == 20 = notifyHelper " has perhaps about fifteen minutes of light left." False
                             | otherwise  = unit
            notifyHelper t b | locIsMob   = do ioHelper $ leadTxt <> inInvTxt <> t
                                               when b . bcastHelper . mkBs . T.concat $ [ serialize d, "'s ", s, t ]
                             | otherwise  = bcastNl . pure $ (the' s <> t, mobIdsInRm)
        in if secs > 0
          then do notify
                  liftIO . delaySecs $ 1
                  tweak $ lightTbl.ind i.lightSecs %~ pred
                  loop
          else if -- Exit the loop.
            | locIsMob  -> let toSelf = leadTxt <> inInvTxt <> " goes out."
                               bs     = mkBs . T.concat $ [ serialize d, "'s ", s, " goes out." ]
                               logMsg = T.concat [ "The light timer for the ", s, mkInInvTxt "in inventory", " is expiring." ]
                           in do logPla "threadLightTimer loop" locId logMsg
                                 setNotLit
                                 ioHelper toSelf
                                 bcastHelper bs
            | otherwise -> do setNotLit
                              bcastNl . pure $ ("The " <> s <> " goes out.", mobIdsInRm)
    setNotLit = tweak $ lightTbl     .ind i.lightIsLit .~ False
    cleanUp   = tweak $ lightAsyncTbl.at  i            .~ Nothing


-----


stopLightTimers :: HasCallStack => Id -> MudStack () -- When a player logs out. The caller is responsible for setting "lightIsLit" to "False" when applicable.
stopLightTimers i = getState >>= \ms -> let is        = getMob'sLights i ms
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
restartLightTimers i = getState >>= \ms ->
    let f lightId | ((&&) <$> uncurry getLightIsLit <*> (> 0) . uncurry getLightSecs) (lightId, ms)
                  = startLightTimer lightId
                  | otherwise = unit
    in logPla "restartLightTimers" i "restarting light timers." >> mapM_ f (getMob'sLights i ms)
