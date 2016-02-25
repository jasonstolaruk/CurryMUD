{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes, TupleSections #-}

module Mud.Threads.Biodegrader ( runBiodegraderAsync
                               , startBiodegraders
                               , stopBiodegraders ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Destroy
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Lens.Operators ((&), (.~), (?~), (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Biodegrader"


-- ==================================================


runBiodegraderAsync :: Id -> MudStack ()
runBiodegraderAsync i = runAsync (threadBiodegrader i) >>= \a -> tweak $ objTbl.ind i.biodegraderAsync ?~ a


startBiodegraders :: MudStack ()
startBiodegraders = do
    logNotice "startBiodegraders" "starting biodegraders."
    mapM_ runBiodegraderAsync . findBiodegradableIds =<< getState


stopBiodegraders :: MudStack ()
stopBiodegraders = do
    logNotice "stopBiodegraders"  "stopping biodegraders."
    mapM_ throwWaitBiodegrader . findBiodegradableIds =<< getState


throwWaitBiodegrader :: Id -> MudStack ()
throwWaitBiodegrader i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.objTbl.ind i.biodegraderAsync
                in (ms & objTbl.ind i.biodegraderAsync .~ Nothing, a)


-----


threadBiodegrader :: Id -> MudStack ()
threadBiodegrader i = handle (threadExHandler threadName) $ getSing i <$> getState >>= \s -> do
    setThreadType . Biodegrader $ i
    logNotice "threadBiodegrader" . T.concat $ [ "biodegrader started for ", s, " ", idTxt, "." ]
    loop 0 Nothing `catch` die Nothing threadName
  where
    threadName               = "biodegrader " <> idTxt
    idTxt                    = parensQuote . showText $ i
    loop secs lastMaybeInvId = getState >>= \ms -> do
        let newMaybeInvId = findInvContaining i ms
        case newMaybeInvId of
          Nothing -> delay >> loop 0 Nothing
          Just invId
            | newMaybeInvId == lastMaybeInvId -> if secs < biodegradationDuration
              then delay >> loop (secs + biodegraderDelay) lastMaybeInvId
              else let pcsInRm = filter (`isPC` ms) . getInv invId $ ms
                       helper  = do
                           logNotice "threadBiodegrader" . T.concat $ [ getSing i ms, " ", idTxt, " has biodegraded." ]
                           destroyHelper . pure $ i
                   in ()!# pcsInRm ? (delay >> loop secs lastMaybeInvId) :? helper
            | otherwise -> (delay >>) $ case getType invId ms of
              RmType -> loop biodegraderDelay newMaybeInvId
              _      -> loop 0 Nothing
    delay = liftIO . threadDelay $ biodegraderDelay * 10 ^ 6
