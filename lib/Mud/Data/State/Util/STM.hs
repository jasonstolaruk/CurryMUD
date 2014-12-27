{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Data.State.Util.STM where

import Mud.Data.State.State
import Mud.Data.State.StateInIORefT

import Control.Applicative (Const)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, readTMVar, takeTMVar)
import Control.Lens.Getter (view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)


-- ============================================================
-- World state:


getWSTMVar :: StateInIORefT MudState IO (TMVar WorldState)
getWSTMVar = gets (view worldStateTMVar)


readWSTMVar :: MudStack WorldState
readWSTMVar = liftIO . atomically . readTMVar =<< getWSTMVar


onWS :: ((TMVar WorldState, WorldState) -> STM a) -> MudStack a
onWS f = liftIO . atomically . transaction =<< getWSTMVar
  where
    transaction t = takeTMVar t >>= \ws ->
        f (t, ws)


modifyWS :: (WorldState -> WorldState) -> MudStack ()
modifyWS f = liftIO . atomically . transaction =<< getWSTMVar
  where
    transaction t = takeTMVar t >>= putTMVar t . f


-- ============================================================
-- Non-world state:


getNWSRec :: ((a -> Const a a) -> NonWorldState -> Const a NonWorldState) -> MudStack a
getNWSRec lens = gets (view (nonWorldState.lens))


readTMVarInNWS :: ((TMVar a -> Const (TMVar a) (TMVar a)) ->
                  NonWorldState                           ->
                  Const (TMVar a) NonWorldState)          ->
                  MudStack a
readTMVarInNWS lens = liftIO . atomically . readTMVar =<< getNWSRec lens


onNWS :: ((TMVar t -> Const (TMVar t) (TMVar t)) -> NonWorldState -> Const (TMVar t) NonWorldState) ->
         ((TMVar t, t) -> STM a)                                                                    ->
         MudStack a
onNWS lens f = liftIO . atomically . transaction =<< getNWSRec lens
  where
    transaction t = takeTMVar t >>= \x ->
        f (t, x)


modifyNWS :: ((TMVar a -> Const (TMVar a) (TMVar a)) -> NonWorldState -> Const (TMVar a) NonWorldState) ->
             (a -> a)                                                                                   ->
             MudStack ()
modifyNWS lens f = liftIO . atomically . transaction =<< getNWSRec lens
  where
    transaction t = takeTMVar t >>= putTMVar t . f
