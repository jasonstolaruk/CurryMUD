{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

{-
Copyright 2014 Jason Stolaruk and Detroit Labs LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Mud.StateInIORefT where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Data.IORef.Lifted


-- Here be dragons!


data MudEnv s = MudEnv { envState :: !(IORef s) }


newtype StateInIORefT s m a = StateInIORefT (ReaderT (MudEnv s) m a) deriving ( Applicative
                                                                              , Functor
                                                                              , Monad
                                                                              , MonadIO
                                                                              , MonadTrans )


instance (MonadBase IO m) => MonadState s (StateInIORefT s m) where
  get   = StateInIORefT $ ask >>= readIORef . envState
  put s = StateInIORefT $ ask >>= flip writeIORef s . envState


runStateInIORefT :: (MonadBase IO m) => StateInIORefT s m a -> s -> m (a, s)
runStateInIORefT (StateInIORefT (ReaderT f)) s = do
    ior <- newIORef s
    a   <- f . MudEnv $ ior
    s'  <- readIORef ior
    return (a, s')


instance (MonadBase b m) => MonadBase b (StateInIORefT s m) where
    liftBase = lift . liftBase


instance MonadTransControl (StateInIORefT s) where
  newtype StT (StateInIORefT s) a = StMyStack { unStMyStack :: a }
  liftWith f = StateInIORefT . ReaderT $ \r -> f $ \(StateInIORefT t) -> liftM StMyStack . runReaderT t $ r
  restoreT   = StateInIORefT . ReaderT . const . liftM unStMyStack


instance (MonadBaseControl b m) => MonadBaseControl b (StateInIORefT s m) where
  newtype StM (StateInIORefT s m) a = ST { unST :: ComposeSt (StateInIORefT s) m a }
  liftBaseWith = defaultLiftBaseWith ST
  restoreM     = defaultRestoreM unST
