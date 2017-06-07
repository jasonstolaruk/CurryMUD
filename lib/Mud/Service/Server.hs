{-# LANGUAGE OverloadedStrings #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Service.Types

import           Control.Lens (at, views)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (toList)
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Proxy(..), Server, (:<|>)(..), err404, errBody)


adminAPI :: HasCallStack => Proxy AdminAPI
adminAPI = Proxy


server :: HasCallStack => IORef MudState -> Server AdminAPI
server ior = getAllPla
        :<|> getPlaById
  where
    getState :: HasCallStack => Handler MudState
    getState = liftIO . readIORef $ ior

    -- curl http://localhost:8081/pla/all
    getAllPla :: HasCallStack => Handler [Object Pla]
    getAllPla = views plaTbl mkObjects <$> liftIO (readIORef ior)

    -- curl http://localhost:8081/pla/0
    getPlaById :: HasCallStack => CaptureInt -> Handler (Object Pla)
    getPlaById (CaptureInt i) = views (plaTbl.at i) (maybe notFound (return . Object i)) =<< getState


notFound :: HasCallStack => Handler (Object Pla)
notFound = throwError err404 { errBody = "Pla ID not found." }


mkObjects :: HasCallStack => PlaTbl -> [Object Pla]
mkObjects = map (uncurry Object) . IM.toList
