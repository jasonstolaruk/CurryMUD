{-# LANGUAGE OverloadedStrings #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Service.Types

import           Control.Lens (at, views)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (IntMap, toList)
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Proxy(..), Server, (:<|>)(..), err401, err404, errBody)
import           Servant.Auth.Server (AuthResult(..), throwAll)


adminAPI :: HasCallStack => Proxy AdminAPI
adminAPI = Proxy


server :: HasCallStack => IORef MudState -> Server AdminAPI
server ior =
         getAllPla
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


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." }


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = map (uncurry Object) . IM.toList


protected :: AuthResult Login -> Server Protected
protected (Authenticated login) =
         return "hello"
    :<|> return (Object 1 login)
protected _ = throwAll err401
