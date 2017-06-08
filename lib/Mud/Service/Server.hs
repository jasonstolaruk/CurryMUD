{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Service.Types

import           Control.Lens (at, views)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (IntMap, elems, mapWithKey)
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Header, Headers, NoContent(..), Server, (:<|>)(..), err401, err404, errBody, serveDirectoryFileServer)
import           Servant.Auth.Server (AuthResult(..), CookieSettings, JWTSettings, SetCookie, acceptLogin, throwAll)


server :: HasCallStack => IORef MudState -> CookieSettings -> JWTSettings -> Server (API auths)
server ior cs jwts = ((:<|>) <$> protected <*> unprotected cs jwts) ior


protected :: HasCallStack => IORef MudState -> AuthResult Login -> Server Protected
protected ior (Authenticated login) =
         return (Object 1 login)
    :<|> getAllPla
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
protected _ _ = throwAll err401 -- TODO: "throwAll" vs. "throwError"? "throwError" can be found below...


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = IM.elems . IM.mapWithKey Object


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." }


 -- TODO: Add curl comments.
unprotected :: HasCallStack => CookieSettings
                            -> JWTSettings
                            -> IORef MudState
                            -> Server Unprotected
unprotected cs jwts _ =
         checkCreds cs jwts
    :<|> serveDirectoryFileServer "example/static"


checkCreds :: HasCallStack => CookieSettings
                           -> JWTSettings
                           -> Login
                           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                                , Header "Set-Cookie" SetCookie ] NoContent)
checkCreds cookieSettings jwtSettings (Login un pw) =
    -- TODO: Usually you would ask a database for the user info. This is just a
    -- regular servant handler, so you can follow your normal database access
    -- patterns (including using 'enter').
    liftIO (acceptLogin cookieSettings jwtSettings . Login un $ pw) >>= \case
      Nothing           -> throwError err401
      Just applyCookies -> return . applyCookies $ NoContent
-- TODO: throwError err401
