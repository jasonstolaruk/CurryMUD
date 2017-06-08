{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Service.Types
import           Mud.Util.Text

import           Control.Lens (at, views)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (IntMap, elems, mapWithKey)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Header, Headers, NoContent(..), Server, (:<|>)(..), err401, err404, errBody, serveDirectoryFileServer)
import           Servant.Auth.Server (AuthResult(..), CookieSettings, JWTSettings, SetCookie, acceptLogin, makeJWT, throwAll)


server :: HasCallStack => CookieSettings -> JWTSettings -> IORef MudState -> Server (API auths)
server cs jwts = (:<|>) <$> protected <*> unprotected cs jwts


protected :: HasCallStack => IORef MudState -> AuthResult Login -> Server Protected
protected ior (Authenticated login) =
         return login
    :<|> getAllPla
    :<|> getPlaById
  where
    getState :: HasCallStack => Handler MudState
    getState = liftIO . readIORef $ ior

    -- TODO: curl http://localhost:7249/pla/all
    getAllPla :: HasCallStack => Handler [Object Pla]
    getAllPla = views plaTbl mkObjects <$> liftIO (readIORef ior)

    -- TODO: curl http://localhost:7249/pla/0
    getPlaById :: HasCallStack => CaptureInt -> Handler (Object Pla)
    getPlaById (CaptureInt i) = views (plaTbl.at i) (maybe notFound (return . Object i)) =<< getState
protected _ _ = throwAll err401
{-
TODO
"throwAll" vs. "throwError"? "throwError" can be found below...
"throwError" is from "Control.Monad.Error.Class".
"throwAll" is from "Servant.Auth.Server".
-}


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = IM.elems . IM.mapWithKey Object


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." }


unprotected :: HasCallStack => CookieSettings
                            -> JWTSettings
                            -> IORef MudState
                            -> Server Unprotected
unprotected cs jwts _ =
         -- TODO: curl http://localhost:7249/login
         -- curl localhost:7249/login -v # Gives an error.
         -- curl -H "Authorization: Bearer tokenHere" localhost:7249/login -v
         tokenHelper
    :<|> loginHelper cs jwts
    :<|> serveDirectoryFileServer "example/static"
  where
    tokenHelper :: HasCallStack => Handler Text
    tokenHelper = liftIO (makeJWT (Login "curry" "curry") jwts Nothing) >>= \case
      Left  e -> do liftIO . T.putStrLn $ "Error generating token: " <> showTxt e
                    undefined -- TODO: "throwError err401"?
      Right v -> do liftIO . T.putStrLn $ "New token: "              <> showTxt v
                    return . showTxt $ v



loginHelper :: HasCallStack => CookieSettings
                            -> JWTSettings
                            -> Login
                            -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie ] NoContent)
loginHelper cookieSettings jwtSettings (Login un pw) =
    -- TODO: Usually you would ask a database for the user info. This is just a
    -- regular servant handler, so you can follow your normal database access
    -- patterns (including using 'enter').
    liftIO (acceptLogin cookieSettings jwtSettings . Login un $ pw) >>= \case
      Nothing           -> throwError err401
      Just applyCookies -> return . applyCookies $ NoContent
