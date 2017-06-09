{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Misc.Database
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
import           Servant (Handler, Header, Headers, NoContent(..), Server, (:<|>)(..), err401, err404, err500, errBody, serveDirectoryFileServer)
import           Servant.Auth.Server (AuthResult(..), CookieSettings, JWTSettings, SetCookie, acceptLogin, makeJWT, throwAll)


server :: HasCallStack => CookieSettings -> JWTSettings -> IORef MudState -> Server (API auths)
server cs jwts = (:<|>) <$> protected <*> unprotected cs jwts


-----


{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer putTokenHere" \
     localhost:7249/pla/all -v
-}
protected :: HasCallStack => IORef MudState -> AuthResult Login -> Server Protected
protected ior (Authenticated _) =
         getAllPla
    :<|> getPlaById
    :<|> getAllAlertExecRec
  where
    state :: HasCallStack => Handler MudState
    state = liftIO . readIORef $ ior

    getAllPla :: HasCallStack => Handler [Object Pla]
    getAllPla = views plaTbl mkObjects <$> state

    getPlaById :: HasCallStack => CaptureInt -> Handler (Object Pla)
    getPlaById (CaptureInt i) = views (plaTbl.at i) (maybe notFound (return . Object i)) =<< state

    getAllAlertExecRec :: HasCallStack => Handler [Object AlertExecRec]
    getAllAlertExecRec = map (uncurry Object) . zip [1..] <$> liftIO (getDbTblRecs "alert_exec")
protected _ _ = throwAll err401 -- Unauthorized


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = IM.elems . IM.mapWithKey Object


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." } -- Not Found


-----


unprotected :: HasCallStack => CookieSettings -> JWTSettings -> IORef MudState -> Server Unprotected
unprotected cs jwts _ =
         -- curl http://localhost:7249/token
         tokenHelper
         -- curl -H "Content-Type: application/json" -d '{"username":"curry","password":"curry"}' localhost:7249/login -v
    :<|> loginHelper cs jwts
         -- Open "http://localhost:7249/" in a browser.
    :<|> serveDirectoryFileServer "notes"
  where
    tokenHelper :: HasCallStack => Handler Text
    tokenHelper = liftIO (makeJWT (Login "curry" "curry") jwts Nothing) >>= \case
      Left  e -> do screen $ "Error generating token: " <> showTxt e
                    throwError err500 -- Internal Server Error TODO: This is appropriate, right?
      Right v -> do screen $ "New token: " <> showTxt v
                    return . showTxt $ v


screen :: HasCallStack => Text -> Handler ()
screen = liftIO . T.putStrLn


loginHelper :: HasCallStack => CookieSettings
                            -> JWTSettings
                            -> Login
                            -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie ] NoContent)
loginHelper cs jwts login@(Login un pw)
  | un == "curry", pw == "curry" = screen "Bad creds." >> throwError err401
  | otherwise                    = liftIO (acceptLogin cs jwts login) >>= \case
    Nothing           -> throwError err401
    Just applyCookies -> screen "Authenticated." >> return (applyCookies NoContent)
