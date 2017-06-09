{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Misc.Database
import           Mud.Service.Types

import           Control.Lens (at, views)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (IntMap, elems, mapWithKey)
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Header, Headers, NoContent(..), Server, (:<|>)(..), err401, err404, errBody, serveDirectoryFileServer)
import           Servant.Auth.Server (AuthResult(..), CookieSettings, JWTSettings, SetCookie, acceptLogin, throwAll)


server :: HasCallStack => CookieSettings -> JWTSettings -> IORef MudState -> Server (API auths)
server cs jwts = (:<|>) <$> protected <*> unprotected cs jwts


-----


protected :: HasCallStack => IORef MudState -> AuthResult Login -> Server Protected
protected ior (Authenticated _) =
         getPlaAll
    :<|> getPla
    :<|> getAlertExecRecAll
    :<|> postAlertExecRec
    :<|> deleteAlertExecRec
  where
    state :: HasCallStack => Handler MudState
    state = liftIO . readIORef $ ior

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/pla/all -v
-}
    getPlaAll :: HasCallStack => Handler [Object Pla]
    getPlaAll = views plaTbl mkObjects <$> state

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/pla/0 -v
-}
    getPla :: HasCallStack => CaptureInt -> Handler (Object Pla)
    getPla (CaptureInt i) = views (plaTbl.at i) (maybe notFound (return . Object i)) =<< state

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexecrec/all -v
-}
    getAlertExecRecAll :: HasCallStack => Handler [AlertExecRec]
    getAlertExecRecAll = liftIO . getDbTblRecs $ "alert_exec"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"timestamp","dbCmdName":"cmdName","dbArgs":"args","dbTarget":"target","dbId":0,"dbName":"name"}' \
     localhost:7249/db/alertexecrec -v
-}
    postAlertExecRec :: HasCallStack => AlertExecRec -> Handler NoContent
    postAlertExecRec rec = liftIO $ insertDbTblAlertExec rec >> return NoContent

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexecrec/1 -v
-}
    deleteAlertExecRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteAlertExecRec (CaptureInt i) = liftIO $ deleteDbTblRec "alert_exec" i >> return NoContent
protected _ _ = throwAll err401 -- Unauthorized


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = IM.elems . IM.mapWithKey Object


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." } -- Not Found


-----


unprotected :: HasCallStack => CookieSettings -> JWTSettings -> IORef MudState -> Server Unprotected
unprotected cs jwts _ =
         -- curl -H "Content-Type: application/json" -d '{"username":"curry","password":"curry"}' localhost:7249/login -v
         handleLogin cs jwts
         -- Open "http://localhost:7249/" in a browser.
    :<|> serveDirectoryFileServer "notes"


handleLogin :: HasCallStack => CookieSettings
                            -> JWTSettings
                            -> Login
                            -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie ] NoContent)
handleLogin cs jwts login@(Login un pw)
  | un == "curry", pw == "curry" = liftIO (acceptLogin cs jwts login) >>= \case
    Nothing           -> throwError err401
    Just applyCookies -> return (applyCookies NoContent)
  | otherwise = throwError err401
