{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Misc.Database
import           Mud.Service.Types

import           Control.Lens (at, both, views)
import           Control.Lens.Operators ((&), (%~))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt (validatePassword)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (IntMap, elems, mapWithKey)
import qualified Data.Text.Encoding as T
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Header, Headers, NoContent(..), Server, (:<|>)(..), err401, err404, errBody, serveDirectoryFileServer)
import           Servant.Auth.Server (AuthResult(..), CookieSettings, JWTSettings, SetCookie, acceptLogin, throwAll)


server :: HasCallStack => IORef MudState -> CookieSettings -> JWTSettings -> Server (API auths)
server ior cs jwts = protected ior :<|> unprotected cs jwts


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
    getAlertExecRecAll = liftIO . getDbTblRecs $ "alert_exec" -- Don't try and factor out into a "getRecs" similar to "deleteRec" and "insertRec".

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"timestamp","dbCmdName":"cmdName","dbArgs":"args","dbTarget":"target","dbId":0,"dbName":"name"}' \
     localhost:7249/db/alertexecrec -v
-}
    postAlertExecRec :: HasCallStack => AlertExecRec -> Handler NoContent
    postAlertExecRec = insertRec insertDbTblAlertExec

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexecrec/1 -v
-}
    deleteAlertExecRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteAlertExecRec (CaptureInt i) = deleteRec "alert_exec" i
protected _ _ = throwAll err401 -- Unauthorized


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = IM.elems . IM.mapWithKey Object


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." } -- Not Found


noContentOp :: HasCallStack => IO a -> Handler NoContent
noContentOp = (>> return NoContent) . liftIO


insertRec :: HasCallStack => (a -> IO ()) -> a -> Handler NoContent
insertRec = (noContentOp .)


deleteRec :: HasCallStack => DbTblName -> Int -> Handler NoContent
deleteRec tblName = noContentOp . deleteDbTblRec tblName


-----


unprotected :: HasCallStack => CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts =
         -- curl -H "Content-Type: application/json" -d '{"username":"Curry","password":"curry"}' localhost:7249/login -v # "username" must be capitalized!
         handleLogin cs jwts
         -- Open "http://localhost:7249/" in a browser.
    :<|> serveDirectoryFileServer "notes"


handleLogin :: HasCallStack => CookieSettings
                            -> JWTSettings
                            -> Login
                            -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie ] NoContent)
handleLogin cs jwts login@(Login un pw) = liftIO (lookupPW un) >>= \case
    Just pw' | uncurry validatePassword ((pw', pw) & both %~ T.encodeUtf8) -> liftIO (acceptLogin cs jwts login) >>= \case
                 Just applyCookies -> return . applyCookies $ NoContent
                 Nothing           -> throw401
             | otherwise -> throw401
    Nothing -> throw401
  where
    throw401 = throwError err401 -- Unauthorized
