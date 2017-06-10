{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get hiding (getPla)
import           Mud.Misc.Database
import           Mud.Service.Types
import           Mud.Util.Misc

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
protected ior (Authenticated (Login un _)) =
         getPlaAll
    :<|> getPla
    -----
    :<|> getAlertExecRecAll
    :<|> postAlertExecRec
    :<|> deleteAlertExecRec
    -----
    :<|> getAlertMsgRecAll
    :<|> postAlertMsgRec
    :<|> deleteAlertMsgRec
    -----
    :<|> getBanHostRecAll
    :<|> postBanHostRec
    :<|> deleteBanHostRec
    -----
    :<|> getBanPCRecAll
    :<|> postBanPCRec
    :<|> deleteBanPCRec
  where
    state :: HasCallStack => Handler MudState
    state = liftIO . readIORef $ ior

    doIfAdmin :: HasCallStack => Handler a -> Handler a
    doIfAdmin = flip (mIf (uncurry isAdminId . ((,) <$> getIdForPCSing un <*> id) <$> state)) (throwError err401)

    -----

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

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexec/all -v
-}
    getAlertExecRecAll :: HasCallStack => Handler [AlertExecRec]
    getAlertExecRecAll = doIfAdmin . liftIO . getDbTblRecs $ "alert_exec"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 16:00:52]","dbCmdName":"rape","dbArgs":"mh","dbTarget":"Curry","dbId":0,"dbName":"Zappy"}' \
     localhost:7249/db/alertexec -v
-}
    postAlertExecRec :: HasCallStack => AlertExecRec -> Handler NoContent
    postAlertExecRec = doIfAdmin . insertRec insertDbTblAlertExec

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexec/1 -v
-}
    deleteAlertExecRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteAlertExecRec (CaptureInt i) = doIfAdmin . deleteRec "alert_exec" $ i

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertmsg/all -v
-}
    getAlertMsgRecAll :: HasCallStack => Handler [AlertMsgRec]
    getAlertMsgRecAll = doIfAdmin . liftIO . getDbTblRecs $ "alert_msg"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbMsg":"You say, \"Rape.\"","dbTimestamp":"[2017-06-09 16:07:54]","dbCmdName":"say","dbTrigger":"rape","dbId":0,"dbName":"Zappy"}' \
     localhost:7249/db/alertmsg -v
-}
    postAlertMsgRec :: HasCallStack => AlertMsgRec -> Handler NoContent
    postAlertMsgRec = doIfAdmin . insertRec insertDbTblAlertMsg

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertmsg/1 -v
-}
    deleteAlertMsgRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteAlertMsgRec (CaptureInt i) = doIfAdmin . deleteRec "alert_msg" $ i

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banhost/all -v
-}
    getBanHostRecAll :: HasCallStack => Handler [BanHostRec]
    getBanHostRecAll = doIfAdmin . liftIO . getDbTblRecs $ "ban_host"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 16:11:26]","dbIsBanned":true,"dbHost":"127.0.0.1","dbReason":"Used by Taro.","dbId":0}' \
     localhost:7249/db/banhost -v
-}
    postBanHostRec :: HasCallStack => BanHostRec -> Handler NoContent
    postBanHostRec = doIfAdmin . insertRec insertDbTblBanHost

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banhost/1 -v
-}
    deleteBanHostRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteBanHostRec (CaptureInt i) = doIfAdmin . deleteRec "ban_host" $ i

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banpc/all -v
-}
    getBanPCRecAll :: HasCallStack => Handler [BanPCRec]
    getBanPCRecAll = doIfAdmin . liftIO . getDbTblRecs $ "ban_pc"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 17:02:57]","dbIsBanned":true,"dbReason":"For harassment.","dbId":0,"dbName":"Zappy"}' \
     localhost:7249/db/banpc -v
-}
    postBanPCRec :: HasCallStack => BanPCRec -> Handler NoContent
    postBanPCRec = doIfAdmin . insertRec insertDbTblBanPC

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banpc/1 -v
-}
    deleteBanPCRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteBanPCRec (CaptureInt i) = doIfAdmin . deleteRec "ban_pc" $ i
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
