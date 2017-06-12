{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, TypeOperators #-}

module Mud.Service.Server where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get hiding (getPla)
import           Mud.Misc.Database
import           Mud.Service.Logging
import           Mud.Service.Types
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow (second)
import           Control.Lens (at, both, views)
import           Control.Lens.Operators ((&), (%~))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.BCrypt (validatePassword)
import           Data.IORef (IORef, readIORef)
import qualified Data.IntMap.Strict as IM (IntMap, elems, mapWithKey)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import           GHC.Stack (HasCallStack)
import           Servant (Handler, Header, Headers, NoContent(..), Server, (:<|>)(..), err401, err404, errBody)
import           Servant.Auth.Server (AuthResult(..), CookieSettings, JWTSettings, SetCookie, acceptLogin, throwAll)


-- ==========
-- Utility functions:


deleteRec :: HasCallStack => DbTblName -> Int -> Handler NoContent
deleteRec tblName = noContentOp . deleteDbTblRec tblName


insertRec :: HasCallStack => (a -> IO ()) -> a -> Handler NoContent
insertRec = (noContentOp .)


mkObjects :: HasCallStack => IM.IntMap a -> [Object a]
mkObjects = IM.elems . IM.mapWithKey Object


noContentOp :: HasCallStack => IO a -> Handler NoContent
noContentOp = (>> return NoContent) . liftIO


notFound :: HasCallStack => Handler (Object a)
notFound = throwError err404 { errBody = "ID not found." } -- Not Found


-- ==========
-- Server:


server :: HasCallStack => IORef MudState -> CookieSettings -> JWTSettings -> Server (API auths)
server ior cs jwts = protected ior :<|> unprotected ior cs jwts


-- ==========
-- Protected:


protected :: HasCallStack => IORef MudState -> AuthResult Login -> Server Protected
protected ior (Authenticated (Login un _)) =
         getPla
    -- ==========
    :<|> getPlaAll
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
    -- ==========
    -- Helper functions:

    deleteHelper :: HasCallStack => CaptureInt -> Text -> Text -> Handler NoContent
    deleteHelper (CaptureInt i') funName tblName = doIfAdmin $ getMudStateId >>= \(ms, i) -> do
        logHelper ms funName i . prd $ "deleting " <> showTxt i'
        deleteRec tblName i'

    doIfAdmin :: HasCallStack => Handler a -> Handler a
    doIfAdmin = flip (mIf (uncurry isAdminId . ((,) <$> getIdForPCSing un <*> id) <$> state)) (throwError err401) -- Unauthorized

    getMudStateId :: HasCallStack => Handler (MudState, Id)
    getMudStateId = second (|&| getIdForPCSing un) . dup <$> state

    getPostHelper :: HasCallStack => Text -> (MudState -> Handler a) -> Handler a
    getPostHelper fn f = doIfAdmin $ getMudStateId >>= \(ms, i) -> logExecuted ms fn i >> f ms

    logExecuted :: HasCallStack => MudState -> Text -> Id -> Handler ()
    logExecuted ms fn i = logHelper ms fn i ""

    logHelper :: HasCallStack => MudState -> Text -> Id -> Text -> Handler ()
    logHelper ms fn i = liftIO . logRestService ms fn (Just un) (Just i)

    notFoundHelper :: HasCallStack => MudState -> Text -> Text -> Id -> Handler (Object a)
    notFoundHelper ms funName tblName i =
        let msg = T.concat [ "ID ", showTxt i, " not found in ", dblQuote tblName, "." ]
        in logHelper ms funName i msg >> notFound

    state :: HasCallStack => Handler MudState
    state = liftIO . readIORef $ ior

    -- ==========
    -- Player endpoints:

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/pla -v
-}
    getPla :: HasCallStack => Handler (Object Pla)
    getPla = getMudStateId >>= \(ms, i) -> let fn    = "getPla"
                                               found = (logExecuted ms fn i >>) . return . Object i
                                           in views (plaTbl.at i) (maybe (notFoundHelper ms fn "plaTbl" i) found) ms

    -- ==========
    -- Admin endpoints:

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/pla/all -v
-}
    getPlaAll :: HasCallStack => Handler [Object Pla]
    getPlaAll = getPostHelper "getPlaAll" $ return . views plaTbl mkObjects

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexec/all -v
-}
    getAlertExecRecAll :: HasCallStack => Handler [AlertExecRec]
    getAlertExecRecAll = getPostHelper "getAlertExecRecAll" . const . liftIO . getDbTblRecs $ "alert_exec"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 16:00:52]","dbCmdName":"rape","dbArgs":"mh","dbTarget":"Curry","dbName":"Zappy"}' \
     localhost:7249/db/alertexec -v
-}
    postAlertExecRec :: HasCallStack => AlertExecRec -> Handler NoContent
    postAlertExecRec = getPostHelper "postAlertExecRec" . const . insertRec insertDbTblAlertExec

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexec/1 -v
-}
    deleteAlertExecRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteAlertExecRec ci = deleteHelper ci "deleteAlertExecRec" "alert_exec"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertmsg/all -v
-}
    getAlertMsgRecAll :: HasCallStack => Handler [AlertMsgRec]
    getAlertMsgRecAll = getPostHelper "getAlertMsgRecAll" . const . liftIO . getDbTblRecs $ "alert_msg"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbMsg":"You say, \"Rape.\"","dbTimestamp":"[2017-06-09 16:07:54]","dbCmdName":"say","dbTrigger":"rape","dbName":"Zappy"}' \
     localhost:7249/db/alertmsg -v
-}
    postAlertMsgRec :: HasCallStack => AlertMsgRec -> Handler NoContent
    postAlertMsgRec = getPostHelper "postAlertMsgRec" . const . insertRec insertDbTblAlertMsg

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertmsg/1 -v
-}
    deleteAlertMsgRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteAlertMsgRec ci = deleteHelper ci "deleteAlertMsgRec" "alert_msg"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banhost/all -v
-}
    getBanHostRecAll :: HasCallStack => Handler [BanHostRec]
    getBanHostRecAll = getPostHelper "getBanHostRecAll" . const . liftIO . getDbTblRecs $ "ban_host"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 16:11:26]","dbIsBanned":true,"dbHost":"127.0.0.1","dbReason":"Used by Taro."}' \
     localhost:7249/db/banhost -v
-}
    postBanHostRec :: HasCallStack => BanHostRec -> Handler NoContent
    postBanHostRec = getPostHelper "postBanHostRec" . const . insertRec insertDbTblBanHost

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banhost/1 -v
-}
    deleteBanHostRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteBanHostRec ci = deleteHelper ci "deleteBanHostRec" "ban_host"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banpc/all -v
-}
    getBanPCRecAll :: HasCallStack => Handler [BanPCRec]
    getBanPCRecAll = getPostHelper "getBanPCRecAll" . const . liftIO . getDbTblRecs $ "ban_pc"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 17:02:57]","dbIsBanned":true,"dbReason":"For harassment.","dbName":"Zappy"}' \
     localhost:7249/db/banpc -v
-}
    postBanPCRec :: HasCallStack => BanPCRec -> Handler NoContent
    postBanPCRec = getPostHelper "postBanPCRec" . const . insertRec insertDbTblBanPC

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banpc/1 -v
-}
    deleteBanPCRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteBanPCRec ci = deleteHelper ci "deleteBanPCRec" "ban_pc"
protected _ _ = throwAll err401 -- Unauthorized


-- ==========
-- Unprotected:


unprotected :: HasCallStack => IORef MudState -> CookieSettings -> JWTSettings -> Server Unprotected
unprotected = loginHandler


-- curl -H "Content-Type: application/json" -d '{"username":"Curry","password":"curry"}' localhost:7249/login -v # Username must be capitalized!
loginHandler :: HasCallStack => IORef MudState
                             -> CookieSettings
                             -> JWTSettings
                             -> Login
                             -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                                  , Header "Set-Cookie" SetCookie ] NoContent)
loginHandler ior cs jwts login@(Login un pw) = liftIO (lookupPW un) >>= \case
    Just pw' | uncurry validatePassword ((pw', pw) & both %~ T.encodeUtf8) -> liftIO (acceptLogin cs jwts login) >>= \case
                 Just applyCookies -> logHelper "" >> return (applyCookies NoContent)
                 Nothing           -> throw401
             | otherwise -> throw401
    Nothing -> throw401
  where
    throw401      = logHelper "unauthorized." >> throwError err401 -- Unauthorized
    logHelper msg = liftIO $ readIORef ior >>= \ms -> logRestService ms "login" (Just un) Nothing msg
