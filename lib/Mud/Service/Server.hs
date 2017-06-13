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


deleteRecs :: HasCallStack => DbTblName -> Handler NoContent
deleteRecs = noContentOp . deleteDbTblRecs


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
    :<|> getAllPla
    -----
    :<|> getAllAlertExecRec
    :<|> deleteAlertExecRec
    -----
    :<|> getAllAlertMsgRec
    :<|> deleteAlertMsgRec
    -----
    :<|> getAllBanHostRec
    :<|> postBanHostRec
    :<|> deleteBanHostRec
    -----
    :<|> getAllBanPCRec
    :<|> postBanPCRec
    :<|> deleteBanPCRec
    -----
    :<|> getAllBugRec
    :<|> deleteBugRec
    -----
    :<|> getAllDiscoverRec
    :<|> deleteAllDiscoverRec
    -----
    :<|> getAllProfRec
    :<|> deleteAllProfRec
    -----
    :<|> getAllPropNameRec
    :<|> deleteAllPropNameRec
    -----
    :<|> getAllTelnetCharsRec
    :<|> deleteAllTelnetCharsRec
    -----
    :<|> getAllTTypeRec
    :<|> deleteAllTTypeRec
    -----
    :<|> getAllTypoRec
    :<|> deleteTypoRec
    -----
    :<|> getAllWordRec
    :<|> deleteAllWordRec
  where
    -- ==========
    -- Helper functions:

    deleteAllHelper :: HasCallStack => Text -> Text -> Handler NoContent
    deleteAllHelper funName = genericHelper funName . const . deleteRecs

    deleteHelper :: HasCallStack => CaptureInt -> Text -> Text -> Handler NoContent
    deleteHelper (CaptureInt i') funName tblName = doIfAdmin $ getMudStateId >>= \(ms, i) -> do
        logHelper ms funName i . prd $ "deleting " <> showTxt i'
        deleteRec tblName i'

    doIfAdmin :: HasCallStack => Handler a -> Handler a
    doIfAdmin = flip (mIf (uncurry isAdminId . ((,) <$> getIdForPCSing un <*> id) <$> state)) (throwError err401) -- Unauthorized

    genericHelper :: HasCallStack => Text -> (MudState -> Handler a) -> Handler a
    genericHelper fn f = doIfAdmin $ getMudStateId >>= \(ms, i) -> logExecuted ms fn i >> f ms

    getMudStateId :: HasCallStack => Handler (MudState, Id)
    getMudStateId = second (|&| getIdForPCSing un) . dup <$> state

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
    getAllPla :: HasCallStack => Handler [Object Pla]
    getAllPla = genericHelper "getAllPla" $ return . views plaTbl mkObjects

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/alertexec/all -v
-}
    getAllAlertExecRec :: HasCallStack => Handler [AlertExecRec]
    getAllAlertExecRec = genericHelper "getAllAlertExecRec" . const . liftIO . getDbTblRecs $ "alert_exec"

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
    getAllAlertMsgRec :: HasCallStack => Handler [AlertMsgRec]
    getAllAlertMsgRec = genericHelper "getAllAlertMsgRec" . const . liftIO . getDbTblRecs $ "alert_msg"

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
    getAllBanHostRec :: HasCallStack => Handler [BanHostRec]
    getAllBanHostRec = genericHelper "getAllBanHostRec" . const . liftIO . getDbTblRecs $ "ban_host"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 16:11:26]","dbIsBanned":true,"dbHost":"127.0.0.1","dbReason":"Used by Taro."}' \
     localhost:7249/db/banhost -v
-}
    postBanHostRec :: HasCallStack => BanHostRec -> Handler NoContent
    postBanHostRec = genericHelper "postBanHostRec" . const . insertRec insertDbTblBanHost

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
    getAllBanPCRec :: HasCallStack => Handler [BanPCRec]
    getAllBanPCRec = genericHelper "getAllBanPCRec" . const . liftIO . getDbTblRecs $ "ban_pc"

{-
curl -X POST \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     -d '{"dbTimestamp":"[2017-06-09 17:02:57]","dbIsBanned":true,"dbReason":"For harassment.","dbName":"Zappy"}' \
     localhost:7249/db/banpc -v
-}
    postBanPCRec :: HasCallStack => BanPCRec -> Handler NoContent
    postBanPCRec = genericHelper "postBanPCRec" . const . insertRec insertDbTblBanPC

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/banpc/1 -v
-}
    deleteBanPCRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteBanPCRec ci = deleteHelper ci "deleteBanPCRec" "ban_pc"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/bug/all -v
-}
    getAllBugRec :: HasCallStack => Handler [BugRec]
    getAllBugRec = genericHelper "getAllBugRec" . const . liftIO . getDbTblRecs $ "bug"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/bug/1 -v
-}
    deleteBugRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteBugRec ci = deleteHelper ci "deleteBugRec" "bug"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/discover/all -v
-}
    getAllDiscoverRec :: HasCallStack => Handler [DiscoverRec]
    getAllDiscoverRec = genericHelper "getAllDiscoverRec" . const . liftIO . getDbTblRecs $ "discover"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/discover/all -v
-}
    deleteAllDiscoverRec :: HasCallStack => Handler NoContent
    deleteAllDiscoverRec = deleteAllHelper "deleteAllDiscoverRec" "discover"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/prof/all -v
-}
    getAllProfRec :: HasCallStack => Handler [ProfRec]
    getAllProfRec = genericHelper "getAllProfRec" . const . liftIO . getDbTblRecs $ "prof"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/prof/all -v
-}
    deleteAllProfRec :: HasCallStack => Handler NoContent
    deleteAllProfRec = deleteAllHelper "deleteAllProfRec" "prof"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/propname/all -v
-}
    getAllPropNameRec :: HasCallStack => Handler [PropNameRec]
    getAllPropNameRec = genericHelper "getAllPropNameRec" . const . liftIO . getDbTblRecs $ "prop_names"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/propname/all -v
-}
    deleteAllPropNameRec :: HasCallStack => Handler NoContent
    deleteAllPropNameRec = deleteAllHelper "deleteAllPropNameRec" "prop_names"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/telnetchars/all -v
-}
    getAllTelnetCharsRec :: HasCallStack => Handler [TelnetCharsRec]
    getAllTelnetCharsRec = genericHelper "getAllTelnetCharsRec" . const . liftIO . getDbTblRecs $ "telnet_chars"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/telnetchars/all -v
-}
    deleteAllTelnetCharsRec :: HasCallStack => Handler NoContent
    deleteAllTelnetCharsRec = deleteAllHelper "deleteAllTelnetCharsRec" "telnet_chars"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/ttype/all -v
-}
    getAllTTypeRec :: HasCallStack => Handler [TTypeRec]
    getAllTTypeRec = genericHelper "getAllTTypeRec" . const . liftIO . getDbTblRecs $ "ttype"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/ttype/all -v
-}
    deleteAllTTypeRec :: HasCallStack => Handler NoContent
    deleteAllTTypeRec = deleteAllHelper "deleteAllTTypeRec" "ttype"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/typo/all -v
-}
    getAllTypoRec :: HasCallStack => Handler [TypoRec]
    getAllTypoRec = genericHelper "getAllTypoRec" . const . liftIO . getDbTblRecs $ "typo"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/typo/1 -v
-}
    deleteTypoRec :: HasCallStack => CaptureInt -> Handler NoContent
    deleteTypoRec ci = deleteHelper ci "deleteTypoRec" "typo"

    -----

{-
curl -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/word/all -v
-}
    getAllWordRec :: HasCallStack => Handler [WordRec]
    getAllWordRec = genericHelper "getAllWordRec" . const . liftIO . getDbTblRecs $ "words"

{-
curl -X DELETE \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer tokenHere" \
     localhost:7249/db/word/all -v
-}
    deleteAllWordRec :: HasCallStack => Handler NoContent
    deleteAllWordRec = deleteAllHelper "deleteAllWordRec" "words"
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
