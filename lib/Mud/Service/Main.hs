{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings #-}

module Mud.Service.Main (startRestService) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Service.Logging
import           Mud.Service.Server
import           Mud.Service.Types
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent (forkIO)
import           Control.Lens.Operators ((.~))
import           Control.Monad (void, when)
import           Crypto.JOSE.JWK (JWK)
import           Data.Aeson (decode)
import           Data.ByteString.Lazy.Char8 as BL
import           Data.IORef (IORef, atomicModifyIORef)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T
import           GHC.Stack (HasCallStack)
import           Network.Wai.Handler.Warp (run)
import           Prelude hiding (log)
import           Servant (Context(..), Proxy(..), serveWithContext)
import           Servant.Auth.Server (JWT, defaultCookieSettings, defaultJWTSettings, generateKey)
import           System.Environment (lookupEnv)


startRestService :: HasCallStack => DoOrDon'tLog -> IORef MudState -> IO ()
startRestService log ior = defaultJWTSettings <$> getJWK >>= \jwtCfg ->
    let cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        api = Proxy :: Proxy (API '[JWT])
    in do void . forkIO . run restServicePort . serveWithContext api cfg . server ior defaultCookieSettings $ jwtCfg
          let msg = prd $ "REST API service started " <> parensQuote ("http://localhost:" <> showTxt restServicePort)
          T.putStrLn msg
          when (log == DoLog) $ do logService <- initRestServiceLogging log
                                   ms         <- atomicModifyIORef ior (dup . (restServiceLogService .~ logService))
                                   logRestServiceSimple ms "startRestService" msg


getJWK :: HasCallStack => IO JWK -- Java Web Key
getJWK = generateKey >>= \dflt ->
    let oops msg = T.putStrLn (msg <> "; generating a new JWK.") >> return dflt
    in lookupEnv "JWK" >>= \case
      Nothing -> oops $ "Environment variable " <> dblQuote "JWK" <> " not found"
      Just x  -> let oops' = oops $ "Unable to decode the value of the " <> dblQuote "JWK" <> " environment variable"
                 in maybeRet oops' . decode . BL.pack $ x
