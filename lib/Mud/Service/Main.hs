{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Mud.Service.Main (startService) where

import           Mud.Data.State.MudData
import           Mud.Service.Server
import           Mud.Service.Types
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Data.IORef (IORef)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T
import           GHC.Stack (HasCallStack)
import           Network.Wai.Handler.Warp (run)
import           Servant (Context(..), Proxy(..), serveWithContext)
import           Servant.Auth.Server (JWT, defaultCookieSettings, defaultJWTSettings, generateKey)


startService :: HasCallStack => IORef MudState -> IO ()
startService ior = (defaultJWTSettings <$> generateKey) >>= \jwtCfg ->
    let cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        api = Proxy :: Proxy (API '[JWT])
    in do void . forkIO . run restServicePort . serveWithContext api cfg . server ior defaultCookieSettings $ jwtCfg
          T.putStrLn . prd $ "REST API service started " <> parensQuote ("http://localhost:" <> showTxt restServicePort)
