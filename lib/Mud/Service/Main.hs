{-# LANGUAGE DataKinds, OverloadedStrings #-}

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
import           Data.IORef (IORef, atomicModifyIORef)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T
import           GHC.Stack (HasCallStack)
import           Network.Wai.Handler.Warp (run)
import           Prelude hiding (log)
import           Servant (Context(..), Proxy(..), serveWithContext)
import           Servant.Auth.Server (JWT, defaultCookieSettings, defaultJWTSettings, generateKey)


startRestService :: HasCallStack => DoOrDon'tLog -> IORef MudState -> IO () -- TODO: Exception handling.
startRestService log ior = defaultJWTSettings <$> generateKey >>= \jwtCfg ->
    let cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        api = Proxy :: Proxy (API '[JWT])
    in do void . forkIO . run restServicePort . serveWithContext api cfg . server ior defaultCookieSettings $ jwtCfg
          let msg = prd $ "REST API service started " <> parensQuote ("http://localhost:" <> showTxt restServicePort)
          T.putStrLn msg
          when (log == DoLog) $ do logService <- initRestServiceLogging log
                                   ms         <- atomicModifyIORef ior (dup . (restServiceLogService .~ logService))
                                   logRestServiceSimple ms "startRestService" msg
