{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings #-}

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


-- TODO: Delete.
{-
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Types (ok200)
import           Servant.Docs (DocIntro(..), docsWithIntros)
import           Servant.Docs.Pandoc (pandoc)
import           Text.Pandoc (writeHtmlString)
import           Text.Pandoc.Options (def)
-}


startService :: HasCallStack => IORef MudState -> IO ()
startService ior = generateKey >>= \myKey -> do
    let jwtCfg = defaultJWTSettings myKey
        cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
        api    = Proxy :: Proxy (API '[JWT])
    void . forkIO . run servicePort . serveWithContext api cfg . server ior defaultCookieSettings $ jwtCfg
    T.putStrLn . prd $ "Service started " <> parensQuote ("http://localhost:" <> showTxt servicePort)

{-
TODO: Use this?
    makeJWT (Login un pw) jwtCfg Nothing >>= \case
      Left  e -> T.putStrLn $ "Error generating token: " <> showTxt e
      Right v -> T.putStrLn $ "New token: "              <> showTxt v
-}
