{-# LANGUAGE OverloadedStrings #-}

module Mud.Service.Main (startService) where

import           Mud.Data.State.MudData
import           Mud.Service.Server
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Data.IORef (IORef)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T (putStrLn)
import           GHC.Stack (HasCallStack)
import           Network.Wai (Application) -- TODO: responseLBS
import           Network.Wai.Handler.Warp (run)
import           Servant (serve)


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
startService ior = do void . forkIO . run servicePort . app $ ior
                      T.putStrLn . prd $ "Service started " <> parensQuote ("http://localhost:" <> showTxt servicePort)


app :: HasCallStack => IORef MudState -> Application
app = serve adminAPI . server
