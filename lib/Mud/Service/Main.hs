{-# LANGUAGE OverloadedStrings #-}

module Mud.Service.Main (startService) where

import           Mud.Data.State.MudData
import           Mud.Service.Server
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T (putStrLn)
import           Network.Wai (Application) -- TODO: responseLBS
import           Network.Wai.Handler.Warp (run)
import           Servant (serve)

-- TODO: Delete.
{-
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lazy (ByteString)
import           Data.IORef (IORef, atomicModifyIORef', readIORef)
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Types (ok200)
import           Servant (err404, errBody)
import           Servant.Docs (DocIntro(..), docsWithIntros)
import           Servant.Docs.Pandoc (pandoc)
import           Text.Pandoc (writeHtmlString)
import           Text.Pandoc.Options (def)
import qualified Data.IntMap.Lazy as IM (IntMap, elems, insert, keys, lookup, member)
import qualified Data.Text.Lazy as T
-}


-- TODO: HasCallStack
startService :: MudData -> IO ()
startService _ = do void . forkIO . run servicePort $ app
                    T.putStrLn . prd $ "Service started " <> parensQuote ("http://localhost:" <> showTxt servicePort)


app :: Application
app = serve adminAPI server
