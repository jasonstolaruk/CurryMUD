{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}

module Mud.Threads.CorpseDecomposer ( pauseCorpseDecomps
                                    , restartCorpseDecomps
                                    , startCorpseDecomp ) where

import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (finally, handle)
import Control.Lens (at, both, views)
import Control.Lens.Operators ((%~), (&), (.~))
-- import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (elems, empty)
-- import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.CorpseDecomposer"


-- ==================================================


startCorpseDecomp :: Id -> Seconds -> MudStack ()
startCorpseDecomp i secs = runAsync (threadCorpseDecomp i secs) >>= \a -> tweak $ corpseDecompAsyncTbl.ind i .~ a


threadCorpseDecomp :: Id -> Seconds -> MudStack ()
threadCorpseDecomp i secs = handle (threadExHandler (Just i) "corpse decomposer") $ do
    setThreadType . CorpseDecomposer $ i
    let msg = prd $ "starting corpse decomposer for ID " <> showText i |<>| parensQuote (commaShow secs <> " seconds")
    logNotice "threadCorpseDecomp" msg
    handle (die (Just i) "corpse decomposer") $ corpseDecomp i (dup secs) `finally` finish
  where
    finish = tweak $ corpseDecompAsyncTbl.at i .~ Nothing


corpseDecomp :: Id -> (Seconds, Seconds) -> MudStack ()
corpseDecomp i pair = finally <$> loop <*> finish =<< liftIO (newIORef pair)
  where
    loop ref = liftIO (readIORef ref) >>= \case
      (_, 0) -> unit
      p      -> do corpseDecompHelper i p
                   liftIO . threadDelay $ 1 * 10 ^ 6
                   liftIO . atomicModifyIORef' ref $ ((, ()) . second pred)
                   loop ref
    finish ref = liftIO (readIORef ref) >>= \case
      (_, 0) -> logHelper $ "corpse decomposer for ID " <> showText i <> " has expired."
      p      -> let msg     = prd $ "pausing corpse decomposer for ID " <> showText i |<>| parensQuote secsTxt
                    secsTxt = uncurry (middle (<>) "/") . (both %~ commaShow) $ p
                in logHelper msg >> tweak (pausedCorpseDecompsTbl.ind i .~ pair)
    logHelper = logNotice "corpseDecomp finish"


corpseDecompHelper :: Id -> (Seconds, Seconds) -> MudStack ()
corpseDecompHelper _ _ = unit


-----


pauseCorpseDecomps :: MudStack ()
pauseCorpseDecomps = do logNotice "pauseCorpseDecomps" "pausing corpse decomposers."
                        modifyStateSeq $ \ms -> let asyncs = views corpseDecompAsyncTbl IM.elems ms
                                                in ( ms & corpseDecompAsyncTbl .~ IM.empty
                                                   , pure . mapM_ throwWait $ asyncs )


-----


restartCorpseDecomps :: MudStack ()
restartCorpseDecomps = unit
