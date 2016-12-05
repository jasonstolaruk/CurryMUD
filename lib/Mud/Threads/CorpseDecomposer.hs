{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}

module Mud.Threads.CorpseDecomposer (startCorpseDecomp) where
-- TODO: pauseCorpseDecomps
-- TODO: restartCorpseDecomps

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
import Control.Lens (at, both)
import Control.Lens.Operators ((%~), (&), (.~))
-- import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
-- import qualified Data.IntMap.Lazy as IM (keys, toList)
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
      (_, _) -> do
          liftIO . threadDelay $ 1 * 10 ^ 6
          liftIO . atomicModifyIORef' ref $ ((, ()) . second pred)
          loop ref
    finish ref = liftIO (readIORef ref) >>= \case
      (_, 0) -> logNotice "corpseDecomp" $ "corpse decomposer for ID " <> showText i <> " is finishing."
      p      -> let secsTxt = uncurry (middle (<>) "/") (p & both %~ commaShow)
                    msg     = prd $ "pausing corpse decomposer for ID " <> showText i |<>| parensQuote secsTxt
                in do
                    logNotice "corpseDecomp" msg
                    tweak $ pausedCorpseDecompsTbl.ind i .~ pair
