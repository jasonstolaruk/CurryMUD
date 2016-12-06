{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.CorpseDecomposer ( pauseCorpseDecomps
                                    , restartCorpseDecomps
                                    , startCorpseDecomp ) where

import Mud.Data.State.MudData
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
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (elems, empty, toList)


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.CorpseDecomposer"


-- ==================================================


type SecondsPair      = (TotalSeconds, RemainingSeconds)
type TotalSeconds     = Seconds
type RemainingSeconds = Seconds


-----


startCorpseDecomp :: Id -> SecondsPair -> MudStack ()
startCorpseDecomp i secs = runAsync (threadCorpseDecomp i secs) >>= \a -> tweak $ corpseDecompAsyncTbl.ind i .~ a


threadCorpseDecomp :: Id -> SecondsPair -> MudStack ()
threadCorpseDecomp i secs = handle (threadExHandler (Just i) "corpse decomposer") $ do
    setThreadType . CorpseDecomposer $ i
    let msg = prd $ "starting corpse decomposer for ID " <> showText i |<>| mkSecsTxt secs
    logNotice "threadCorpseDecomp" msg
    handle (die (Just i) "corpse decomposer") $ corpseDecomp i secs `finally` finish
  where
    finish = tweak $ corpseDecompAsyncTbl.at i .~ Nothing


mkSecsTxt :: SecondsPair -> Text
mkSecsTxt = parensQuote . uncurry (middle (<>) "/") . (both %~ commaShow)


corpseDecomp :: Id -> SecondsPair -> MudStack ()
corpseDecomp i pair = finally <$> loop <*> finish =<< liftIO (newIORef pair)
  where
    loop ref = liftIO (readIORef ref) >>= \case
      (_, 0) -> unit
      secs   -> do corpseDecompHelper i secs
                   liftIO . threadDelay $ 1 * 10 ^ 6
                   liftIO . writeIORef ref . second pred $ secs
                   loop ref
    finish ref = liftIO (readIORef ref) >>= \case
      (_, 0) -> logHelper $ "corpse decomposer for ID " <> showText i <> " has expired."
      secs   -> let msg = prd $ "pausing corpse decomposer for ID " <> showText i |<>| mkSecsTxt secs
                in logHelper msg >> tweak (pausedCorpseDecompsTbl.ind i .~ secs)
    logHelper = logNotice "corpseDecomp finish"


corpseDecompHelper :: Id -> SecondsPair -> MudStack ()
corpseDecompHelper _ _ = unit


-----


pauseCorpseDecomps :: MudStack ()
pauseCorpseDecomps = do logNotice "pauseCorpseDecomps" "pausing corpse decomposers."
                        modifyStateSeq $ \ms -> let asyncs = views corpseDecompAsyncTbl IM.elems ms
                                                in ( ms & corpseDecompAsyncTbl .~ IM.empty
                                                   , pure . mapM_ throwWait $ asyncs )


-----


restartCorpseDecomps :: MudStack ()
restartCorpseDecomps = do logNotice "restartCorpseDecomps" "restarting corpse decomposers."
                          modifyStateSeq $ \ms -> let pairs = views pausedCorpseDecompsTbl IM.toList ms
                                                  in ( ms & pausedCorpseDecompsTbl .~ IM.empty
                                                     , pure . mapM_ (uncurry startCorpseDecomp) $ pairs )
