{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings #-}

module Mud.Threads.CorpseDecomposer ( pauseCorpseDecomps
                                    , restartCorpseDecomps
                                    , startCorpseDecomp ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (finally, handle)
import Control.Lens (at, both, set, views)
import Control.Lens.Operators ((%~), (&), (.~), (?~))
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.IntMap.Lazy as IM (elems, empty, toList)


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.CorpseDecomposer"


-- ==================================================


startCorpseDecomp :: Id -> SecondsPair -> MudStack ()
startCorpseDecomp i secs = runAsync (threadCorpseDecomp i secs) >>= \a -> tweak $ corpseDecompAsyncTbl.ind i .~ a


threadCorpseDecomp :: Id -> SecondsPair -> MudStack ()
threadCorpseDecomp i secs = handle (threadExHandler (Just i) "corpse decomposer") $ do
    setThreadType . CorpseDecomposer $ i
    let msg = prd $ "starting corpse decomposer for ID " <> showText i |<>| mkSecsTxt secs
    logNotice "threadCorpseDecomp" msg
    handle (die Nothing "corpse decomposer") $ corpseDecomp i secs `finally` finish
  where
    finish = tweak $ corpseDecompAsyncTbl.at i .~ Nothing


mkSecsTxt :: SecondsPair -> Text
mkSecsTxt = parensQuote . uncurry (middle (<>) "/") . (both %~ commaShow)


corpseDecomp :: Id -> SecondsPair -> MudStack ()
corpseDecomp i pair = finally <$> loop <*> finish =<< liftIO (newIORef pair)
  where
    loop ref = liftIO (readIORef ref) >>= \case
      (0, _) -> unit
      secs   -> do corpseDecompHelper i secs
                   liftIO . threadDelay $ 1 * 10 ^ 6
                   liftIO . writeIORef ref . first pred $ secs
                   loop ref
    finish ref = liftIO (readIORef ref) >>= \case
      (0, _) -> logHelper $ "corpse decomposer for ID " <> showText i <> " has expired."
      secs   -> let msg = prd $ "pausing corpse decomposer for ID " <> showText i |<>| mkSecsTxt secs
                in logHelper msg >> tweak (pausedCorpseDecompsTbl.ind i .~ secs)
      where
        logHelper = logNotice "corpseDecomp finish"


corpseDecompHelper :: Id -> SecondsPair -> MudStack ()
corpseDecompHelper i (x, total) =
    let step           = total `intDivide` 4
        [ a, b, c, d ] = [ step, step * 2, step * 3, total ]
    in isPCCorpse . getCorpse i <$> getState >>= \ipc -> do
        let lens = bool npcCorpseDesc pcCorpseDesc ipc
        if | x == d ->
                tweaks [ corpseTbl.ind i.lens     .~ mkCorpseTxt ("You see the lifeless ", ".")
                       , entTbl   .ind i.entSmell ?~ corpseSmellLvl1
                       , objTbl   .ind i.objTaste ?~ (thrice prd "What did you expect? At least the corpse hasn't \
                                                                 \yet decomposed much") ]
            | x == c ->
                tweaks [ corpseTbl.ind i.lens     .~ mkCorpseTxt ("The ", " has begun to decompose.")
                       , entTbl   .ind i.entSmell ?~ corpseSmellLvl2
                       , objTbl   .ind i.objTaste ?~ "As you may have anticipated, the taste of the decomposing corpse \
                                                     \is decidedly unappetizing." ]
            | x == b ->
                tweaks [ corpseTbl.ind i.lens     .~ mkCorpseTxt ("The ", " has decomposed significantly.")
                       , entTbl   .ind i.entSmell ?~ corpseSmellLvl3
                       , objTbl   .ind i.objTaste ?~ "The decomposing corpse could very well be the most vile thing \
                                                     \you have ever tasted in your life." ]
            | x == a ->
                tweaks [ corpseTbl.ind i.lens     .~ "The unidentifiable corpse is in an advanced stage of decomposition."
                       , entTbl   .ind i.entSmell ?~ corpseSmellLvl4
                       , objTbl   .ind i.objTaste ?~ "Ugh! Why? WHY?"
                       , entTbl   .ind i.sing     .~ "decomposed corpse"
                       , corpseTbl.ind i          %~ (ipc ? set pcCorpseSing corpsePlaceholder :? id) ]
            | otherwise -> unit


-----


pauseCorpseDecomps :: MudStack ()
pauseCorpseDecomps = do logNotice "pauseCorpseDecomps" "pausing corpse decomposers."
                        views corpseDecompAsyncTbl (mapM_ throwWait . IM.elems) =<< getState


-----


restartCorpseDecomps :: MudStack ()
restartCorpseDecomps = do logNotice "restartCorpseDecomps" "restarting corpse decomposers."
                          modifyStateSeq $ \ms -> let pairs = views pausedCorpseDecompsTbl IM.toList ms
                                                  in ( ms & pausedCorpseDecompsTbl .~ IM.empty
                                                     , pure . mapM_ (uncurry startCorpseDecomp) $ pairs )
