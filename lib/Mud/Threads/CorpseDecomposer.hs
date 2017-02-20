{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings #-}

module Mud.Threads.CorpseDecomposer ( pauseCorpseDecomps
                                    , restartCorpseDecomps
                                    , startCorpseDecomp ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Destroy
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)
import qualified Mud.Util.Misc as U (blowUp)

import Control.Arrow (first)
import Control.Concurrent.Async (asyncThreadId, wait)
import Control.Exception (Exception)
import Control.Exception.Lifted (catch, finally, handle, throwTo)
import Control.Lens (at, both, set, views)
import Control.Lens.Operators ((?~), (.~), (&), (%~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.IntMap.Strict as IM (elems, empty, toList)
import qualified Data.Text as T


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Threads.CorpseDecomposer"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.CorpseDecomposer"


-- ==================================================


data PauseCorpseDecomp = PauseCorpseDecomp deriving (Show, Typeable)


instance Exception PauseCorpseDecomp


-----


startCorpseDecomp :: Id -> SecondsPair -> MudStack ()
startCorpseDecomp i secs = runAsync (threadCorpseDecomp i secs) >>= \a -> tweak $ corpseDecompAsyncTbl.ind i .~ a


threadCorpseDecomp :: Id -> SecondsPair -> MudStack ()
threadCorpseDecomp i secs = handle (threadExHandler (Just i) "corpse decomposer") $ do
    setThreadType . CorpseDecomposer $ i
    singId <- descSingId i <$> getState
    logNotice "threadCorpseDecomp" . prd $ "starting corpse decomposer for " <> singId |<>| mkSecsTxt secs
    handle (die Nothing $ "corpse decomposer for " <> singId) $ corpseDecomp i secs `finally` finish
  where
    finish = tweak $ corpseDecompAsyncTbl.at i .~ Nothing


mkSecsTxt :: SecondsPair -> Text
mkSecsTxt = parensQuote . uncurry (middle (<>) "/") . (both %~ commaShow)


corpseDecomp :: Id -> SecondsPair -> MudStack ()
corpseDecomp i pair = getObjWeight i <$> getState >>= \w -> catch <$> loop w <*> handler =<< liftIO (newIORef pair)
  where
    loop w ref = liftIO (readIORef ref) >>= \case
      (0, _) -> logHelper ("corpse decomposer for ID " <> showText i <> " has expired.") >> finishDecomp i
      secs   -> do corpseDecompHelper i w secs
                   liftIO . delaySecs $ 1
                   liftIO . writeIORef ref . first pred $ secs
                   loop w ref
    handler :: IORef SecondsPair -> PauseCorpseDecomp -> MudStack ()
    handler ref = const $ liftIO (readIORef ref) >>= \secs ->
      let msg = prd $ "pausing corpse decomposer for ID " <> showText i |<>| mkSecsTxt secs
      in logHelper msg >> tweak (pausedCorpseDecompsTbl.ind i .~ secs)
    logHelper = logNotice "corpseDecomp finish"


corpseDecompHelper :: Id -> Weight -> SecondsPair -> MudStack ()
corpseDecompHelper i w (x, total) = getState >>= \ms ->
    let step           = total `intDivide` 4
        [ a, b, c, d ] = [ step, step * 2, step * 3, total ]
        ipc            = isPCCorpse . getCorpse i $ ms
        lens           = bool npcCorpseDesc pcCorpseDesc ipc
    in tweaks $ if
      | x == d -> [ corpseTbl.ind i.lens      .~ mkCorpseTxt ("You see the lifeless ", ".")
                  , entTbl   .ind i.entSmell  ?~ corpseSmellLvl1
                  , objTbl   .ind i.objTaste  ?~ thrice prd "Really? What did you expect? At least the corpse hasn't \
                                                            \decomposed much yet" ]
      | x == c -> [ corpseTbl.ind i.lens      .~ mkCorpseTxt ("The ", " has begun to decompose.")
                  , entTbl   .ind i.entSmell  ?~ corpseSmellLvl2
                  , objTbl   .ind i.objTaste  ?~ "As you may have anticipated, the taste of the decomposing corpse is \
                                                 \decidedly unappetizing."
                  , objTbl   .ind i.objWeight .~ minusTenth w ]
      | x == b -> [ corpseTbl.ind i.lens      .~ mkCorpseTxt ("The ", " has decomposed significantly.")
                  , entTbl   .ind i.entSmell  ?~ corpseSmellLvl3
                  , objTbl   .ind i.objTaste  ?~ "The decomposing corpse could very well be the most vile thing you \
                                                 \have ever tasted in your life."
                  , objTbl   .ind i.objWeight .~ minusQuarter w ]
      | x == a -> [ corpseTbl.ind i.lens      .~ "The unidentifiable corpse is in an advanced stage of decomposition."
                  , entTbl   .ind i.entSmell  ?~ corpseSmellLvl4
                  , objTbl   .ind i.objTaste  ?~ "Ugh! Why? WHY?"
                  , objTbl   .ind i.objWeight .~ minusHalf w
                  , entTbl   .ind i.sing      .~ "decomposed corpse"
                  , corpseTbl.ind i           %~ (ipc ? set pcCorpseSing corpsePlaceholder :? id) ]
      | otherwise -> []


finishDecomp :: Id -> MudStack ()
finishDecomp i = modifyStateSeq $ \ms ->
    let invId          = fromMaybe oops . findInvContaining i $ ms
        bs             = if | getType invId ms == RmType -> foldr f [] . findMobIds ms . getInv invId $ ms
                            | isNpcPC invId ms           -> mkCarriedBs
                            | otherwise                  -> []
        f targetId acc | isPC targetId ms = let n = mkCorpseAppellation targetId ms i
                                            in (("The " <> n <> " disintegrates.", pure targetId) : acc)
                       | otherwise        = acc
        mkCarriedBs    = let n = mkCorpseAppellation invId ms i
                         in pure (T.concat [ "The ", n, " ", parensQuote "carried", " disintegrates." ], pure invId)
        oops           = blowUp "finishDecomp" (descSingId i ms <> " is in limbo") ""
    in (ms, [ destroy . pure $ i, bcastNl bs ])


-----


pauseCorpseDecomps :: MudStack ()
pauseCorpseDecomps = do logNotice "pauseCorpseDecomps" "pausing corpse decomposers."
                        views corpseDecompAsyncTbl (mapM_ f . IM.elems) =<< getState
  where
    f :: CorpseDecompAsync -> MudStack ()
    f a = sequence_ [ throwTo (asyncThreadId a) PauseCorpseDecomp, liftIO . void . wait $ a ]


-----


restartCorpseDecomps :: MudStack ()
restartCorpseDecomps = do logNotice "restartCorpseDecomps" "restarting corpse decomposers."
                          modifyStateSeq $ \ms -> let pairs = views pausedCorpseDecompsTbl IM.toList ms
                                                  in ( ms & pausedCorpseDecompsTbl .~ IM.empty
                                                     , pure . mapM_ (uncurry startCorpseDecomp) $ pairs )
