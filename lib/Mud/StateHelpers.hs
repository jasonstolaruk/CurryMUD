{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}

module Mud.StateHelpers ( BothGramNos
                        , dispAssocList
                        , dispGenericErrorMsg
                        , divider
                        , dumpFile
                        , dumpFileWithDividers
                        , getEntBothGramNos
                        , getWS
                        , mkCoinsFromList
                        , mkListFromCoins
                        , mkPlurFromBoth
                        , modifyWS
                        , negateCoins
                        , onWS
                        , output
                        , outputCon
                        , outputConIndent
                        , outputIndent
                        , sortInv ) where

import Mud.StateDataTypes
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, readTMVar, takeTMVar, TMVar)
import Control.Lens (_1, each, folded)
import Control.Lens.Operators ((%~), (^.), (^..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (gets)
import Data.IntMap.Lazy ((!))
import Data.List (sortBy)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStr, putStrLn, readFile)


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.StateHelpers"


-- ============================================================
-- Higher level abstractions for working with STM:


getWS :: MudStack WorldState
getWS = liftIO . atomically . readTMVar =<< gets (^.worldStateTMVar)


onWS :: ((TMVar WorldState, WorldState) -> STM a) -> MudStack a
onWS f = liftIO . atomically . transaction =<< gets (^.worldStateTMVar)
  where
    transaction t = takeTMVar t >>= \ws ->
        f (t, ws)


modifyWS :: (WorldState -> WorldState) -> MudStack ()
modifyWS f = liftIO . atomically . transaction =<< gets (^.worldStateTMVar)
  where
    transaction t = takeTMVar t >>= \ws ->
        let ws' = f ws
        in putTMVar t ws'


-- ============================================================
-- Misc. helpers:


sortInv :: WorldState -> Inv -> Inv
sortInv ws is = ((^..folded._1) . sortBy nameThenSing) zipped
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped = zip3 is names sings
    names  = [ let e = (ws^.entTbl) ! i in e^.name | i <- is ]
    sings  = [ let e = (ws^.entTbl) ! i in e^.sing | i <- is ]


type BothGramNos = (Sing, Plur)


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkListFromCoins :: Coins -> [Int]
mkListFromCoins (Coins (c, g, s)) = [c, g, s]


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [cop, sil, gol] = Coins (cop, sil, gol)
mkCoinsFromList xs              = patternMatchFail "mkCoinsFromList" [ showText xs ]


negateCoins :: Coins -> Coins
negateCoins (Coins c) = Coins (each %~ negate $ c)


getPla :: Id -> MudStack Pla
getPla i = gets (^.nonWorldState.plaTbl) >>= \pt -> return (pt ! i)


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = (^.columns) <$> getPla i


-- ==================================================
-- "output" and related helpers:


output :: T.Text -> MudStack ()
output t = getPlaColumns 0 >>= \cols ->
    liftIO . T.putStr . T.unlines . concatMap (wordWrap cols) . T.lines $ t


outputIndent :: Int -> T.Text -> MudStack ()
outputIndent n t = getPlaColumns 0 >>= \cols ->
    liftIO . T.putStr . T.unlines . concatMap (wordWrapIndent n cols) . T.lines $ t


outputCon :: [T.Text] -> MudStack () -- Prefer over "output" when there would be more than two "<>"s.
outputCon ts = getPlaColumns 0 >>= \cols ->
    liftIO . T.putStrLn . T.concat . wordWrap cols . T.concat $ ts


outputConIndent :: Int -> [T.Text] -> MudStack ()
outputConIndent n ts = getPlaColumns 0 >>= \cols ->
    liftIO . T.putStrLn . T.concat . wordWrapIndent n cols . T.concat $ ts


dumpFile :: FilePath -> MudStack () -- TODO: Implement paging.
dumpFile fn = takeADump =<< (liftIO . T.readFile $ fn)
  where
    takeADump contents = getPlaColumns 0 >>= \cols ->
        liftIO . T.putStr . T.unlines . concat . wordWrapLines cols . T.lines $ contents


dumpFileWithDividers :: FilePath -> MudStack ()
dumpFileWithDividers fn = divider >> dumpFile fn >> divider


divider :: MudStack ()
divider = getPlaColumns 0 >>= \cols ->
    liftIO . T.putStrLn . T.replicate cols $ "="


dispAssocList :: (Show a, Show b) => [(a, b)] -> MudStack ()
dispAssocList = mapM_ takeADump
  where
    takeADump (a, b) = outputIndent 2 $ (unquote . showText $ a) <> ": " <> showText b


dispGenericErrorMsg :: MudStack ()
dispGenericErrorMsg = output "Unfortunately, an error occured while executing your command."
