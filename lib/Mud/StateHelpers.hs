{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}

module Mud.StateHelpers ( BothGramNos
                        , getEntBothGramNos
                        , getPlaColumns
                        , getWS
                        , mkAssocListTxt
                        , mkCoinsFromList
                        , mkDividerTxt
                        , mkListFromCoins
                        , mkPlurFromBoth
                        , modifyWS
                        , negateCoins
                        , onWS
                        , putArm
                        , putCloth
                        , putCon
                        , putMob
                        , putObj
                        , putPC
                        , putPla
                        , putRm
                        , putWpn
                        , send
                        , sortInv ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, readTMVar, takeTMVar, TMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (_1, at, each, folded)
import Control.Lens.Operators ((%~), (&), (?~), (^.), (^.), (^..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (gets)
import Data.Functor ((<$>))
import Data.IntMap.Lazy ((!))
import Data.List (sortBy)
import Data.Monoid ((<>))
import qualified Data.Text as T


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
-- Helper functions for registering world elements:


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ObjType & entTbl.at i ?~ e & objTbl.at i ?~ o


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ClothType & entTbl.at i ?~ e & objTbl.at i ?~ o & clothTbl.at i ?~ c


putCon :: Id -> Ent -> Obj -> Inv -> Coins -> Con -> MudStack ()
putCon i e o is coi con = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ConType & entTbl.at i ?~ e & objTbl.at i ?~ o & invTbl.at i ?~ is & coinsTbl.at i ?~ coi & conTbl.at i ?~ con


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ WpnType & entTbl.at i ?~ e & objTbl.at i ?~ o & wpnTbl.at i ?~ w


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ArmType & entTbl.at i ?~ e & objTbl.at i ?~ o & armTbl.at i ?~ a


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ MobType & entTbl.at i ?~ e & invTbl.at i ?~ is & coinsTbl.at i ?~ c & eqTbl.at i ?~ em & mobTbl.at i ?~ m


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ PCType & entTbl.at i ?~ e & invTbl.at i ?~ is & coinsTbl.at i ?~ c & eqTbl.at i ?~ em & mobTbl.at i ?~ m & pcTbl.at i ?~ p


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ RmType & invTbl.at i ?~ is & coinsTbl.at i ?~ c & rmTbl.at i ?~ r


-- ============================================================
-- Helper functions for working with "Pla":


putPla :: Id -> Pla -> MudStack () -- TODO: Currently not used.
putPla i p = liftIO . atomically . transaction =<< gets (^.nonWorldState.plaTblTMVar)
  where
    transaction t = takeTMVar t >>= \pt ->
        putTMVar t (pt & at i ?~ p)


getPla :: Id -> MudStack Pla
getPla i = (! i) <$> (liftIO . atomically . readTMVar =<< gets (^.nonWorldState.plaTblTMVar))


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = (^.columns) <$> getPla i


-- ============================================================
-- Misc. helpers:


sortInv :: WorldState -> Inv -> Inv -- TODO: Should we be using this more?
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


-- ============================================================
-- Helper functions relating to output:


send :: MsgQueue -> T.Text -> MudStack ()
send mq = liftIO . atomically . writeTQueue mq . FromServer


{-
output :: MsgQueueId -> [T.Text] -> MudStack ()
output (mq, i) ts = getPlaColumns i >>= \cols ->
    liftIO . atomically . writeTQueue mq . FromServer . T.unlines . concatMap (wordWrap cols) $ ts


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
-}


mkDividerTxt :: Cols -> T.Text
mkDividerTxt = flip T.replicate "="


mkAssocListTxt :: (Show a, Show b) => Cols -> [(a, b)] -> T.Text
mkAssocListTxt cols = T.concat . map helper
  where
    helper (a, b) = T.unlines . wordWrapIndent 2 cols $ (unquote . showText $ a) <> ": " <> showText b
