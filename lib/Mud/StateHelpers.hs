{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Mud.StateHelpers ( allKeys
                        , BothGramNos
                        , broadcast
                        , findPCIds
                        , getEntBothGramNos
                        , getLog
                        , getLogAsyncs
                        , getNWS
                        , getNWSTMVar
                        , getPlaColumns
                        , getUnusedId
                        , getWS
                        , getWSTMVar
                        , mkAssocListTxt
                        , mkCoinsFromList
                        , mkDividerTxt
                        , mkListFromCoins
                        , mkPlurFromBoth
                        , modifyNWS
                        , modifyWS
                        , negateCoins
                        , onNWS
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
import Mud.StateInIORefT
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative (Const)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, readTMVar, takeTMVar, TMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (_1, at, each, to)
import Control.Lens.Operators ((%~), (&), (?~), (^.), (^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (gets)
import Control.Monad.State.Class (MonadState)
import Data.Functor ((<$>))
import Data.IntMap.Lazy ((!))
import Data.List ((\\), sortBy)
import Data.Monoid ((<>))
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.StateHelpers"


-- ============================================================
-- Higher level abstractions for working with STM:


getWSTMVar :: StateInIORefT MudState IO (TMVar WorldState)
getWSTMVar = gets (^.worldStateTMVar)


getNWSTMVar :: forall a (m :: * -> *) . MonadState MudState m => ((a -> Const a a) -> NonWorldState -> Const a NonWorldState) -> m a
getNWSTMVar lens = gets (^.nonWorldState.lens)


getWS :: MudStack WorldState
getWS = liftIO . atomically . readTMVar =<< getWSTMVar


onWS :: ((TMVar WorldState, WorldState) -> STM a) -> MudStack a
onWS f = liftIO . atomically . transaction =<< getWSTMVar
  where
    transaction t = takeTMVar t >>= \ws ->
        f (t, ws)


modifyWS :: (WorldState -> WorldState) -> MudStack ()
modifyWS f = liftIO . atomically . transaction =<< getWSTMVar
  where
    transaction t = takeTMVar t >>= putTMVar t . f


getNWS :: forall (m :: * -> *) a . (MonadIO m, MonadState MudState m) => ((TMVar a -> Const (TMVar a) (TMVar a)) -> NonWorldState -> Const (TMVar a) NonWorldState) -> m a
getNWS lens = liftIO . atomically . readTMVar =<< getNWSTMVar lens


onNWS :: forall t (m :: * -> *) a . (MonadIO m, MonadState MudState m) => ((TMVar t -> Const (TMVar t) (TMVar t)) -> NonWorldState -> Const (TMVar t) NonWorldState) -> ((TMVar t, t) -> STM a) -> m a
onNWS lens f = liftIO . atomically . transaction =<< getNWSTMVar lens
  where
    transaction t = takeTMVar t >>= \x ->
        f (t, x)


modifyNWS :: forall a (m :: * -> *) . (MonadIO m, MonadState MudState m) => ((TMVar a -> Const (TMVar a) (TMVar a)) -> NonWorldState -> Const (TMVar a) NonWorldState) -> (a -> a) -> m ()
modifyNWS lens f = liftIO . atomically . transaction =<< getNWSTMVar lens
  where
    transaction t = takeTMVar t >>= putTMVar t . f


getLog :: forall a (m :: * -> *) . MonadState MudState m => ((a -> Const a a) -> LogServices -> Const a LogServices) -> m a
getLog l = gets (^.nonWorldState.logServices.l)


getLogAsyncs :: MudStack (LogAsync, LogAsync)
getLogAsyncs = helper <$> gets (^.nonWorldState.logServices)
  where
    helper ls = let Just (nla, _) = ls^.noticeLog
                    Just (ela, _) = ls^.errorLog
                in (nla, ela)


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
putPla i p = modifyNWS plaTblTMVar $ \pt -> pt & at i ?~ p


getPla :: Id -> MudStack Pla
getPla i = (! i) <$> getNWS plaTblTMVar


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = (^.columns) <$> getPla i


-- ============================================================
-- Misc. helpers:


allKeys :: WorldState -> Inv
allKeys = (^.typeTbl.to IM.keys)


getUnusedId :: WorldState -> Id
getUnusedId = head . (\\) [0..] . allKeys


sortInv :: WorldState -> Inv -> Inv
sortInv ws is = let ts         = [ (ws^.typeTbl) ! i | i <- is ]
                    pcIs       = map fst . filter ((== PCType) . snd) . zip is $ ts
                    sortedPCIs = map fst . sortBy pcSorter . zip pcIs . names $ pcIs
                in (sortedPCIs ++) . sortNonPCs . deleteFirstOfEach pcIs $ is
  where
    names is'                          = [ let e = (ws^.entTbl) ! i in e^.name | i <- is' ]
    pcSorter (_, n) (_, n')            = n `compare` n'
    sortNonPCs is'                     = map (^._1) . sortBy nameThenSing . zip3 is' (names is') . sings $ is'
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    sings is'                          = [ let e = (ws^.entTbl) ! i in e^.sing | i <- is' ]


type BothGramNos = (Sing, Plur)


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkListFromCoins :: Coins -> [Int]
mkListFromCoins (Coins (c, g, s)) = [ c, g, s ]


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [ cop, sil, gol ] = Coins (cop, sil, gol)
mkCoinsFromList xs                = patternMatchFail "mkCoinsFromList" [ showText xs ]


negateCoins :: Coins -> Coins
negateCoins (Coins c) = Coins (each %~ negate $ c)


findPCIds :: WorldState -> [Id] -> [Id]
findPCIds ws haystack = [ i | i <- haystack, (ws^.typeTbl) ! i == PCType ]


-- ============================================================
-- Helper functions relating to output:


send :: MsgQueue -> T.Text -> MudStack ()
send mq = liftIO . atomically . writeTQueue mq . FromServer


broadcast :: [(T.Text, [Id])] -> MudStack ()
broadcast bs = do
    mqtTMVar  <- getNWSTMVar msgQueueTblTMVar
    ptTMVar   <- getNWSTMVar plaTblTMVar
    (mqt, pt) <- liftIO . atomically $ do
        mqt <- readTMVar mqtTMVar
        pt  <- readTMVar ptTMVar
        return (mqt, pt)
    let helper msg i = let mq   = mqt ! i
                           cols = (pt ! i)^.columns
                       in send mq . nl . T.unlines . wordWrap cols $ msg
    forM_ bs $ \(msg, is) -> mapM_ (helper msg) is


mkDividerTxt :: Cols -> T.Text
mkDividerTxt = flip T.replicate "="


mkAssocListTxt :: (Show a, Show b) => Cols -> [(a, b)] -> T.Text
mkAssocListTxt cols = T.concat . map helper
  where
    helper (a, b) = T.unlines . wordWrapIndent 2 cols $ (unquote . showText $ a) <> ": " <> showText b
