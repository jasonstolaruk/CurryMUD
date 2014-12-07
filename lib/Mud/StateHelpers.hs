{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes, RecordWildCards, ViewPatterns #-}

-- This module is considered to have sufficient test coverage as of 2014-10-13.

module Mud.StateHelpers ( allKeys
                        , BothGramNos
                        , bcast
                        , bcastNl
                        , bcastOthersInRm
                        , findPCIds
                        , frame
                        , getEffBothGramNos
                        , getEffName
                        , getLogAsyncs
                        , getNWSRec
                        , getPlaColumns
                        , getPlaLogQueue
                        , getSexRace
                        , getUnusedId
                        , getWSTMVar
                        , massMsg
                        , massSend
                        , mkAssocListTxt
                        , mkBroadcast
                        , mkCoinsFromList
                        , mkDividerTxt
                        , mkListFromCoins
                        , mkNTBroadcast
                        , mkPlur
                        , mkPlurFromBoth
                        , mkUnknownPCEntName
                        , modifyNWS
                        , modifyWS
                        , multiWrapSend
                        , negateCoins
                        , ok
                        , onNWS
                        , onWS
                        , parsePCDesig
                        , putArm
                        , putCloth
                        , putCon
                        , putMob
                        , putObj
                        , putPC
                        , putPla
                        , putRm
                        , putWpn
                        , readTMVarInNWS
                        , readWSTMVar
                        , send
                        , sortInv
                        , splitRmInv
                        , wrapSend ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateInIORefT
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>), Const, pure)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, readTMVar, takeTMVar, TMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (_1, _2, at, both, each, over, to)
import Control.Lens.Operators ((%~), (&), (?~), (^.), (^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (gets)
import Control.Monad.State.Class (MonadState)
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete, elemIndex, foldl', nub, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (elems, IntMap, keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.StateHelpers"


-- ============================================================
-- Higher-level abstractions for working with STM:


getWSTMVar :: StateInIORefT MudState IO (TMVar WorldState)
getWSTMVar = gets (^.worldStateTMVar)


readWSTMVar :: MudStack WorldState
readWSTMVar = liftIO . atomically . readTMVar =<< getWSTMVar


onWS :: ((TMVar WorldState, WorldState) -> STM a) -> MudStack a
onWS f = liftIO . atomically . transaction =<< getWSTMVar
  where
    transaction t = takeTMVar t >>= \ws ->
        f (t, ws)


modifyWS :: (WorldState -> WorldState) -> MudStack ()
modifyWS f = liftIO . atomically . transaction =<< getWSTMVar
  where
    transaction t = takeTMVar t >>= putTMVar t . f


getNWSRec :: forall a (m :: * -> *). MonadState MudState m => ((a -> Const a a) -> NonWorldState -> Const a NonWorldState) -> m a
getNWSRec lens = gets (^.nonWorldState.lens)


readTMVarInNWS :: forall (m :: * -> *) a. (MonadIO m, MonadState MudState m) => ((TMVar a -> Const (TMVar a) (TMVar a)) -> NonWorldState -> Const (TMVar a) NonWorldState) -> m a
readTMVarInNWS lens = liftIO . atomically . readTMVar =<< getNWSRec lens


getMqtPt :: MudStack (IM.IntMap MsgQueue, IM.IntMap Pla)
getMqtPt = do
    (mqtTMVar, ptTMVar) <- (,) <$> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    liftIO . atomically $  (,) <$> readTMVar mqtTMVar <*> readTMVar ptTMVar


onNWS :: forall t (m :: * -> *) a. (MonadIO m, MonadState MudState m) => ((TMVar t -> Const (TMVar t) (TMVar t)) -> NonWorldState -> Const (TMVar t) NonWorldState) -> ((TMVar t, t) -> STM a) -> m a
onNWS lens f = liftIO . atomically . transaction =<< getNWSRec lens
  where
    transaction t = takeTMVar t >>= \x ->
        f (t, x)


modifyNWS :: forall a (m :: * -> *). (MonadIO m, MonadState MudState m) => ((TMVar a -> Const (TMVar a) (TMVar a)) -> NonWorldState -> Const (TMVar a) NonWorldState) -> (a -> a) -> m ()
modifyNWS lens f = liftIO . atomically . transaction =<< getNWSRec lens
  where
    transaction t = takeTMVar t >>= putTMVar t . f


getLogAsyncs :: MudStack (LogAsync, LogAsync)
getLogAsyncs = helper <$> gets (^.nonWorldState)
  where
    helper nws | Just (nla, _) <- nws^.noticeLog, Just (ela, _) <- nws^.errorLog = (nla, ela)
    helper _ = patternMatchFail "getLogAsyncs helper" [ bracketQuote "elided" ]


getPlaLogQueue :: Id -> MudStack LogQueue
getPlaLogQueue i = snd . (! i) <$> readTMVarInNWS plaLogTblTMVar


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
    ws & typeTbl.at i  ?~ ConType
       & entTbl.at i   ?~ e
       & objTbl.at i   ?~ o
       & invTbl.at i   ?~ is
       & coinsTbl.at i ?~ coi
       & conTbl.at i   ?~ con


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ WpnType & entTbl.at i ?~ e & objTbl.at i ?~ o & wpnTbl.at i ?~ w


putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
putArm i e o a = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ ArmType & entTbl.at i ?~ e & objTbl.at i ?~ o & armTbl.at i ?~ a


putMob :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> MudStack ()
putMob i e is c em m = modifyWS $ \ws ->
    ws & typeTbl.at i  ?~ MobType
       & entTbl.at i   ?~ e
       & invTbl.at i   ?~ is
       & coinsTbl.at i ?~ c
       & eqTbl.at i    ?~ em
       & mobTbl.at i   ?~ m


putPC :: Id -> Ent -> Inv -> Coins -> EqMap -> Mob -> PC -> MudStack ()
putPC i e is c em m p = modifyWS $ \ws ->
    ws & typeTbl.at i  ?~ PCType
       & entTbl.at i   ?~ e
       & invTbl.at i   ?~ is
       & coinsTbl.at i ?~ c
       & eqTbl.at i    ?~ em
       & mobTbl.at i   ?~ m
       & pcTbl.at i    ?~ p


putRm :: Id -> Inv -> Coins -> Rm -> MudStack ()
putRm i is c r = modifyWS $ \ws ->
    ws & typeTbl.at i ?~ RmType & invTbl.at i ?~ is & coinsTbl.at i ?~ c & rmTbl.at i ?~ r


-- ============================================================
-- Helper functions for working with "Pla":


putPla :: Id -> Pla -> MudStack () -- TODO: Currently not used.
putPla i p = modifyNWS plaTblTMVar $ \pt ->
    pt & at i ?~ p


getPla :: Id -> MudStack Pla
getPla i = (! i) <$> readTMVarInNWS plaTblTMVar


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = (^.columns) <$> getPla i


-- ============================================================
-- Helper functions relating to output:


send :: MsgQueue -> T.Text -> MudStack ()
send mq = liftIO . atomically . writeTQueue mq . FromServer


wrapSend :: MsgQueue -> Cols -> T.Text -> MudStack ()
wrapSend mq cols = send mq . wrapUnlinesNl cols


multiWrapSend :: MsgQueue -> Cols -> [T.Text] -> MudStack ()
multiWrapSend mq cols = send mq . nl . multiWrap cols


bcast :: [Broadcast] -> MudStack ()
bcast bs = getMqtPt >>= \(mqt, pt) -> do
    let helper msg i | mq <- mqt ! i, cols <- (pt ! i)^.columns = readWSTMVar >>= \ws ->
          send mq . T.unlines . concatMap (wordWrap cols) . T.lines . parsePCDesig i ws $ msg
    forM_ bs $ \(msg, is) -> mapM_ (helper msg) is


parsePCDesig :: Id -> WorldState -> T.Text -> T.Text
parsePCDesig i ws msg | ((^.introduced) -> intros) <- (ws^.pcTbl) ! i = helper intros msg
  where
    helper intros msg'
      | T.pack [stdDesigDelimiter] `T.isInfixOf` msg'
      , (left, pcd, rest) <- extractPCDesigTxt stdDesigDelimiter msg'
      = case pcd of
        StdDesig { stdPCEntSing = Just pes, .. } ->
          left <>
          (if pes `elem` intros then pes else expandPCEntName i ws isCap pcEntName pcId pcIds) <>
          helper intros rest
        StdDesig { stdPCEntSing = Nothing,  .. } ->
          left <> expandPCEntName i ws isCap pcEntName pcId pcIds <> helper intros rest
        _                                        -> patternMatchFail "parsePCDesig helper" [ showText pcd ]
      | T.pack [nonStdDesigDelimiter] `T.isInfixOf` msg'
      , (left, NonStdDesig { .. }, rest) <- extractPCDesigTxt nonStdDesigDelimiter msg'
      = left <> (if nonStdPCEntSing `elem` intros then nonStdPCEntSing else nonStdDesc) <> helper intros rest
      | otherwise = msg'
    extractPCDesigTxt c (T.span (/= c) -> (left, T.span (/= c) . T.tail -> (pcdTxt, T.tail -> rest)))
      | pcd <- deserialize . quoteWith (T.pack [c]) $ pcdTxt :: PCDesig = (left, pcd, rest)


expandPCEntName :: Id -> WorldState -> Bool -> T.Text -> Id -> Inv -> T.Text
expandPCEntName i ws ic pen@(headTail' -> (h, t)) pi ((i `delete`) -> pis) =
    T.concat [ leading, "he ", xth, expandSex h, " ", t ]
  where
    leading | ic        = "T"
            | otherwise = "t"
    xth = let matches = foldr (\i' acc -> if mkUnknownPCEntName i' ws == pen then i' : acc else acc) [] pis
          in case matches of [_] -> ""
                             _   -> (<> " ") . mkOrdinal . (+ 1) . fromJust . elemIndex pi $ matches
    expandSex 'm'                  = "male"
    expandSex 'f'                  = "female"
    expandSex (T.pack . pure -> x) = patternMatchFail "expandPCEntName expandSex" [x]


bcastNl :: [Broadcast] -> MudStack ()
bcastNl bs = bcast . (bs ++) . concat $ [ mkBroadcast i "\n" | i <- nub . concatMap snd $ bs ]


mkBroadcast :: Id -> T.Text -> [Broadcast]
mkBroadcast i msg = [(msg, [i])]


mkNTBroadcast :: Id -> T.Text -> [ClassifiedBroadcast]
mkNTBroadcast i msg = [NonTargetBroadcast (msg, [i])]


bcastOthersInRm :: Id -> T.Text -> MudStack ()
bcastOthersInRm i msg = bcast =<< helper
  where
    helper = onWS $ \(t, ws) ->
        let ((^.rmId)     -> ri)  = (ws^.pcTbl)  ! i
            ((i `delete`) -> ris) = (ws^.invTbl) ! ri
        in putTMVar t ws >> return [(msg, findPCIds ws ris)]


massSend :: T.Text -> MudStack ()
massSend msg = getMqtPt >>= \(mqt, pt) -> do
    let helper i = let mq   = mqt ! i
                       cols = (pt ! i)^.columns
                   in send mq . nl' . frame cols . wrapUnlines cols $ msg
    forM_ (IM.keys pt) helper


frame :: Cols -> T.Text -> T.Text
frame cols | divider <- nl . mkDividerTxt $ cols = nl . (<> divider) . (divider <>)


mkDividerTxt :: Cols -> T.Text
mkDividerTxt = flip T.replicate "="


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"


massMsg :: Msg -> MudStack () -- TODO: Should really use a "Broadcast Chan".
massMsg m = readTMVarInNWS msgQueueTblTMVar >>= \(IM.elems -> is) ->
    forM_ is $ liftIO . atomically . flip writeTQueue m


mkAssocListTxt :: (Show a, Show b) => Cols -> [(a, b)] -> T.Text
mkAssocListTxt cols = T.concat . map helper
  where
    helper (unquote . showText -> a, showText -> b) = T.unlines . wordWrapIndent 2 cols $ a <> ": " <> b


-- ============================================================
-- Misc. helpers:


allKeys :: WorldState -> Inv
allKeys = (^.typeTbl.to IM.keys)


getUnusedId :: WorldState -> Id
getUnusedId = head . (\\) [0..] . allKeys


sortInv :: WorldState -> Inv -> Inv
sortInv ws is | (foldl' helper ([], []) . zip is -> (pcIs, nonPCIs)) <- [ (ws^.typeTbl) ! i | i <- is ]
              = (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper a (i, t) | t == PCType      = over _1 (++ [i]) a
                    | otherwise        = over _2 (++ [i]) a
    sortNonPCs is'                     = map (^._1) . sortBy nameThenSing . zip3 is' (names is') . sings $ is'
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    names is'                          = [ let e = (ws^.entTbl) ! i in fromJust $ e^.entName | i <- is' ]
    sings is'                          = [ let e = (ws^.entTbl) ! i in e^.sing               | i <- is' ]


type BothGramNos = (Sing, Plur)


getEffBothGramNos :: Id -> WorldState -> Id -> BothGramNos
getEffBothGramNos i ws i'
  | e <- (ws^.entTbl) ! i', mn <- e^.entName = case mn of
    Nothing | ((^.introduced) -> intros) <- (ws^.pcTbl)  ! i
            , n                          <- e^.sing
            , (pp -> s, pp -> r)         <- getSexRace i' ws
            -> if n `elem` intros
                 then (n, "")
                 else over both ((s <>) . (" " <>)) (r, pluralize r)
    Just _  -> (e^.sing, e^.plur)
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


mkPlur :: Ent -> Plur
mkPlur e@((^.plur) -> p) | T.null p  = e^.sing <> "s"
                         | otherwise = p


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkListFromCoins :: Coins -> [Int]
mkListFromCoins (Coins (c, g, s)) = [ c, g, s ]


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [ cop, sil, gol ]       = Coins (cop, sil, gol)
mkCoinsFromList (pure . showText -> xs) = patternMatchFail "mkCoinsFromList" xs


negateCoins :: Coins -> Coins
negateCoins (Coins (each %~ negate -> c)) = Coins c


findPCIds :: WorldState -> [Id] -> [Id]
findPCIds ws haystack = [ i | i <- haystack, (ws^.typeTbl) ! i == PCType ]


getEffName :: Id -> WorldState -> Id -> T.Text
getEffName i ws i'@(((ws^.entTbl) !) -> e) = fromMaybe helper $ e^.entName
  where
    helper | n `elem` intros   = uncapitalize n
           | otherwise         = mkUnknownPCEntName i' ws
    n                          = e^.sing
    ((^.introduced) -> intros) = (ws^.pcTbl) ! i


mkUnknownPCEntName :: Id -> WorldState -> T.Text
mkUnknownPCEntName i ws | ((^.sex) -> s)  <- (ws^.mobTbl) ! i
                        , ((^.race) -> r) <- (ws^.pcTbl)  ! i = T.pack [ T.head . pp $ s ] <> pp r


getSexRace :: Id -> WorldState -> (Sex, Race)
getSexRace i ws | ((^.sex) -> s) <- (ws^.mobTbl) ! i, ((^.race) -> r) <- (ws^.pcTbl) ! i = (s, r)


splitRmInv :: WorldState -> Inv -> (Inv, Inv)
splitRmInv ws = span (\i -> (ws^.typeTbl) ! i == PCType)
