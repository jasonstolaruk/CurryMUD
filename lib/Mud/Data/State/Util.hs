{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards, ViewPatterns #-}

module Mud.Data.State.Util ( BothGramNos
                           , allKeys
                           , bcast
                           , bcastNl
                           , bcastOthersInRm
                           , findPCIds
                           , frame
                           , getEffBothGramNos
                           , getEffName
                           , getLogAsyncs
                           , getNWSRec
                           , getPla
                           , getPlaColumns
                           , getPlaLogQueue
                           , getSexRace
                           , getUnusedId
                           , getWSTMVar
                           , massMsg
                           , massSend
                           , mkBroadcast
                           , mkCoinsFromList
                           , mkDividerTxt
                           , mkListFromCoins
                           , mkNTBroadcast
                           , mkPlur
                           , mkPlurFromBoth
                           , mkPronoun
                           , mkReflexive
                           , mkUnknownPCEntName
                           , modifyArm
                           , modifyEnt
                           , modifyMob
                           , modifyNWS
                           , modifyObj
                           , modifyPC
                           , modifyPla
                           , modifyRm
                           , modifyWS
                           , modifyWpn
                           , multiWrapSend
                           , negateCoins
                           , ok
                           , onNWS
                           , onWS
                           , parsePCDesig
                           , prompt
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
                           , statefulFork
                           , wrapSend ) where

import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.StateInIORefT
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>), Const)
import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, readTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (_1, _2, at, both, each, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((%~), (&), (?~), (.~), (^.), (^.))
import Control.Lens.Setter (ASetter, set)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, gets)
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete, elemIndex, foldl', nub, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (IntMap, elems, keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util"


-- ============================================================
-- Higher-level abstractions for working with STM:


-- World state:


getWSTMVar :: StateInIORefT MudState IO (TMVar WorldState)
getWSTMVar = gets (view worldStateTMVar)


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


-- Non-world state:


getNWSRec :: ((a -> Const a a) -> NonWorldState -> Const a NonWorldState) -> MudStack a
getNWSRec lens = gets (view (nonWorldState.lens))


readTMVarInNWS :: ((TMVar a -> Const (TMVar a) (TMVar a)) ->
                  NonWorldState                           ->
                  Const (TMVar a) NonWorldState)          ->
                  MudStack a
readTMVarInNWS lens = liftIO . atomically . readTMVar =<< getNWSRec lens


onNWS :: ((TMVar t -> Const (TMVar t) (TMVar t)) -> NonWorldState -> Const (TMVar t) NonWorldState) ->
         ((TMVar t, t) -> STM a)                                                                    ->
         MudStack a
onNWS lens f = liftIO . atomically . transaction =<< getNWSRec lens
  where
    transaction t = takeTMVar t >>= \x ->
        f (t, x)


modifyNWS :: ((TMVar a -> Const (TMVar a) (TMVar a)) -> NonWorldState -> Const (TMVar a) NonWorldState) ->
             (a -> a)                                                                                   ->
             MudStack ()
modifyNWS lens f = liftIO . atomically . transaction =<< getNWSRec lens
  where
    transaction t = takeTMVar t >>= putTMVar t . f


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
-- Helper functions for modifying world elements:


modifyEnt :: Id -> ASetter Ent Ent a b -> b -> MudStack Ent
modifyEnt i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.entTbl) ! i
    in putTMVar t (ws & entTbl.at i ?~ e) >> return e


modifyObj :: Id -> ASetter Obj Obj a b -> b -> MudStack Obj
modifyObj i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.objTbl) ! i
    in putTMVar t (ws & objTbl.at i ?~ e) >> return e


modifyWpn :: Id -> ASetter Wpn Wpn a b -> b -> MudStack Wpn
modifyWpn i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.wpnTbl) ! i
    in putTMVar t (ws & wpnTbl.at i ?~ e) >> return e


modifyArm :: Id -> ASetter Arm Arm a b -> b -> MudStack Arm
modifyArm i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.armTbl) ! i
    in putTMVar t (ws & armTbl.at i ?~ e) >> return e


modifyMob :: Id -> ASetter Mob Mob a b -> b -> MudStack Mob
modifyMob i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.mobTbl) ! i
    in putTMVar t (ws & mobTbl.at i ?~ e) >> return e


modifyPC :: Id -> ASetter PC PC a b -> b -> MudStack PC
modifyPC i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.pcTbl) ! i
    in putTMVar t (ws & pcTbl.at i ?~ e) >> return e


modifyRm :: Id -> ASetter Rm Rm a b -> b -> MudStack Rm
modifyRm i lens val = onWS $ \(t, ws) ->
    let (set lens val -> e) = (ws^.rmTbl) ! i
    in putTMVar t (ws & rmTbl.at i ?~ e) >> return e


-- ============================================================
-- Helper functions for working with "Pla":


getPla :: Id -> MudStack Pla
getPla i = (! i) <$> readTMVarInNWS plaTblTMVar


putPla :: Id -> Pla -> MudStack ()
putPla i p = modifyNWS plaTblTMVar $ \pt ->
    pt & at i ?~ p


modifyPla :: Id -> ASetter Pla Pla a b -> b -> MudStack Pla
modifyPla i lens val = onNWS plaTblTMVar $ \(ptTMVar, pt) ->
    let p  = pt ! i
        p' = p & lens .~ val
    in putTMVar ptTMVar (pt & at i ?~ p') >> return p'


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = view columns <$> getPla i


-- ============================================================
-- Helper functions relating to output:


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


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
parsePCDesig i ws | (view introduced -> intros) <- (ws^.pcTbl) ! i = helper intros
  where
    helper intros msg
      | T.singleton stdDesigDelimiter `T.isInfixOf` msg
      , (left, pcd, rest) <- extractPCDesigTxt stdDesigDelimiter msg
      = case pcd of
        StdDesig { stdPCEntSing = Just pes, .. } ->
          left <>
          (if pes `elem` intros then pes else expandPCEntName i ws isCap pcEntName pcId pcIds) <>
          helper intros rest
        StdDesig { stdPCEntSing = Nothing,  .. } ->
          left <> expandPCEntName i ws isCap pcEntName pcId pcIds <> helper intros rest
        _                                        -> patternMatchFail "parsePCDesig helper" [ showText pcd ]
      | T.singleton nonStdDesigDelimiter `T.isInfixOf` msg
      , (left, NonStdDesig { .. }, rest) <- extractPCDesigTxt nonStdDesigDelimiter msg
      = left <> (if nonStdPCEntSing `elem` intros then nonStdPCEntSing else nonStdDesc) <> helper intros rest
      | otherwise = msg
    extractPCDesigTxt c (T.span (/= c) -> (left, T.span (/= c) . T.tail -> (pcdTxt, T.tail -> rest)))
      | pcd <- deserialize . quoteWith (T.singleton c) $ pcdTxt :: PCDesig = (left, pcd, rest)


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
    expandSex (T.singleton -> x) = patternMatchFail "expandPCEntName expandSex" [x]


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
        let (view rmId    -> ri)  = (ws^.pcTbl)  ! i
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


massMsg :: Msg -> MudStack ()
massMsg m = readTMVarInNWS msgQueueTblTMVar >>= \(IM.elems -> is) ->
    forM_ is $ liftIO . atomically . flip writeTQueue m


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"


-- ============================================================
-- Misc. helpers:


allKeys :: WorldState -> Inv
allKeys = views typeTbl IM.keys


findPCIds :: WorldState -> [Id] -> [Id]
findPCIds ws haystack = [ i | i <- haystack, (ws^.typeTbl) ! i == PCType ]


getEffBothGramNos :: Id -> WorldState -> Id -> BothGramNos
getEffBothGramNos i ws i'
  | e <- (ws^.entTbl) ! i', mn <- e^.entName = case mn of
    Nothing | (view introduced -> intros) <- (ws^.pcTbl)  ! i
            , n                           <- e^.sing
            , (pp *** pp -> (s, r))       <- getSexRace i' ws
            -> if n `elem` intros
                 then (n, "")
                 else over both ((s <>) . (" " <>)) (r, pluralize r)
    Just _  -> (view sing *** view plur) . dup $ e
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


getEffName :: Id -> WorldState -> Id -> T.Text
getEffName i ws i'@(((ws^.entTbl) !) -> e) = fromMaybe helper $ e^.entName
  where
    helper | n `elem` intros    = uncapitalize n
           | otherwise          = mkUnknownPCEntName i' ws
    n                           = e^.sing
    (view introduced -> intros) = (ws^.pcTbl) ! i


getLogAsyncs :: MudStack (LogAsync, LogAsync)
getLogAsyncs = helper <$> gets (view nonWorldState)
  where
    helper     = (getAsync noticeLog *** getAsync errorLog) . dup
    getAsync l = fst . fromJust . view l


getMqtPt :: MudStack (IM.IntMap MsgQueue, IM.IntMap Pla)
getMqtPt = do
    (mqtTMVar, ptTMVar) <- (,) <$> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    liftIO . atomically $  (,) <$> readTMVar mqtTMVar         <*> readTMVar ptTMVar


getPlaLogQueue :: Id -> MudStack LogQueue
getPlaLogQueue i = snd . (! i) <$> readTMVarInNWS plaLogTblTMVar


getSexRace :: Id -> WorldState -> (Sex, Race)
getSexRace i ws = (view sex *** view race) . (((ws^.mobTbl) !) *** ((ws^.pcTbl) !)) . dup $ i


getUnusedId :: WorldState -> Id
getUnusedId = head . (\\) [0..] . allKeys


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [ cop, sil, gol ] = Coins (cop, sil, gol)
mkCoinsFromList xs                = patternMatchFail "mkCoinsFromList" [ showText xs ]


mkListFromCoins :: Coins -> [Int]
mkListFromCoins (Coins (c, g, s)) = [ c, g, s ]


mkPlur :: Ent -> Plur
mkPlur e@(view plur -> p) | T.null p  = e^.sing <> "s"
                          | otherwise = p


type BothGramNos = (Sing, Plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkPronoun :: Sex -> T.Text
mkPronoun Male   = "he"
mkPronoun Female = "she"
mkPronoun s      = patternMatchFail "mkPronoun" [ showText s ]


mkReflexive :: Sex -> T.Text
mkReflexive Male   = "himself"
mkReflexive Female = "herself"
mkReflexive s      = patternMatchFail "mkReflexive" [ showText s ]


mkUnknownPCEntName :: Id -> WorldState -> T.Text
mkUnknownPCEntName i ws | (view sex  -> s) <- (ws^.mobTbl) ! i
                        , (view race -> r) <- (ws^.pcTbl)  ! i = (T.singleton . T.head . pp $ s) <> pp r


negateCoins :: Coins -> Coins
negateCoins (Coins (each %~ negate -> c)) = Coins c


sortInv :: WorldState -> Inv -> Inv
sortInv ws is | (foldl' helper ([], []) . zip is -> (pcIs, nonPCIs)) <- [ (ws^.typeTbl) ! i | i <- is ]
              = (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper a (i, t) | t == PCType      = over _1 (++ [i]) a
                    | otherwise        = over _2 (++ [i]) a
    sortNonPCs is'                     = map (view _1) . sortBy nameThenSing . zip3 is' (names is') . sings $ is'
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    names is'                          = [ let e = (ws^.entTbl) ! i in fromJust $ e^.entName | i <- is' ]
    sings is'                          = [ let e = (ws^.entTbl) ! i in e^.sing               | i <- is' ]


splitRmInv :: WorldState -> Inv -> (Inv, Inv)
splitRmInv ws = span (\i -> (ws^.typeTbl) ! i == PCType)


statefulFork :: StateInIORefT MudState IO () -> MudStack ()
statefulFork f = liftIO . void . forkIO . void . runStateInIORefT f =<< get
