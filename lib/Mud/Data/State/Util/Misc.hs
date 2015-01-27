{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, TransformListComp, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( BothGramNos
                                , findPCIds
                                , getEffBothGramNos
                                , getEffName
                                , getMqtPt
                                , getSexRace
                                , mkPlaIdsSingsList
                                , mkPlurFromBoth
                                , mkSerializedNonStdDesig
                                , mkUnknownPCEntName
                                , sortInv
                                , statefulFork
                                , statefulFork_ ) where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.StateInIORefT
import Mud.Data.State.Util.STM
import Mud.Util.Misc

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Lens (_1, _2, both, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.IntMap.Lazy ((!))
import Data.List (foldl', sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM (IntMap, keys)
import qualified Data.Text as T


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


getMqtPt :: MudStack (IM.IntMap MsgQueue, IM.IntMap Pla)
getMqtPt = (,) <$> readTMVarInNWS msgQueueTblTMVar <*> readTMVarInNWS plaTblTMVar


getSexRace :: Id -> WorldState -> (Sex, Race)
getSexRace i ws = (view sex *** view race) . (views mobTbl (!) ws *** views pcTbl (!) ws) . dup $ i


mkPlaIdsSingsList :: IM.IntMap Ent -> IM.IntMap Pla -> [(Id, Sing)]
mkPlaIdsSingsList et pt = [ (i, s) | i <- IM.keys pt
                                   , not . getPlaFlag IsAdmin $ (pt ! i)
                                   , let s = (et ! i)^.sing
                                   , then sortWith by s ]


type BothGramNos = (Sing, Plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkSerializedNonStdDesig :: Id -> WorldState -> Sing -> AOrThe -> T.Text
mkSerializedNonStdDesig i ws s (capitalize . pp -> aot) | (pp *** pp -> (s', r)) <- getSexRace i ws =
    serialize NonStdDesig { nonStdPCEntSing = s
                          , nonStdDesc      = T.concat [ aot, " ", s', " ", r ] }


mkUnknownPCEntName :: Id -> WorldState -> T.Text
mkUnknownPCEntName i ws | (view sex  -> s) <- (ws^.mobTbl) ! i
                        , (view race -> r) <- (ws^.pcTbl)  ! i = (T.singleton . T.head . pp $ s) <> pp r


sortInv :: WorldState -> Inv -> Inv
sortInv ws is | (foldl' helper ([], []) -> (pcIs, nonPCIs)) <- [ (i, (ws^.typeTbl) ! i) | i <- is ]
              = (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper a (i, t) | t == PCType      = over _1 (++ [i]) a
                    | otherwise        = over _2 (++ [i]) a
    sortNonPCs                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped is'                         = [ (i, fromJust $ e^.entName, e^.sing) | i <- is', let e = (ws^.entTbl) ! i ]


statefulFork :: StateInIORefT MudState IO () -> MudStack MudState
statefulFork f = get >>= \s ->
    (liftIO . void . forkIO . void . runStateInIORefT f $ s) >> return s


statefulFork_ :: StateInIORefT MudState IO () -> MudStack ()
statefulFork_ f = liftIO . void . forkIO . void . runStateInIORefT f =<< get
