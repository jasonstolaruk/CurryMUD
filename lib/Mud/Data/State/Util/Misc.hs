{-# OPTIONS_GHC -Wall -Werror #-}
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
import Mud.Util.Text

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Lens (_1, _2, both, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (<>~), (^.))
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
getEffBothGramNos i ws targetI
  | targetE <- (ws^.entTbl) ! targetI = case targetE^.entName of
    Nothing | (view introduced -> intros)          <- (ws^.pcTbl) ! i
            , targetS                              <- targetE^.sing
            , (pp *** pp -> (targetSexy, targetR)) <- getSexRace targetI ws
            -> if targetS `elem` intros
              then (targetS, "")
              else over both ((targetSexy <>) . (" " <>)) (targetR, pluralize targetR)
    Just _  -> (view sing *** view plur) . dup $ targetE
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


getEffName :: Id -> WorldState -> Id -> T.Text
getEffName i ws targetI@(((ws^.entTbl) !) -> targetE) = fromMaybe helper $ targetE^.entName
  where
    helper | views introduced ((targetE^.sing) `elem`) ((ws^.pcTbl) ! i) = uncapitalize targetS
           | otherwise                                                   = mkUnknownPCEntName targetI ws
    targetS                                                              = targetE^.sing


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
mkSerializedNonStdDesig i ws s (capitalize . pp -> aot) | (pp *** pp -> (sexy, r)) <- getSexRace i ws =
    serialize NonStdDesig { nonStdPCEntSing = s
                          , nonStdDesc      = T.concat [ aot, " ", sexy, " ", r ] }


mkUnknownPCEntName :: Id -> WorldState -> T.Text
mkUnknownPCEntName i ws | (view sex  -> s) <- (ws^.mobTbl) ! i
                        , (view race -> r) <- (ws^.pcTbl)  ! i = (T.singleton . T.head . pp $ s) <> pp r


sortInv :: WorldState -> Inv -> Inv
sortInv ws is | (foldl' helper ([], []) -> (pcIs, nonPCIs)) <- [ (i, (ws^.typeTbl) ! i) | i <- is ]
              = (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper a (i, t) | t == PCType      = a & _1 <>~ [i]
                    | otherwise        = a & _2 <>~ [i]
    sortNonPCs                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped nonPCIs                     = [ (i, fromJust $ e^.entName, e^.sing) | i <- nonPCIs
                                                                               , let e = (ws^.entTbl) ! i ]


statefulFork :: StateInIORefT MudState IO () -> MudStack MudState
statefulFork f = get >>= \s ->
    (liftIO . void . forkIO . void . runStateInIORefT f $ s) >> return s


statefulFork_ :: StateInIORefT MudState IO () -> MudStack ()
statefulFork_ f = liftIO . void . forkIO . void . runStateInIORefT f =<< get
