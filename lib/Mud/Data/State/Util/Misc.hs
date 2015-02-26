{-# LANGUAGE OverloadedStrings, TransformListComp, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( BothGramNos
                                , findPCIds
                                , getEffBothGramNos
                                , getEffName
                                , getSexRace
                                , mkPlaIdsSingsList
                                , mkPlurFromBoth
                                , mkSerializedNonStdDesig
                                , mkUnknownPCEntName
                                , sortInv ) where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.STM
import Mud.Util.Misc
import Mud.Util.Text

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Lens (_1, _2, both, over)
import Control.Lens.Cons (cons)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (<>~), (^.))
import Data.IntMap.Lazy ((!))
import Data.List (foldl', sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM (IntMap, keys)
import qualified Data.Text as T


findPCIds :: TypeTbl -> [Id] -> [Id]
findPCIds tt haystack = [ i | i <- haystack, tt ! i == PCType ]


getEffBothGramNos :: Id -> EntTbl -> MobTbl -> PCTbl -> Id -> BothGramNos
getEffBothGramNos i et mt pt targetI | targetE <- et ! targetI = case targetE^.entName of
    Nothing | intros                               <- (pt ! i)^.introduced
            , targetS                              <- targetE^.sing
            , (pp *** pp -> (targetSexy, targetR)) <- getSexRace targetI mt pt
            -> if targetS `elem` intros
              then (targetS, "")
              else over both ((targetSexy <>) . (" " <>)) (targetR, pluralize targetR)
    Just _  -> (view sing *** view plur) . dup $ targetE
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


getEffName :: Id -> EntTbl -> MobTbl -> PCTbl -> Id -> T.Text
getEffName i et mt pt targetI@((et !) -> targetE) = fromMaybe helper $ targetE^.entName
  where
    helper | views introduced ((targetE^.sing) `elem`) (pt ! i) = uncapitalize targetS
           | otherwise                                          = mkUnknownPCEntName targetI mt pt
    targetS                                                     = targetE^.sing


getSexRace :: Id -> MobTbl -> PCTbl -> (Sex, Race)
getSexRace i mt pt = (view sex *** view race) (mt ! i, pt ! i)


mkPlaIdsSingsList :: IM.IntMap Ent -> IM.IntMap Pla -> [(Id, Sing)]
mkPlaIdsSingsList et pt = [ (i, s) | i <- IM.keys pt
                                   , not . getPlaFlag IsAdmin $ (pt ! i)
                                   , let s = (et ! i)^.sing
                                   , then sortWith by s ]


type BothGramNos = (Sing, Plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkSerializedNonStdDesig :: Id -> MobTbl -> PCTbl -> Sing -> AOrThe -> T.Text
mkSerializedNonStdDesig i mt pt s (capitalize . pp -> aot) | (pp *** pp -> (sexy, r)) <- getSexRace i mt pt =
    serialize NonStdDesig { nonStdPCEntSing = s
                          , nonStdDesc      = T.concat [ aot, " ", sexy, " ", r ] }


mkUnknownPCEntName :: Id -> MobTbl -> PCTbl -> T.Text
mkUnknownPCEntName i mt pt | s <- (mt ! i)^.sex
                           , r <- (pt ! i)^.race = (T.singleton . T.head . pp $ s) <> pp r


-- TODO: Changed fold from "foldl'" to "foldr"... everything OK?
sortInv :: TypeTbl -> EntTbl -> Inv -> Inv
sortInv tt et is | (foldr helper ([], []) -> (pcIs, nonPCIs)) <- [ (i, tt ! i) | i <- is ]
                 = (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper (i, t) a                    = let consTo lens = over lens (cons i) a
                                         in if t == PCType then consTo _1 else consTo _2
    sortNonPCs                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped nonPCIs                     = [ (i, fromJust $ e^.entName, e^.sing) | i <- nonPCIs
                                                                               , let e = et ! i ]
