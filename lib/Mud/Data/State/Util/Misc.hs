{-# LANGUAGE OverloadedStrings, TransformListComp, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( BothGramNos
                                , emptied
                                , findPCIds
                                , getEffBothGramNos
                                , getEffName
                                , getState
                                , mkPlaIdsSingsList
                                , mkPlurFromBoth
                                , mkSerializedNonStdDesig
                                , mkUnknownPCEntName
                                , modifyState
                                , sortInv ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Util.Misc
import Mud.Util.Text

import Control.Arrow ((***))
import Control.Lens (_1, _2, both, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.IntMap.Lazy ((!))
import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>), Monoid, mempty)
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Text as T


emptied :: (Monad m, Monoid b) => m a -> m b
emptied m = m >> return mempty


findPCIds :: MudState -> [Id] -> [Id]
findPCIds ms haystack = [ i | i <- haystack, getType i ms == PCType ]


getEffBothGramNos :: Id -> MudState -> Id -> BothGramNos
getEffBothGramNos i ms targetId =
    let targetEnt  = getEnt targetId ms
        targetSing = targetEnt^.sing
    in case targetEnt^.entName of
      Nothing -> let (pp *** pp -> (targetSexy, targetRace)) = getSexRace targetId ms
                 in if targetSing `elem` getIntroduced i ms
                   then (targetSing, "")
                   else over both ((targetSexy <>) . (" " <>)) (targetRace, pluralize targetRace)
      Just _  -> (targetSing, targetEnt^.plur)
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


getEffName :: Id -> MudState -> Id -> T.Text
getEffName i ms targetId = let targetEnt = getEnt targetId ms
                           in fromMaybe (helper $ targetEnt^.sing) $ targetEnt^.entName
  where
    helper targetSing | views introduced (targetSing `elem`) (getPC i ms) = uncapitalize targetSing
                      | otherwise                                         = mkUnknownPCEntName targetId ms


getState :: MudStack MudState
getState = liftIO . readIORef . view mudStateIORef =<< ask


mkPlaIdsSingsList :: MudState -> [(Id, Sing)]
mkPlaIdsSingsList ms@(view plaTbl -> pt) = [ (i, s) | i <- IM.keys pt
                                           , not . getPlaFlag IsAdmin $ pt ! i
                                           , let s = getSing i ms
                                           , then sortWith by s ]


type BothGramNos = (Sing, Plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkSerializedNonStdDesig :: Id -> MudState -> Sing -> AOrThe -> T.Text
mkSerializedNonStdDesig i ms s (capitalize . pp -> aot) = let (pp *** pp -> (sexy, r)) = getSexRace i ms in
    serialize NonStdDesig { nonStdPCEntSing = s, nonStdDesc = T.concat [ aot, " ", sexy, " ", r ] }


mkUnknownPCEntName :: Id -> MudState -> T.Text
mkUnknownPCEntName i ms = let (T.head . pp *** pp -> (h, r)) = getSexRace i ms in h `T.cons` r


modifyState :: (MudState -> (MudState, a)) -> MudStack a
modifyState f = ask >>= \md -> liftIO .  atomicModifyIORef (md^.mudStateIORef) $ f


sortInv :: MudState -> Inv -> Inv
sortInv ms is = let (foldr helper ([], []) -> (pcIs, nonPCIs)) = [ (i, getType i ms) | i <- is ]
                in (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper (i, t) acc                  = let consTo lens = over lens (i :) acc
                                         in t == PCType ? consTo _1 :? consTo _2
    sortNonPCs                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped nonPCIs                     = [ (i, views entName fromJust e, e^.sing) | i <- nonPCIs
                                                                                  , let e = getEnt i ms ]
