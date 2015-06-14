{-# LANGUAGE LambdaCase, OverloadedStrings, TransformListComp, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( BothGramNos
                                , findPCIds
                                , getEffBothGramNos
                                , getEffName
                                , getState
                                , mkAdminIdSingList
                                , mkAdminPlaIdSingList
                                , mkCapsFun
                                , mkPlaIdSingList
                                , mkPlurFromBoth
                                , mkRetainedMsgFromPerson
                                , mkSerializedNonStdDesig
                                , mkStdDesig
                                , mkUnknownPCEntName
                                , modifyState
                                , onEnv
                                , removeAdHoc
                                , sortInv ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Misc.ANSI
import Mud.TheWorld.Ids
import Mud.TopLvlDefs.Chars
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Arrow ((***))
import Control.Lens (_1, _2, at, both)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IORef (atomicModifyIORef, readIORef)
import Data.IntMap.Lazy ((!))
import Data.List (delete, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Text as T


findPCIds :: MudState -> [Id] -> [Id]
findPCIds ms haystack = [ i | i <- haystack, getType i ms == PCType ]


-----


getEffBothGramNos :: Id -> MudState -> Id -> BothGramNos
getEffBothGramNos i ms targetId =
    let targetEnt  = getEnt targetId ms
        targetSing = targetEnt^.sing
    in case targetEnt^.entName of
      Nothing -> let (pp *** pp -> (targetSexy, targetRace)) = getSexRace targetId ms
                 in if targetSing `elem` getIntroduced i ms
                   then (targetSing, "")
                   else (targetRace, pluralize targetRace) & both %~ ((targetSexy <>) . (" " <>))
      Just {} -> (targetSing, targetEnt^.plur)
  where
    pluralize "dwarf" = "dwarves"
    pluralize "elf"   = "elves"
    pluralize r       = r <> "s"


-----


getEffName :: Id -> MudState -> Id -> T.Text
getEffName i ms targetId = let targetEnt = getEnt targetId ms
                           in fromMaybe (helper $ targetEnt^.sing) $ targetEnt^.entName
  where
    helper targetSing | views introduced (targetSing `elem`) (getPC i ms) = uncapitalize targetSing
                      | otherwise                                         = mkUnknownPCEntName targetId ms


mkUnknownPCEntName :: Id -> MudState -> T.Text
mkUnknownPCEntName i ms = let (T.head . pp *** pp -> (h, r)) = getSexRace i ms in h `T.cons` r


-----


getState :: MudStack MudState
getState = onEnv $ liftIO . readIORef . view mudStateIORef


onEnv :: (MudData -> MudStack a) -> MudStack a
onEnv = (ask >>=)


-----


mkAdminIdSingList :: MudState -> [(Id, Sing)]
mkAdminIdSingList = mkIdSingListHelper id


mkIdSingListHelper :: (Bool -> Bool) -> MudState -> [(Id, Sing)]
mkIdSingListHelper f ms@(view plaTbl -> pt) = [ (i, s) | i <- IM.keys pt
                                                       , f . getPlaFlag IsAdmin $ pt ! i
                                                       , let s = getSing i ms
                                                       , then sortWith by s ]


-----


mkAdminPlaIdSingList :: MudState -> [(Id, Sing)]
mkAdminPlaIdSingList = mkIdSingListHelper (const True)


-----


mkPlaIdSingList :: MudState -> [(Id, Sing)]
mkPlaIdSingList = mkIdSingListHelper not


-----


type BothGramNos = (Sing, Plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


-----


mkRetainedMsgFromPerson :: Sing -> T.Text -> T.Text
mkRetainedMsgFromPerson s msg = fromPersonMarker `T.cons` T.concat [ bracketQuote s
                                                                   , " "
                                                                   , retainedMsgColor
                                                                   , msg
                                                                   , dfltColor ]


-----


mkSerializedNonStdDesig :: Id -> MudState -> Sing -> AOrThe -> ShouldCap -> T.Text
mkSerializedNonStdDesig i ms s aot (mkCapsFun -> f) = let (pp *** pp -> (sexy, r)) = getSexRace i ms in
    serialize NonStdDesig { nonStdPCEntSing = s, nonStdDesc = T.concat [ f . pp $ aot, " ", sexy, " ", r ] }


mkCapsFun :: ShouldCap -> T.Text -> T.Text
mkCapsFun = \case DoCap    -> capitalize
                  Don'tCap -> id


-----


mkStdDesig :: Id -> MudState -> ShouldCap -> PCDesig
mkStdDesig i ms sc = StdDesig { stdPCEntSing = Just . getSing i $ ms
                              , shouldCap    = sc
                              , pcEntName    = mkUnknownPCEntName i ms
                              , pcId         = i
                              , pcIds        = findPCIds ms . getPCRmInv i $ ms }


-----


modifyState :: (MudState -> (MudState, a)) -> MudStack a
modifyState f = ask >>= \md -> liftIO .  atomicModifyIORef (md^.mudStateIORef) $ f


-----


removeAdHoc :: Id -> MudState -> MudState
removeAdHoc i ms = ms & coinsTbl   .at  i        .~ Nothing
                      & entTbl     .at  i        .~ Nothing
                      & eqTbl      .at  i        .~ Nothing
                      & invTbl     .at  i        .~ Nothing
                      & invTbl     .ind iWelcome %~ (i `delete`)
                      & mobTbl     .at  i        .~ Nothing
                      & msgQueueTbl.at  i        .~ Nothing
                      & pcTbl      .at  i        .~ Nothing
                      & plaTbl     .at  i        .~ Nothing
                      & typeTbl    .at  i        .~ Nothing


-----


sortInv :: MudState -> Inv -> Inv
sortInv ms is = let (foldr helper ([], []) -> (pcIs, nonPCIs)) = [ (i, getType i ms) | i <- is ]
                in (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper (i, t) acc                  = let consTo lens = acc & lens %~ (i :)
                                         in t == PCType ? consTo _1 :? consTo _2
    sortNonPCs                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped nonPCIs                     = [ (i, views entName fromJust e, e^.sing) | i <- nonPCIs
                                                                                  , let e = getEnt i ms ]
