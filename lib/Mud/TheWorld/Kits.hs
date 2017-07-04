{-# LANGUAGE ViewPatterns #-}

module Mud.TheWorld.Kits (kit) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Clone
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Random
import           Mud.TheWorld.Zones.WarehouseIds
import           Mud.Util.Misc

import           Control.Arrow (second)
import           Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as V (Vector, head)
import           GHC.Stack (HasCallStack)


-- TODO: Include light source.
kit :: HasCallStack => Id -> V.Vector Int -> MudStack ()
kit i (V.head -> x) = modifyStateSeq helper
  where
    helper ms = holySymbolHelper . dropFst . cloneCommon . clone i ([], ms, []) . getInvHelper $ case r of
        Dwarf     -> iDwarfKit
        Elf       -> iElfKit
        Felinoid  -> iFelinoidKit
        Hobbit    -> iHobbitKit
        Human     -> iHumanKit
        Lagomorph -> iLagomorphKit
        Nymph     -> iNymphKit
        Vulpenoid -> iVulpenoidKit
      where
        r                  = getRace i ms
        getInvHelper       = filter ((`notElem` [ CorpseType, NpcType, PlaType ]) . (`getType` ms)) . (`getInv` ms)
        cloneCommon triple = clone i triple . getInvHelper $ iCommonKit
    holySymbolHelper (ms, fs) = second (fs ++) . holySymbolFactory i ms 1 . rndmIntToElem x $ gns
      where
        gns     = case catMaybes [ checkMa, checkPs, checkSt ] of
                    [] -> [ Aule, Caila, Iminye, Itulvatar, Rumialys ]
                    xs -> xs
        checkMa = checkHelper getBaseMa Drogo
        checkPs = checkHelper getBasePs Celoriel
        checkSt = checkHelper getBaseSt Rhayk
        checkHelper f a | f i ms >= 70 = Just a
                        | otherwise    = Nothing
