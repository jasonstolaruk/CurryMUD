{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.TheWorld.Kits (kit) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Clone
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Random
import           Mud.TheWorld.Zones.WarehouseIds
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)

import           Control.Arrow (second)
import           Control.Lens (_1, each)
import           Control.Lens.Operators ((.~), (&), (%~))
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Vector.Unboxed as V (Vector, toList)
import           GHC.Stack (HasCallStack)


pmf :: PatternMatchFail
pmf = U.pmf "Mud.TheWorld.Kits"


-- ==================================================


-- TODO: Include light source.
kit :: HasCallStack => Id -> V.Vector Int -> MudStack ()
kit i (V.toList -> [ va, vb, vc, vd, ve, vf, vg, vh, vi ]) = modifyStateSeq helper
  where
    helper ms = coinsHelper . holySymbolHelper . dropFst . ringHelper . potionsHelper . cloneCommon . clone i ([], ms, []) . getInvHelper $ case r of
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
        coinsHelper        = _1.coinsTbl.ind i .~ f (Coins (x, y, z))
          where
            x = rndmIntToRange vb (1,  50)
            y = rndmIntToRange vc (5,  15)
            z = rndmIntToRange vd (10, 20)
            f coins@(Coins triple) | r == Human = Coins (triple & each %~ (* 2))
                                   | otherwise  = coins
    potionsHelper triple = clone i triple . mapMaybe f $ [ (ve, iPotInstantHp), (vf, iPotInstantFp), (vg, iPotInstantSt) ]
      where
        f (vx, potId) = onTrue (rndmIntToRange vx (1, 20) == 1) (const . Just $ potId) Nothing
    ringHelper triple = clone i triple . onTrue (rndmIntToRange vh (1, 20) == 1) (ringId :) $ []
      where
        ringId = rndmIntToElem vi [ iNoseRing, iAmethystRing, iAquamarineRing, iEmeraldRing, iGarnetRing ]
    holySymbolHelper (ms, fs) = second (fs ++) . holySymbolFactory i ms 1 . rndmIntToElem va $ gns
      where
        gns     = case catMaybes [ checkSt, checkMa, checkPs ] of [] -> [ Aule, Caila, Iminye, Itulvatar, Rumialys ]
                                                                  xs -> xs
        checkSt = checkHelper getBaseSt Rhayk
        checkMa = checkHelper getBaseMa Drogo
        checkPs = checkHelper getBasePs Celoriel
        checkHelper f x | f i ms >= 80 = Just x
                        | otherwise    = Nothing
kit _ v = pmf "kit" . V.toList $ v
