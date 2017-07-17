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
kit i (V.toList -> [ va, vb, vc, vd, ve, vf, vg, vh, vi, vj ]) = modifyStateSeq helper
  where
    helper ms = coinsHelper . holySymbolHelper . dropFst . ringHelper . potionsHelper . clone i ([], ms, []) . (commonIds ++) $ case r of
        Dwarf     -> []
        Elf       -> []
        Felinoid  -> []
        Hobbit    -> []
        Human     -> []
        Lagomorph -> []
        Nymph     -> []
        Vulpenoid -> []
      where
        r           = getRace i ms
        commonIds   = let fruitIds = [ iApple1, iBanana1, iOrange1 ]
                          is       = case r of Hobbit -> fruitIds
                                               Human  -> pure . succ $ iBread1
                                               Nymph  -> [ iGorhna1..iGorhna1 + 49 ]
                                               _      -> pure . rndmIntToElem vj $ fruitIds
                      in [ iBread1, iSack, iSandalsLeather, iWaterskinWithWater ] ++ is
        coinsHelper = _1.coinsTbl.ind i .~ f (Coins (x, y, z))
          where
            x = rndmIntToRange vb (1,  50)
            y = rndmIntToRange vc (1,  50)
            z = rndmIntToRange vd (20, 50)
            f coins@(Coins triple) | r == Human = Coins (triple & each %~ (* 2))
                                   | otherwise  = coins
    potionsHelper triple = clone i triple . mapMaybe f $ [ (ve, iPotInstantHp), (vf, iPotInstantFp), (vg, iPotInstantSt) ]
      where
        f (vx, potId) = onTrue (rndmIntToRange vx (1, 20) == 1) (const . Just $ potId) Nothing
    ringHelper triple = clone i triple . onTrue (rndmIntToRange vh (1, 20) == 1) (ringId :) $ []
      where
        ringId = rndmIntToElem vi [ iRingAmethyst, iRingAquamarine, iRingEmerald, iRingGarnet, iNoseRing ]
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
