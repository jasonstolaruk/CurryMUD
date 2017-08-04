{-# OPTIONS_GHC -fno-warn-type-defaults -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RankNTypes, TupleSections, ViewPatterns #-}

module Mud.Data.State.Util.Calc ( calcBarLen
                                , calcBonus
                                , calcCarriedVol
                                , calcConPerFull
                                , calcCorpseCapacity
                                , calcCorpseDecompSecs
                                , calcCorpseDecompSecsForMobSize
                                , calcCorpseVol
                                , calcCorpseWeight
                                , calcDigesterDelay
                                , calcEffAttrib
                                , calcEffAttribs
                                , calcEffDx
                                , calcEffHt
                                , calcEffMa
                                , calcEffPs
                                , calcEffSt
                                , calcEncPer
                                , calcFoodPerRem
                                , calcInvCoinsVol
                                , calcLanternSecsPerMouthfulOfOil
                                , calcLightPerRem
                                , calcLvl
                                , calcLvlExps
                                , calcLvlForExp
                                , calcLvlUpFp
                                , calcLvlUpHp
                                , calcLvlUpMp
                                , calcLvlUpPp
                                , calcLvlUpSkillPts
                                , calcMaxEnc
                                , calcMaxGodNameLen
                                , calcMaxMouthfuls
                                , calcMaxRaceLen
                                , calcModifierDx
                                , calcModifierEffDx
                                , calcModifierEffHt
                                , calcModifierEffMa
                                , calcModifierEffPs
                                , calcModifierEffSt
                                , calcModifierHt
                                , calcModifierMa
                                , calcModifierPs
                                , calcModifierSt
                                , calcProbConnectBlink
                                , calcProbLinkFlinch
                                , calcProbSpiritizeShiver
                                , calcProbTeleportDizzy
                                , calcProbTeleportShudder
                                , calcRegenFpAmt
                                , calcRegenFpDelay
                                , calcRegenHpAmt
                                , calcRegenHpDelay
                                , calcRegenMpAmt
                                , calcRegenMpDelay
                                , calcRegenPpAmt
                                , calcRegenPpDelay
                                , calcRetainedLinks
                                , calcSpiritTime
                                , calcStomachAvailSize
                                , calcStomachPerFull
                                , calcStomachSize
                                , calcStomachSizeForMobSize
                                , calcStomachSizeForRace
                                , calcVesselPerFull
                                , calcVol
                                , calcWeight ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Coins
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Hierarchy
import           Mud.Data.State.Util.Random
import           Mud.TopLvlDefs.Misc
import           Mud.TopLvlDefs.Seconds
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Weights
import           Mud.Util.List
import qualified Mud.Util.Misc as U (blowUp, pmf)
import           Mud.Util.Misc hiding (blowUp, pmf)
import           Mud.Util.Operators
import           Mud.Util.Text

import           Control.Arrow (first)
import           Control.Lens (both, view, views)
import           Control.Lens.Getter (Getter)
import           Control.Lens.Operators ((&), (%~))
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import           GHC.Stack (HasCallStack)
import           Prelude hiding (getContents)
import qualified Data.Map.Strict as M (elems)
import qualified Data.Text as T


default (Int, Double)


-----


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Data.State.Util.Calc"


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Data.State.Util.Calc"


-- ==================================================


calcBarLen :: Cols -> Int
calcBarLen cols = cols < 59 ? (cols - 9) :? 50


-----


calcBonus :: HasCallStack => Id -> MudState -> Exp -- Used by the "bonus" command.
calcBonus i ms = let l = getLvl i ms
                 in case drop l $ dup 0 : calcLvlExps of ((_, a):(_, b):_) -> ((b - a) `divideRound` 10) `max` 315
                                                         xs                -> pmf "calcBonus" xs


-----


calcCarriedVol :: HasCallStack => Id -> MudState -> Vol
calcCarriedVol i ms = sum [ calcInvVol i ms, calcCoinsVol i ms, calcEqVol i ms ]


calcEqVol :: HasCallStack => Id -> MudState -> Vol
calcEqVol i ms = sum . map (`calcVol` ms) . M.elems . getEqMap i $ ms


-----


calcConPerFull :: HasCallStack => Id -> MudState -> Int
calcConPerFull i = uncurry percent . (calcInvCoinsVol `fanUncurry` getConCapacity) . (i, )


-----


calcCorpseCapacity :: HasCallStack => Race -> Vol
calcCorpseCapacity = let f = (calcCorpseCapacity Human |&|) in \case
  Dwarf     -> f (\x -> round $ fromIntegral x * dwarfToHumanWeightRatio)
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> f (\x -> round $ fromIntegral x * hobbitToHumanWeightRatio)
  Human     -> 265000
  Lagomorph -> f id
  Nymph     -> f minusQuarter
  Vulpenoid -> f plusQuarter


-----


calcCorpseDecompSecs :: HasCallStack => Race -> Seconds
calcCorpseDecompSecs = let f = (calcCorpseDecompSecs Human |&|) in \case
  Dwarf     -> f (\x -> round $ fromIntegral x * dwarfToHumanWeightRatio)
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> f (\x -> round $ fromIntegral x * hobbitToHumanWeightRatio)
  Human     -> oneHrInSecs
  Lagomorph -> f id
  Nymph     -> f minusThird
  Vulpenoid -> f plusQuarter


-----


calcCorpseDecompSecsForMobSize :: MobSize -> Seconds
calcCorpseDecompSecsForMobSize = (* oneMinInSecs) . \case SmlMinus -> 20 {- mins -}
                                                          SmlPlus  -> 25
                                                          MedMinus -> 30
                                                          MedPlus  -> 35
                                                          LrgMinus -> 40
                                                          LrgPlus  -> 45


-----


calcCorpseVol :: HasCallStack => Race -> Vol
calcCorpseVol = let f = (calcCorpseVol Human |&|) in \case
  Dwarf     -> f (\x -> round $ fromIntegral x * dwarfToHumanWeightRatio)
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> f (\x -> round $ fromIntegral x * hobbitToHumanWeightRatio)
  Human     -> 452000 -- 4,520 cubic in
  Lagomorph -> f id
  Nymph     -> f minusQuarter
  Vulpenoid -> f plusQuarter


-----


calcCorpseWeight :: HasCallStack => Race -> Weight
calcCorpseWeight = let f = (calcCorpseWeight Human |&|) in \case
  Dwarf     -> 15000 -- 150 lbs, 4'6"
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> 11250 -- 112.5 lbs, 3'6"
  Human     -> 16000 -- 160 lbs
  Lagomorph -> f id
  Nymph     -> f minusQuarter
  Vulpenoid -> f plusQuarter


dwarfToHumanWeightRatio :: HasCallStack => Double
dwarfToHumanWeightRatio = calcCorpseWeight Dwarf `divide` calcCorpseWeight Human


hobbitToHumanWeightRatio :: HasCallStack => Double
hobbitToHumanWeightRatio = calcCorpseWeight Hobbit `divide` calcCorpseWeight Human


-----


calcDigesterDelay :: HasCallStack => Race -> Seconds
calcDigesterDelay = let f = (calcDigesterDelay Human |&|) in \case
  Elf       -> f minusFifth
  Hobbit    -> f minusFifth
  Human     -> 30 -- A full human stomach (34 mouthfuls) will digest completely in 17 mins.
  Nymph     -> f minusQuarter
  Vulpenoid -> f plusFifth
  _         -> f id


-----


calcEffAttribs :: HasCallStack => Id -> MudState -> (Int, Int, Int, Int, Int)
calcEffAttribs i ms = listToTuple [ calcEffAttrib a i ms | a <- allValues ]


calcEffAttrib :: HasCallStack => Attrib -> Id -> MudState -> Int
calcEffAttrib attrib i ms = let effects = select effect . getDurEffects i $ ms
                            in max1 . foldl' helper (getBaseAttrib attrib i ms) $ effects
  where
    helper acc (Effect _ (EffectAttrib a) (Just (EffectFixedVal x)) _ _) | a == attrib = acc + x
    helper acc _                                                                       = acc


calcEffDx :: HasCallStack => Id -> MudState -> Int
calcEffDx = calcEffAttrib Dx


calcEffHt :: HasCallStack => Id -> MudState -> Int
calcEffHt = calcEffAttrib Ht


calcEffMa :: HasCallStack => Id -> MudState -> Int
calcEffMa = calcEffAttrib Ma


calcEffPs :: HasCallStack => Id -> MudState -> Int
calcEffPs = calcEffAttrib Ps


calcEffSt :: HasCallStack => Id -> MudState -> Int
calcEffSt = calcEffAttrib St


-----


calcEncPer :: HasCallStack => Id -> MudState -> Int
calcEncPer i ms = calcWeight i ms `percent` calcMaxEnc i ms


calcMaxEnc :: HasCallStack => Id -> MudState -> Weight
calcMaxEnc i ms = calcEffSt i ms ^ 2 `percent` 13


-----


calcFoodPerRem :: HasCallStack => Id -> MudState -> Int
calcFoodPerRem i ms | f@(view foodRemMouthfuls -> x) <- getFood i ms
                    , y                              <- view foodMouthfuls . getDistinctFoodForFood f $ ms
                    = x `percent` y


-----


{-
A lantern burns for 24 hours on a pint of fuel.
There are 2,888 "Vol"s in a pint.
If there are 175 "Vol"s in a mouthful, then there are 17 mouthfuls in a pint and about 1.4 hours of light per mouthful (5,082 seconds).
-}
calcLanternSecsPerMouthfulOfOil :: HasCallStack => Seconds
calcLanternSecsPerMouthfulOfOil = let mouthfulsPerPint = 2888 `divideRound` mouthfulVol
                                  in oneDayInSecs `divideRound` mouthfulsPerPint


-----


calcLightPerRem :: HasCallStack => Id -> MudState -> Int
calcLightPerRem i ms | x <- getLightSecs i ms
                     , y <- case getLightSub i ms of Torch   -> torchSecs
                                                     Lantern -> maxLanternSecs
                     = x `percent` y


-----


calcLvl :: HasCallStack => Id -> MudState -> Lvl -- Effective level.
calcLvl i = calcLvlForExp . getExp i


calcLvlForExp :: HasCallStack => Exp -> Lvl
calcLvlForExp amt = let helper ((l, x):rest) | amt < x   = pred l
                                             | otherwise = helper rest
                        helper xs                        = pmf "calcLvlForExp" xs
                    in helper calcLvlExps


-----


calcLvlExps :: [LvlExp]
calcLvlExps = [ (l, 1250 * l ^ 2) | l <- [1..] ]


-----


calcLvlUpHp :: HasCallStack => Id -> MudState -> Int -> Int
calcLvlUpHp i ms x = max1 (rndmIntToRange x r + calcModifierHt i ms)
  where
    r = case getRace i ms of Dwarf     -> (3, 12)
                             Elf       -> (1, 7)
                             Felinoid  -> (1, 8)
                             Hobbit    -> (1, 7)
                             Human     -> (1, 8)
                             Lagomorph -> (1, 7)
                             Nymph     -> (1, 8)
                             Vulpenoid -> (2, 10)


calcLvlUpMp :: HasCallStack => Id -> MudState -> Int -> Int
calcLvlUpMp i ms x = max1 (rndmIntToRange x r + calcModifierMa i ms)
  where
    r = case getRace i ms of Dwarf     -> (1, 7)
                             Elf       -> (2, 10)
                             Felinoid  -> (1, 7)
                             Hobbit    -> (2, 10)
                             Human     -> (1, 8)
                             Lagomorph -> (1, 7)
                             Nymph     -> (3, 12)
                             Vulpenoid -> (1, 7)


calcLvlUpPp :: HasCallStack => Id -> MudState -> Int -> Int
calcLvlUpPp i ms x = max1 (rndmIntToRange x r + calcModifierPs i ms)
  where
    r = case getRace i ms of Dwarf     -> (1, 7)
                             Elf       -> (1, 8)
                             Felinoid  -> (1, 7)
                             Hobbit    -> (1, 8)
                             Human     -> (1, 8)
                             Lagomorph -> (3, 12)
                             Nymph     -> (1, 7)
                             Vulpenoid -> (1, 7)


calcLvlUpFp :: HasCallStack => Id -> MudState -> Int -> Int
calcLvlUpFp i ms x = let a = calcModifierHt i ms
                         b = calcModifierSt i ms
                         y = (a + b) `divideRound` 2
                     in max1 (rndmIntToRange x r + y)
  where
    r = case getRace i ms of Dwarf     -> (1, 8)
                             Elf       -> (1, 8)
                             Felinoid  -> (3, 12)
                             Hobbit    -> (1, 8)
                             Human     -> (1, 8)
                             Lagomorph -> (1, 8)
                             Nymph     -> (1, 7)
                             Vulpenoid -> (2, 10)


calcLvlUpSkillPts :: HasCallStack => Id -> MudState -> Int -> Int
calcLvlUpSkillPts i ms x = rndmIntToRange x (20, 30) + y
  where
    y = case getRace i ms of Human -> 5
                             _     -> 0


-----


calcMaxGodNameLen :: HasCallStack => Int
calcMaxGodNameLen = calcMaxHelper (allValues :: [GodName])


-----


calcMaxHelper :: (HasCallStack, Show a) => [a] -> Int
calcMaxHelper = maximum . map (T.length . showTxt)


-----


calcMaxMouthfuls :: HasCallStack => Obj -> Mouthfuls
calcMaxMouthfuls = views objVol (`divideRound` mouthfulVol)


-----


calcMaxRaceLen :: HasCallStack => Int
calcMaxRaceLen = calcMaxHelper (allValues :: [Race])


-----


calcModifierSt :: HasCallStack => Id -> MudState -> Int
calcModifierSt i ms = calcModifierForAttrib st i ms + racialStModifier (getRace i ms)


calcModifierForAttrib :: HasCallStack => Getter Mob Int -> Int -> MudState -> Int
calcModifierForAttrib l i = views (mobTbl.ind i.l) calcModifierForEffAttrib


calcModifierForEffAttrib :: Int -> Int
calcModifierForEffAttrib x = (x - 50) `divideRound` 10


racialStModifier :: Race -> Int
racialStModifier = \case Dwarf     -> 2
                         Elf       -> -1
                         Felinoid  -> 0
                         Hobbit    -> -2
                         Human     -> 0
                         Lagomorph -> 0
                         Nymph     -> -1
                         Vulpenoid -> 1


calcModifierEffSt :: HasCallStack => Id -> MudState -> Int
calcModifierEffSt i = calcModifierForEffAttrib . calcEffSt i


-----


calcModifierDx :: HasCallStack => Id -> MudState -> Int
calcModifierDx i ms = calcModifierForAttrib dx i ms + racialDxModifier (getRace i ms)


racialDxModifier :: Race -> Int
racialDxModifier = \case Dwarf     -> 0
                         Elf       -> 1
                         Felinoid  -> 2
                         Hobbit    -> 2
                         Human     -> 0
                         Lagomorph -> 0
                         Nymph     -> 0
                         Vulpenoid -> 0


calcModifierEffDx :: HasCallStack => Id -> MudState -> Int
calcModifierEffDx i = calcModifierForEffAttrib . calcEffDx i


-----


calcModifierHt :: HasCallStack => Id -> MudState -> Int
calcModifierHt i ms = calcModifierForAttrib ht i ms + racialHtModifier (getRace i ms)


racialHtModifier :: Race -> Int
racialHtModifier = \case Dwarf     -> 2
                         Elf       -> -1
                         Felinoid  -> 0
                         Hobbit    -> -1
                         Human     -> 0
                         Lagomorph -> -1
                         Nymph     -> 0
                         Vulpenoid -> 1


calcModifierEffHt :: HasCallStack => Id -> MudState -> Int
calcModifierEffHt i = calcModifierForEffAttrib . calcEffHt i


-----


calcModifierMa :: HasCallStack => Id -> MudState -> Int
calcModifierMa i ms = calcModifierForAttrib ma i ms + racialMaModifier (getRace i ms)


racialMaModifier :: Race -> Int
racialMaModifier = \case Dwarf     -> -2
                         Elf       -> 1
                         Felinoid  -> -1
                         Hobbit    -> 2
                         Human     -> 0
                         Lagomorph -> -2
                         Nymph     -> 3
                         Vulpenoid -> -1


calcModifierEffMa :: HasCallStack => Id -> MudState -> Int
calcModifierEffMa i = calcModifierForEffAttrib . calcEffMa i


-----


calcModifierPs :: HasCallStack => Id -> MudState -> Int
calcModifierPs i ms = calcModifierForAttrib ps i ms + racialPsModifier (getRace i ms)


racialPsModifier :: Race -> Int
racialPsModifier = \case Dwarf     -> -2
                         Elf       -> 0
                         Felinoid  -> -1
                         Hobbit    -> -1
                         Human     -> 0
                         Lagomorph -> 3
                         Nymph     -> -2
                         Vulpenoid -> -1


calcModifierEffPs :: HasCallStack => Id -> MudState -> Int
calcModifierEffPs i = calcModifierForEffAttrib . calcEffPs i


-----


calcProbConnectBlink :: HasCallStack => Id -> MudState -> Int
calcProbConnectBlink i ms = (avgHelper calcEffHt calcEffPs i ms - 100) ^ 2 `quot` 125


avgHelper :: HasCallStack => (Id -> MudState -> Int) -> (Id -> MudState -> Int) -> Id -> MudState -> Int
avgHelper f g i ms = let pair = (uncurry f, uncurry g) & both %~ (fromIntegral . ((i, ms) |&|))
                     in uncurry (+) pair `divideRound` 2


calcProbLinkFlinch :: HasCallStack => Id -> MudState -> Int
calcProbLinkFlinch = calcProbConnectBlink


calcProbSpiritizeShiver :: HasCallStack => Id -> MudState -> Int
calcProbSpiritizeShiver = calcProbConnectBlink


calcProbTeleportDizzy :: HasCallStack => Id -> MudState -> Int
calcProbTeleportDizzy i ms = (calcEffHt i ms - 100) ^ 2 `quot` 250


calcProbTeleportShudder :: HasCallStack => Id -> MudState -> Int
calcProbTeleportShudder i ms = (calcEffHt i ms - 100) ^ 2 `quot` 125


-----


calcRegenAmt :: Double -> Int
calcRegenAmt x = max1 . round $ x / 13


calcRegenHpAmt :: HasCallStack => Id -> MudState -> Int
calcRegenHpAmt i = calcRegenAmt . fromIntegral . calcEffHt i


calcRegenMpAmt :: HasCallStack => Id -> MudState -> Int
calcRegenMpAmt i = calcRegenAmt . weightedAvgHt calcEffMa i


weightedAvgHt :: HasCallStack => (Id -> MudState -> Int) -> Id -> MudState -> Double
weightedAvgHt f i ms = a + b
  where
    a = fromIntegral (calcEffHt i ms) * 0.25
    b = fromIntegral (f         i ms) * 0.75


calcRegenPpAmt :: HasCallStack => Id -> MudState -> Int
calcRegenPpAmt i = calcRegenAmt . weightedAvgHt calcEffPs i


calcRegenFpAmt :: HasCallStack => Id -> MudState -> Int
calcRegenFpAmt i = calcRegenAmt . weightedAvgHt calcEffSt i


-----


calcRegenDelay :: Double -> Int
calcRegenDelay x = (30 +) . round $ ((x - 50) ^ 2) / 250


calcRegenHpDelay :: HasCallStack => Id -> MudState -> Int
calcRegenHpDelay i = calcRegenDelay . fromIntegral . calcEffHt i


calcRegenMpDelay :: HasCallStack => Id -> MudState -> Int
calcRegenMpDelay i = calcRegenDelay . weightedAvgHt calcEffMa i


calcRegenPpDelay :: HasCallStack => Id -> MudState -> Int
calcRegenPpDelay i = calcRegenDelay . weightedAvgHt calcEffPs i


calcRegenFpDelay :: HasCallStack => Id -> MudState -> Int
calcRegenFpDelay i = calcRegenDelay . weightedAvgHt calcEffSt i


-----


calcRetainedLinks :: HasCallStack => Id -> MudState -> Int
calcRetainedLinks i ms = ceiling $ getBasePs i ms `divide` 10


-----


calcSpiritTime :: HasCallStack => Id -> MudState -> Seconds
calcSpiritTime i ms = maxSpiritSecs `min` helper (getLvl i ms)
  where
    helper = \case 0 -> 0
                   1 -> oneMinInSecs
                   2 -> helper 3
                   x -> round $ (x `divide` 2) * fromIntegral oneMinInSecs


-----


calcStomachAvailSize :: HasCallStack => Id -> MudState -> (Mouthfuls, Mouthfuls)
calcStomachAvailSize i ms | size <- calcStomachSize i ms, avail <- size - length (getStomach i ms)
                          = (avail, size)


calcStomachSize :: HasCallStack => Id -> MudState -> Mouthfuls
calcStomachSize i ms =
    let (ip, r, s) = ((,,) <$> uncurry isPla <*> uncurry getRace <*> uncurry getMobSize) (i, ms)
    in ip ? calcStomachSizeForRace r :? calcStomachSizeForMobSize (fromMaybe oops s)
  where
    oops = blowUp "calcStomachSize" "NPC mob size of Nothing" ""


calcStomachSizeForRace :: HasCallStack => Race -> Mouthfuls
calcStomachSizeForRace = helper
  where
    helper = let f = (helper Human |&|) in \case
      Dwarf     -> f minusQuarter
      Elf       -> f minusFifth
      Felinoid  -> f plusFifth
      Hobbit    -> f minusThird
      Human     -> 60 * 100 `divideRound` mouthfulVol -- 34 mouthfuls.
      Lagomorph -> f id
      Nymph     -> f minusFifth
      Vulpenoid -> f plusQuarter


calcStomachSizeForMobSize :: HasCallStack => MobSize -> Mouthfuls
calcStomachSizeForMobSize = \case SmlMinus -> 5
                                  SmlPlus  -> 20
                                  MedMinus -> x
                                  MedPlus  -> round $ fromIntegral x * (1.33 :: Double)
                                  LrgMinus -> round $ fromIntegral x * (1.66 :: Double)
                                  LrgPlus  -> x * 2
  where
    x = calcStomachSizeForRace Human


calcStomachPerFull :: HasCallStack => Id -> MudState -> Int
calcStomachPerFull i = uncurry percent . first length . (getStomach `fanUncurry` calcStomachSize) . (i, )


-----


calcVesselPerFull :: HasCallStack => Vessel -> Mouthfuls -> Int
calcVesselPerFull (view vesselMaxMouthfuls -> m) = (`percent` m)


-----


calcVol :: HasCallStack => Id -> MudState -> Vol
calcVol i ms = getObjVol i ms + (hasConId i ms ? calcInvCoinsVol i ms :? 0)


calcInvCoinsVol :: HasCallStack => Id -> MudState -> Vol
calcInvCoinsVol i = uncurry (+) . (calcInvVol `fanUncurry` calcCoinsVol) . (i, )


calcInvVol :: HasCallStack => Id -> MudState -> Vol
calcInvVol i ms = sum . map (`calcVol` ms) . getInv i $ ms


calcCoinsVol :: HasCallStack => Id -> MudState -> Vol
calcCoinsVol i = (* coinVol) . sum . coinsToList . getCoins i


-----


calcWeight :: HasCallStack => Id -> MudState -> Weight
calcWeight i ms = case getType i ms of
  ConType    -> sum [ getObjWeight i ms, calcInvWeight, calcCoinsWeight ]
  NpcType    -> npcPla
  PlaType    -> npcPla
  RmType     -> blowUp "calcWeight" "cannot calculate the weight of a room" . showTxt $ i
  VesselType -> getObjWeight i ms + calcVesselContWeight
  _          -> getObjWeight i ms
  where
    npcPla               = sum [ calcInvWeight, calcCoinsWeight, calcEqWeight ]
    calcInvWeight        = helper .           getInv   i $ ms
    calcEqWeight         = helper . M.elems . getEqMap i $ ms
    helper               = sum . map (`calcWeight` ms)
    calcCoinsWeight      = (* coinWeight) . sum . coinsToList . getCoins i $ ms
    calcVesselContWeight = maybe 0 ((* mouthfulWeight) . snd) . getVesselCont i $ ms
