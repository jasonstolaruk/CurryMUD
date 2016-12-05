{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
                                , calcInvCoinsVol
                                , calcLvl
                                , calcLvlExps
                                , calcLvlForExp
                                , calcLvlUpFp
                                , calcLvlUpHp
                                , calcLvlUpMp
                                , calcLvlUpPp
                                , calcLvlUpSkillPts
                                , calcMaxEnc
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

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Data.State.Util.Random
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Lens (both, view, views)
import Control.Lens.Getter (Getter)
import Control.Lens.Operators ((%~), (&))
import Data.List (foldl')
import Prelude hiding (getContents)
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


default (Int, Double)


-----


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Data.State.Util.Calc"


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Calc"


-- ==================================================


calcBarLen :: Cols -> Int
calcBarLen cols = cols < 59 ? (cols - 9) :? 50


-----


calcBonus :: Id -> MudState -> Exp -- Used by the "bonus" command.
calcBonus i ms = let l                 = getLvl i ms
                     ((_, a):(_, b):_) = drop l $ (0, 0) : calcLvlExps
                     diff              = b - a
                 in ((diff `divideRound` 20) `max` 100) `min` 5000


-----


calcCarriedVol :: Id -> MudState -> Vol
calcCarriedVol i ms = sum [ calcInvVol i ms, calcCoinsVol i ms, calcEqVol i ms ]


calcEqVol :: Id -> MudState -> Vol
calcEqVol i ms = sum . map (`calcVol` ms) . M.elems . getEqMap i $ ms


-----


calcConPerFull :: Id -> MudState -> Int
calcConPerFull i = uncurry percent . (calcInvCoinsVol `fanUncurry` getConCapacity) . (i, )


-----


calcCorpseDecompSecs :: Race -> Seconds
calcCorpseDecompSecs = let f = (calcCorpseDecompSecs Human |&|) in \case
  Dwarf     -> f (\x -> round $ fromIntegral x * dwarfToHumanWeightRatio)
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> f (\x -> round $ fromIntegral x * hobbitToHumanWeightRatio)
  Human     -> 60 {- mins -} * 60 {- secs -} -- one hr
  Lagomorph -> f id
  Nymph     -> f minusThird
  Vulpenoid -> f plusQuarter


-----


calcCorpseDecompSecsForMobSize :: MobSize -> Seconds
calcCorpseDecompSecsForMobSize = (* 60 {- secs -}) . \case SmlMinus -> 20 {- mins -}
                                                           SmlPlus  -> 25
                                                           MedMinus -> 30
                                                           MedPlus  -> 35
                                                           LrgMinus -> 40
                                                           LrgPlus  -> 45


-----


calcCorpseCapacity :: Race -> Vol
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


calcCorpseVol :: Race -> Vol
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


calcCorpseWeight :: Race -> Weight
calcCorpseWeight = let f = (calcCorpseWeight Human |&|) in \case
  Dwarf     -> 15000 -- 150 lbs, 4'6"
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> 11250 -- 112.5 lbs, 3'6"
  Human     -> 16000 -- 160 lbs
  Lagomorph -> f id
  Nymph     -> f minusQuarter
  Vulpenoid -> f plusQuarter


dwarfToHumanWeightRatio :: Double
dwarfToHumanWeightRatio = calcCorpseWeight Dwarf `divide` calcCorpseWeight Human


hobbitToHumanWeightRatio :: Double
hobbitToHumanWeightRatio = calcCorpseWeight Hobbit `divide` calcCorpseWeight Human


-----


calcDigesterDelay :: Race -> Seconds
calcDigesterDelay = let f = (calcDigesterDelay Human |&|) in \case
  Elf       -> f minusFifth
  Hobbit    -> f minusFifth
  Human     -> 30 -- A full human stomach (34 mouthfuls) will digest completely in 17 mins.
  Nymph     -> f minusQuarter
  Vulpenoid -> f plusFifth
  _         -> f id


-----


calcEffAttribs :: Id -> MudState -> (Int, Int, Int, Int, Int)
calcEffAttribs i ms = listToTuple [ calcEffAttrib a i ms | a <- allValues ]


calcEffAttrib :: Attrib -> Id -> MudState -> Int
calcEffAttrib attrib i ms =
    let effects = select effect . getActiveEffects i $ ms
    in max1 . foldl' helper (getBaseAttrib attrib i ms) $ effects
  where
    helper acc (Effect (MobEffectAttrib a) (Just (DefiniteVal x)) _ _) | a == attrib = acc + x
    helper acc _                                                                     = acc


-----


calcEffDx :: Id -> MudState -> Int
calcEffDx = calcEffAttrib Dx


-----


calcEffHt :: Id -> MudState -> Int
calcEffHt = calcEffAttrib Ht


-----


calcEffMa :: Id -> MudState -> Int
calcEffMa = calcEffAttrib Ma


-----


calcEffPs :: Id -> MudState -> Int
calcEffPs = calcEffAttrib Ps


-----


calcEffSt :: Id -> MudState -> Int
calcEffSt = calcEffAttrib St


-----


calcEncPer :: Id -> MudState -> Int
calcEncPer i ms = calcWeight i ms `percent` calcMaxEnc i ms


calcMaxEnc :: Id -> MudState -> Weight
calcMaxEnc i ms = calcEffSt i ms ^ 2 `percent` 13


-----


calcMaxMouthfuls :: Obj -> Mouthfuls
calcMaxMouthfuls = views objVol (`divideRound` mouthfulVol)


-----


calcMaxRaceLen :: Int
calcMaxRaceLen = maximum . map (T.length . showText) $ (allValues :: [Race])


-----


calcLvl :: Id -> MudState -> Lvl -- Effective level.
calcLvl i = calcLvlForExp . getExp i


calcLvlForExp :: Exp -> Lvl
calcLvlForExp amt = let helper ((l, x):rest) | amt < x   = pred l
                                             | otherwise = helper rest
                        helper xs                        = patternMatchFail "calcLvlForExp" . showText $ xs
                    in helper calcLvlExps


-----


calcLvlExps :: [LvlExp]
calcLvlExps = [ (l, 1250 * l ^ 2) | l <- [1..] ]


-----


calcLvlUpHp :: Id -> MudState -> Int -> Int
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


calcLvlUpMp :: Id -> MudState -> Int -> Int
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


calcLvlUpPp :: Id -> MudState -> Int -> Int
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


calcLvlUpFp :: Id -> MudState -> Int -> Int
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


calcLvlUpSkillPts :: Id -> MudState -> Int -> Int
calcLvlUpSkillPts i ms x = rndmIntToRange x (20, 30) + y
  where
    y = case getRace i ms of Human -> 5
                             _     -> 0


-----


calcModifierSt :: Id -> MudState -> Int
calcModifierSt i ms = calcModifierForAttrib st i ms + racialStModifier (getRace i ms)


calcModifierForAttrib :: Getter Mob Int -> Int -> MudState -> Int
calcModifierForAttrib l i = views (mobTbl.ind i.l) calcModifierForEffAttrib


calcModifierForEffAttrib :: Int -> Int
calcModifierForEffAttrib x = (x - 50) `divideRound` 10


racialStModifier :: Race -> Int
racialStModifier = \case Dwarf     -> 1
                         Elf       -> -1
                         Felinoid  -> 0
                         Hobbit    -> -2
                         Human     -> 0
                         Lagomorph -> 0
                         Nymph     -> -1
                         Vulpenoid -> 2


calcModifierEffSt :: Id -> MudState -> Int
calcModifierEffSt i = calcModifierForEffAttrib . calcEffSt i


-----


calcModifierDx :: Id -> MudState -> Int
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


calcModifierEffDx :: Id -> MudState -> Int
calcModifierEffDx i = calcModifierForEffAttrib . calcEffDx i


-----


calcModifierHt :: Id -> MudState -> Int
calcModifierHt i ms = calcModifierForAttrib ht i ms + racialHtModifier (getRace i ms)


racialHtModifier :: Race -> Int
racialHtModifier = \case Dwarf     -> 1
                         Elf       -> -1
                         Felinoid  -> 0
                         Hobbit    -> -1
                         Human     -> 0
                         Lagomorph -> -1
                         Nymph     -> 0
                         Vulpenoid -> 2


calcModifierEffHt :: Id -> MudState -> Int
calcModifierEffHt i = calcModifierForEffAttrib . calcEffHt i


-----


calcModifierMa :: Id -> MudState -> Int
calcModifierMa i ms = calcModifierForAttrib ma i ms + racialMaModifier (getRace i ms)


racialMaModifier :: Race -> Int
racialMaModifier = \case Dwarf     -> -1
                         Elf       -> 1
                         Felinoid  -> -1
                         Hobbit    -> 2
                         Human     -> 0
                         Lagomorph -> -2
                         Nymph     -> 3
                         Vulpenoid -> -2


calcModifierEffMa :: Id -> MudState -> Int
calcModifierEffMa i = calcModifierForEffAttrib . calcEffMa i


-----


calcModifierPs :: Id -> MudState -> Int
calcModifierPs i ms = calcModifierForAttrib ps i ms + racialPsModifier (getRace i ms)


racialPsModifier :: Race -> Int
racialPsModifier = \case Dwarf     -> -1
                         Elf       -> 0
                         Felinoid  -> -1
                         Hobbit    -> -1
                         Human     -> 0
                         Lagomorph -> 3
                         Nymph     -> -2
                         Vulpenoid -> -2


calcModifierEffPs :: Id -> MudState -> Int
calcModifierEffPs i = calcModifierForEffAttrib . calcEffPs i


-----


calcProbConnectBlink :: Id -> MudState -> Int
calcProbConnectBlink i ms = (avgHelper calcEffHt calcEffPs i ms - 100) ^ 2 `quot` 125


avgHelper :: (Id -> MudState -> Int) -> (Id -> MudState -> Int) -> Id -> MudState -> Int
avgHelper f g i ms = let pair = (uncurry f, uncurry g) & both %~ (fromIntegral . ((i, ms) |&|))
                     in uncurry (+) pair `divideRound` 2


calcProbLinkFlinch :: Id -> MudState -> Int
calcProbLinkFlinch = calcProbConnectBlink


calcProbSpiritizeShiver :: Id -> MudState -> Int
calcProbSpiritizeShiver = calcProbConnectBlink


calcProbTeleportDizzy :: Id -> MudState -> Int
calcProbTeleportDizzy i ms = (calcEffHt i ms - 100) ^ 2 `quot` 250


calcProbTeleportShudder :: Id -> MudState -> Int
calcProbTeleportShudder i ms = (calcEffHt i ms - 100) ^ 2 `quot` 125


-----


calcRegenAmt :: Double -> Int
calcRegenAmt x = max1 . round $ x / 13


calcRegenHpAmt :: Id -> MudState -> Int
calcRegenHpAmt i = calcRegenAmt . fromIntegral . calcEffHt i


calcRegenMpAmt :: Id -> MudState -> Int
calcRegenMpAmt i = calcRegenAmt . weightedAvgHt calcEffMa i


weightedAvgHt :: (Id -> MudState -> Int) -> Id -> MudState -> Double
weightedAvgHt f i ms = a + b
  where
    a = fromIntegral (calcEffHt i ms) * 0.25
    b = fromIntegral (f         i ms) * 0.75


calcRegenPpAmt :: Id -> MudState -> Int
calcRegenPpAmt i = calcRegenAmt . weightedAvgHt calcEffPs i


calcRegenFpAmt :: Id -> MudState -> Int
calcRegenFpAmt i = calcRegenAmt . weightedAvgHt calcEffSt i


-----


calcRegenDelay :: Double -> Int
calcRegenDelay x = (30 +) . round $ ((x - 50) ^ 2) / 250


calcRegenHpDelay :: Id -> MudState -> Int
calcRegenHpDelay i = calcRegenDelay . fromIntegral . calcEffHt i


calcRegenMpDelay :: Id -> MudState -> Int
calcRegenMpDelay i = calcRegenDelay . weightedAvgHt calcEffMa i


calcRegenPpDelay :: Id -> MudState -> Int
calcRegenPpDelay i = calcRegenDelay . weightedAvgHt calcEffPs i


calcRegenFpDelay :: Id -> MudState -> Int
calcRegenFpDelay i = calcRegenDelay . weightedAvgHt calcEffSt i


-----


calcRetainedLinks :: Id -> MudState -> Int
calcRetainedLinks i ms = ceiling $ getBasePs i ms `divide` 10


-----


calcSpiritTime :: Id -> MudState -> Seconds
calcSpiritTime i ms = maxSpiritSecs `min` helper (getLvl i ms)
  where
    helper = \case 0 -> 0
                   1 -> 60
                   2 -> helper 3
                   x -> round $ (x `divide` 2) * 60


-----


calcStomachAvailSize :: Id -> MudState -> (Mouthfuls, Mouthfuls)
calcStomachAvailSize i ms | size <- calcStomachSize i ms, avail <- size - length (getStomach i ms)
                          = (avail, size)


calcStomachSize :: Id -> MudState -> Mouthfuls
calcStomachSize i ms = if isPC i ms
  then calcStomachSizeForRace . getRace i $ ms
  else calcStomachSizeForMobSize . mobSizeHelper . getMobSize i $ ms
  where
    mobSizeHelper (Just x) = x
    mobSizeHelper x        = blowUp "calcStomachSize mobSizeHelper" "NPC mob size of Nothing" . showText $ x


calcStomachSizeForRace :: Race -> Mouthfuls
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


calcStomachSizeForMobSize :: MobSize -> Mouthfuls
calcStomachSizeForMobSize = \case SmlMinus -> 5
                                  SmlPlus  -> 20
                                  MedMinus -> x
                                  MedPlus  -> round $ fromIntegral x * (1.33 :: Double)
                                  LrgMinus -> round $ fromIntegral x * (1.66 :: Double)
                                  LrgPlus  -> x * 2
  where
    x = calcStomachSizeForRace Human


calcStomachPerFull :: Id -> MudState -> Int
calcStomachPerFull i ms = let mouths = length . getStomach i $ ms
                              size   = calcStomachSize i ms
                          in mouths `percent` size


-----


calcVesselPerFull :: Vessel -> Mouthfuls -> Int
calcVesselPerFull (view vesselMaxMouthfuls -> m) = (`percent` m)


-----


calcVol :: Id -> MudState -> Vol
calcVol i ms = getObjVol i ms + (hasConId i ms ? calcInvCoinsVol i ms :? 0)


calcInvCoinsVol :: Id -> MudState -> Vol
calcInvCoinsVol i = uncurry (+) . (calcInvVol `fanUncurry` calcCoinsVol) . (i, )


calcInvVol :: Id -> MudState -> Vol
calcInvVol i ms = sum . map (`calcVol` ms) . getInv i $ ms


calcCoinsVol :: Id -> MudState -> Vol
calcCoinsVol i = (* coinVol) . sum . coinsToList . getCoins i


-----


calcWeight :: Id -> MudState -> Weight
calcWeight i ms = case getType i ms of
  ConType    -> sum [ getObjWeight i ms, calcInvWeight, calcCoinsWeight ]
  NpcType    -> npcPC
  PCType     -> npcPC
  RmType     -> blowUp "calcWeight" "cannot calculate the weight of a room" . showText $ i
  VesselType -> getObjWeight i ms + calcVesselContWeight
  _          -> getObjWeight i ms
  where
    npcPC                = sum [ calcInvWeight, calcCoinsWeight, calcEqWeight ]
    calcInvWeight        = helper .           getInv   i $ ms
    calcEqWeight         = helper . M.elems . getEqMap i $ ms
    helper               = sum . map (`calcWeight` ms)
    calcCoinsWeight      = (* coinWeight) . sum . coinsToList . getCoins i $ ms
    calcVesselContWeight = maybe 0 ((* mouthfulWeight) . snd) . getVesselCont i $ ms
