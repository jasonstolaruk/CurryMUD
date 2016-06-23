{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.Calc ( calcBarLen
                                , calcBonus
                                , calcConPerFull
                                , calcDigesterDelay
                                , calcEffAttrib
                                , calcEffDx
                                , calcEffHt
                                , calcEffMa
                                , calcEffPs
                                , calcEffSt
                                , calcEncPer
                                , calcLvl
                                , calcLvlExp
                                , calcLvlExps
                                , calcMaxEnc
                                , calcMaxMouthfuls
                                , calcMaxRaceLen
                                , calcProbConnectBlink
                                , calcProbLinkFlinch
                                , calcProbTeleDizzy
                                , calcProbTeleShudder
                                , calcRegenFpAmt
                                , calcRegenFpDelay
                                , calcRegenHpAmt
                                , calcRegenHpDelay
                                , calcRegenMpAmt
                                , calcRegenMpDelay
                                , calcRegenPpAmt
                                , calcRegenPpDelay
                                , calcStomachAvailSize
                                , calcStomachPerFull
                                , calcStomachSize
                                , calcVesselPerFull
                                , calcVol
                                , calcWeight ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***))
import Control.Lens (view, views)
import Data.List (foldl')
import Data.Text (Text)
import Prelude hiding (getContents)
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


default (Int, Double)


-----


blowUp :: Text -> Text -> [Text] -> a
blowUp = U.blowUp "Mud.Data.State.Util.Calc"


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Calc"


-- ==================================================


calcBarLen :: Cols -> Int
calcBarLen cols = cols < 59 ? (cols - 9) :? 50


-----


calcBonus :: Id -> MudState -> Exp
calcBonus i ms = let l                 = calcLvl i ms
                     ((_, a):(_, b):_) = drop l $ (0, 0) : calcLvlExps
                     diff              = b - a
                 in round $ diff `divide` 25


-----


calcConPerFull :: Id -> MudState -> Int
calcConPerFull i ms = let total           = foldr helper 0 . getInv i $ ms
                          helper targetId = (calcVol targetId ms +)
                      in total `percent` getCapacity i ms


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


calcEffDx :: Id -> MudState -> Int
calcEffDx = calcEffAttrib Dx


calcEffAttrib :: Attrib -> Id -> MudState -> Int
calcEffAttrib attrib i ms =
    let effects = select effect . getActiveEffects i $ ms
    in 1 `max` foldl' helper (getBaseAttrib attrib i ms) effects
  where
    helper acc (Effect (MobEffectAttrib a) (Just (DefiniteVal x)) _ _) | a == attrib = acc + x
    helper acc _                                                       = acc


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
calcMaxMouthfuls = views vol (round . (`divide` mouthfulVol))


-----


calcMaxRaceLen :: Int
calcMaxRaceLen = maximum . map (T.length . showText) $ (allValues :: [Race])


-----


calcLvl :: Id -> MudState -> Lvl
calcLvl i ms = let myExp                            = getExp i ms
                   helper ((l, x):rest) | myExp < x = pred l
                                        | otherwise = helper rest
                   helper xs                        = patternMatchFail "calcLvl" [ showText xs ]
               in helper calcLvlExps


-----


calcLvlExp :: Id -> MudState -> LvlExp
calcLvlExp i = (calcLvl i *** getExp i) . dup


calcLvlExps :: [LvlExp]
calcLvlExps = [ (lvl, 1250 * lvl ^ 2) | lvl <- [1..] ]


-----


calcProbLinkFlinch :: Id -> MudState -> Int
calcProbLinkFlinch i ms = (calcEffHt i ms - 100) ^ 2 `quot` 125


calcProbConnectBlink :: Id -> MudState -> Int
calcProbConnectBlink = calcProbLinkFlinch


calcProbTeleDizzy :: Id -> MudState -> Int
calcProbTeleDizzy i ms = (calcEffHt i ms - 100) ^ 2 `quot` 250


calcProbTeleShudder :: Id -> MudState -> Int
calcProbTeleShudder = calcProbLinkFlinch


-----


calcRegenAmt :: Double -> Int
calcRegenAmt x = round $ x / 10


calcRegenHpAmt :: Id -> MudState -> Int
calcRegenHpAmt i = calcRegenAmt . fromIntegral . calcEffHt i


calcRegenMpAmt :: Id -> MudState -> Int
calcRegenMpAmt i ms = calcRegenAmt $ (calcEffHt i ms + calcEffMa i ms) `divide` 2


calcRegenPpAmt :: Id -> MudState -> Int
calcRegenPpAmt i ms = calcRegenAmt $ (calcEffHt i ms + calcEffPs i ms) `divide` 2


calcRegenFpAmt :: Id -> MudState -> Int
calcRegenFpAmt i ms = calcRegenAmt $ (calcEffHt i ms + calcEffSt i ms) `divide` 2


-----


calcRegenDelay :: Double -> Int
calcRegenDelay x = (30 +) . round $ ((x - 50) ^ 2) / 250


calcRegenHpDelay :: Id -> MudState -> Int
calcRegenHpDelay i = calcRegenDelay . fromIntegral . calcEffHt i


calcRegenMpDelay :: Id -> MudState -> Int
calcRegenMpDelay i ms = calcRegenDelay $ (calcEffHt i ms + calcEffMa i ms) `divide` 2


calcRegenPpDelay :: Id -> MudState -> Int
calcRegenPpDelay i ms = calcRegenDelay $ (calcEffHt i ms + calcEffPs i ms) `divide` 2


calcRegenFpDelay :: Id -> MudState -> Int
calcRegenFpDelay i ms = calcRegenDelay $ (calcEffHt i ms + calcEffSt i ms) `divide` 2


-----


calcStomachAvailSize :: Id -> MudState -> (Int, Int)
calcStomachAvailSize i ms | size <- calcStomachSize i ms, avail <- size - length (getStomach i ms)
                          = (avail, size)


calcStomachSize :: Id -> MudState -> Int
calcStomachSize i ms | isPC i ms = helper . getRace i $ ms
                     | otherwise = helper Human
  where
    helper = let f = (helper Human |&|) in \case
      Dwarf     -> f minusQuarter
      Elf       -> f minusFifth
      Felinoid  -> f plusFifth
      Hobbit    -> f minusThird
      Human     -> round $ 60 * 100 `divide` mouthfulVol -- 34 mouthfuls.
      Lagomorph -> f id
      Nymph     -> f minusFifth
      Vulpenoid -> f plusQuarter


calcStomachPerFull :: Id -> MudState -> Int
calcStomachPerFull i ms = let mouths = length . getStomach i $ ms
                              size   = calcStomachSize i ms
                          in mouths `percent` size


-----


calcVesselPerFull :: Vessel -> Mouthfuls -> Int
calcVesselPerFull (view maxMouthfuls -> m) x = x `percent` m


-----


calcVol :: Id -> MudState -> Vol
calcVol i ms = calcHelper i
  where
    calcHelper i' = case getType i' ms of
      ConType -> sum [ onTrue (i' /= i) (+ getVol i' ms) 0, calcInvVol, calcCoinsVol ]
      _       -> getVol i' ms
      where
        calcInvVol   = helper . getInv i' $ ms
        helper       = sum . map calcHelper
        calcCoinsVol = (* coinVol) . sum . coinsToList . getCoins i' $ ms


-----


calcWeight :: Id -> MudState -> Weight
calcWeight i ms = case getType i ms of
  ConType    -> sum [ getWeight i ms, calcInvWeight, calcCoinsWeight ]
  NpcType    -> npcPC
  PCType     -> npcPC
  RmType     -> blowUp "calcWeight" "cannot calculate the weight of a room" [ showText i ]
  VesselType -> getWeight i ms + calcVesselContWeight
  _          -> getWeight i ms
  where
    npcPC                = sum [ calcInvWeight, calcCoinsWeight, calcEqWeight ]
    calcInvWeight        = helper .           getInv   i $ ms
    calcEqWeight         = helper . M.elems . getEqMap i $ ms
    helper               = sum . map (`calcWeight` ms)
    calcCoinsWeight      = (* coinWeight) . sum . coinsToList . getCoins i $ ms
    calcVesselContWeight = maybe 0 ((* mouthfulWeight) . snd) . getVesselCont i $ ms
