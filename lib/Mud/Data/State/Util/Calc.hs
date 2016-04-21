{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.Calc ( calcBarLen
                                , calcConPerFull
                                , calcDigesterDelay
                                , calcEffAttrib
                                , calcEffDx
                                , calcEffHt
                                , calcEffMa
                                , calcEffPs
                                , calcEffSt
                                , calcEncPer
                                , calcLvlExps
                                , calcMaxEnc
                                , calcMaxMouthfuls
                                , calcMaxRaceLen
                                , calcProbConnectBlink
                                , calcProbLinkFlinch
                                , calcProbTeleShudder
                                , calcProbTeleVomit
                                , calcRegenFpAmt
                                , calcRegenFpDelay
                                , calcRegenHpAmt
                                , calcRegenHpDelay
                                , calcRegenMpAmt
                                , calcRegenMpDelay
                                , calcRegenPpAmt
                                , calcRegenPpDelay
                                , calcStomachAvailSize
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
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp)

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


-- ==================================================


calcBarLen :: Cols -> Int
calcBarLen cols = cols < 59 ? (cols - 9) :? 50


-----


calcConPerFull :: Id -> MudState -> Int
calcConPerFull i ms = let total           = foldr helper 0 . getInv i $ ms
                          helper targetId = (calcVol targetId ms +)
                      in round . (100 *) $ total `divide` getCapacity i ms


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
        helper acc (Effect (MobEffectAttrib a) (Just (DefiniteVal x)) _) | a == attrib = acc + x
        helper acc _                                                     = acc
    in foldl' helper (getBaseAttrib attrib i ms) effects


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
calcEncPer i ms = round . (100 *) $ calcWeight i ms `divide` calcMaxEnc i ms


calcMaxEnc :: Id -> MudState -> Weight
calcMaxEnc i ms = round . (100 *) $ calcEffSt i ms ^ 2 `divide` 13


-----


calcMaxMouthfuls :: Obj -> Mouthfuls
calcMaxMouthfuls = views vol (round . (`divide` mouthfulVol))


-----


calcMaxRaceLen :: Int
calcMaxRaceLen = maximum . map (T.length . showText) $ (allValues :: [Race])


-----


calcLvlExps :: [LvlExp]
calcLvlExps = [ (lvl, 1250 * lvl ^ 2) | lvl <- [1..] ]


-----


calcProbLinkFlinch :: Id -> MudState -> Int
calcProbLinkFlinch i ms = (calcEffHt i ms - 100) ^ 2 `quot` 125


calcProbConnectBlink :: Id -> MudState -> Int
calcProbConnectBlink = calcProbLinkFlinch


calcProbTeleShudder :: Id -> MudState -> Int
calcProbTeleShudder = calcProbLinkFlinch


calcProbTeleVomit :: Id -> MudState -> Int
calcProbTeleVomit i ms = (calcEffHt i ms - 100) ^ 2 `quot` 250


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
calcStomachAvailSize i ms = let avail = size - length (getStomach i ms)
                                size  | isPC i ms = calcStomachSize . getRace i $ ms
                                      | otherwise = calcStomachSize Human
                            in (avail, size)


calcStomachSize :: Race -> Int -- Stomach capacty in terms of mouthfuls.
calcStomachSize = let f = (calcStomachSize Human |&|) in \case
  Dwarf     -> f minusQuarter
  Elf       -> f minusFifth
  Felinoid  -> f plusFifth
  Hobbit    -> f minusThird
  Human     -> round $ 60 * 100 `divide` mouthfulVol -- 34 mouthfuls.
  Lagomorph -> f id
  Nymph     -> f minusFifth
  Vulpenoid -> f plusQuarter


-----


calcVesselPerFull :: Vessel -> Mouthfuls -> Int
calcVesselPerFull (view maxMouthfuls -> m) x = round . (100 *) $ x `divide` m


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
