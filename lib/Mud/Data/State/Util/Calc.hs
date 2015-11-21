{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Calc ( calcBarLen
                                , calcEncPer
                                , calcLvlExps
                                , calcMaxEnc
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
                                , calcWeight
                                , coinWeight ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp)

import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


default (Int, Double)


-----


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Data.State.Util.Calc"


-- ==================================================


calcBarLen :: Cols -> Int
calcBarLen cols = cols < 59 ? (cols - 9) :? 50


-----


calcEncPer :: Id -> MudState -> Int
calcEncPer i ms = round . (100 *) $ calcWeight i ms `divide` calcMaxEnc i ms


calcMaxEnc :: Id -> MudState -> Int
calcMaxEnc i ms = round . (100 *) $ getSt i ms ^ 2 `divide` 13


-----


calcMaxRaceLen :: Int
calcMaxRaceLen = maximum . map (T.length . showText) $ (allValues :: [Race])


-----


calcLvlExps :: [LvlExp]
calcLvlExps = [ (lvl, 1250 * lvl ^ 2) | lvl <- [1..] ]


-----


calcProbLinkFlinch :: Id -> MudState -> Int
calcProbLinkFlinch i ms = (getHt i ms - 100) ^ 2 `quot` 125


calcProbConnectBlink :: Id -> MudState -> Int
calcProbConnectBlink = calcProbLinkFlinch


calcProbTeleShudder :: Id -> MudState -> Int
calcProbTeleShudder = calcProbLinkFlinch


calcProbTeleVomit :: Id -> MudState -> Int
calcProbTeleVomit i ms = (getHt i ms - 100) ^ 2 `quot` 250


-----


calcRegenAmt :: Double -> Int
calcRegenAmt x = round $ x / 10


calcRegenHpAmt :: Id -> MudState -> Int
calcRegenHpAmt i = calcRegenAmt . fromIntegral . getHt i


calcRegenMpAmt :: Id -> MudState -> Int
calcRegenMpAmt i ms = calcRegenAmt $ (getHt i ms + getMa i ms) `divide` 2


calcRegenPpAmt :: Id -> MudState -> Int
calcRegenPpAmt i ms = calcRegenAmt $ (getHt i ms + getPs i ms) `divide` 2


calcRegenFpAmt :: Id -> MudState -> Int
calcRegenFpAmt i ms = calcRegenAmt $ (getHt i ms + getSt i ms) `divide` 2


-----


calcRegenDelay :: Double -> Int
calcRegenDelay x = (30 +) . round $ ((x - 50) ^ 2) / 250


calcRegenHpDelay :: Id -> MudState -> Int
calcRegenHpDelay i = calcRegenDelay . fromIntegral . getHt i


calcRegenMpDelay :: Id -> MudState -> Int
calcRegenMpDelay i ms = calcRegenDelay $ (getHt i ms + getMa i ms) `divide` 2


calcRegenPpDelay :: Id -> MudState -> Int
calcRegenPpDelay i ms = calcRegenDelay $ (getHt i ms + getPs i ms) `divide` 2


calcRegenFpDelay :: Id -> MudState -> Int
calcRegenFpDelay i ms = calcRegenDelay $ (getHt i ms + getSt i ms) `divide` 2


-----


calcWeight :: Id -> MudState -> Int
calcWeight i ms = case getType i ms of
  ConType -> sum [ getWeight i ms, calcInvWeight, calcCoinsWeight ]
  NpcType -> npcPC
  PCType  -> npcPC
  RmType  -> blowUp "calcWeight" "cannot calculate the weight of a room" [ showText i ]
  _       -> getWeight i ms
  where
    npcPC           = sum [ calcInvWeight, calcCoinsWeight, calcEqWeight ]
    calcInvWeight   = helper .           getInv   i $ ms
    calcEqWeight    = helper . M.elems . getEqMap i $ ms
    helper          = sum . map (`calcWeight` ms)
    calcCoinsWeight = (* coinWeight) . sum . coinsToList . getCoins i $ ms


coinWeight :: Int
coinWeight = 2
