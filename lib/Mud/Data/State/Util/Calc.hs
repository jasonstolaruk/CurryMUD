{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Calc where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp)

import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


default (Int, Double)


-----


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Data.State.Util.Calc"


-- ==================================================


calcEncPer :: Id -> MudState -> Int
calcEncPer i ms = round $ calcWeight i ms `divide` calcMaxEnc i ms * 100


calcMaxEnc :: Id -> MudState -> Int
calcMaxEnc i ms = getSt i ms ^ 2 `quot` 13 * 100


-----


calcProbLinkFlinch :: Id -> MudState -> Int
calcProbLinkFlinch i ms = (getHt i ms - 100) ^ 2 `quot` 125


calcProbTeleShudder :: Id -> MudState -> Int
calcProbTeleShudder = calcProbLinkFlinch


calcProbTeleVomit :: Id -> MudState -> Int
calcProbTeleVomit i ms = (getHt i ms - 100) ^ 2 `quot` 250


-----


calcWeight :: Id -> MudState -> Int
calcWeight i ms = case getType i ms of
  ConType -> sum [ getWeight i ms, calcInvWeight, calcCoinsWeight ]
  MobType -> mobPC
  PCType  -> mobPC
  RmType  -> blowUp "calcWeight" "cannot calculate the weight of a room" [ showText i ]
  _       -> getWeight i ms
  where
    mobPC           = sum [ calcInvWeight, calcCoinsWeight, calcEqWeight ]
    calcInvWeight   = helper .           getInv   i $ ms
    calcEqWeight    = helper . M.elems . getEqMap i $ ms
    helper          = sum . map (`calcWeight` ms)
    calcCoinsWeight = (* coinWeight) . sum . coinsToList . getCoins i $ ms


coinWeight :: Int
coinWeight = 2
