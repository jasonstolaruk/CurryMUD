module Mud.Data.State.Util.Calc where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp)

import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Data.State.Util.Calc"


-- ==================================================


calcWeight :: Id -> MudState -> Int
calcWeight i ms = case getType i ms of
  ConType -> sum [ getWeight i ms, calcInvWeight, calcCoinsWeight ]
  MobType -> sum [                 calcInvWeight, calcCoinsWeight, calcEqWeight ]
  PCType  -> sum [                 calcInvWeight, calcCoinsWeight, calcEqWeight ]
  RmType  -> blowUp "calcWeight" "cannot calculate the weight of a room" [ showText i ]
  _       -> getWeight i ms
  where
    calcInvWeight   = helper .           getInv   i $ ms
    calcEqWeight    = helper . M.elems . getEqMap i $ ms
    helper          = sum . map (flip calcWeight ms)
    calcCoinsWeight = (* 2) . sum . coinsToList . getCoins i $ ms
