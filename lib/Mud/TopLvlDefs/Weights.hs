{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Weights where

import Mud.Data.State.MudData


apronHeavyWeight :: Weight
apronHeavyWeight = 225


coatWeight, coatHeavyWeight :: Weight
coatWeight      = 150
coatHeavyWeight = coatWeight * 5


coinWeight :: Weight
coinWeight = 2


dollWeight :: Weight
dollWeight = 25


earWeight :: Weight
earWeight = 2


neckWeight :: Weight
neckWeight = 4


noseWeight :: Weight
noseWeight = earWeight


ringWeight :: Weight
ringWeight = 5


shirtWeight :: Weight
shirtWeight = 100


tabardWeight :: Weight
tabardWeight = 150


trousersWeight, trousersBaggyWeight :: Weight
trousersWeight      = 100
trousersBaggyWeight = trousersWeight + 25


tunicWeight :: Weight
tunicWeight = 225
