{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Weights where

import Mud.Data.State.MudData
import Mud.Util.Misc


-- 100 "Weight" = 1 lb


apronWeight, apronHeavyWeight :: Weight
apronWeight      = 75
apronHeavyWeight = plusQuarter apronWeight


backWeight, backSmlWeight, backLrgWeight :: Weight
backWeight    = 100
backSmlWeight = minusQuarter backWeight
backLrgWeight = plusQuarter  backWeight


bootsWeight :: Weight
bootsWeight = 250


clubWeight :: Weight
clubWeight = 325


coatWeight, coatHeavyWeight :: Weight
coatWeight      = 125
coatHeavyWeight = plusQuarter coatWeight


coinWeight :: Weight
coinWeight = 2


cubeWeight :: Weight
cubeWeight = 25


dollWeight :: Weight
dollWeight = 100


earWeight :: Weight
earWeight = 1


helmLeatherWeight :: Weight
helmLeatherWeight = 30


knifeWeight :: Weight
knifeWeight = 15


knitCapWeight :: Weight
knitCapWeight = 5


neckWeight :: Weight
neckWeight = 5


noseWeight :: Weight
noseWeight = earWeight


overallsWeight :: Weight
overallsWeight = tabardWeight + trousersWeight


ringWeight :: Weight
ringWeight = 2


sackWeight, sackSmlWeight, sackLrgWeight :: Weight
sackWeight    = 40
sackSmlWeight = minusQuarter sackWeight
sackLrgWeight = plusQuarter  sackWeight


sandalsWeight :: Weight
sandalsWeight = 50


shirtWeight :: Weight
shirtWeight = 35


swordWeight, swordLongWeight :: Weight
swordWeight     = 300
swordLongWeight = plusQuarter swordWeight


tabardWeight :: Weight
tabardWeight = 50


trousersWeight, trousersBaggyWeight :: Weight
trousersWeight      = 100
trousersBaggyWeight = plusQuarter trousersWeight


tunicWeight, tunicHeavyWeight :: Weight
tunicWeight      = 50
tunicHeavyWeight = plusQuarter tunicWeight
