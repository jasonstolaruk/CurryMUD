{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Weights where

import Mud.Data.State.MudData
import Mud.Util.Misc


-- 100 "Weight" = 1 lb


apronWeight, apronHeavyWeight :: Weight
apronWeight      = 75
apronHeavyWeight = plusQuarter apronWeight


backSmlWeight, backWeight, backLrgWeight :: Weight
backSmlWeight = minusQuarter backWeight
backWeight    = 100
backLrgWeight = plusQuarter  backWeight


bootsWeight :: Weight
bootsWeight = 250


bottleSmlWeight, bottleWeight, bottleLrgWeight :: Weight
bottleSmlWeight = minusThird bottleWeight
bottleWeight    = 250
bottleLrgWeight = plusThird  bottleWeight


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


flowerWeight :: Weight
flowerWeight = 10


helmLeatherWeight :: Weight
helmLeatherWeight = 30


jarSmlWeight, jarWeight, jarLrgWeight :: Weight
jarSmlWeight = minusThird jarWeight
jarWeight    = 120
jarLrgWeight = plusThird  jarWeight


jugSmlWeight, jugWeight, jugLrgWeight :: Weight
jugSmlWeight = minusThird jugWeight
jugWeight    = 340
jugLrgWeight = plusThird  jugWeight


knifeWeight :: Weight
knifeWeight = 15


knitCapWeight :: Weight
knitCapWeight = 5


mouthfulWeight :: Weight
mouthfulWeight = 6


neckWeight :: Weight
neckWeight = 5


noseWeight :: Weight
noseWeight = earWeight


overallsWeight :: Weight
overallsWeight = tabardWeight + trousersWeight


paperWeight :: Weight
paperWeight = 1


potionFlaskWeight, potionFlaskLrgWeight :: Weight
potionFlaskWeight    = 30
potionFlaskLrgWeight = potionFlaskWeight * 2


ringWeight :: Weight
ringWeight = 2


rockCavyCorpseWeight :: Weight
rockCavyCorpseWeight = 220


sackSmlWeight, sackWeight, sackLrgWeight :: Weight
sackSmlWeight = minusQuarter sackWeight
sackWeight    = 40
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


waterskinWeight, waterskinLrgWeight :: Weight
waterskinWeight    = 20
waterskinLrgWeight = 35
