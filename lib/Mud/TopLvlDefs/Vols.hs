{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Vols where

import Mud.Data.State.MudData
import Mud.Util.Misc


{-# ANN braceletVol ("HLint: ignore" :: String) #-}
{-# ANN swordVol    ("HLint: ignore" :: String) #-}


-- ==================================================


-- 100 "Vol" = 1 cubic in


apronVol, apronHeavyVol :: Vol
apronVol      = round (12 * 8 * 0.25 * 100 :: Double)
apronHeavyVol = plusQuarter apronVol


backSmlCap, backCap, backLrgCap :: Vol
backSmlCap = minusQuarter backCap
backCap    = 2450 * 100
backLrgCap = plusQuarter  backCap


backSmlVol, backVol, backLrgVol :: Vol
backSmlVol = minusQuarter backVol
backVol    = 12 * 8 * 3 * 100
backLrgVol = plusQuarter  backVol


bottleSmlVol, bottleVol, bottleLrgVol :: Vol
bottleSmlVol = minusThird bottleVol
bottleVol    = 45 * 100
bottleLrgVol = plusThird  bottleVol


bootsVol :: Vol
bootsVol = 3 * 2 * 4 * 100


braceletVol :: Vol
braceletVol = 3 * 1 * 3 * 100


clubVol :: Vol
clubVol = swordVol


coatVol, coatHeavyVol :: Vol
coatVol      = backVol
coatHeavyVol = backLrgVol


coinVol :: Vol
coinVol = 10


cubeVol :: Vol
cubeVol = 6 * 6 * 6 * 100


dollVol :: Vol
dollVol = 4 * 2 * 10 * 100


earVol :: Vol
earVol = 2


flowerVol :: Vol
flowerVol = round (0.5 * 0.5 * 10 * 100 :: Double)


helmLeatherVol :: Vol
helmLeatherVol = 10 * 8 * 8 * 100


jarSmlVol, jarVol, jarLrgVol :: Vol
jarSmlVol = minusThird jarVol
jarVol    = 40 * 100
jarLrgVol = plusThird jarVol


jugSmlVol, jugVol, jugLrgVol :: Vol
jugSmlVol = minusThird jugVol
jugVol    = 115 * 100
jugLrgVol = plusThird jugVol


knifeVol :: Vol
knifeVol = round (0.75 * 0.75 * 7 * 100 :: Double)


knitCapVol :: Vol
knitCapVol = round (7 * 7 * 0.25 * 100 :: Double)


neckVol :: Vol
neckVol = round (2 * 2 * 0.25 * 100 :: Double)


noseVol :: Vol
noseVol = earVol


overallsVol :: Vol
overallsVol = tabardVol + trousersVol


paperVol :: Vol
paperVol = round (8.5 * 0.002 * 11 * 100 :: Double)


potionFlaskVol :: Vol
potionFlaskVol = 14 * 100 -- 1 cup


quaffVol :: Vol
quaffVol = 175 -- 1/8th cup


ringVol :: Vol
ringVol = round (0.75 * 0.25 * 0.75 * 100 :: Double)


sackSmlCap, sackCap, sackLrgCap :: Vol
sackSmlCap = backSmlCap
sackCap    = backCap
sackLrgCap = backLrgCap


sackSmlVol, sackVol, sackLrgVol :: Vol
sackSmlVol = minusQuarter sackVol
sackVol    = round (12 * 8 * 0.5 * 100 :: Double)
sackLrgVol = plusQuarter  sackVol


sandalsVol :: Vol
sandalsVol = round (8 * 10 * 0.25 * 100 :: Double)


shirtVol :: Vol
shirtVol = round (12 * 8 * 0.25 * 100 :: Double)


swordVol, swordLongVol :: Vol
swordVol     = 6 * 1 * 30 * 100
swordLongVol = plusQuarter swordVol


tabardVol :: Vol
tabardVol = shirtVol


trousersVol, trousersBaggyVol :: Vol
trousersVol      = shirtVol
trousersBaggyVol = plusQuarter trousersVol


tunicHeavyVol :: Vol
tunicHeavyVol = plusQuarter shirtVol


waterskinVol, waterskinLrgVol :: Vol
waterskinVol    = 60  * 100
waterskinLrgVol = 120 * 100
