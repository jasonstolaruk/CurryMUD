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


backCap, backSmlCap, backLrgCap :: Vol
backCap    = 2450 * 100
backSmlCap = minusQuarter backCap
backLrgCap = plusQuarter  backCap


backVol, backSmlVol, backLrgVol :: Vol
backVol    = 12 * 8 * 3 * 100
backSmlVol = minusQuarter backVol
backLrgVol = plusQuarter  backVol


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


knifeVol :: Vol
knifeVol = round (0.75 * 0.75 * 7 * 100 :: Double)


knitCapVol :: Vol
knitCapVol = round (7 * 7 * 0.25 * 100 :: Double)


neckVol :: Vol
neckVol = round (2 * 2 * 0.25 * 100 :: Double)


noseVol :: Vol
noseVol = earVol


overallsVol :: Weight
overallsVol = tabardVol + trousersVol


ringVol :: Vol
ringVol = round (0.75 * 0.25 * 0.75 * 100 :: Double)


sackCap, sackSmlCap, sackLrgCap :: Vol
sackCap    = backCap
sackSmlCap = backSmlCap
sackLrgCap = backLrgCap


sackVol, sackSmlVol, sackLrgVol :: Vol
sackVol    = round (12 * 8 * 0.5 * 100 :: Double)
sackSmlVol = minusQuarter sackVol
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
