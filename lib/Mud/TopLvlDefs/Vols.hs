module Mud.TopLvlDefs.Vols where

import Mud.Data.State.MudData
import Mud.Util.Misc


-- 100 "Vol" = 1 cubic in
-- length * depth * height * 100


appleVol :: Vol
appleVol = 3 * 3 * 3


apronVol, apronHeavyVol :: Vol
apronVol      = round (12 * 8 * 0.25 * 100 :: Double)
apronHeavyVol = plusQuarter apronVol


axeSmlVol :: Vol
axeSmlVol = 4 * 2 * 13 * 100


backSmlCap, backCap, backLrgCap :: Vol
backSmlCap = minusQuarter backCap
backCap    = 2450 * 100
backLrgCap = plusQuarter  backCap


backSmlVol, backVol, backLrgVol :: Vol
backSmlVol = minusQuarter backVol
backVol    = 12 * 8 * 3 * 100
backLrgVol = plusQuarter  backVol


bananaVol :: Vol
bananaVol = round (9 * 1.25 * 1.5 * 100 :: Double)


bottleSmlVol, bottleVol, bottleLrgVol :: Vol
bottleSmlVol = minusThird bottleVol
bottleVol    = 45 * 100
bottleLrgVol = plusThird  bottleVol


bootsVol :: Vol
bootsVol = 3 * 2 * 4 * 100


braceletVol :: Vol
braceletVol = 3 * 1 * 3 * 100


breadVol :: Vol
breadVol = round (7.25 * 5.5 * 4.25 * 100 :: Double)


broadswordVol :: Vol
broadswordVol = round (3 * 1.5 * 42 * 100 :: Double)


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


gorhnaVol :: Vol
gorhnaVol = round (1 * 0.25 * 0.25 * 100 :: Double)


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


mouthfulVol :: Vol
mouthfulVol = 175 -- 1/8th cup


neckVol :: Vol
neckVol = round (2 * 2 * 0.25 * 100 :: Double)


noseVol :: Vol
noseVol = earVol


orangeVol :: Vol
orangeVol = appleVol -- TODO


overallsVol :: Vol
overallsVol = tabardVol + trousersVol


paperVol :: Vol
paperVol = round (8.5 * 0.002 * 11 * 100 :: Double)


potionFlaskVol, potionFlaskLrgVol :: Vol
potionFlaskVol    = mouthfulVol    * 4 -- 1/2 cup
potionFlaskLrgVol = potionFlaskVol * 2


quarterstaffVol :: Vol
quarterstaffVol = round (1.25 * 1.25 * 60 :: Double)


ringVol :: Vol
ringVol = round (0.75 * 0.25 * 0.75 * 100 :: Double)


rockCavyCorpseCap :: Vol
rockCavyCorpseCap = 0


rockCavyCorpseVol :: Vol
rockCavyCorpseVol = 12 * 4 * 4 * 100


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


shortswordVol :: Vol
shortswordVol = round (3 * 1.5 * 20 * 100 :: Double)


spearVol :: Vol
spearVol = round (1.5 * 1.5 * 48 * 100 :: Double)


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
