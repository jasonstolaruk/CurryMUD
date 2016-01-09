{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Vols where

import Mud.Data.State.MudData


apronHeavyVol :: Vol
apronHeavyVol = 650


braceletVol :: Vol
braceletVol = 200


coatVol, coatHeavyVol :: Vol
coatVol      = 700
coatHeavyVol = coatVol * 2


coinVol :: Vol
coinVol = 1


dollVol :: Vol
dollVol = 500


earVol :: Vol
earVol = 1


neckVol :: Vol
neckVol = 5


noseVol :: Vol
noseVol = earVol


ringVol :: Vol
ringVol = 1


shirtVol :: Vol
shirtVol = 500


tabardVol :: Vol
tabardVol = shirtVol


trousersVol, trousersBaggyVol :: Vol
trousersVol      = shirtVol
trousersBaggyVol = trousersVol + 75


tunicVol :: Vol
tunicVol = shirtVol
