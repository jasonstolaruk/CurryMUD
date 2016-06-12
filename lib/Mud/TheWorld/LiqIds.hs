{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.LiqIds where

import Mud.Data.State.MudData (Id)


iLiqWater = 0 :: Id

iLiqPotHp        = 100 :: Id
iLiqPotInstantHp = 101 :: Id

iLiqPotSt           = 102 :: Id
iLiqPotInstantSt    = 103 :: Id
iLiqPotNegSt        = 104 :: Id
iLiqPotInstantNegSt = 105 :: Id

iLiqPotTinnitus        = 106 :: Id
iLiqPotInstantTinnitus = 107 :: Id
