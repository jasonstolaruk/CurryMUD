{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.LiqIds where

import Mud.Data.State.MudData (Id)


iLiqWater = 0 :: Id

iLiqPotHealing        = 100 :: Id
iLiqPotInstantHealing = 101 :: Id

iLiqPotSt        = 102 :: Id
iLiqPotInstantSt = 103 :: Id

iLiqPotTinnitus        = 104 :: Id
iLiqPotInstantTinnitus = 105 :: Id
