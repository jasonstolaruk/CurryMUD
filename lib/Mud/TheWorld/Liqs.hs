{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Liqs ( distinctLiqList
                         , waterLiq ) where

import Mud.Data.State.MudData


distinctLiqList :: [(Id, DistinctLiq)]
distinctLiqList = pure (iLiqWater, waterDistinctLiq)


iLiqWater :: Id
iLiqWater = 0


waterDistinctLiq :: DistinctLiq
waterDistinctLiq = DistinctLiq . EdibleEffects Nothing $ Nothing


waterLiq :: Liq
waterLiq = Liq (DistinctLiqId iLiqWater)
               "water"
               "The water is entirely odorless."
               "The water is entirely tasteless."
               "The cool, clear water feels refreshing as it goes down."
