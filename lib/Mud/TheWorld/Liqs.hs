{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Liqs ( distinctLiqList
                         , potInstantStLiq
                         , waterLiq ) where

import Mud.Data.State.MudData


distinctLiqList :: [(Id, DistinctLiq)]
distinctLiqList = [ (iLiqWater,        waterDistinctLiq)
                  , (iLiqPotInstantSt, potInstantStDistinctLiq) ]


iLiqWater :: Id
iLiqWater = 0


iLiqPotInstantSt :: Id
iLiqPotInstantSt = 1


-----


waterDistinctLiq :: DistinctLiq
waterDistinctLiq = DistinctLiq . EdibleEffects Nothing $ Nothing


waterLiq :: Liq
waterLiq = Liq (DistinctLiqId iLiqWater)
               "water"
               "The water is entirely odorless."
               "The water is entirely tasteless."
               "The cool, clear water feels refreshing as it goes down."


-----


potInstantStDistinctLiq :: DistinctLiq
potInstantStDistinctLiq = DistinctLiq . EdibleEffects Nothing . Just $ ce
  where
    ce = ConsumpEffects 8 30 el
    el = EffectList . pure . Right $ e
    e  = Effect (MobEffectAttrib St) (Just . RangeVal $ (8, 12)) $ 5 * 60


potInstantStLiq :: Liq
potInstantStLiq = Liq (DistinctLiqId iLiqPotInstantSt)
                      "a frothy brown liquid"
                      ""
                      ""
                      ""
