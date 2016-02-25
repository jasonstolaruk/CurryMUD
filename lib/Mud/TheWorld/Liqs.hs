{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Liqs ( distinctLiqList
                         , potHealingLiq
                         , potInstantHealingLiq
                         , potInstantStLiq
                         , potInstantTinnitusLiq
                         , potStLiq
                         , potTinnitusLiq
                         , waterLiq ) where

import Mud.Data.State.MudData
import Mud.Misc.EffectFuns


distinctLiqList :: [(Id, DistinctLiq)]
distinctLiqList = [ (iLiqPotHealing,         potHealingDistinctLiq        )
                  , (iLiqPotInstantHealing,  potInstantHealingDistinctLiq )
                  , (iLiqPotInstantSt,       potInstantStDistinctLiq      )
                  , (iLiqPotInstantTinnitus, potInstantTinnitusDistinctLiq)
                  , (iLiqPotSt,              potStDistinctLiq             )
                  , (iLiqPotTinnitus,        potTinnitusDistinctLiq       )
                  , (iLiqWater,              waterDistinctLiq             ) ]


iLiqWater :: Id -- TODO: Make a "LiqIds" module?
iLiqWater = 0


iLiqPotHealing, iLiqPotInstantHealing :: Id
iLiqPotHealing        = 100
iLiqPotInstantHealing = 101


iLiqPotSt, iLiqPotInstantSt :: Id
iLiqPotSt        = 102
iLiqPotInstantSt = 103


iLiqPotTinnitus, iLiqPotInstantTinnitus :: Id
iLiqPotTinnitus        = 104
iLiqPotInstantTinnitus = 105


-----


waterLiq :: Liq
waterLiq = Liq (DistinctLiqId iLiqWater)
               "water"
               "The water is entirely odorless."
               "The water is entirely tasteless."
               "The cool, clear water feels refreshing as it goes down."


waterDistinctLiq :: DistinctLiq
waterDistinctLiq = DistinctLiq . EdibleEffects Nothing $ Nothing


-----


potHealingLiq :: Liq
potHealingLiq = Liq (DistinctLiqId iLiqPotHealing)
                    "a milky, off-white liquid"
                    "" -- TODO
                    ""
                    ""


potHealingDistinctLiq :: DistinctLiq
potHealingDistinctLiq = DistinctLiq . EdibleEffects (Just el) $ Nothing
  where
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = MobInstaEffectPts CurHp
                     , _instaEffectVal = Just . DefiniteVal $ 2 }


potInstantHealingLiq :: Liq
potInstantHealingLiq = Liq (DistinctLiqId iLiqPotInstantHealing)
                           "a murky, rust-colored liquid"
                           "" -- TODO
                           ""
                           ""


potInstantHealingDistinctLiq :: DistinctLiq
potInstantHealingDistinctLiq = DistinctLiq . EdibleEffects Nothing . Just $ ce
  where
    ce = ConsumpEffects 8 30 el
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = MobInstaEffectPts CurHp
                     , _instaEffectVal = Just . RangeVal $ (8, 12) }

-----


potStLiq :: Liq
potStLiq = Liq (DistinctLiqId iLiqPotSt)
               "a thick, muddy liquid"
               "" -- TODO
               ""
               ""


potStDistinctLiq :: DistinctLiq
potStDistinctLiq = DistinctLiq . EdibleEffects (Just el) $ Nothing
  where
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . DefiniteVal $ 2
                , _dur       = 5 * 60 }


potInstantStLiq :: Liq
potInstantStLiq = Liq (DistinctLiqId iLiqPotInstantSt)
                      "a brown, frothy liquid"
                      "" -- TODO
                      ""
                      ""


potInstantStDistinctLiq :: DistinctLiq
potInstantStDistinctLiq = DistinctLiq . EdibleEffects Nothing . Just $ ce
  where
    ce = ConsumpEffects 8 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . RangeVal $ (8, 12)
                , _dur       = 5 * 60 }


-----


potTinnitusLiq :: Liq
potTinnitusLiq = Liq (DistinctLiqId iLiqPotTinnitus)
                     "an olive-green liquid"
                     "" -- TODO
                     ""
                     ""


potTinnitusDistinctLiq :: DistinctLiq
potTinnitusDistinctLiq = DistinctLiq . EdibleEffects (Just el) $ Nothing
  where
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = InstaEffectOther tinnitusInstaEffectFunName
                     , _instaEffectVal = Nothing }


potInstantTinnitusLiq :: Liq
potInstantTinnitusLiq = Liq (DistinctLiqId iLiqPotInstantTinnitus)
                            "an oily, puce liquid"
                            "" -- TODO
                            ""
                            ""


potInstantTinnitusDistinctLiq :: DistinctLiq
potInstantTinnitusDistinctLiq = DistinctLiq . EdibleEffects Nothing . Just $ ce
  where
    ce = ConsumpEffects 8 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = EffectOther tinnitusEffectFunName
                , _effectVal = Nothing
                , _dur       = 2 * 60 }
