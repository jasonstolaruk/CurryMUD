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
import Mud.TheWorld.LiqIds


distinctLiqList :: [(Id, DistinctLiq)]
distinctLiqList = [ (iLiqPotHealing,         potHealingDistinctLiq        )
                  , (iLiqPotInstantHealing,  potInstantHealingDistinctLiq )
                  , (iLiqPotInstantSt,       potInstantStDistinctLiq      )
                  , (iLiqPotInstantTinnitus, potInstantTinnitusDistinctLiq)
                  , (iLiqPotSt,              potStDistinctLiq             )
                  , (iLiqPotTinnitus,        potTinnitusDistinctLiq       )
                  , (iLiqWater,              waterDistinctLiq             ) ]


-----


waterLiq :: Liq
waterLiq = Liq (DistinctLiqId iLiqWater)
               (Don'tArticle "water")
               "The water is entirely odorless."
               "The water is entirely tasteless."
               "The cool, clear water feels refreshing as it goes down."


waterDistinctLiq :: DistinctLiq
waterDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                             , _consumpEffects = Nothing }


-----


potHealingLiq :: Liq
potHealingLiq = Liq (DistinctLiqId iLiqPotHealing)
                    (DoArticle "milky, off-white liquid")
                    "A bitterly pungent smell bombards you."
                    "" -- TODO
                    "Owing to its distinctly acrid taste, the elixir is difficult to ingest."


potHealingDistinctLiq :: DistinctLiq
potHealingDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just el
                                                  , _consumpEffects = Nothing }
  where
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = MobInstaEffectPts CurHp
                     , _instaEffectVal = Just . DefiniteVal $ 2 }


potInstantHealingLiq :: Liq
potInstantHealingLiq = Liq (DistinctLiqId iLiqPotInstantHealing)
                           (DoArticle "murky, rust-colored liquid")
                           "Almost immediately your nostrils burn and your eyes water in reaction to the tonic's \
                           \metallic bouquet."
                           "" -- TODO
                           "Ugh! The potion is truly revolting. Concentrating, you will yourself to keep it down."


potInstantHealingDistinctLiq :: DistinctLiq
potInstantHealingDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                         , _consumpEffects = Just ce }
  where
    ce = ConsumpEffects 4 30 el
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = MobInstaEffectPts CurHp
                     , _instaEffectVal = Just . RangeVal $ (8, 12) }

-----


potStLiq :: Liq
potStLiq = Liq (DistinctLiqId iLiqPotSt)
               (DoArticle "thick, muddy liquid")
               "You detect earthy scents of nuts and grasses in the draft's highly nuanced odor."
               "" -- TODO
               "The lumpy, viscous liquid is difficult to quaff."


potStDistinctLiq :: DistinctLiq
potStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just el
                                             , _consumpEffects = Nothing }
  where
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . DefiniteVal $ 2
                , _dur       = 5 * 60 }


potInstantStLiq :: Liq
potInstantStLiq = Liq (DistinctLiqId iLiqPotInstantSt)
                      (DoArticle "brown, frothy liquid")
                      "The liquid's earthy scents are reminiscent of soil and dried leaves."
                      "" -- TODO
                      "Though its taste is not particularly pleasing, the concoction goes down easily."


potInstantStDistinctLiq :: DistinctLiq
potInstantStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = ConsumpEffects 4 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . RangeVal $ (8, 12)
                , _dur       = 5 * 60 }


-----


potTinnitusLiq :: Liq
potTinnitusLiq = Liq (DistinctLiqId iLiqPotTinnitus)
                     (DoArticle "olive-green liquid")
                     "The tonic smells pleasingly sweet."
                     "" -- TODO
                     "The thirst-quenching liquid leaves you feeling refreshed."


potTinnitusDistinctLiq :: DistinctLiq
potTinnitusDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just el
                                                   , _consumpEffects = Nothing }
  where
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = InstaEffectOther tinnitusInstaEffectFunName
                     , _instaEffectVal = Nothing }


potInstantTinnitusLiq :: Liq
potInstantTinnitusLiq = Liq (DistinctLiqId iLiqPotInstantTinnitus)
                            (DoArticle "oily, puce liquid")
                            "You are greeted by a vaguely spicy, buttery odor."
                            "" -- TODO
                            "The draft coats your mouth and throat with a slippery residue."


potInstantTinnitusDistinctLiq :: DistinctLiq
potInstantTinnitusDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                          , _consumpEffects = Just ce }
  where
    ce = ConsumpEffects 4 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = EffectOther tinnitusEffectFunName
                , _effectVal = Nothing
                , _dur       = 2 * 60 }
