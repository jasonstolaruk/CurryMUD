{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Liqs ( distinctLiqList
                         , potHpLiq
                         , potInstantHpLiq
                         , potInstantNegStLiq
                         , potInstantStLiq
                         , potInstantTinnitusLiq
                         , potNegStLiq
                         , potStLiq
                         , potTinnitusLiq
                         , waterLiq ) where

import Mud.Data.State.MudData
import Mud.Misc.EffectFuns
import Mud.TheWorld.LiqIds

import Data.Text (Text)


distinctLiqList :: [(Id, DistinctLiq)]
distinctLiqList = [ (iLiqPotHp,              potHpDistinctLiq             )
                  , (iLiqPotInstantHp,       potInstantHpDistinctLiq      )
                  , (iLiqPotInstantNegSt,    potInstantNegStDistinctLiq   )
                  , (iLiqPotInstantSt,       potInstantStDistinctLiq      )
                  , (iLiqPotInstantTinnitus, potInstantTinnitusDistinctLiq)
                  , (iLiqPotNegSt,           potNegStDistinctLiq          )
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


potHpLiq :: Liq
potHpLiq = Liq (DistinctLiqId iLiqPotHp)
               (DoArticle "milky, off-white liquid")
               "A bitterly pungent smell bombards you."
               "You imagine that the rind of some intolerably bitter, inedible fruit might taste like this."
               "Owing to its distinctly acrid taste, the elixir is difficult to ingest."


potHpDistinctLiq :: DistinctLiq
potHpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just el
                                             , _consumpEffects = Nothing }
  where
    el = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub = MobInstaEffectPts CurHp
                     , _instaEffectVal = Just . DefiniteVal $ 2 }


potInstantHpLiq :: Liq
potInstantHpLiq = Liq (DistinctLiqId iLiqPotInstantHp)
                      (DoArticle "murky, rust-colored liquid")
                      "Almost immediately your nostrils burn and your eyes water in reaction to the tonic's metallic \
                      \bouquet."
                      "The caustic taste nearly causes you to gag."
                      "Ugh! The potion is truly revolting. Concentrating, you will yourself to keep it down."


potInstantHpDistinctLiq :: DistinctLiq
potInstantHpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
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
               "You detect earthy scents of nuts and grasses in the highly nuanced odors."
               "There is a very starchy taste reminiscent of grass clippings. You feel small, soft lumps in the liquid."
               "The lumpy, viscous liquid is difficult to quaff."


potStDistinctLiq :: DistinctLiq
potStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just el
                                             , _consumpEffects = Nothing }
  where
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . DefiniteVal $ 2
                , _effectDur = 5 * 60 }


potInstantStLiq :: Liq
potInstantStLiq = Liq (DistinctLiqId iLiqPotInstantSt)
                      (DoArticle "brown, frothy liquid")
                      "The liquid's earthy scents are reminiscent of soil and dried leaves."
                      "The taste is on the bitter side. You detect some small, grainy particles dispersed among the liquid."
                      "Though its taste is not particularly pleasing, the concoction goes down easily."


potInstantStDistinctLiq :: DistinctLiq
potInstantStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = ConsumpEffects 4 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . RangeVal $ (8, 12)
                , _effectDur = 5 * 60 }


potNegStLiq :: Liq
potNegStLiq = Liq (DistinctLiqId iLiqPotNegSt)
                  (DoArticle "creamy white liquid")
                  potNegStSmell
                  potNegStTaste
                  potNegStDrink


potNegStSmell, potNegStTaste, potNegStDrink :: Text
potNegStSmell = "The creamy liquid has a faint, sweet odor."
potNegStTaste = "The creamy liquid has a consistency reminiscent of coconut juice and a sweet, vaguely fruity taste."
potNegStDrink = "The sweet, creamy liquid is delicious."


potNegStDistinctLiq :: DistinctLiq
potNegStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just el
                                                , _consumpEffects = Nothing }
  where
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . DefiniteVal $ -2
                , _effectDur = 5 * 60 }


potInstantNegStLiq :: Liq
potInstantNegStLiq = Liq (DistinctLiqId iLiqPotInstantNegSt)
                         (DoArticle "creamy indigo liquid")
                         potNegStSmell
                         potNegStTaste
                         potNegStDrink


potInstantNegStDistinctLiq :: DistinctLiq
potInstantNegStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                       , _consumpEffects = Just ce }
  where
    ce = ConsumpEffects 4 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = MobEffectAttrib St
                , _effectVal = Just . RangeVal $ (-12, -8)
                , _effectDur = 5 * 60 }


-----


potTinnitusLiq :: Liq
potTinnitusLiq = Liq (DistinctLiqId iLiqPotTinnitus)
                     (DoArticle "olive-green liquid")
                     "The tonic smells pleasingly sweet."
                     "A vague, leafy-green taste is offset by a palatable sweetness that is far from overpowering."
                     "The thirst-quenching draft leaves you feeling refreshed."


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
                            "You are greeted by a spicy, buttery odor."
                            "The oily liquid greases your tongue. Its rich, spicy taste is not particularly pleasing."
                            "The draft coats your mouth and throat with a slippery residue."


potInstantTinnitusDistinctLiq :: DistinctLiq
potInstantTinnitusDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                          , _consumpEffects = Just ce }
  where
    ce = ConsumpEffects 4 30 el
    el = EffectList . pure . Right $ e
    e  = Effect { _effectSub = EffectOther tinnitusEffectFunName
                , _effectVal = Nothing
                , _effectDur = 2 * 60 }
