{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Liqs ( distinctLiqList
                         , potDxLiq
                         , potFpLiq
                         , potHpLiq
                         , potHtLiq
                         , potInstantDxLiq
                         , potInstantFpLiq
                         , potInstantHpLiq
                         , potInstantHtLiq
                         , potInstantMaLiq
                         , potInstantMpLiq
                         , potInstantNegStLiq
                         , potInstantPpLiq
                         , potInstantPsLiq
                         , potInstantStLiq
                         , potInstantTinnitusLiq
                         , potMaLiq
                         , potMpLiq
                         , potNegStLiq
                         , potPpLiq
                         , potPsLiq
                         , potStLiq
                         , potTinnitusLiq
                         , waterLiq ) where

import Control.Lens (both)
import Control.Lens.Operators ((&), (%~))
import Mud.Cmds.Msgs.Misc
import Mud.Data.State.MudData
import Mud.Misc.EffectFuns
import Mud.TheWorld.LiqIds
import Mud.TopLvlDefs.Misc

import Data.Text (Text)


distinctLiqList :: [(Id, DistinctLiq)]
distinctLiqList = [ (iLiqPotDx,              potDxDistinctLiq             )
                  , (iLiqPotFp,              potFpDistinctLiq             )
                  , (iLiqPotHp,              potHpDistinctLiq             )
                  , (iLiqPotHt,              potHtDistinctLiq             )
                  , (iLiqPotInstantDx,       potInstantDxDistinctLiq      )
                  , (iLiqPotInstantFp,       potInstantFpDistinctLiq      )
                  , (iLiqPotInstantHp,       potInstantHpDistinctLiq      )
                  , (iLiqPotInstantHt,       potInstantHtDistinctLiq      )
                  , (iLiqPotInstantMa,       potInstantMaDistinctLiq      )
                  , (iLiqPotInstantMp,       potInstantMpDistinctLiq      )
                  , (iLiqPotInstantNegSt,    potInstantNegStDistinctLiq   )
                  , (iLiqPotInstantPp,       potInstantPpDistinctLiq      )
                  , (iLiqPotInstantPs,       potInstantPsDistinctLiq      )
                  , (iLiqPotInstantSt,       potInstantStDistinctLiq      )
                  , (iLiqPotInstantTinnitus, potInstantTinnitusDistinctLiq)
                  , (iLiqPotMa,              potMaDistinctLiq             )
                  , (iLiqPotMp,              potMpDistinctLiq             )
                  , (iLiqPotNegSt,           potNegStDistinctLiq          )
                  , (iLiqPotPp,              potPpDistinctLiq             )
                  , (iLiqPotPs,              potPsDistinctLiq             )
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
potHpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurHp
                     , _instaEffectVal     = Just . EffectFixedVal $ potXpEffectVal
                     , _instaEffectFeeling = mkPotEffectFeeling "potHp" }


potXpEffectVal :: Int
potXpEffectVal = 2


mkPotEffectFeeling :: FeelingTag -> Maybe EffectFeeling
mkPotEffectFeeling tag = Just . EffectFeeling tag $ 10


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
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurHp
                     , _instaEffectVal     = Just . EffectRangedVal $ potInstantXpEffectRange
                     , _instaEffectFeeling = mkPotEffectFeeling "potHp" }


mkPotConsumpEffects :: EffectList -> ConsumpEffects
mkPotConsumpEffects = ConsumpEffects 4 30


potInstantXpEffectRange :: Range
potInstantXpEffectRange = (8, 12)


-----


potMpLiq :: Liq
potMpLiq = Liq (DistinctLiqId iLiqPotMp)
               (DoArticle "cloudy, semi-transparent liquid")
               noSmellMsg
               "Save for a nearly indetectable chalkiness, the liquid is tasteless."
               "The cloudy liquid goes down like water."


potMpDistinctLiq :: DistinctLiq
potMpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurMp
                     , _instaEffectVal     = Just . EffectFixedVal $ potXpEffectVal
                     , _instaEffectFeeling = mkPotEffectFeeling "potMp" }


potInstantMpLiq :: Liq
potInstantMpLiq = Liq (DistinctLiqId iLiqPotInstantMp)
                      (DoArticle "crimson liquid")
                      "There is a fruity smell of strawberries or perhaps raspberries."
                      "The tart taste makes you want to smack your lips."
                      "The juicy, piquant liquid makes a pleasant beverage."


potInstantMpDistinctLiq :: DistinctLiq
potInstantMpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurMp
                     , _instaEffectVal     = Just . EffectRangedVal $ potInstantXpEffectRange
                     , _instaEffectFeeling = mkPotEffectFeeling "potMp" }


-----


potPpLiq :: Liq
potPpLiq = Liq (DistinctLiqId iLiqPotPp)
               (DoArticle "shocking pink liquid")
               noSmellMsg
               "The thick, powdery concoction tastes sweet and creamy."
               "Despite its pink color and slight grittiness, the tonic reminds you of sweetened cow milk."


potPpDistinctLiq :: DistinctLiq
potPpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurPp
                     , _instaEffectVal     = Just . EffectFixedVal $ potXpEffectVal
                     , _instaEffectFeeling = mkPotEffectFeeling "potPp" }


potInstantPpLiq :: Liq
potInstantPpLiq = Liq (DistinctLiqId iLiqPotInstantPp)
                      (DoArticle "pale yellow liquid")
                      "There is a yeast-like smell that reminds you of freashly-kneaded dough."
                      "You don't taste much until you are hit with a grain-like, malty aftertaste."
                      "The scents of yeast and fermentation fill your nostrils."


potInstantPpDistinctLiq :: DistinctLiq
potInstantPpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurPp
                     , _instaEffectVal     = Just . EffectRangedVal $ potInstantXpEffectRange
                     , _instaEffectFeeling = mkPotEffectFeeling "potPp" }


-----


potFpLiq :: Liq
potFpLiq = Liq (DistinctLiqId iLiqPotFp)
               (DoArticle "dark black liquid")
               "The dark black liquid looks as though it could be coffee, but it smells heavily of licorice."
               "The dark black liquid tastes bitingly of licorice and salt."
               "The concoction's briny flavors are far too strong for your tastes."


potFpDistinctLiq :: DistinctLiq
potFpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurFp
                     , _instaEffectVal     = Just . EffectFixedVal $ potXpEffectVal
                     , _instaEffectFeeling = mkPotEffectFeeling "potFp" }


potInstantFpLiq :: Liq
potInstantFpLiq = Liq (DistinctLiqId iLiqPotInstantFp)
                      (DoArticle "clear, shimmering liquid")
                      "You sniff the fumes omitted by the clear liquid. The heavy scent is similar to that of alcohol, \
                      \but with sharp, vinegar-like overtones."
                      "You are overwhelmed by a stunning mix of sharp and bitingly bad flavors."
                      "You fight back the urge to gag."


potInstantFpDistinctLiq :: DistinctLiq
potInstantFpDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts CurFp
                     , _instaEffectVal     = Just . EffectRangedVal $ potInstantXpEffectRange
                     , _instaEffectFeeling = mkPotEffectFeeling "potFp" }


-----


potStLiq :: Liq
potStLiq = Liq (DistinctLiqId iLiqPotSt)
               (DoArticle "thick, muddy liquid")
               "You detect earthy scents of nuts and grasses in the highly nuanced odors."
               "There is a very starchy taste reminiscent of grass clippings. You sense small, soft lumps in the liquid."
               "The lumpy, viscous liquid is difficult to quaff."


potStDistinctLiq :: DistinctLiq
potStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib St
                , _effectVal     = Just . EffectFixedVal $ potAttribEffectVal
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


potAttribEffectVal :: Int
potAttribEffectVal = 2


potAttribEffectDur :: Seconds
potAttribEffectDur = 5 * 60


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
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib St
                , _effectVal     = Just . EffectRangedVal $ potInstantAtttribEffectRange
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


potInstantAtttribEffectRange :: Range
potInstantAtttribEffectRange = (8, 12)


-----


potDxLiq :: Liq
potDxLiq = Liq (DistinctLiqId iLiqPotDx)
               (DoArticle "oily, maroon liquid")
               "A fishy smell bombards you. Gross!"
               "The dense, oily liquid leaves a strong fishy taste in your mouth."
               "The scent of raw fish and the taste of fish oil overwhelm your senses."


potDxDistinctLiq :: DistinctLiq
potDxDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Dx
                , _effectVal     = Just . EffectFixedVal $ potAttribEffectVal
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


potInstantDxLiq :: Liq
potInstantDxLiq = Liq (DistinctLiqId iLiqPotInstantDx)
                      (DoArticle "heavy, translucent liquid")
                      "There is a discinctly acidic aroma of lemon and lime."
                      "The liquid, though colorless, is rather thick, and has a notably acidic aftertaste."
                      "The dense tonic is oddly tart. Your eyes water slightly."


potInstantDxDistinctLiq :: DistinctLiq
potInstantDxDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Dx
                , _effectVal     = Just . EffectRangedVal $ potInstantAtttribEffectRange
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


-----


potHtLiq :: Liq
potHtLiq = Liq (DistinctLiqId iLiqPotHt)
               (DoArticle "")
               "" -- TODO
               ""
               ""


potHtDistinctLiq :: DistinctLiq
potHtDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Ht
                , _effectVal     = Just . EffectFixedVal $ potAttribEffectVal
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


potInstantHtLiq :: Liq
potInstantHtLiq = Liq (DistinctLiqId iLiqPotInstantHt)
                      (DoArticle "")
                      "" -- TODO
                      ""
                      ""


potInstantHtDistinctLiq :: DistinctLiq
potInstantHtDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Ht
                , _effectVal     = Just . EffectRangedVal $ potInstantAtttribEffectRange
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


-----


potMaLiq :: Liq
potMaLiq = Liq (DistinctLiqId iLiqPotMa)
               (DoArticle "")
               "" -- TODO
               ""
               ""


potMaDistinctLiq :: DistinctLiq
potMaDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Ma
                , _effectVal     = Just . EffectFixedVal $ potAttribEffectVal
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


potInstantMaLiq :: Liq
potInstantMaLiq = Liq (DistinctLiqId iLiqPotInstantMa)
                      (DoArticle "")
                      "" -- TODO
                      ""
                      ""


potInstantMaDistinctLiq :: DistinctLiq
potInstantMaDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Ma
                , _effectVal     = Just . EffectRangedVal $ potInstantAtttribEffectRange
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


-----


potPsLiq :: Liq
potPsLiq = Liq (DistinctLiqId iLiqPotPs)
               (DoArticle "")
               "" -- TODO
               ""
               ""


potPsDistinctLiq :: DistinctLiq
potPsDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                             , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Ps
                , _effectVal     = Just . EffectFixedVal $ potAttribEffectVal
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


potInstantPsLiq :: Liq
potInstantPsLiq = Liq (DistinctLiqId iLiqPotInstantPs)
                      (DoArticle "")
                      "" -- TODO
                      ""
                      ""


potInstantPsDistinctLiq :: DistinctLiq
potInstantPsDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Nothing
                                                    , _consumpEffects = Just ce }
  where
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib Ps
                , _effectVal     = Just . EffectRangedVal $ potInstantAtttribEffectRange
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


-----


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
potNegStDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                                , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib St
                , _effectVal     = Just . EffectFixedVal . negate $ potAttribEffectVal
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


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
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = MobEffectAttrib St
                , _effectVal     = Just . EffectRangedVal $ (potInstantAtttribEffectRange & both %~ negate)
                , _effectDur     = potAttribEffectDur
                , _effectFeeling = Nothing }


-----


potTinnitusLiq :: Liq
potTinnitusLiq = Liq (DistinctLiqId iLiqPotTinnitus)
                     (DoArticle "olive-green liquid")
                     "The tonic smells pleasingly sweet."
                     "A vague, leafy-green taste is offset by a palatable sweetness that is far from overpowering."
                     "The thirst-quenching draft leaves you feeling refreshed."


potTinnitusDistinctLiq :: DistinctLiq
potTinnitusDistinctLiq = DistinctLiq EdibleEffects { _digestEffects  = Just es
                                                   , _consumpEffects = Nothing }
  where
    es = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = InstaEffectOther tinnitusInstaEffectFunName
                     , _instaEffectVal     = Nothing
                     , _instaEffectFeeling = Just . EffectFeeling "potTinnitus" $ 30 }


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
    ce = mkPotConsumpEffects es
    es = EffectList . pure . Right $ e
    e  = Effect { _effectTag     = Nothing
                , _effectSub     = EffectOther tinnitusEffectFunName
                , _effectVal     = Nothing
                , _effectDur     = 2 * 60
                , _effectFeeling = Just . EffectFeeling "potTinnitus" $ 2 * 60 }
