{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Foods ( breadFood
                          , foodList ) where

import Mud.Data.State.MudData
import Mud.TheWorld.FoodIds


foodList :: [(Id, DistinctFood, Food)]
foodList = [(iFoodBread, breadDistinctFood, breadFood)]


-----


breadFood :: Food
breadFood = Food (DistinctFoodId iFoodBread)
                 "eat desc"
                 50


breadDistinctFood :: DistinctFood
breadDistinctFood = DistinctFood 50 EdibleEffects { _digestEffects  = Just de
                                                  , _consumpEffects = Nothing }
  where
    de = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts Hp
                     , _instaEffectVal     = Just . EffectFixedVal $ foodEffectVal
                     , _instaEffectFeeling = Nothing }


foodEffectVal :: Int
foodEffectVal = 1
