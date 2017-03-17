{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Foods ( appleFood
                          , breadFood
                          , foodList ) where

import Mud.Data.State.MudData
import Mud.TheWorld.FoodIds


foodList :: [(Id, DistinctFood, Food)]
foodList = [ (iFoodApple, appleDistinctFood, appleFood)
           , (iFoodBread, breadDistinctFood, breadFood) ]


mkDistinctFood :: Mouthfuls -> DistinctFood
mkDistinctFood m = DistinctFood m EdibleEffects { _digestEffects  = Just de
                                                , _consumpEffects = Nothing }
  where
    de = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts Hp
                     , _instaEffectVal     = Just . EffectFixedVal $ 1
                     , _instaEffectFeeling = Nothing }


-----


appleFood :: Food
appleFood = Food (DistinctFoodId iFoodApple)
                 "eat desc"
                 fruitMouths


fruitMouths :: Mouthfuls
fruitMouths = 5


appleDistinctFood :: DistinctFood
appleDistinctFood = mkDistinctFood fruitMouths


-----


breadFood :: Food
breadFood = Food (DistinctFoodId iFoodBread)
                 "eat desc"
                 breadMouths


breadMouths :: Mouthfuls
breadMouths = 60


breadDistinctFood :: DistinctFood
breadDistinctFood = mkDistinctFood breadMouths
