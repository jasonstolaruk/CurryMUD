{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Foods ( appleFood
                          , bananaFood
                          , breadFood
                          , foodList
                          , orangeFood ) where

import Mud.Data.State.MudData
import Mud.TheWorld.FoodIds


foodList :: [(Id, DistinctFood, Food)]
foodList = [ (iFoodApple,  appleDistinctFood,  appleFood )
           , (iFoodBanana, bananaDistinctFood, bananaFood)
           , (iFoodBread,  breadDistinctFood,  breadFood )
           , (iFoodOrange, orangeDistinctFood, orangeFood) ]


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
                 "The crisp apple is juicy and sweet."
                 fruitMouths


fruitMouths :: Mouthfuls
fruitMouths = 5


appleDistinctFood :: DistinctFood
appleDistinctFood = mkDistinctFood fruitMouths


-----


bananaFood :: Food
bananaFood = Food (DistinctFoodId iFoodBanana)
                  "The pulpous banana is sufficiently tasty."
                  fruitMouths


bananaDistinctFood :: DistinctFood
bananaDistinctFood = mkDistinctFood fruitMouths


-----


breadFood :: Food
breadFood = Food (DistinctFoodId iFoodBread)
                 "Though bland and dry, the bread serves well as a utilitarian nutriment."
                 breadMouths


breadMouths :: Mouthfuls
breadMouths = 60


breadDistinctFood :: DistinctFood
breadDistinctFood = mkDistinctFood breadMouths


-----


orangeFood :: Food
orangeFood = Food (DistinctFoodId iFoodOrange)
                  "Tart, tangy, and more than a little juicy!"
                  fruitMouths


orangeDistinctFood :: DistinctFood
orangeDistinctFood = mkDistinctFood fruitMouths
