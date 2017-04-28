{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Foods ( appleEntTemplate
                          , appleFood
                          , appleObjTemplate
                          , bananaEntTemplate
                          , bananaFood
                          , bananaObjTemplate
                          , breadEntTemplate
                          , breadFood
                          , breadObjTemplate
                          , foodList
                          , newFoodApple
                          , newFoodBanana
                          , newFoodBread
                          , newFoodOrange
                          , orangeEntTemplate
                          , orangeFood
                          , orangeObjTemplate ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Make
import Mud.TheWorld.FoodIds
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights

import Data.Bits (zeroBits)
import Data.Text (Text)


foodList :: [(Id, DistinctFood, Food)]
foodList = [ (iFoodApple,  appleDistinctFood,  appleFood )
           , (iFoodBanana, bananaDistinctFood, bananaFood)
           , (iFoodBread,  breadDistinctFood,  breadFood )
           , (iFoodOrange, orangeDistinctFood, orangeFood) ]


mkDistinctFood :: FoodName -> Mouthfuls -> DistinctFood
mkDistinctFood n m = DistinctFood n m EdibleEffects { _digestEffects  = Just de
                                                    , _consumpEffects = Nothing }
  where
    de = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts Hp
                     , _instaEffectVal     = Just . EffectFixedVal $ 1
                     , _instaEffectFeeling = Nothing }


mkFruitObjTemplate :: Text -> ObjTemplate
mkFruitObjTemplate t = ObjTemplate fruitWeight
                                   fruitVol
                                   (Just t)
                                   zeroBits


-----


appleEntTemplate :: EntTemplate -- TODO: Finish food definitions.
appleEntTemplate = EntTemplate (Just "apple")
                               "apple" ""
                               "apple desc"
                               (Just "apple smell")
                               zeroBits


appleObjTemplate :: ObjTemplate
appleObjTemplate = mkFruitObjTemplate "apple taste"


appleFood :: Food
appleFood = Food (DistinctFoodId iFoodApple)
                 "The crisp apple is juicy and sweet."
                 fruitMouths


appleDistinctFood :: DistinctFood
appleDistinctFood = mkDistinctFood "apple" fruitMouths


newFoodApple :: MudState -> InvId -> (Id, MudState, Funs)
newFoodApple ms = newFood ms appleEntTemplate appleObjTemplate appleFood


-----


bananaEntTemplate :: EntTemplate
bananaEntTemplate = EntTemplate (Just "banana")
                                "banana" ""
                                "banana desc"
                                (Just "banana smell")
                                zeroBits


bananaObjTemplate :: ObjTemplate
bananaObjTemplate = mkFruitObjTemplate "banana taste"


bananaFood :: Food
bananaFood = Food (DistinctFoodId iFoodBanana)
                  "The pulpous banana is sufficiently tasty."
                  fruitMouths


bananaDistinctFood :: DistinctFood
bananaDistinctFood = mkDistinctFood "banana" fruitMouths


newFoodBanana :: MudState -> InvId -> (Id, MudState, Funs)
newFoodBanana ms = newFood ms bananaEntTemplate bananaObjTemplate bananaFood


-----


breadEntTemplate :: EntTemplate
breadEntTemplate = EntTemplate (Just "bread")
                               "loaf of bread" "loaves of bread"
                               "bread desc"
                               (Just "bread smell")
                               zeroBits


breadObjTemplate :: ObjTemplate
breadObjTemplate = mkFruitObjTemplate "bread taste"


breadFood :: Food
breadFood = Food (DistinctFoodId iFoodBread)
                 "Though bland and dry, the bread serves well as a utilitarian nutriment."
                 breadMouths


breadDistinctFood :: DistinctFood
breadDistinctFood = mkDistinctFood "bread" breadMouths


newFoodBread :: MudState -> InvId -> (Id, MudState, Funs)
newFoodBread ms = newFood ms breadEntTemplate breadObjTemplate breadFood


-----


orangeEntTemplate :: EntTemplate
orangeEntTemplate = EntTemplate (Just "orange")
                                "orange" ""
                                "orange desc"
                                (Just "orange smell")
                                zeroBits


orangeObjTemplate :: ObjTemplate
orangeObjTemplate = mkFruitObjTemplate "orange taste"


orangeFood :: Food
orangeFood = Food (DistinctFoodId iFoodOrange)
                  "Tart, tangy, and more than a little juicy!"
                  fruitMouths


orangeDistinctFood :: DistinctFood
orangeDistinctFood = mkDistinctFood "orange" fruitMouths


newFoodOrange :: MudState -> InvId -> (Id, MudState, Funs)
newFoodOrange ms = newFood ms orangeEntTemplate orangeObjTemplate orangeFood
