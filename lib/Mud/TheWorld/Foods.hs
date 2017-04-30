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
                          , foodTag
                          , newFoodApple
                          , newFoodBanana
                          , newFoodBread
                          , newFoodFuns
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


-----


mkFoodEdibleEffects :: EdibleEffects
mkFoodEdibleEffects = EdibleEffects { _digestEffects  = Just de
                                    , _consumpEffects = Nothing }
  where
    de = EffectList . pure . Left $ ie
    ie = InstaEffect { _instaEffectSub     = MobInstaEffectPts Hp
                     , _instaEffectVal     = Just . EffectFixedVal $ 1
                     , _instaEffectFeeling = mkFoodEffectFeeling }


mkFoodEffectFeeling :: Maybe EffectFeeling
mkFoodEffectFeeling = Just . EffectFeeling foodTag $ foodWaterEffDur


foodTag :: FeelingTag
foodTag = "food"


-----


mkFruitObjTemplate :: Text -> ObjTemplate
mkFruitObjTemplate t = ObjTemplate fruitWeight
                                   fruitVol
                                   (Just t)
                                   zeroBits


-----


newFoodFuns :: [(Id, NewFoodFun)]
newFoodFuns = [ (iFoodApple,  newFoodApple )
              , (iFoodBanana, newFoodBanana)
              , (iFoodBread,  newFoodBread )
              , (iFoodOrange, newFoodOrange) ]



type NewFoodFun = MudState -> InvId -> (Id, MudState, Funs)


-- ==================================================


appleEntTemplate :: EntTemplate
appleEntTemplate = EntTemplate (Just "apple")
                               "apple" ""
                               "It's a common apple, bulbous in shape and red in color, with yellow-green splotches \
                               \here and there."
                               (Just "The ripe apple gives off a sweet, fruity smell.")
                               zeroBits


appleObjTemplate :: ObjTemplate
appleObjTemplate = mkFruitObjTemplate "You sample a small morsel of the ripe apple. It's delectable!"


appleFood :: Food
appleFood = Food (DistinctFoodId iFoodApple)
                 "The crisp apple is juicy and sweet."
                 5 -- TODO: Come up with an accurate number.


appleDistinctFood :: DistinctFood
appleDistinctFood = DistinctFood "apple" 5 30 mkFoodEdibleEffects -- TODO: Come up with accurate numbers.


newFoodApple :: NewFoodFun
newFoodApple ms = newFood ms appleEntTemplate appleObjTemplate appleFood


-----


bananaEntTemplate :: EntTemplate
bananaEntTemplate = EntTemplate (Just "banana")
                                "banana" ""
                                "The yellow banana is elongated and curved, with soft flesh rich in starch."
                                (Just "A pleasant, sugary smell emanates from the ripe banana.")
                                zeroBits


bananaObjTemplate :: ObjTemplate
bananaObjTemplate = mkFruitObjTemplate "You take a nibble off the ripe banana. The pulpous fruit is sufficiently tasty."


bananaFood :: Food
bananaFood = Food (DistinctFoodId iFoodBanana)
                  "The soft flesh has a mellow, sweet taste. This particular banana is not overly starchy."
                  5 -- TODO: Come up with an accurate number.


bananaDistinctFood :: DistinctFood
bananaDistinctFood = DistinctFood "banana" 5 30 mkFoodEdibleEffects -- TODO: Come up with accurate numbers.


newFoodBanana :: NewFoodFun
newFoodBanana ms = newFood ms bananaEntTemplate bananaObjTemplate bananaFood


-----


breadEntTemplate :: EntTemplate
breadEntTemplate = EntTemplate (Just "bread")
                               "loaf of bread" "loaves of bread"
                               "It's a loaf of common white bread made from wheat-flower dough."
                               (Just "The bread gives off the familiar inviting smell of baked goods. You don't detect \
                                     \the sourness associated with certain types of bread.")
                               zeroBits


breadObjTemplate :: ObjTemplate
breadObjTemplate = ObjTemplate 50 -- TODO: Come up with accurate numbers.
                               500
                               (Just "You sample the bread. It's light and airy, a bit on the dry side, and not \
                                     \particularly tasty.")
                               zeroBits


breadFood :: Food
breadFood = Food (DistinctFoodId iFoodBread)
                 "Though bland and dry, the bread serves well as a utilitarian nutriment."
                 50 -- TODO: Come up with an accurate number.


breadDistinctFood :: DistinctFood
breadDistinctFood = DistinctFood "bread" 50 10 mkFoodEdibleEffects -- TODO: Come up with accurate numbers.


newFoodBread :: NewFoodFun
newFoodBread ms = newFood ms breadEntTemplate breadObjTemplate breadFood


-----


orangeEntTemplate :: EntTemplate
orangeEntTemplate = EntTemplate (Just "orange")
                                "orange" ""
                                "The orange is perfectly spherical with a fragrant, rugged rind."
                                (Just "The orange has that comforting fragrance associated with citrus fruit.")
                                zeroBits


orangeObjTemplate :: ObjTemplate
orangeObjTemplate = mkFruitObjTemplate "The orange has a familiar citrus taste. This one might be a bit on the bitter \
                                       \side."


orangeFood :: Food
orangeFood = Food (DistinctFoodId iFoodOrange)
                  "Tart, tangy, and more than a little juicy!"
                  5 -- TODO: Come up with an accurate number.


orangeDistinctFood :: DistinctFood
orangeDistinctFood = DistinctFood "orange" 5 30 mkFoodEdibleEffects -- TODO: Come up with accurate numbers.


newFoodOrange :: NewFoodFun
newFoodOrange ms = newFood ms orangeEntTemplate orangeObjTemplate orangeFood
