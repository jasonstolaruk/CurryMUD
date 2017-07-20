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
                          , gorhnaEntTemplate
                          , gorhnaFood
                          , gorhnaObjTemplate
                          , gorhnaTag
                          , newFoodApple
                          , newFoodBanana
                          , newFoodBread
                          , newFoodFuns
                          , newFoodGorhna
                          , newFoodOrange
                          , orangeEntTemplate
                          , orangeFood
                          , orangeObjTemplate ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Make
import Mud.TheWorld.FoodIds
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Vals
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights

import Data.Bits (zeroBits)


foodList :: [(Id, DistinctFood, Food)]
foodList = [ (iFoodApple,  appleDistinctFood,  appleFood )
           , (iFoodBanana, bananaDistinctFood, bananaFood)
           , (iFoodBread,  breadDistinctFood,  breadFood )
           , (iFoodGorhna, gorhnaDistinctFood, gorhnaFood)
           , (iFoodOrange, orangeDistinctFood, orangeFood) ]


newFoodFuns :: [(Id, NewFoodFun)]
newFoodFuns = [ (iFoodApple,  newFoodApple )
              , (iFoodBanana, newFoodBanana)
              , (iFoodBread,  newFoodBread )
              , (iFoodGorhna, newFoodGorhna)
              , (iFoodOrange, newFoodOrange) ]


type NewFoodFun = MudState -> InvId -> (Id, MudState, Funs)


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
mkFoodEffectFeeling = Just . EffectFeeling foodTag $ foodWaterFeelDur


foodTag :: FeelingTag
foodTag = "food"


-- ==================================================


appleEntTemplate :: EntTemplate
appleEntTemplate = EntTemplate (Just "apple")
                               "apple" ""
                               "It's a common apple, bulbous in shape and red in color, with yellow-green splotches \
                               \here and there."
                               (Just "The ripe apple gives off a sweet, fruity smell.")
                               zeroBits


appleObjTemplate :: ObjTemplate
appleObjTemplate = ObjTemplate appleWeight
                               appleVol
                               (Just "You sample a small morsel of the ripe apple. It's delectable!")
                               fruitVal
                               Nothing
                               zeroBits


appleFood :: Food
appleFood = Food (DistinctFoodId iFoodApple)
                 "The crisp apple is juicy and sweet."
                 7


appleDistinctFood :: DistinctFood
appleDistinctFood = DistinctFood "apple" 7 15 mkFoodEdibleEffects


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
bananaObjTemplate = ObjTemplate bananaWeight
                                bananaVol
                                (Just "You pull back the peel a bit and take a nibble off the ripe banana. The pulpous \
                                      \fruit is sufficiently tasty.")
                                fruitVal
                                Nothing
                                zeroBits


bananaFood :: Food
bananaFood = Food (DistinctFoodId iFoodBanana)
                  "The soft flesh has a mellow, sweet taste. This particular banana is not overly starchy."
                  5


bananaDistinctFood :: DistinctFood
bananaDistinctFood = DistinctFood "banana" 5 6 mkFoodEdibleEffects


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
breadObjTemplate = ObjTemplate breadWeight
                               breadVol
                               (Just "You sample the bread. It's light and airy, a bit on the dry side, and not \
                                     \particularly tasty.")
                               breadVal
                               Nothing
                               zeroBits


breadFood :: Food
breadFood = Food (DistinctFoodId iFoodBread)
                 "Though bland and dry, the bread serves well as a utilitarian nutriment."
                 64


breadDistinctFood :: DistinctFood
breadDistinctFood = DistinctFood "bread" 64 {- 8 slices, 8 mouthfuls per slice -} 15 mkFoodEdibleEffects


newFoodBread :: NewFoodFun
newFoodBread ms = newFood ms breadEntTemplate breadObjTemplate breadFood


-----


gorhnaEntTemplate :: EntTemplate
gorhnaEntTemplate = EntTemplate (Just "nut")
                                "gorhna nut" ""
                                "The purple gorhna nut is about one inch long and roughly cylindrical in shape."
                                (Just "The gorhna nut has a unique floral scent decorated with overtones of mildew.")
                                zeroBits


gorhnaObjTemplate :: ObjTemplate
gorhnaObjTemplate = ObjTemplate gorhnaWeight
                                gorhnaVol
                                (Just "You sample the nut. It's not particularly appetizing. Is this what worms taste like?")
                                gorhnaVal
                                Nothing
                                zeroBits


gorhnaFood :: Food
gorhnaFood = Food (DistinctFoodId iFoodGorhna)
                  "The soft, fatty flesh of the gorhna nut nearly melts in your mouth."
                  1


gorhnaDistinctFood :: DistinctFood
gorhnaDistinctFood = DistinctFood "gorhna" 1 4 mkGorhnaEdibleEffects


mkGorhnaEdibleEffects :: EdibleEffects
mkGorhnaEdibleEffects = EdibleEffects { _digestEffects  = Just de
                                      , _consumpEffects = Nothing }
  where
    de    = EffectList [ Left effHp, Left effFp ]
    effHp = InstaEffect { _instaEffectSub     = MobInstaEffectPts Hp
                        , _instaEffectVal     = Just . EffectFixedVal $ 2
                        , _instaEffectFeeling = mkGorhnaEffectFeeling }
    effFp = InstaEffect { _instaEffectSub     = MobInstaEffectPts Fp
                        , _instaEffectVal     = Just . EffectFixedVal $ 2
                        , _instaEffectFeeling = mkGorhnaEffectFeeling }


mkGorhnaEffectFeeling :: Maybe EffectFeeling
mkGorhnaEffectFeeling = Just . EffectFeeling gorhnaTag $ potFeelDur


gorhnaTag :: FeelingTag
gorhnaTag = "gorhna"


newFoodGorhna :: NewFoodFun
newFoodGorhna ms = newFood ms gorhnaEntTemplate gorhnaObjTemplate gorhnaFood


-----


orangeEntTemplate :: EntTemplate
orangeEntTemplate = EntTemplate (Just "orange")
                                "orange" ""
                                "The orange is perfectly spherical with a fragrant, rugged rind."
                                (Just "The orange has that comforting fragrance associated with citrus fruit.")
                                zeroBits


orangeObjTemplate :: ObjTemplate
orangeObjTemplate = ObjTemplate orangeWeight
                                orangeVol
                                (Just "You pull back the peel a bit and sample the orange. There's that familiar citrus \
                                      \taste, with somewhat bitter overtones.")
                                fruitVal
                                Nothing
                                zeroBits


orangeFood :: Food
orangeFood = Food (DistinctFoodId iFoodOrange)
                  "Tart, tangy, and more than a little juicy!"
                  6


orangeDistinctFood :: DistinctFood
orangeDistinctFood = DistinctFood "orange" 6 15 mkFoodEdibleEffects


newFoodOrange :: NewFoodFun
newFoodOrange ms = newFood ms orangeEntTemplate orangeObjTemplate orangeFood
