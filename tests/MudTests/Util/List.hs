{-# LANGUAGE ViewPatterns #-}

module MudTests.Util.List where

import Mud.Util.List

import Control.Applicative (liftA2)
import Data.List (elemIndices)
import Test.QuickCheck.Modifiers (NonNegative(..))
import Test.Tasty.QuickCheck ((==>), Property)


prop_countOcc :: Int -> [Int] -> Bool
prop_countOcc needle hay = countOcc needle hay == matches
  where
    matches = length . elemIndices needle $ hay


prop_dropElemAt :: NonNegative Int -> [Int] -> Property
prop_dropElemAt (NonNegative i) xs = i < length xs &&
                                   countOcc x xs == 1 ==>
    liftA2 (&&) ((== length xs - 1) . length) (x `notElem`) res
  where
    x   = xs !! i
    res = dropElemAt i xs


prop_mkCountList :: [Int] -> Bool
prop_mkCountList xs = mkCountList xs == mkCountList' xs
  where
    mkCountList' xs'@(sortGroup -> grouped) | ((<$> grouped) -> elemCountList) <- (,) <$> head <*> length =
      let getCountForElem x = snd (head . filter ((== x) . fst) $ elemCountList)
      in map getCountForElem xs'
