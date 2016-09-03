{-# LANGUAGE ViewPatterns #-}

module MudTests.Util.List where

import Mud.Util.List

import Data.List (elemIndices, group, sort)
import Test.QuickCheck.Modifiers (NonNegative(..))
import Test.Tasty.QuickCheck ((==>), Property)


prop_countOcc :: Int -> [Int] -> Bool
prop_countOcc needle hay = countOcc needle hay == matches
  where
    matches = length . elemIndices needle $ hay


prop_dropElem :: NonNegative Int -> [Int] -> Property
prop_dropElem (NonNegative i) xs = i < length xs &&
                                   countOcc x xs == 1 ==>
    length res == length xs - 1 && x `notElem` res
  where
    x   = xs !! i
    res = dropElem i xs


prop_mkCountList :: [Int] -> Bool
prop_mkCountList xs = mkCountList xs == mkCountList' xs
  where
    mkCountList' xs'@(group . sort -> grouped) | ((<$> grouped) -> elemCountList) <- (,) <$> head <*> length =
      let getCountForElem x = snd (head . filter ((== x) . fst) $ elemCountList)
      in map getCountForElem xs'
