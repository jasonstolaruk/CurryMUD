{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}

module MudTests.Util.List where

import Mud.Util.List

import Control.Applicative ((<$>), (<*>))
import Data.List (elemIndices, group, sort)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


prop_countOcc :: Int -> [Int] -> Bool
prop_countOcc needle hay = countOcc needle hay == matches
  where
    matches = length . elemIndices needle $ hay


prop_mkCountList :: [Int] -> Bool
prop_mkCountList xs = mkCountList xs == mkCountList' xs
  where
    mkCountList' xs'@(group . sort -> grouped) | ((<$> grouped) -> elemCountList) <- (,) <$> head <*> length =
      let getCountForElem x = snd (head . filter ((== x) . fst) $ elemCountList)
      in map getCountForElem xs'
