{-# OPTIONS_GHC -Wall -Werror #-}

module Mud.Util.List ( appendIfUnique
                     , countOcc
                     , headLast
                     , headTail
                     , mkCountList
                     , nubSort ) where

import Mud.Util.Misc

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import qualified Data.Set as S (fromList, toList)


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ [x]


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> x == needle ? succ acc :? acc) 0


headLast :: [a] -> (a, a)
headLast = (,) <$> head <*> last


headTail :: [a] -> (a, [a])
headTail = (,) <$> head <*> tail


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


nubSort :: (Ord a) => [a] -> [a]
nubSort = S.toList . S.fromList
