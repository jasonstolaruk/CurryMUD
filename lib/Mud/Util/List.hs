module Mud.Util.List ( allValues
                     , appendIfUnique
                     , countOcc
                     , countOccs
                     , dropEmpties
                     , fstList
                     , headLast
                     , headTail
                     , mkCountList
                     , nubSort ) where

import Mud.Util.Misc
import Mud.Util.Operators

import Control.Arrow ((***))
import Data.List (foldl', group, sort)
import qualified Data.Set as S (fromList, toList)


allValues :: (Enum a, Bounded a) => [a]
allValues = [ minBound .. ]


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ pure x


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> x == needle ? succ acc :? acc) 0


countOccs :: (Eq a, Ord a) => [a] -> [(a, Int)]
countOccs = map ((head *** length) . dup) . group . sort


dropEmpties :: (Eq a, Monoid a) => [a] -> [a] -- TODO: Use this.
dropEmpties = filter (()!#)


fstList :: (a -> a) -> [a] -> [a]
fstList _ []     = []
fstList f (x:xs) = f x : xs


headLast :: [a] -> (a, a)
headLast = (,) <$> head <*> last


headTail :: [a] -> (a, [a])
headTail = (,) <$> head <*> tail


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


nubSort :: (Ord a) => [a] -> [a]
nubSort = S.toList . S.fromList
