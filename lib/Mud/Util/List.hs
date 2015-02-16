module Mud.Util.List ( appendIfUnique
                     , countOcc
                     , countOccs
                     , headLast
                     , headTail
                     , mkCountList
                     , nubSort ) where

import Mud.Util.Misc

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Data.List (foldl', group, sort)
import qualified Data.Set as S (fromList, toList)


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ [x]


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> x == needle ? succ acc :? acc) 0


countOccs :: (Eq a, Ord a) => [a] -> [(a, Int)]
countOccs = map ((head *** length) . dup) . group . sort


headLast :: [a] -> (a, a)
headLast = (,) <$> head <*> last


headTail :: [a] -> (a, [a])
headTail = (,) <$> head <*> tail


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


nubSort :: (Ord a) => [a] -> [a]
nubSort = S.toList . S.fromList
