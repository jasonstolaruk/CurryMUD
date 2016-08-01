{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Mud.Util.List ( allValues
                     , appendIfUnique
                     , countOcc
                     , countOccs
                     , dropEmpties
                     , dropSynonyms
                     , headLast
                     , headTail
                     , listToTuple
                     , mkCountList
                     , nubSort
                     , select ) where

import Mud.Util.Misc
import Mud.Util.Operators

import Control.Arrow ((***))
import Control.Lens (Lens', each, partsOf, view)
import Control.Lens.Each (Each)
import Control.Lens.Operators ((&), (.~))
import Data.List (foldl', group, sort)
import qualified Data.Set as S (fromList, toList)


allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound..]


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ pure x


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> x == needle ? succ acc :? acc) 0


countOccs :: (Ord a) => [a] -> [(a, Int)]
countOccs = map ((head *** length) . dup) . group . sort


dropEmpties :: (Eq a, Monoid a) => [a] -> [a]
dropEmpties = filter (()!#)


dropSynonyms :: (Eq a) => [a] -> [a] -> [a]
dropSynonyms _        []                         = []
dropSynonyms synonyms (x:xs) | x `elem` synonyms = x : filter (`notElem` synonyms) xs
                             | otherwise         = x : dropSynonyms synonyms xs


headLast :: [a] -> (a, a)
headLast = (,) <$> head <*> last


headTail :: [a] -> (a, [a])
headTail = (,) <$> head <*> tail


listToTuple :: Each s t a a => [a] -> t
listToTuple xs = undefined & partsOf each .~ xs


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


nubSort :: (Ord a) => [a] -> [a]
nubSort = S.toList . S.fromList


select :: Lens' a b -> [] a -> [] b
select l = map (view l)
