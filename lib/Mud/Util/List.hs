{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Mud.Util.List ( allValues
                     , appendIfUnique
                     , countOcc
                     , countOccs
                     , dropElemAt
                     , dropEmpties
                     , findDelimitedSubList
                     , headLast
                     , headTail
                     , listToTuple
                     , lookupValue
                     , mkCountList
                     , nubSort
                     , select
                     , selects
                     , sortGroup ) where

import Mud.Util.Operators

import Control.Arrow ((&&&), second)
import Control.Lens (Lens', LensLike', each, partsOf, view, views)
import Control.Lens.Each (Each)
import Control.Lens.Operators ((&), (.~))
import Data.Functor.Const (Const)
import Data.List (foldl', group, isPrefixOf, sort)
import Data.List.Utils (breakList)
import qualified Data.Set as S (fromList, toList)


allValues :: (Enum a, Bounded a) => [a]
allValues = enumFrom minBound


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ pure x


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> x == needle ? succ acc :? acc) 0


countOccs :: (Ord a) => [a] -> [(a, Int)]
countOccs = map (head &&& length) . sortGroup


dropElemAt :: Int -> [a] -> [a]
dropElemAt i = uncurry (++) . second tail . splitAt i


dropEmpties :: (Eq a, Monoid a) => [a] -> [a]
dropEmpties = filter (()!#)


findDelimitedSubList :: (Eq a) => ([a], [a]) -> [a] -> Maybe [a]
findDelimitedSubList _             [] = Nothing
findDelimitedSubList (left, right) xs = case breakList (left `isPrefixOf`) xs of
    (_, [] ) -> Nothing
    (_, xs') -> case breakList (right `isPrefixOf`) . drop (length left) $ xs' of (_,    []) -> Nothing
                                                                                  (xs'', _ ) -> Just xs''


headLast :: [a] -> (,) a a
headLast = (,) <$> head <*> last


headTail :: [a] -> (,) a ([] a)
headTail = (,) <$> head <*> tail


listToTuple :: (Each s t a a) => [a] -> t
listToTuple xs = undefined & partsOf each .~ xs


lookupValue :: (Eq v) => v -> [(k, v)] -> Maybe k
lookupValue v assocs = case filter ((== v) . snd) assocs of []         -> Nothing
                                                            ((k, _):_) -> Just k


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


nubSort :: (Ord a) => [a] -> [a]
nubSort = S.toList . S.fromList


select :: Lens' a b -> [] a -> [] b
select l = map (view l)


selects :: LensLike' (Const b) s a -> (a -> b) -> [s] -> [b]
selects l f = map (views l f)


sortGroup :: (Ord a) => [a] -> [[a]]
sortGroup = group . sort
