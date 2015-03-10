{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Text ( aOrAn
                     , aOrAnOnLower
                     , capitalize
                     , dropBlanks
                     , findFullNameForAbbrev
                     , findFullNameForAbbrevSnd
                     , headTail
                     , isCapital
                     , mkDateTimeTxt
                     , mkOrdinal
                     , mkTimestamp
                     , nl
                     , nlPrefix
                     , nlnl
                     , notInfixOf
                     , showText
                     , stripControl
                     , stripTelnet
                     , theOnLower
                     , theOnLowerCap
                     , uncapitalize ) where

import Mud.TopLvlDefs.Chars
import Mud.Util.Misc

import Control.Arrow ((***))
import Control.Monad (guard)
import Data.Char (isUpper, toLower, toUpper)
import Data.List (sort)
import Data.Monoid ((<>))
import qualified Data.Text as T


aOrAn :: T.Text -> T.Text
aOrAn (T.strip -> t) | T.null t             = ""
                     | isVowel . T.head $ t = "an " <> t
                     | otherwise            = "a "  <> t


aOrAnOnLower :: T.Text -> T.Text
aOrAnOnLower t | isCapital t = t
               | otherwise   = aOrAn t


isCapital :: T.Text -> Bool
isCapital ""            = False
isCapital (T.head -> h) = isUpper h


-----


capitalize :: T.Text -> T.Text
capitalize = capsHelper toUpper


uncapitalize :: T.Text -> T.Text
uncapitalize = capsHelper toLower


capsHelper :: (Char -> Char) -> T.Text -> T.Text
capsHelper f (headTail -> (T.singleton . f -> h, t)) = h <> t


-----


dropBlanks :: [T.Text] -> [T.Text]
dropBlanks []      = []
dropBlanks ("":xs) =     dropBlanks xs
dropBlanks ( x:xs) = x : dropBlanks xs


-----


-- TODO: Is it possible/practical to write a single function that can handle both types?
findFullNameForAbbrev :: T.Text -> [T.Text] -> Maybe T.Text
findFullNameForAbbrev needle hay = let res = sort . filter (needle `T.isPrefixOf`) $ hay in
    (guard . not . null $ res) >> (return . head $ res)


findFullNameForAbbrevSnd :: (Ord a) => T.Text -> [(a, T.Text)] -> Maybe (a, T.Text)
findFullNameForAbbrevSnd needle hay = let res = sort . filter ((needle `T.isPrefixOf`) . snd) $ hay in
    (guard . not . null $ res) >> (return . head $ res)


----


headTail :: T.Text -> (Char, T.Text)
headTail = (T.head *** T.tail) . dup


-----


mkOrdinal :: Int -> T.Text
mkOrdinal 11              = "11th"
mkOrdinal 12              = "12th"
mkOrdinal 13              = "13th"
mkOrdinal (showText -> n) = n <> case T.last n of '1' -> "st"
                                                  '2' -> "nd"
                                                  '3' -> "rd"
                                                  _   -> "th"


-----


nl :: T.Text -> T.Text
nl = (<> "\n")


nlnl :: T.Text -> T.Text
nlnl = nl . nl


nlPrefix :: T.Text -> T.Text
nlPrefix = ("\n" <>)


-----


notInfixOf :: T.Text -> T.Text -> Bool
notInfixOf needle haystack = not $  needle `T.isInfixOf` haystack


-----


showText :: (Show a) => a -> T.Text
showText = T.pack . show


-----


stripControl :: T.Text -> T.Text
stripControl = T.filter (\c -> c > '\31' && c < '\127')


-----


stripTelnet :: T.Text -> T.Text
stripTelnet t
  | T.singleton telnetIAC `T.isInfixOf` t, (left, right) <- T.breakOn (T.singleton telnetIAC) t = left <> helper right
  | otherwise = t
  where
    helper (T.uncons -> Just (_, T.uncons -> Just (x, T.uncons -> Just (_, rest))))
      | x == telnetSB = case T.breakOn (T.singleton telnetSE) rest of (_, "")              -> ""
                                                                      (_, T.tail -> rest') -> stripTelnet rest'
      | otherwise     = stripTelnet rest
    helper _ = ""


-----


theOnLower :: T.Text -> T.Text
theOnLower t | isCapital t = t
             | otherwise   = "the " <> t


theOnLowerCap :: T.Text -> T.Text
theOnLowerCap = capitalize . theOnLower
