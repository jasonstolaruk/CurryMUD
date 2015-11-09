{-# LANGUAGE FlexibleInstances, OverloadedStrings, ViewPatterns #-}

module Mud.Util.Text ( aOrAn
                     , aOrAnOnLower
                     , capitalize
                     , commaEvery3
                     , commas
                     , divider
                     , dropBlanks
                     , findFullNameForAbbrev
                     , frame
                     , headTail
                     , intercalateDivider
                     , isCapital
                     , mkDateTimeTxt
                     , mkOrdinal
                     , mkTimestamp
                     , nl
                     , nlPrefix
                     , nlnl
                     , nlnlPrefix
                     , notInfixOf
                     , readNum
                     , replace
                     , showText
                     , slashes
                     , spaces
                     , stripControl
                     , stripTelnet
                     , theOnLower
                     , theOnLowerCap
                     , uncapitalize ) where

import Mud.TopLvlDefs.Chars
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import qualified Mud.Util.Misc as U (blowUp)

import Control.Arrow ((***))
import Control.Monad (guard)
import Data.Char (isUpper, toLower, toUpper)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (intercalate, sortBy)
import Data.Monoid ((<>))
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Util.Text"


-- ==================================================


aOrAn :: T.Text -> T.Text
aOrAn (T.strip -> t) | ()# t                = ""
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


commaEvery3 :: T.Text -> T.Text
commaEvery3 = T.reverse . T.intercalate "," . T.chunksOf 3 . T.reverse


-----


commas :: [T.Text] -> T.Text
commas = T.intercalate ", "


-----


type Cols = Int


divider :: Cols -> T.Text
divider = (`T.replicate` "=")


-----


dropBlanks :: [T.Text] -> [T.Text]
dropBlanks []      = []
dropBlanks ("":xs) =     dropBlanks xs
dropBlanks ( x:xs) = x : dropBlanks xs


-----


class HasText a where
  extractText :: a -> T.Text


instance HasText T.Text where
  extractText = id


instance HasText (a, T.Text) where
  extractText = snd


findFullNameForAbbrev :: (Eq a, HasText a) => T.Text -> [a] -> Maybe a
findFullNameForAbbrev needle hay =
    let res = sortBy (compare `on` extractText) . filter ((needle `T.isPrefixOf`) . extractText) $ hay
    in guard (()!# res) >> (return . head $ res)


-----


frame :: Cols -> T.Text -> T.Text
frame cols | d <- nl . divider $ cols = nl . (<> d) . (d <>)


-----


headTail :: T.Text -> (Char, T.Text)
headTail = (T.head *** T.tail) . dup


-----


intercalateDivider :: Cols -> [[T.Text]] -> [T.Text]
intercalateDivider cols = intercalate [ "", divider cols, "" ]


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


nlnlPrefix :: T.Text -> T.Text
nlnlPrefix = nlPrefix . nlPrefix


-----


readNum :: T.Text -> Int
readNum txt = case reads . T.unpack $ txt :: [(Int, String)] of
  [(x, "")] -> x
  _         -> blowUp "readNum" "parse failed" . pure $ txt


-----


replace :: [(T.Text, T.Text)] -> T.Text -> T.Text
replace = foldr ((.) . uncurry T.replace) id


-----


notInfixOf :: T.Text -> T.Text -> Bool
notInfixOf needle = not . T.isInfixOf needle


-----


showText :: (Show a) => a -> T.Text
showText = T.pack . show


-----


slashes :: [T.Text] -> T.Text
slashes = T.intercalate " / "


-----


spaces :: [T.Text] -> T.Text
spaces = T.intercalate " "


-----


stripControl :: T.Text -> T.Text
stripControl = T.filter (inRange ('\32', '\126'))


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
