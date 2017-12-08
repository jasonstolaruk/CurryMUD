{-# LANGUAGE FlexibleInstances, OverloadedStrings, ViewPatterns #-}

module Mud.Util.Text ( aOrAn
                     , aOrAnOnLower
                     , capitalize
                     , commaEvery3
                     , commaShow
                     , commas
                     , concatMapTxt
                     , countOcc
                     , divider
                     , dropBlanks
                     , expandDividers
                     , fillerToSpcs
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
                     , nlTxt
                     , nlnl
                     , nlnlPrefix
                     , nls
                     , none
                     , noneOnNull
                     , notInfixOf
                     , periods
                     , prd
                     , readNum
                     , replace
                     , sOnNon1
                     , sOnTrue
                     , showTxt
                     , slashes
                     , spaces
                     , spcL
                     , spcR
                     , spcsToFiller
                     , strictTxtToLazyBS
                     , stripControl
                     , the
                     , the'
                     , theOnLower
                     , theOnLowerCap
                     , uncapitalize
                     , yesNo ) where

import           Mud.TopLvlDefs.Chars
import qualified Mud.Util.Misc as U (blowUp)
import           Mud.Util.Misc hiding (blowUp)
import           Mud.Util.Operators
import           Mud.Util.Quoting

import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import           Data.Char (isUpper, toLower, toUpper)
import           Data.Function (on)
import           Data.Ix (inRange)
import           Data.List (intercalate, sortBy)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Util.Text"

-- ==================================================

class HasTxt a where
  extractTxt :: a -> Text

instance HasTxt Text where
  extractTxt = id

instance HasTxt (a, Text) where
  extractTxt = snd

-----

class Nullable a where
  isNull :: a -> Bool
  none   :: a

instance Nullable Text where
  isNull = T.null
  none   = "none"

instance Nullable [Text] where
  isNull = null
  none   = pure "none"

-----

aOrAn :: Text -> Text
aOrAn (T.strip -> t) | ()# t                = ""
                     | isVowel . T.head $ t = "an " <> t
                     | otherwise            = "a "  <> t

aOrAnOnLower :: Text -> Text
aOrAnOnLower t | isCapital t = t
               | otherwise   = aOrAn t

isCapital :: Text -> Bool
isCapital ""            = False
isCapital (T.head -> h) = isUpper h

-----

capitalize :: Text -> Text
capitalize = capsHelper toUpper

uncapitalize :: Text -> Text
uncapitalize = capsHelper toLower

capsHelper :: (Char -> Char) -> Text -> Text
capsHelper f (headTail -> (T.singleton . f -> h, t)) = h <> t

-----

commaEvery3 :: Text -> Text
commaEvery3 = T.reverse . T.intercalate "," . T.chunksOf 3 . T.reverse

commaShow :: (Show a) => a -> Text
commaShow = commaEvery3 . showTxt

-----

commas :: [Text] -> Text
commas = T.intercalate ", "

-----

concatMapTxt :: (a -> Text) -> [a] -> Text
concatMapTxt f = T.concat . map f

-----

countOcc :: Text -> Text -> Int
countOcc ""                     = const  0
countOcc needle@(T.length -> l) = helper 0
  where
    helper 0 ""                                             = 0
    helper x haystack | not (needle `T.isInfixOf` haystack) = x
                      | otherwise                           = let (_, T.drop l -> rest) = T.breakOn needle haystack
                                                              in helper (succ x) rest

-----

type Cols = Int

divider :: Cols -> Text
divider = (`T.replicate` "=")

-----

dropBlanks :: [Text] -> [Text]
dropBlanks []      = []
dropBlanks ("":xs) =     dropBlanks xs
dropBlanks ( x:xs) = x : dropBlanks xs

-----

expandDividers :: Cols -> Text -> Text
expandDividers cols t | t == T.singleton dividerToken = T.replicate cols "-"
                      | otherwise                     = t

-----

fillerToSpcs :: Text -> Text
fillerToSpcs = T.replace (T.singleton indentFiller) " "

spcsToFiller :: Text -> Text
spcsToFiller = T.replace " " (T.singleton indentFiller)

-----

findFullNameForAbbrev :: (Eq a, HasTxt a) => Text -> [a] -> Maybe a
findFullNameForAbbrev needle hay =
    let res = sortBy (compare `on` extractTxt) . filter ((needle `T.isPrefixOf`) . extractTxt) $ hay
    in guard (()!# res) >> return (head res)

-----

frame :: Cols -> Text -> Text
frame cols | d <- nl . divider $ cols = nl . quoteWith d

-----

headTail :: Text -> (Char, Text)
headTail = T.head &&& T.tail

-----

intercalateDivider :: Cols -> [[Text]] -> [Text]
intercalateDivider cols = intercalate [ "", divider cols, "" ]

-----

mkOrdinal :: Int -> Text
mkOrdinal 11             = "11th"
mkOrdinal 12             = "12th"
mkOrdinal 13             = "13th"
mkOrdinal (showTxt -> n) = n <> case T.last n of '1' -> "st"
                                                 '2' -> "nd"
                                                 '3' -> "rd"
                                                 _   -> "th"

-----

nl :: Text -> Text
nl = (<> nlTxt)

nlTxt :: Text
nlTxt = T.singleton '\n'

nlnl :: Text -> Text
nlnl = nl . nl

nlPrefix :: Text -> Text
nlPrefix = (nlTxt <>)

nlnlPrefix :: Text -> Text
nlnlPrefix = nlPrefix . nlPrefix

nls :: [Text] -> Text
nls = T.intercalate nlTxt

-----

noneOnNull :: (Nullable a) => a -> a
noneOnNull a = isNull a ? none :? a

-----

notInfixOf :: Text -> Text -> Bool
notInfixOf needle = not . T.isInfixOf needle

-----

periods :: [Text] -> Text
periods = T.intercalate ". "

-----

prd :: Text -> Text
prd = (<> ".")

-----

readNum :: Text -> Int
readNum txt = case reads . T.unpack $ txt of [(x, "")] -> x
                                             _         -> blowUp "readNum" "parse failed" txt

-----

replace :: [(Text, Text)] -> Text -> Text
replace = foldr ((.) . uncurry T.replace) id

-----

showTxt :: (Show a) => a -> Text
showTxt = T.pack . show

-----

slashes :: [Text] -> Text
slashes = T.intercalate " / "

-----

sOnNon1 :: (Eq a, Num a) => a -> Text
sOnNon1 = sOnTrue . (/= 1)

sOnTrue :: Bool -> Text
sOnTrue = (|?| "s")

-----

spaces :: [Text] -> Text
spaces = T.intercalate " "

-----

spcL :: Text -> Text
spcL = (" " <>)

spcR :: Text -> Text
spcR = (<> " ")

-----

strictTxtToLazyBS :: Text -> LB.ByteString
strictTxtToLazyBS = LTE.encodeUtf8 . LT.fromStrict

-----

stripControl :: Text -> Text
stripControl = T.filter (inRange ('\32', '\126'))

-----

the :: Text -> Text
the = ("the " <>)

the' :: Text -> Text
the' = capitalize . the

-----

theOnLower :: Text -> Text
theOnLower t | isCapital t = t
             | otherwise   = the t

theOnLowerCap :: Text -> Text
theOnLowerCap = capitalize . theOnLower

-----

yesNo :: Bool -> Text
yesNo True  = "yes"
yesNo False = "no"
