{-# LANGUAGE FlexibleInstances, OverloadedStrings, ViewPatterns #-}

module Mud.Util.Text ( aOrAn
                     , aOrAnOnLower
                     , capitalize
                     , commaEvery3
                     , commas
                     , commaShow
                     , countOcc
                     , divider
                     , dropBlanks
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
                     , nlnl
                     , nlnlPrefix
                     , nlPrefix
                     , none
                     , noneOnNull
                     , notInfixOf
                     , prd
                     , readNum
                     , replace
                     , showText
                     , slashes
                     , spaces
                     , spcL
                     , spcR
                     , spcsToFiller
                     , strictTextToLazyBS
                     , stripControl
                     , the
                     , theLetterS
                     , theNl
                     , theOnLower
                     , theOnLowerCap
                     , uncapitalize
                     , yesNo ) where

import Mud.TopLvlDefs.Chars
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Mud.Util.Misc as U (blowUp)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (isUpper, toLower, toUpper)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (intercalate, sortBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Util.Text"


-- ==================================================


class HasText a where
  extractText :: a -> Text


instance HasText Text where
  extractText = id


instance HasText (a, Text) where
  extractText = snd


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
commaShow = commaEvery3 . showText


-----


commas :: [Text] -> Text
commas = T.intercalate ", "


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


fillerToSpcs :: Text -> Text
fillerToSpcs = T.replace (T.singleton indentFiller) " "


spcsToFiller :: Text -> Text
spcsToFiller = T.replace " " (T.singleton indentFiller)


-----


findFullNameForAbbrev :: (Eq a, HasText a) => Text -> [a] -> Maybe a
findFullNameForAbbrev needle hay =
    let res = sortBy (compare `on` extractText) . filter ((needle `T.isPrefixOf`) . extractText) $ hay
    in guard $ ()!# res >> return (head res)


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
mkOrdinal 11              = "11th"
mkOrdinal 12              = "12th"
mkOrdinal 13              = "13th"
mkOrdinal (showText -> n) = n <> case T.last n of '1' -> "st"
                                                  '2' -> "nd"
                                                  '3' -> "rd"
                                                  _   -> "th"


-----


nl :: Text -> Text
nl = (<> theNl)


theNl :: Text
theNl = T.singleton '\n'


nlnl :: Text -> Text
nlnl = nl . nl


nlPrefix :: Text -> Text
nlPrefix = (theNl <>)


nlnlPrefix :: Text -> Text
nlnlPrefix = nlPrefix . nlPrefix


-----


noneOnNull :: (Nullable a) => a -> a
noneOnNull a = isNull a ? none :? a


-----


notInfixOf :: Text -> Text -> Bool
notInfixOf needle = not . T.isInfixOf needle


-----


prd :: Text -> Text
prd = (<> ".")


-----


readNum :: Text -> Int
readNum txt = case reads . T.unpack $ txt :: [(Int, String)] of
  [(x, "")] -> x
  _         -> blowUp "readNum" "parse failed" txt


-----


replace :: [(Text, Text)] -> Text -> Text
replace = foldr ((.) . uncurry T.replace) id


-----


showText :: (Show a) => a -> Text
showText = T.pack . show


-----


slashes :: [Text] -> Text
slashes = T.intercalate " / "


-----


spaces :: [Text] -> Text
spaces = T.intercalate " "


-----


spcL :: Text -> Text
spcL = (" " <>)


spcR :: Text -> Text
spcR = (<> " ")


-----


strictTextToLazyBS :: Text -> B.ByteString
strictTextToLazyBS = LT.encodeUtf8 . LT.fromStrict


-----


stripControl :: Text -> Text
stripControl = T.filter (inRange ('\32', '\126'))


-----


the :: Text -> Text
the = ("the " <>)


-----


theLetterS :: Bool -> Text
theLetterS = (|?| "s")


-----


theOnLower :: Text -> Text
theOnLower t | isCapital t = t
             | otherwise   = "the " <> t


theOnLowerCap :: Text -> Text
theOnLowerCap = capitalize . theOnLower


-----


yesNo :: Bool -> Text
yesNo True  = "yes"
yesNo False = "no"
