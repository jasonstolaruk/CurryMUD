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
                     , isTelnetTTypeResponse
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
                     , parseTelnet
                     , parseTelnetTTypeResponse
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
                     , stripTelnet
                     , the
                     , theLetterS
                     , theNl
                     , theOnLower
                     , theOnLowerCap
                     , uncapitalize
                     , yesNo ) where

import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Telnet
import Mud.Util.Misc hiding (blowUp)
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Mud.Util.Misc as U (blowUp)

import Control.Arrow ((&&&), second)
import Control.Monad (guard)
import Data.Char (isUpper, ord, toLower, toUpper)
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
    in do { guard $ ()!# res; return . head $ res }


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


isTelnetTTypeResponse :: Text -> Bool
isTelnetTTypeResponse = (telnetTTypeResponseL `T.isInfixOf`)


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


parseTelnet :: Text -> Maybe [Text]
parseTelnet x = case helper x of [] -> Nothing
                                 xs -> Just xs
  where
    helper :: Text -> [Text]
    helper "" = []
    helper t  | telnetIAC_SB `T.isInfixOf` t =
                  let (left, T.drop 2 -> right) = T.breakOn telnetIAC_SB t
                  in (helper left ++) . ([ "IAC", "SB" ] ++) $ case T.breakOn telnetIAC_SE right of
                    (_,   ""              ) -> mkAsciiNoTxts right -- There is no IAC SE.
                    (sub, T.drop 2 -> rest) -> mkAsciiNoTxts sub ++ [ "IAC", "SE" ] ++ helper rest
              | otherwise = case T.breakOn (T.singleton telnetIAC) t of
                (_, "") -> [] -- There is no IAC.
                (_, t') -> let (codes, rest) = T.splitAt 3 t'
                           in "IAC" : mkAsciiNoTxts (T.drop 1 codes) ++ helper rest
    mkAsciiNoTxts txt = [ showText . ord $ c | c <- T.unpack txt ]


-----


-- Assumes "telnetTTypeResponseL" is infix of msg.
parseTelnetTTypeResponse :: Text -> (Text, Text)
parseTelnetTTypeResponse msg | (l, T.drop (T.length telnetTTypeResponseL) -> r) <- T.breakOn telnetTTypeResponseL msg
                             = second (l |&|) $ case T.breakOn telnetTTypeResponseR r of
                                 (ttype, "") -> (ttype, id)
                                 (ttype, r') -> (ttype, (<> T.drop (T.length telnetTTypeResponseR) r'))

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


stripTelnet :: Text -> Text
stripTelnet t
  | T.singleton telnetIAC `T.isInfixOf` t, (left, right) <- T.breakOn (T.singleton telnetIAC) t = left <> helper right
  | otherwise = t
  where
    -- IAC should be followed by 2 bytes.
    -- The exception is IAC SB, in which case a subnegotiation sequence continues until IAC SE.
    helper (T.uncons -> Just (_, T.uncons -> Just (x, T.uncons -> Just (_, rest))))
      | x == telnetSB = case T.breakOn (T.singleton telnetSE) rest of (_, "")              -> ""
                                                                      (_, T.tail -> rest') -> stripTelnet rest'
      | otherwise     = stripTelnet rest
    helper _ = ""


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
