{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, OverloadedStrings, ViewPatterns #-}

module Mud.Util.Misc ( (?)
                     , (|*|)
                     , (|?|)
                     , Cond
                     , aOrAn
                     , aOrAnOnLower
                     , appendIfUnique
                     , blowUp
                     , capitalize
                     , countOcc
                     , dropBlanks
                     , dup
                     , eitherRet
                     , findFullNameForAbbrev
                     , headLast
                     , headTail
                     , headTail'
                     , ifThenElse
                     , isCapital
                     , isVowel
                     , maybeRet
                     , maybeVoid
                     , mkCountList
                     , mkDateTimeTxt
                     , mkOrdinal
                     , mkTimestamp
                     , mIf
                     , nl
                     , nl'
                     , nlnl
                     , notInfixOf
                     , nubSort
                     , patternMatchFail
                     , reverseLookup
                     , showText
                     , stripControl
                     , stripTelnet
                     , theOnLower
                     , theOnLower'
                     , toMaybe
                     , uncapitalize
                     , uncurry3
                     , uncurry4 ) where

import Mud.TopLvlDefs.Chars
import Mud.Util.Quoting

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad (guard)
import Data.Char (isUpper, toLower, toUpper)
import Data.List (foldl', sort)
import Data.Monoid ((<>), Monoid, mempty)
import Data.Time (getZonedTime)
import qualified Data.Map.Lazy as M (Map, assocs)
import qualified Data.Set as S (fromList, toList)
import qualified Data.Text as T


infixl 1 :?
data Cond a = a :? a


infixl 0 ?
(?) :: Bool -> Cond a -> a -- TODO: Use this.
True  ? (x :? _) = x
False ? (_ :? y) = y


(|*|) :: (Eq a, Monoid a, Eq b, Monoid b) => (a, b) -> (c, c) -> c
(a, b) |*| (c, d) = a /= mempty || b /= mempty ? c :? d


infixr 7 |?|
(|?|) :: (Eq a, Monoid a, Monoid b) => a -> b -> b
a |?| b = if a /= mempty then b else mempty


aOrAn :: T.Text -> T.Text
aOrAn (T.strip -> t) | T.null t             = ""
                     | isVowel . T.head $ t = "an " <> t
                     | otherwise            = "a "  <> t


aOrAnOnLower :: T.Text -> T.Text
aOrAnOnLower t | isCapital t = t
               | otherwise   = aOrAn t


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ [x]


blowUp :: T.Text -> T.Text -> T.Text -> [T.Text] -> a
blowUp modName funName msg (bracketQuote . T.intercalate ", " . map singleQuote -> vals) =
    error . T.unpack . T.concat $ [ modName, " ", funName, ": ", msg, ". ", vals ]


capitalize :: T.Text -> T.Text
capitalize = capsHelper toUpper


uncapitalize :: T.Text -> T.Text
uncapitalize = capsHelper toLower


capsHelper :: (Char -> Char) -> T.Text -> T.Text
capsHelper f (headTail' -> (T.singleton . f -> h, t)) = h <> t


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> if x == needle then succ acc else acc) 0


dropBlanks :: [T.Text] -> [T.Text]
dropBlanks []      = []
dropBlanks ("":xs) =     dropBlanks xs
dropBlanks ( x:xs) = x : dropBlanks xs


dup :: a -> (a, a)
dup x = (x, x)


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return


findFullNameForAbbrev :: T.Text -> [T.Text] -> Maybe T.Text
findFullNameForAbbrev needle hay = do
    guard . not . null $ res
    return . head $ res
  where
    res = sort . filter (needle `T.isPrefixOf`) $ hay


headLast :: [a] -> (a, a)
headLast = (,) <$> head <*> last


headTail :: [a] -> (a, [a])
headTail = (,) <$> head <*> tail


headTail' :: T.Text -> (Char, T.Text)
headTail' = (T.head *** T.tail) . dup


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y


isCapital :: T.Text -> Bool
isCapital ""            = False
isCapital (T.head -> h) = isUpper h


isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")


maybeRet :: Monad m => m a -> Maybe a -> m a
maybeRet = flip maybe return


maybeVoid :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe (return ())


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


mkDateTimeTxt :: IO (T.Text, T.Text)
mkDateTimeTxt = helper <$> (T.words . showText) `fmap` getZonedTime
  where
    helper = (,) <$> head <*> (T.init . T.dropWhileEnd (/= '.') . head . tail)


mkOrdinal :: Int -> T.Text
mkOrdinal 11              = "11th"
mkOrdinal 12              = "12th"
mkOrdinal 13              = "13th"
mkOrdinal (showText -> n) = n <> case T.last n of '1' -> "st"
                                                  '2' -> "nd"
                                                  '3' -> "rd"
                                                  _   -> "th"


mkTimestamp :: IO T.Text
mkTimestamp = [ bracketQuote $ date <> " " <> time | (date, time) <- mkDateTimeTxt ]


mIf :: (Monad m) => m Bool -> m a -> m a -> m a
mIf p x y = p >>= \case True  -> x
                        False -> y


nl :: T.Text -> T.Text
nl = (<> "\n")


nlnl :: T.Text -> T.Text
nlnl = nl . nl


nl' :: T.Text -> T.Text
nl' = ("\n" <>)


notInfixOf :: T.Text -> T.Text -> Bool
notInfixOf needle haystack = not $  needle `T.isInfixOf` haystack


nubSort :: (Ord a) => [a] -> [a]
nubSort = S.toList . S.fromList


patternMatchFail :: T.Text -> T.Text -> [T.Text] -> a
patternMatchFail modName funName = blowUp modName funName "pattern match failure"


reverseLookup :: (Eq v) => v -> M.Map k v -> k
reverseLookup v = fst . head . filter ((== v) . snd) . M.assocs


showText :: (Show a) => a -> T.Text
showText = T.pack . show


stripControl :: T.Text -> T.Text
stripControl = T.filter (\c -> c > '\31' && c < '\127')


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


theOnLower :: T.Text -> T.Text
theOnLower t | isCapital t = t
             | otherwise   = "the " <> t


theOnLower' :: T.Text -> T.Text
theOnLower' = capitalize . theOnLower


toMaybe :: Bool -> a -> Maybe a
toMaybe b = (guard b >>) . return


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c


uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d
