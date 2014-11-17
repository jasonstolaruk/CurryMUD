{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE KindSignatures, LambdaCase, OverloadedStrings, RankNTypes, ViewPatterns #-}

-- This module is considered to have sufficient test coverage as of 2014-10-13.

module Mud.Util ( adjustIndent
                , aOrAn
                , appendIfUnique
                , blowUp
                , bracketPad
                , bracketQuote
                , calcIndent
                , capitalize
                , countOcc
                , dblQuote
                , dblQuoteStr
                , deleteFirstOfEach
                , dropBlanks
                , eitherRet
                , findFullNameForAbbrev
                , grepTextList
                , isVowel
                , maybeRet
                , maybeVoid
                , mkCountList
                , mkOrdinal
                , nl
                , nl'
                , nlnl
                , padOrTrunc
                , parensPad
                , parensQuote
                , patternMatchFail
                , quoteWith
                , quoteWith'
                , quoteWithAndPad
                , reverseLookup
                , showText
                , singleQuote
                , stripTelnet
                , uncapitalize
                , unquote
                , wordWrap
                , wordWrapIndent
                , wordWrapLines
                , wrapLineWithIndentTag
                , xformLeading ) where

import Mud.TopLvlDefs

import Control.Lens (both, folded, over, to)
import Control.Lens.Operators ((^..))
import Control.Monad (guard)
import Data.Char (isDigit, isSpace, toLower, toUpper)
import Data.List (delete, foldl', sort)
import Data.Monoid ((<>))
import qualified Data.Map.Lazy as M (assocs, Map)
import qualified Data.Text as T


-- ==================================================
-- Error handling:


blowUp :: T.Text -> T.Text -> T.Text -> [T.Text] -> a
blowUp modName funName msg vals = error . T.unpack $ errorMsg
  where
    errorMsg = T.concat [ modName, " ", funName, ": ", msg, ". ", valsText ]
    valsText = bracketQuote . T.intercalate ", " . map singleQuote $ vals


patternMatchFail :: T.Text -> T.Text -> [T.Text] -> a
patternMatchFail modName funName = blowUp modName funName "pattern match failure"


-- ==================================================
-- Word wrapping and indenting:


wordWrap :: Int -> T.Text -> [T.Text]
wordWrap cols t
  | T.null afterMax         = [t]
  | T.any isSpace beforeMax = beforeSpace : wordWrap cols (afterSpace <> afterMax)
  | otherwise               = beforeMax   : wordWrap cols afterMax
  where
    (beforeMax, afterMax)     = T.splitAt cols t
    (beforeSpace, afterSpace) = breakEnd beforeMax


breakEnd :: T.Text -> (T.Text, T.Text)
breakEnd (T.break isSpace . T.reverse -> (after, before)) = over both T.reverse (before, after)


wordWrapIndent :: Int -> Int -> T.Text -> [T.Text]
wordWrapIndent n cols = map leadingNullsToSpcs . wrapIt . leadingSpcsToNulls
  where
    wrapIt t
      | T.null afterMax         = [t]
      | T.any isSpace beforeMax = beforeSpace : wordWrapIndent n cols (leadingIndent <> afterSpace <> afterMax)
      | otherwise               = beforeMax   : wordWrapIndent n cols (leadingIndent <> afterMax)
      where
        leadingIndent             = T.replicate (adjustIndent n cols) "\NUL"
        (beforeMax,   afterMax)   = T.splitAt cols t
        (beforeSpace, afterSpace) = breakEnd beforeMax


leadingSpcsToNulls :: T.Text -> T.Text
leadingSpcsToNulls = xformLeading ' ' '\NUL'


leadingNullsToSpcs :: T.Text -> T.Text
leadingNullsToSpcs = xformLeading '\NUL' ' '


xformLeading :: Char -> Char -> T.Text -> T.Text
xformLeading _ _ "" = ""
xformLeading a b (T.break (/= a) -> (T.length -> n, rest)) = T.replicate n (T.pack [b]) <> rest


adjustIndent :: Int -> Int -> Int
adjustIndent n cols = if n >= cols then cols - 1 else n


wordWrapLines :: Int -> [T.Text] -> [[T.Text]]
wordWrapLines _    []  = []
wordWrapLines cols [t] = [ wordWrapIndent (numOfLeadingSpcs t) cols t ]
wordWrapLines cols (a:b:rest) | T.null a  = [""]     : wrapNext
                              | otherwise = helper a : wrapNext
  where
    wrapNext         = wordWrapLines cols $ b : rest
    helper
      | hasIndentTag = wrapLineWithIndentTag cols
      | nolsa > 0    = wordWrapIndent nolsa  cols
      | nolsb > 0    = wordWrapIndent nolsb  cols
      | otherwise    = wordWrap cols
    hasIndentTag     = T.last a == indentTagChar
    (nolsa, nolsb)   = over both numOfLeadingSpcs (a, b)


numOfLeadingSpcs :: T.Text -> Int
numOfLeadingSpcs = T.length . T.takeWhile isSpace


wrapLineWithIndentTag :: Int -> T.Text -> [T.Text]
wrapLineWithIndentTag cols (T.break (not . isDigit) . T.reverse . T.init -> broken) = wordWrapIndent i cols t
  where
    (numTxt, t) = over both T.reverse broken
    readsRes    = reads . T.unpack $ numTxt :: [(Int, String)]
    extractInt []               = 0
    extractInt [(x, _)] | x > 0 = x
    extractInt xs               = patternMatchFail "Mud.Util" "wrapLineWithIndentTag extractInt" [ showText xs ]
    indent          = extractInt readsRes
    i | indent == 0 = calcIndent t
      | otherwise   = adjustIndent indent cols


calcIndent :: T.Text -> Int
calcIndent (T.break isSpace -> (T.length -> lenOfFirstWord, rest))
  | T.null rest = 0
  | otherwise   = lenOfFirstWord + numOfLeadingSpcs rest


-- ==================================================
-- Quoting:


quoteWith :: T.Text -> T.Text -> T.Text
quoteWith q t = T.concat [ q, t, q ]


quoteWith' :: (T.Text, T.Text) -> T.Text -> T.Text
quoteWith' (a, b) t = T.concat [ a, t, b ]


singleQuote :: T.Text -> T.Text
singleQuote = quoteWith "'"


dblQuote :: T.Text -> T.Text
dblQuote = quoteWith "\""


dblQuoteStr :: String -> String
dblQuoteStr = T.unpack . dblQuote . T.pack


bracketQuote :: T.Text -> T.Text
bracketQuote = quoteWith' ("[", "]")


parensQuote :: T.Text -> T.Text
parensQuote = quoteWith' ("(", ")")


unquote :: T.Text -> T.Text
unquote = T.init . T.tail


-- ==================================================
-- Padding:


quoteWithAndPad :: (T.Text, T.Text) -> Int -> T.Text -> T.Text
quoteWithAndPad q x t = quoteWith' q t' <> T.replicate p " "
  where
    t' = T.take (x - l - 1) t
    l  = sum $ [ fst q, snd q ]^..folded.to T.length
    p  = x - T.length t' - 2


bracketPad :: Int -> T.Text -> T.Text
bracketPad = quoteWithAndPad ("[", "]")


parensPad :: Int -> T.Text -> T.Text
parensPad = quoteWithAndPad ("(", ")")


padOrTrunc :: Int -> T.Text -> T.Text
padOrTrunc x _                 | x < 0      = ""
padOrTrunc x t@(T.length -> l) | l < x = t <> T.replicate (x - l) " "
                               | l > x = T.take x t
padOrTrunc _ t = t


-- ==================================================
-- Misc.:


nl :: T.Text -> T.Text
nl = (<> "\n")


nlnl :: T.Text -> T.Text
nlnl = nl . nl


nl' :: T.Text -> T.Text
nl' = ("\n" <>)


stripTelnet :: String -> String
stripTelnet msg@(x:y:_:rest)
  | x == telnetIAC, y == telnetSB = tail . dropWhile (/= telnetSE) $ rest
  | x == telnetIAC                = stripTelnet rest
  | otherwise                     = msg
stripTelnet msg = msg


showText :: (Show a) => a -> T.Text
showText = T.pack . show


capitalize :: T.Text -> T.Text
capitalize txt = T.pack [ toUpper . T.head $ txt ] <> T.tail txt


uncapitalize :: T.Text -> T.Text
uncapitalize txt = T.pack [ toLower . T.head $ txt ] <> T.tail txt


aOrAn :: T.Text -> T.Text
aOrAn t | T.null t'             = ""
        | isVowel . T.head $ t' = "an " <> t'
        | otherwise             = "a "  <> t'
  where
    t' = T.strip t


isVowel :: Char -> Bool
isVowel = (`elem` T.unpack "aeiou")


findFullNameForAbbrev :: T.Text -> [T.Text] -> Maybe T.Text
findFullNameForAbbrev needle hay = guard (not . null $ res) >> (Just . head $ res)
  where
    res = sort . filter (needle `T.isPrefixOf`) $ hay


mkCountList :: (Eq a) => [a] -> [Int]
mkCountList xs = map (`countOcc` xs) xs


countOcc :: (Eq a) => a -> [a] -> Int
countOcc needle = foldl' (\acc x -> if x == needle then succ acc else acc) 0


deleteFirstOfEach :: (Eq a) => [a] -> [a] -> [a]
deleteFirstOfEach delThese fromThis = foldl' (flip delete) fromThis delThese


dropBlanks :: [T.Text] -> [T.Text]
dropBlanks []      = []
dropBlanks ("":xs) =     dropBlanks xs
dropBlanks ( x:xs) = x : dropBlanks xs


appendIfUnique :: (Eq a) => [a] -> a -> [a]
xs `appendIfUnique` x | x `elem` xs = xs
                      | otherwise   = xs ++ [x]


mkOrdinal :: Int -> T.Text
mkOrdinal 11 = "11th"
mkOrdinal 12 = "12th"
mkOrdinal 13 = "13th"
mkOrdinal (T.last . showText -> c) | c == '1' = "st"
                                   | c == '2' = "nd"
                                   | c == '3' = "rd"
mkOrdinal _ = "th"


grepTextList :: T.Text -> [T.Text] -> [T.Text]
grepTextList needle = filter (needle `T.isInfixOf`)


reverseLookup :: (Eq v) => v -> M.Map k v -> k
reverseLookup v = fst . head . filter ((== v) . snd) . M.assocs


maybeVoid :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe (return ())


maybeRet :: forall a (m :: * -> *). Monad m => m a -> Maybe a -> m a
maybeRet dflt = maybe dflt return


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return
