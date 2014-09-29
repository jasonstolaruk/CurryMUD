{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE KindSignatures, LambdaCase, OverloadedStrings, RankNTypes #-}

-- This module is considered to have sufficient test coverage as of 2014-09-13.

module Mud.Util ( adjustIndent
                , aOrAn
                , blowUp
                , bracketPad
                , bracketQuote
                , calcIndent
                , countOcc
                , dblQuote
                , dblQuoteStr
                , deleteFirstOfEach
                , dropBlanks
                , eitherRet
                , findFullNameForAbbrev
                , grepTextList
                , injectCR
                , isVowel
                , maybeRet
                , maybeVoid
                , mkCountList
                , mkOrdinal
                , nl
                , nlnl
                , padOrTrunc
                , parensPad
                , parensQuote
                , patternMatchFail
                , quoteWithAndPad
                , reverseLookup
                , showText
                , singleQuote
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
import Data.Char (isDigit, isSpace)
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
breakEnd t = over both T.reverse (before, after)
  where (after, before) = T.break isSpace . T.reverse $ t


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
xformLeading a b t  = let (as, t') = T.break (/= a) t
                          n        = T.length as
                      in T.replicate n (T.pack [b]) <> t'


adjustIndent :: Int -> Int -> Int
adjustIndent n cols = if n >= cols then cols - 1 else n


wordWrapLines :: Int -> [T.Text] -> [[T.Text]]
wordWrapLines _    []  = []
wordWrapLines cols [t] = let nolst = numOfLeadingSpcs t
                         in [ wordWrapIndent nolst cols t ]
wordWrapLines cols (a:b:rest) = if T.null a
  then [""]     : wrapNext
  else helper a : wrapNext
    where
      wrapNext = wordWrapLines cols $ b : rest
      helper
        | hasIndentTag = wrapLineWithIndentTag cols
        | nolsa > 0    = wordWrapIndent nolsa  cols
        | nolsb > 0    = wordWrapIndent nolsb  cols
        | otherwise    = wordWrap cols
      hasIndentTag = T.last a == indentTagChar
      nolsa        = numOfLeadingSpcs a
      nolsb        = numOfLeadingSpcs b


numOfLeadingSpcs :: T.Text -> Int
numOfLeadingSpcs = T.length . T.takeWhile isSpace


wrapLineWithIndentTag :: Int -> T.Text -> [T.Text]
wrapLineWithIndentTag cols t = wordWrapIndent n' cols t'
  where
    parseIndentTag = T.break (not . isDigit) . T.reverse . T.init $ t
    (numText, t')  = over both T.reverse parseIndentTag
    readsRes       = reads . T.unpack $ numText :: [(Int, String)]
    n              = \case []       -> 0
                           [(x, _)] -> x
                           xs       -> patternMatchFail "Mud.Util" "wrapLineWithIndentTag n" [ showText xs ]
    n'             = if n readsRes == 0 then calcIndent t' else adjustIndent (n readsRes) cols


calcIndent :: T.Text -> Int
calcIndent t
  | T.null b  = 0
  | otherwise = lenOfFirstWord + numOfFollowingSpcs
  where
    (a, b)             = T.break isSpace t
    lenOfFirstWord     = T.length a
    numOfFollowingSpcs = numOfLeadingSpcs b


-- ==================================================
-- Quoting:


quoteWith :: (T.Text, T.Text) -> T.Text -> T.Text
quoteWith (a, b) t = T.concat [ a, t, b ]


singleQuote :: T.Text -> T.Text
singleQuote = quoteWith ("'", "'")


dblQuote :: T.Text -> T.Text
dblQuote = quoteWith ("\"", "\"")


dblQuoteStr :: String -> String
dblQuoteStr = T.unpack . dblQuote . T.pack


bracketQuote :: T.Text -> T.Text
bracketQuote = quoteWith ("[", "]")


parensQuote :: T.Text -> T.Text
parensQuote = quoteWith ("(", ")")


unquote :: T.Text -> T.Text
unquote = T.init . T.tail


-- ==================================================
-- Padding:


quoteWithAndPad :: (T.Text, T.Text) -> Int -> T.Text -> T.Text
quoteWithAndPad q x t = quoteWith q t' <> T.replicate p " "
  where
    t' = T.take (x - l - 1) t
    l  = sum $ [ fst q, snd q ]^..folded.to T.length
    p  = x - T.length t' - 2


bracketPad :: Int -> T.Text -> T.Text
bracketPad = quoteWithAndPad ("[", "]")


parensPad :: Int -> T.Text -> T.Text
parensPad = quoteWithAndPad ("(", ")")


padOrTrunc :: Int -> T.Text -> T.Text
padOrTrunc x t
  | x < 0 = ""
  | otherwise = let l = T.length t
                in case l `compare` x of EQ -> t
                                         LT -> let diff = x - l in t <> T.replicate diff " "
                                         GT -> T.take x t


-- ==================================================
-- Misc.:


nl :: T.Text -> T.Text
nl = (<> "\n")


nlnl :: T.Text -> T.Text
nlnl = (<> "\n\n")


injectCR :: T.Text -> T.Text
injectCR = T.replace "\n" "\r\n"


showText :: (Show a) => a -> T.Text
showText = T.pack . show


aOrAn :: T.Text -> T.Text
aOrAn t | T.null t' = ""
        | isVowel . T.head $ t' = "an " <> t'
        | otherwise = "a " <> t'
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


mkOrdinal :: Int -> T.Text
mkOrdinal 11 = "11th"
mkOrdinal 12 = "12th"
mkOrdinal 13 = "13th"
mkOrdinal x  = let t = showText x
               in t <> case T.last t of '1' -> "st"
                                        '2' -> "nd"
                                        '3' -> "rd"
                                        _   -> "th"


grepTextList :: T.Text -> [T.Text] -> [T.Text]
grepTextList needle = filter (needle `T.isInfixOf`)


reverseLookup :: (Eq v) => v -> M.Map k v -> k
reverseLookup v = fst . head . filter (\(_, v') -> v' == v) . M.assocs


maybeVoid :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe (return ())


maybeRet :: forall a (m :: * -> *). Monad m => m a -> Maybe a -> m a
maybeRet dflt = maybe dflt return


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return
