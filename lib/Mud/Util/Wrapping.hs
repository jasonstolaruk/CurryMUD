{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Wrapping ( adjustIndent
                         , calcIndent
                         , multiWrap
                         , multiWrapNl
                         , wrap
                         , wrapIndent
                         , wrapLineWithIndentTag
                         , wrapLines
                         , wrapUnlines
                         , wrapUnlinesNl
                         , xformLeading ) where

import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.ANSI
import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (both, over)
import Data.Char (isDigit, isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Util.Wrapping"


-- ==================================================


wrap :: Int -> T.Text -> [T.Text]
wrap cols t = let extracted = extractANSI t
                  wrapped   = wrapIt . T.concat . map fst $ extracted
              in insertANSI extracted wrapped
  where
    wrapIt t'
      | T.null afterMax                                 = [t']
      | T.any isSpace beforeMax
      , (beforeSpace, afterSpace) <- breakEnd beforeMax = beforeSpace : wrapIt (afterSpace <> afterMax)
      | otherwise                                       = beforeMax   : wrapIt afterMax
      where
        (beforeMax, afterMax) = T.splitAt cols t'


breakEnd :: T.Text -> (T.Text, T.Text)
breakEnd (T.break isSpace . T.reverse -> (after, before)) = over both T.reverse (before, after)


-----


wrapUnlines :: Int -> T.Text -> T.Text
wrapUnlines cols = T.unlines . wrap cols


wrapUnlinesNl :: Int -> T.Text -> T.Text
wrapUnlinesNl cols = nl . wrapUnlines cols


-----


multiWrap :: Int -> [T.Text] -> T.Text
multiWrap cols = T.unlines . concatMap (wrap cols)


multiWrapNl :: Int -> [T.Text] -> T.Text
multiWrapNl cols = nl . multiWrap cols


-----


wrapIndent :: Int -> Int -> T.Text -> [T.Text]
wrapIndent n cols t = let extracted = extractANSI t
                          wrapped   = helper . T.concat . map fst $ extracted
                      in map leadingFillerToSpcs . insertANSI extracted $ wrapped
  where
    helper = wrapIt . leadingSpcsToFiller
    wrapIt t'
      | T.null afterMax = [t']
      | T.any isSpace beforeMax, (beforeSpace, afterSpace) <- breakEnd beforeMax =
                      beforeSpace : helper (leadingIndent <> afterSpace <> afterMax)
      | otherwise   = beforeMax   : helper (leadingIndent               <> afterMax)
      where
        (beforeMax, afterMax) = T.splitAt cols t'
        leadingIndent         = T.replicate (adjustIndent n cols) . T.singleton $ indentFiller


leadingSpcsToFiller :: T.Text -> T.Text
leadingSpcsToFiller = xformLeading ' ' indentFiller


leadingFillerToSpcs :: T.Text -> T.Text
leadingFillerToSpcs = xformLeading indentFiller ' '


xformLeading :: Char -> Char -> T.Text -> T.Text
xformLeading _ _                  ""                                       = ""
xformLeading a (T.singleton -> b) (T.span (== a) -> (T.length -> n, rest)) = T.replicate n b <> rest


adjustIndent :: Int -> Int -> Int
adjustIndent n cols = if n >= cols then pred cols else n


-----


wrapLines :: Int -> [T.Text] -> [[T.Text]]
wrapLines _    []                     = []
wrapLines cols [t]                    = [ wrapIndent (numOfLeadingSpcs t) cols t ]
wrapLines cols (a:b:rest) | T.null a  = [""]     : wrapNext
                          | otherwise = helper a : wrapNext
  where
    wrapNext         = wrapLines cols $ b : rest
    helper
      | hasIndentTag = wrapLineWithIndentTag cols
      | nolsa > 0    = wrapIndent nolsa cols
      | nolsb > 0    = wrapIndent nolsb cols
      | otherwise    = wrap cols
    hasIndentTag     = T.last a == indentTagChar
    (nolsa, nolsb)   = over both numOfLeadingSpcs (a, b)


numOfLeadingSpcs :: T.Text -> Int
numOfLeadingSpcs = T.length . T.takeWhile isSpace


wrapLineWithIndentTag :: Int -> T.Text -> [T.Text]
wrapLineWithIndentTag cols (T.break (not . isDigit) . T.reverse . T.init -> broken) = wrapIndent n cols t
  where
    (numTxt, t) = over both T.reverse broken
    readsRes    = reads . T.unpack $ numTxt :: [(Int, String)]
    extractInt []               = 0
    extractInt [(x, _)] | x > 0 = x
    extractInt xs               = patternMatchFail "wrapLineWithIndentTag extractInt" [ showText xs ]
    indent          = extractInt readsRes
    n | indent == 0 = calcIndent . dropANSI $ t
      | otherwise   = adjustIndent indent cols


dropANSI :: T.Text -> T.Text
dropANSI t | ansiCSI `notInfixOf` t = t
           | otherwise              =
               let (left, rest)      = T.break     (== ansiEsc)          t
                   (T.tail -> right) = T.dropWhile (/= ansiSGRDelimiter) rest
               in if T.null right then left else left <> dropANSI right


calcIndent :: T.Text -> Int
calcIndent (T.break isSpace -> (T.length -> lenOfFirstWord, rest))
  | T.null rest = 0
  | otherwise   = lenOfFirstWord + numOfLeadingSpcs rest
