{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Wrapping ( adjustIndent
                         , calcIndent
                         , leadingFillerToSpcs
                         , multiWrap
                         , multiWrapNl
                         , wrap
                         , wrapIndent
                         , wrapLines
                         , wrapLineWithIndentTag
                         , wrapUnlines
                         , wrapUnlinesInit
                         , wrapUnlinesNl
                         , xformLeading ) where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (both)
import Control.Lens.Operators ((%~), (&))
import Data.Char (isDigit, isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Util.Wrapping"


-- ==================================================


wrap :: Cols -> Text -> [Text]
wrap cols t | extracted <- extractANSI t
            , wrapped   <- wrapIt . T.concat . map fst $ extracted = insertANSI extracted wrapped
  where
    wrapIt txt
      | ()# afterMax                                    = pure txt
      | T.any isSpace beforeMax
      , (beforeSpace, afterSpace) <- breakEnd beforeMax = beforeSpace : wrapIt (afterSpace <> afterMax)
      | otherwise                                       = beforeMax   : wrapIt afterMax
      where
        (beforeMax, afterMax) = T.splitAt cols txt


breakEnd :: Text -> (Text, Text)
breakEnd (T.break isSpace . T.reverse -> (after, before)) = (before, after) & both %~ T.reverse


-----


wrapUnlines :: Cols -> Text -> Text
wrapUnlines cols = T.unlines . wrap cols


wrapUnlinesNl :: Cols -> Text -> Text
wrapUnlinesNl cols = nl . wrapUnlines cols


wrapUnlinesInit :: Cols -> Text -> Text
wrapUnlinesInit cols = T.intercalate theNl . wrap cols


-----


multiWrap :: Cols -> [Text] -> Text
multiWrap cols = T.unlines . concatMap (wrap cols)


multiWrapNl :: Cols -> [Text] -> Text
multiWrapNl cols = nl . multiWrap cols


-----


wrapIndent :: Int -> Cols -> Text -> [Text]
wrapIndent n cols t = let extracted = extractANSI t
                          wrapped   = helper . T.concat . map fst $ extracted
                      in map leadingFillerToSpcs . insertANSI extracted $ wrapped
  where
    helper = wrapIt . leadingSpcsToFiller
    wrapIt txt
      | ()# afterMax = pure txt
      | T.any isSpace beforeMax, (beforeSpace, afterSpace) <- breakEnd beforeMax =
                      beforeSpace : helper (leadingIndent <> afterSpace <> afterMax)
      | otherwise   = beforeMax   : helper (leadingIndent               <> afterMax)
      where
        (beforeMax, afterMax) = T.splitAt cols txt
        leadingIndent         = T.replicate (adjustIndent n cols) . T.singleton $ indentFiller


leadingSpcsToFiller :: Text -> Text
leadingSpcsToFiller = xformLeading ' ' indentFiller


leadingFillerToSpcs :: Text -> Text
leadingFillerToSpcs = xformLeading indentFiller ' '


xformLeading :: Char -> Char -> Text -> Text
xformLeading _ _                  ""                                       = ""
xformLeading a (T.singleton -> b) (T.span (== a) -> (T.length -> n, rest)) = T.replicate n b <> rest


adjustIndent :: Int -> Cols -> Int
adjustIndent n cols = n >= cols ? pred cols :? n


-----


wrapLines :: Cols -> [Text] -> [[Text]]
wrapLines _    []                     = []
wrapLines cols [t]                    = pure . wrapIndent (noOfLeadingSpcs t) cols $ t
wrapLines cols (a:b:rest) | ()# a     = [""]     : wrapNext
                          | otherwise = helper a : wrapNext
  where
    wrapNext         = wrapLines cols $ b : rest
    helper
      | hasIndentTag = wrapLineWithIndentTag cols
      | nolsa > 0    = wrapIndent nolsa cols
      | nolsb > 0    = wrapIndent nolsb cols
      | otherwise    = wrap cols
    hasIndentTag     = T.last a == indentTagChar
    (nolsa, nolsb)   = (a, b) & both %~ noOfLeadingSpcs


noOfLeadingSpcs :: Text -> Int
noOfLeadingSpcs = T.length . T.takeWhile isSpace


wrapLineWithIndentTag :: Cols -> Text -> [Text]
wrapLineWithIndentTag cols (T.break (not . isDigit) . T.reverse . T.init -> broken) = wrapIndent n cols t
  where
    (numTxt, t)                 = broken & both %~ T.reverse
    readsRes                    = reads . T.unpack $ numTxt :: [(Int, String)]
    extractInt []               = 0
    extractInt [(x, _)] | x > 0 = x
    extractInt xs               = patternMatchFail "wrapLineWithIndentTag extractInt" . showText $ xs
    indent                      = extractInt readsRes
    n | indent == 0             = calcIndent . dropANSI $ t
      | otherwise               = adjustIndent indent cols


calcIndent :: Text -> Int
calcIndent (T.break isSpace -> (T.length -> lenOfFirstWord, rest))
  | ()# rest  = 0
  | otherwise = lenOfFirstWord + noOfLeadingSpcs rest
