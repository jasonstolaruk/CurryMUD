{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Wrapping ( adjustIndent
                         , calcIndent
                         , leadingFillerToSpcs
                         , multiWrap
                         , multiWrapNl
                         , wrap
                         , wrapIndent
                         , wrapLineWithIndentTag
                         , wrapLines
                         , wrapUnlines
                         , wrapUnlinesInit
                         , wrapUnlinesNl
                         , xformLeading
                         , xformLeadingSpaceChars ) where

import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Misc.ANSI
import           Mud.TopLvlDefs.Chars
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Text

import           Control.Lens (both)
import           Control.Lens.Operators ((&), (%~))
import           Data.Char (isDigit, isSpace)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import qualified Data.Text as T

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Util.Wrapping"

-- ==================================================

wrap :: HasCallStack => Cols -> Text -> [Text]
wrap cols t | extracted <- extractANSI t
            , wrapped   <- wrapIt . concatMapTxt fst $ extracted = insertANSI extracted wrapped
  where
    wrapIt txt
      | ()# afterMax                                    = pure txt
      | T.any isSpace beforeMax
      , (beforeSpace, afterSpace) <- breakEnd beforeMax = beforeSpace : wrapIt (afterSpace <> afterMax)
      | otherwise                                       = beforeMax   : wrapIt afterMax
      where
        (beforeMax, afterMax) = T.splitAt cols txt

breakEnd :: HasCallStack => Text -> (Text, Text)
breakEnd (T.break isSpace . T.reverse -> (after, before)) = (before, after) & both %~ T.reverse

-----

wrapUnlines :: HasCallStack => Cols -> Text -> Text
wrapUnlines cols = T.unlines . wrap cols

wrapUnlinesNl :: HasCallStack => Cols -> Text -> Text
wrapUnlinesNl cols = nl . wrapUnlines cols

wrapUnlinesInit :: HasCallStack => Cols -> Text -> Text
wrapUnlinesInit cols = nls . wrap cols

-----

multiWrap :: HasCallStack => Cols -> [Text] -> Text
multiWrap cols = T.unlines . concatMap (wrap cols)

multiWrapNl :: HasCallStack => Cols -> [Text] -> Text
multiWrapNl cols = nl . multiWrap cols

-----

wrapIndent :: HasCallStack => Int -> Cols -> Text -> [Text]
wrapIndent n cols t = let extracted = extractANSI t
                          wrapped   = helper . concatMapTxt fst $ extracted
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

leadingSpcsToFiller :: HasCallStack => Text -> Text
leadingSpcsToFiller = xformLeading ' ' indentFiller

leadingFillerToSpcs :: HasCallStack => Text -> Text
leadingFillerToSpcs = xformLeading indentFiller ' '

xformLeading :: HasCallStack => Char -> Char -> Text -> Text
xformLeading _ _                  ""                                       = ""
xformLeading a (T.singleton -> b) (T.span (== a) -> (T.length -> n, rest)) = T.replicate n b <> rest

adjustIndent :: HasCallStack => Int -> Cols -> Int
adjustIndent n cols = n >= cols ? pred cols :? n

-----

wrapLines :: HasCallStack => Cols -> [Text] -> [[Text]]
wrapLines _    []                          = []
wrapLines cols [t]        | hasIndentTag t = pure . wrapLineWithIndentTag cols $ t
                          | otherwise      = pure . wrapIndent (noOfLeadingSpcs t) cols $ t
wrapLines cols (a:b:rest) | ()# a          = mMempty  : wrapNext
                          | otherwise      = helper a : wrapNext
  where
    wrapNext           = wrapLines cols $ b : rest
    helper
      | hasIndentTag a = wrapLineWithIndentTag cols
      | nolsa > 0      = wrapIndent nolsa cols
      | nolsb > 0      = wrapIndent nolsb cols
      | otherwise      = wrap cols
    (nolsa, nolsb)     = (a, b) & both %~ noOfLeadingSpcs

hasIndentTag :: HasCallStack => Text -> Bool
hasIndentTag "" = False
hasIndentTag t  = T.last t == indentTagChar

noOfLeadingSpcs :: HasCallStack => Text -> Int
noOfLeadingSpcs = T.length . T.takeWhile isSpace

wrapLineWithIndentTag :: HasCallStack => Cols -> Text -> [Text]
wrapLineWithIndentTag cols (T.break (not . isDigit) . T.reverse . T.init -> broken) = wrapIndent n cols t
  where
    (numTxt, t)                 = broken & both %~ T.reverse
    readsRes                    = reads . T.unpack $ numTxt
    extractInt []               = 0
    extractInt [(x, _)] | x > 0 = x
    extractInt xs               = pmf "wrapLineWithIndentTag extractInt" xs
    indent                      = extractInt readsRes
    n | isZero indent           = calcIndent . dropANSI $ t
      | otherwise               = adjustIndent indent cols

calcIndent :: HasCallStack => Text -> Int
calcIndent (T.break isSpace -> (T.length -> lenOfFirstWord, rest)) | ()# rest  = 0
                                                                   | otherwise = lenOfFirstWord + noOfLeadingSpcs rest

-----

xformLeadingSpaceChars :: HasCallStack => Text -> Text
xformLeadingSpaceChars = xformLeading leadingSpaceChar ' '
