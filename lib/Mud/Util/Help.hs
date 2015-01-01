{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Help ( charCodeToTxt
                     , parseCharCodes
                     , parseStyleCodes
                     , styleCodeToANSI ) where

import Mud.ANSI
import Mud.TopLvlDefs.Chars
import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Util.Help"


-- ==================================================


parseCharCodes :: T.Text -> T.Text
parseCharCodes t
  | T.singleton charCodeDelimiter `notInfixOf` t = t
  | otherwise = let (left, headTail' . T.tail -> (c, right)) = T.break (== charCodeDelimiter) t
                in left <> charCodeToTxt c <> parseCharCodes right


charCodeToTxt :: Char -> T.Text
charCodeToTxt (toLower -> code) = T.singleton $ case code of
  'a' -> allChar
  'i' -> indexChar
  'm' -> amountChar
  'r' -> rmChar
  's' -> slotChar
  x   -> patternMatchFail "charCodeToTxt" [ T.singleton x ]


-----


parseStyleCodes :: T.Text -> T.Text
parseStyleCodes t
  | T.singleton styleCodeDelimiter `notInfixOf` t = t
  | otherwise = let (left, T.tail -> rest)  = T.break (== styleCodeDelimiter) t
                    (code, T.tail -> right) = T.break (== styleCodeDelimiter) rest
                in left <> styleCodeToANSI code <> parseStyleCodes right


styleCodeToANSI :: T.Text -> T.Text
styleCodeToANSI (T.toLower -> code) = case code of
  "d" -> dfltColorANSI
  "n" -> noUnderlineANSI
  "q" -> quoteColorANSI
  "t" -> topicColorANSI
  "u" -> underlineANSI
  "z" -> zingColorANSI
  x   -> patternMatchFail "styleCodeToANSI" [x]
