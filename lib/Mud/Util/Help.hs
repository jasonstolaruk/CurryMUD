{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Help ( charCodeToTxt
                     , colorCodeToANSI
                     , parseCharCodes
                     , parseColorCodes ) where

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


parseColorCodes :: T.Text -> T.Text
parseColorCodes t
  | T.singleton colorCodeDelimiter `notInfixOf` t = t
  | otherwise = let (left, T.tail -> rest)  = T.break (== colorCodeDelimiter) t
                    (code, T.tail -> right) = T.break (== colorCodeDelimiter) rest
                in left <> colorCodeToANSI code <> parseColorCodes right


colorCodeToANSI :: T.Text -> T.Text
colorCodeToANSI (T.toLower -> code) = case code of
  "d"  -> dfltColorANSI
  "hh" -> headingColorANSI
  "ht" -> topicColorANSI
  x    -> patternMatchFail "colorCodeToANSI" [x]
