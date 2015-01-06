{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Token ( parseCharTokens
                      , parseMsgTokens
                      , parseStyleTokens ) where

import Mud.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Msgs
import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Util.Token"


-- ==================================================


parseCharTokens :: T.Text -> T.Text
parseCharTokens t
  | T.singleton charTokenDelimiter `notInfixOf` t = t
  | (left, headTail' . T.tail -> (c, right)) <- T.break (== charTokenDelimiter) t
  = left <> charCodeToTxt c <> parseCharTokens right


charCodeToTxt :: Char -> T.Text
charCodeToTxt (toLower -> code) = T.singleton $ case code of
  'a' -> allChar
  'i' -> indexChar
  'm' -> amountChar
  'r' -> rmChar
  's' -> slotChar
  'w' -> wizCmdChar
  x   -> patternMatchFail "charCodeToTxt" [ T.singleton x ]


-----


parseMsgTokens :: T.Text -> T.Text
parseMsgTokens t
  | T.singleton msgTokenDelimiter `notInfixOf` t = t
  | (left, headTail' . T.tail -> (c, right)) <- T.break (== msgTokenDelimiter) t
  = left <> msgCodeToTxt c <> parseMsgTokens right


msgCodeToTxt :: Char -> T.Text
msgCodeToTxt (toLower -> code) = case code of
  'b' -> dfltBootMsg
  's' -> dfltShutdownMsg
  x   -> patternMatchFail "msgCodeToTxt" [ T.singleton x ]


-----


parseStyleTokens :: T.Text -> T.Text
parseStyleTokens t
  | T.singleton styleTokenDelimiter `notInfixOf` t = t
  | (left, T.tail -> rest)  <- T.break (== styleTokenDelimiter) t
  , (code, T.tail -> right) <- T.break (== styleTokenDelimiter) rest
  = left <> styleCodeToANSI code <> parseStyleTokens right


styleCodeToANSI :: T.Text -> T.Text
styleCodeToANSI (T.toLower -> code) = case code of
  "d" -> dfltColor
  "h" -> headerColor
  "n" -> noUnderline
  "q" -> quoteColor
  "u" -> underline
  "z" -> zingColor
  x   -> patternMatchFail "styleCodeToANSI" [x]
