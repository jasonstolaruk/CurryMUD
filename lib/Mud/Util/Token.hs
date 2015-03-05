{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Token (parseTokens) where

import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Msgs
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Util.Token"


-- ==================================================


parseTokens :: T.Text -> T.Text
parseTokens = parseCharTokens . parseMsgTokens . parseStyleTokens


-----


type Delimiter = Char

parser :: (Char -> T.Text) -> Delimiter -> T.Text -> T.Text
parser f d t
  | T.singleton d `notInfixOf` t = t
  | (left, headTail . T.tail -> (c, right)) <- T.breakOn (T.singleton d) t = left <> f c <> parser f d right


-----


parseCharTokens :: T.Text -> T.Text
parseCharTokens = parser expandCharCode charTokenDelimiter


expandCharCode :: Char -> T.Text
expandCharCode (toLower -> code) = T.singleton $ case code of
  'a' -> allChar
  'c' -> adverbCloseChar
  'd' -> adminCmdChar
  'e' -> emoteNameChar
  'i' -> indexChar
  'm' -> amountChar
  'o' -> adverbOpenChar
  'r' -> rmChar
  's' -> slotChar
  't' -> sayToChar
  x   -> patternMatchFail "expandCharCode" [ T.singleton x ]


-----


parseMsgTokens :: T.Text -> T.Text
parseMsgTokens = parser expandMsgCode msgTokenDelimiter


expandMsgCode :: Char -> T.Text
expandMsgCode (toLower -> code) = case code of
  'b' -> dfltBootMsg
  's' -> dfltShutdownMsg
  x   -> patternMatchFail "expandMsgCode" [ T.singleton x ]


-----


parseStyleTokens :: T.Text -> T.Text
parseStyleTokens = parser expandStyleCode styleTokenDelimiter


expandStyleCode :: Char -> T.Text
expandStyleCode (toLower -> code) = case code of
  'a' -> abbrevColor
  'd' -> dfltColor
  'h' -> headerColor
  'n' -> noUnderlineANSI
  'q' -> quoteColor
  'r' -> arrowColor
  's' -> syntaxSymbolColor
  'u' -> underlineANSI
  'z' -> zingColor
  x   -> patternMatchFail "expandStyleCode" [ T.singleton x ]
