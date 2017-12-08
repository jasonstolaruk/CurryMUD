{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Token (parseTokens) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Data.State.MudData
import           Mud.Misc.ANSI
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Misc
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Text

import           Data.Char (toLower)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Util.Token"

-- ==================================================

parseTokens :: ServerSettings -> Text -> Text
parseTokens s = parseCharTokens . parseStyleTokens . parseMiscTokens s

-----

type Delimiter = Char

parser :: (Char -> Text) -> Delimiter -> Text -> Text
parser f d t | T.singleton d `notInfixOf` t                                           = t
             | (left, headTail . T.tail -> (c, right)) <- T.breakOn (T.singleton d) t = left <> f c <> parser f d right

-----

parseCharTokens :: Text -> Text
parseCharTokens = parser expandCharCode charTokenDelimiter

expandCharCode :: Char -> Text -- '#'
expandCharCode c | c == charTokenDelimiter = T.singleton charTokenDelimiter
expandCharCode (toLower -> code)           = T.singleton $ case code of
  'a' -> allChar
  'b' -> debugCmdChar
  'c' -> adverbCloseChar
  'd' -> adminCmdChar
  'e' -> emoteNameChar
  'h' -> chanTargetChar
  'i' -> indexChar
  'l' -> selectorChar
  'm' -> amountChar
  'o' -> adverbOpenChar
  'p' -> expCmdChar
  'q' -> quoteChar -- Used in ":set".
  'r' -> emoteTargetChar
  's' -> slotChar
  't' -> sayToChar
  'u' -> multiLineEndChar
  'x' -> emoteChar
  x   -> pmf "expandCharCode" x

-----

parseMiscTokens :: ServerSettings -> Text -> Text
parseMiscTokens s = miscTokenParser (expandMiscCode s) miscTokenDelimiter
  where
    miscTokenParser :: (Char -> Text) -> Delimiter -> Text -> Text -- The expanded text itself may contain a misc token.
    miscTokenParser f d t
      | T.singleton d `notInfixOf` t = t
      | (left, headTail . T.tail -> (c, right)) <- T.breakOn (T.singleton d) t =
          let expanded  = f c
              expanded' = onFalse (expanded == T.singleton miscTokenDelimiter) (miscTokenParser f d) expanded
          in left <> expanded' <> miscTokenParser f d right

expandMiscCode :: ServerSettings -> Char -> Text -- '@'
expandMiscCode _ c | c == miscTokenDelimiter = T.singleton miscTokenDelimiter
expandMiscCode s (toLower -> code)           = case code of
  'b' -> dfltBootMsg
  'c' -> descRule5
  'd' -> yesNo . settingDebug $ s
  'e' -> descRulesMsg
  'l' -> T.singleton leadingSpaceChar
  'o' -> showTxt dfltZoom
  'p' -> pwWarningMsg
  'r' -> rulesIntroMsg
  's' -> dfltShutdownMsg
  'u' -> rulesMsg
  'v' -> violationMsg
  'z' -> yesNo . ((&&) <$> settingDebug <*> settingZBackDoor) $ s
  x   -> pmf "expandMiscCode" x

-----

parseStyleTokens :: Text -> Text
parseStyleTokens = parser expandStyleCode styleTokenDelimiter

expandStyleCode :: Char -> Text -- '\'
expandStyleCode c | c == styleTokenDelimiter = T.singleton styleTokenDelimiter
expandStyleCode (toLower -> code)            = case code of
  'a' -> abbrevColor
  'd' -> dfltColor
  'e' -> emphasisColor
  'h' -> headerColor
  'i' -> asteriskColor
  'k' -> knownNameColor
  'l' -> selectorColor
  'm' -> tempDescColor
  'n' -> noUnderlineANSI
  'o' -> unknownNameColor
  'p' -> prefixColor
  'q' -> quoteColor
  'r' -> arrowColor
  's' -> syntaxSymbolColor
  't' -> toNpcColor
  'u' -> underlineANSI
  'z' -> zingColor
  x   -> pmf "expandStyleCode" x
