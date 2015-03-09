{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Misc.ANSI ( abbrevColor
                     , adminBroadcastColor
                     , adminMsgColor
                     , adminTellColor
                     , announceColor
                     , arrowColor
                     , asteriskColor
                     , blinkANSI
                     , bootMsgColor
                     , colorizeFileTxt
                     , colors
                     , dfltColor
                     , dfltColor'
                     , dropANSI
                     , envVarColor
                     , exitsColor
                     , extractANSI
                     , fromPeepedColor
                     , headerColor
                     , hintANSI
                     , insertANSI
                     , intensities
                     , knownNameColor
                     , mkBgColorANSI
                     , mkColorANSI
                     , mkFgColorANSI
                     , motdColor
                     , newRecordColor
                     , noBlinkANSI
                     , noHintANSI
                     , noUnderlineANSI
                     , pagerPromptColor
                     , printConsoleColor
                     , promptColor
                     , quoteColor
                     , resetANSI
                     , selfColor
                     , shutdownMsgColor
                     , syntaxSymbolColor
                     , toPeepedColor
                     , underlineANSI
                     , unknownNameColor
                     , wtfColor
                     , zingColor ) where

import Mud.TopLvlDefs.Chars
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import System.Console.ANSI (BlinkSpeed(..), Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), Underlining(..), setSGRCode)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Misc.ANSI"


-- ==================================================
-- Misc. definitions and helpers:


ansiCSI :: T.Text
ansiCSI = T.pack [ ansiEsc, ansiBracket ]


resetANSI :: T.Text
resetANSI = T.pack . setSGRCode $ [Reset]


colors :: [Color]
colors = [ Black .. White ]


intensities :: [ColorIntensity]
intensities = [ Dull, Vivid ]


mkFgColorANSI :: (ColorIntensity, Color) -> T.Text
mkFgColorANSI fg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg ]


mkBgColorANSI :: (ColorIntensity, Color) -> T.Text
mkBgColorANSI bg = T.pack . setSGRCode $ [ uncurry (SetColor Background) bg ]


mkColorANSI :: (ColorIntensity, Color) -> (ColorIntensity, Color) -> T.Text
mkColorANSI fg bg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg, uncurry (SetColor Background) bg ]


-- ==================================================
-- ANSI color codes by color name:


blue, cyan, green, magenta, red, yellow :: T.Text
blue    = mkFgColorANSI (Dull, Blue)
cyan    = mkFgColorANSI (Dull, Cyan)
green   = mkFgColorANSI (Dull, Green)
magenta = mkFgColorANSI (Dull, Magenta)
red     = mkFgColorANSI (Dull, Red)
yellow  = mkFgColorANSI (Dull, Yellow)


-- ==================================================
-- ANSI color codes by usage:


abbrevColor :: T.Text
abbrevColor = cyan


adminBroadcastColor :: T.Text
adminBroadcastColor = yellow


adminMsgColor :: T.Text
adminMsgColor = magenta


adminTellColor :: T.Text
adminTellColor = magenta


announceColor :: T.Text
announceColor = magenta


arrowColor :: T.Text
arrowColor = yellow


asteriskColor :: T.Text
asteriskColor = magenta


blinkANSI :: T.Text
blinkANSI = T.pack . setSGRCode $ [ SetBlinkSpeed SlowBlink ]


bootMsgColor :: T.Text
bootMsgColor = red


dfltColor :: T.Text
dfltColor = resetANSI


dfltColor' :: T.Text
dfltColor' = ansiEsc `T.cons` ansiBracket `T.cons` "39;49" `T.snoc` ansiSGRDelimiter


envVarColor :: T.Text
envVarColor = cyan


exitsColor :: T.Text
exitsColor = magenta


fromPeepedColor :: T.Text
fromPeepedColor = mkColorANSI (Vivid, White) (Dull, Blue)


headerColor :: T.Text
headerColor = mkColorANSI (Dull, White) (Dull, Red)


hintANSI :: T.Text
hintANSI = blinkANSI <> underlineANSI


knownNameColor :: T.Text
knownNameColor = green


motdColor :: T.Text
motdColor = yellow


newRecordColor :: T.Text
newRecordColor = magenta


noBlinkANSI :: T.Text
noBlinkANSI = T.pack . setSGRCode $ [ SetBlinkSpeed NoBlink ]


noHintANSI :: T.Text
noHintANSI = noUnderlineANSI <> noBlinkANSI


noUnderlineANSI :: T.Text
noUnderlineANSI = T.pack . setSGRCode $ [ SetUnderlining NoUnderline ]


pagerPromptColor :: T.Text
pagerPromptColor = mkColorANSI (Dull, Black) (Dull, White)


printConsoleColor :: T.Text
printConsoleColor = magenta


promptColor :: T.Text
promptColor = yellow


quoteColor :: T.Text
quoteColor = green


selfColor :: T.Text
selfColor = green


shutdownMsgColor :: T.Text
shutdownMsgColor = red


syntaxSymbolColor :: T.Text
syntaxSymbolColor = blue


toPeepedColor :: T.Text
toPeepedColor = mkColorANSI (Vivid, White) (Dull, Green)


underlineANSI :: T.Text
underlineANSI = T.pack . setSGRCode $ [ SetUnderlining SingleUnderline ]


unknownNameColor :: T.Text
unknownNameColor = yellow


wtfColor :: T.Text
wtfColor = magenta


zingColor :: T.Text
zingColor = red


-- ==================================================
-- Helpers for working with embedded ANSI codes:


colorizeFileTxt :: T.Text -> T.Text -> T.Text
colorizeFileTxt c t | T.last t == '\n' = nl . T.concat $ [ c, T.init t, dfltColor ]
                    | otherwise        = c <> t <> dfltColor


-----


dropANSI :: T.Text -> T.Text
dropANSI t | ansiCSI `notInfixOf` t = t
           | otherwise              = let (left, rest)      = T.breakOn   (T.singleton ansiEsc) t
                                          (T.tail -> right) = T.dropWhile (/= ansiSGRDelimiter) rest
                                      in T.null right ? left :? left <> dropANSI right


-----


type EscSeq = T.Text


extractANSI :: T.Text -> [(T.Text, EscSeq)]
extractANSI t | (T.length -> l, rest) <- T.span (== ' ') t
              , l > 0     = helper $ T.replicate l (T.singleton indentFiller) <> rest
              | otherwise = helper t
  where
    helper txt
      | ansiCSI `notInfixOf` txt = [(txt, "")]
      | (txt',                                  rest)            <- T.breakOn (T.singleton ansiEsc)          txt
      , ((`T.snoc` ansiSGRDelimiter) -> escSeq, T.tail -> rest') <- T.breakOn (T.singleton ansiSGRDelimiter) rest
      = T.null rest' ? [(txt', escSeq)] :? (txt', escSeq) : helper rest'


-----


insertANSI :: [(T.Text, EscSeq)] -> [T.Text] -> [T.Text]
insertANSI [(_, "")] wrapped                                        = wrapped
insertANSI extracted (T.intercalate (T.singleton breakMarker) -> t) =
    T.split (== breakMarker) . loopOverExtractedList extracted $ t


loopOverExtractedList :: [(T.Text, EscSeq)] -> T.Text -> T.Text
loopOverExtractedList []                  ys = ys
loopOverExtractedList [("", escSeq)]      "" = escSeq
loopOverExtractedList ((xs, escSeq):rest) ys
  | T.null xs = escSeq <> loopOverExtractedList rest ys
  | left         <- loopOverExtractedTxt xs ys
  , (Just right) <- left `T.stripPrefix` ys = left <> escSeq <> loopOverExtractedList rest right
loopOverExtractedList xs ys = patternMatchFail "loopOverExtractedList" [ showText xs, ys ]


loopOverExtractedTxt :: T.Text -> T.Text -> T.Text
loopOverExtractedTxt a@(T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys))
  | x == y            = x            `T.cons` loopOverExtractedTxt xs ys
  | y == indentFiller = indentFiller `T.cons` loopOverExtractedTxt a  ys
  | y == breakMarker  = breakMarker  `T.cons` loopOverExtractedTxt a  ys
loopOverExtractedTxt "" _ = ""
loopOverExtractedTxt a  b = patternMatchFail "loopOverExtractedTxt" [ a, b ]
