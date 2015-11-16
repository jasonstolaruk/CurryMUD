{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Misc.ANSI ( abbrevColor
                     , adminBroadcastColor
                     , announceColor
                     , arrowColor
                     , asteriskColor
                     , black
                     , blink
                     , blinkANSI
                     , blue
                     , bootMsgColor
                     , colorizeFileTxt
                     , colors
                     , colorWith
                     , cyan
                     , dfltColor
                     , dfltColor'
                     , dropANSI
                     , emoteTargetColor
                     , envVarColor
                     , exitsColor
                     , extractANSI
                     , fromPeepedColor
                     , green
                     , greenBarColor
                     , headerColor
                     , hintANSI
                     , insertANSI
                     , intensities
                     , knownNameColor
                     , loggedInColor
                     , magenta
                     , mkBgColorANSI
                     , mkColorANSI
                     , mkFgColorANSI
                     , motdColor
                     , newRecordColor
                     , noBlinkANSI
                     , noHintANSI
                     , noUnderlineANSI
                     , pagerPromptColor
                     , prefixColor
                     , printConsoleColor
                     , promoteDemoteColor
                     , promptIndentColor
                     , questionArrivalColor
                     , quoteColor
                     , red
                     , redBarColor
                     , regexMatchColor
                     , resetANSI
                     , selectorColor
                     , shutdownMsgColor
                     , syntaxSymbolColor
                     , toPeepedColor
                     , tunedInColor
                     , underline
                     , underlineANSI
                     , unknownNameColor
                     , unlinkColor
                     , white
                     , wiretapColor
                     , wtfColor
                     , yellow
                     , zingColor ) where

import Mud.TopLvlDefs.Chars
import Mud.Util.Operators
import Mud.Util.Quoting
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


colorWith :: T.Text -> T.Text -> T.Text
colorWith = quoteWith' . (, dfltColor)


blink :: T.Text -> T.Text
blink = quoteWith' (blinkANSI, noBlinkANSI)


underline :: T.Text -> T.Text
underline = quoteWith' (underlineANSI, noUnderlineANSI)


-- ==================================================
-- ANSI color codes by color name:


black, blue, cyan, green, magenta, red, white, yellow :: T.Text
black   = mkFgColorANSI (Dull, Black  )
blue    = mkFgColorANSI (Dull, Blue   )
cyan    = mkFgColorANSI (Dull, Cyan   )
green   = mkFgColorANSI (Dull, Green  )
magenta = mkFgColorANSI (Dull, Magenta)
red     = mkFgColorANSI (Dull, Red    )
white   = mkFgColorANSI (Dull, White  )
yellow  = mkFgColorANSI (Dull, Yellow )


-- ==================================================
-- ANSI color codes by usage:


abbrevColor :: T.Text
abbrevColor = cyan


adminBroadcastColor :: T.Text
adminBroadcastColor = yellow


announceColor :: T.Text
announceColor = magenta


arrowColor :: T.Text
arrowColor = magenta


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


emoteTargetColor :: T.Text
emoteTargetColor = green


envVarColor :: T.Text
envVarColor = cyan


exitsColor :: T.Text
exitsColor = magenta


fromPeepedColor :: T.Text
fromPeepedColor = mkColorANSI (Dull, White) (Dull, Blue)


greenBarColor :: T.Text
greenBarColor = mkColorANSI (Dull, White) (Dull, Green)


headerColor :: T.Text
headerColor = mkColorANSI (Dull, White) (Dull, Red)


hintANSI :: T.Text
hintANSI = blinkANSI <> underlineANSI


knownNameColor :: T.Text
knownNameColor = green


loggedInColor :: T.Text
loggedInColor = yellow


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


prefixColor :: T.Text
prefixColor = red


printConsoleColor :: T.Text
printConsoleColor = magenta


promoteDemoteColor :: T.Text
promoteDemoteColor = mkColorANSI (Dull, White) (Dull, Red)


promptIndentColor :: T.Text
promptIndentColor = mkColorANSI (Dull, White) (Dull, White)


questionArrivalColor :: T.Text
questionArrivalColor = yellow


quoteColor :: T.Text
quoteColor = green


redBarColor :: T.Text
redBarColor = mkColorANSI (Dull, White) (Dull, Red)


regexMatchColor :: T.Text
regexMatchColor = cyan


selectorColor :: T.Text
selectorColor = yellow


shutdownMsgColor :: T.Text
shutdownMsgColor = red


syntaxSymbolColor :: T.Text
syntaxSymbolColor = yellow


toPeepedColor :: T.Text
toPeepedColor = mkColorANSI (Dull, White) (Dull, Green)


tunedInColor :: T.Text
tunedInColor = loggedInColor


underlineANSI :: T.Text
underlineANSI = T.pack . setSGRCode $ [ SetUnderlining SingleUnderline ]


unknownNameColor :: T.Text
unknownNameColor = yellow


unlinkColor :: T.Text
unlinkColor = magenta


wiretapColor :: T.Text
wiretapColor = mkColorANSI (Dull, White) (Dull, Yellow)


wtfColor :: T.Text
wtfColor = magenta


zingColor :: T.Text
zingColor = red


-- ==================================================
-- Helpers for working with embedded ANSI codes:


colorizeFileTxt :: T.Text -> T.Text -> T.Text
colorizeFileTxt c t | T.last t == '\n' = nl . colorWith c . T.init $ t
                    | otherwise        = colorWith c t


-----


dropANSI :: T.Text -> T.Text
dropANSI t | ansiCSI `notInfixOf` t = t
           | otherwise              = let (left, rest)      = T.breakOn   (T.singleton ansiEsc) t
                                          (T.tail -> right) = T.dropWhile (/= ansiSGRDelimiter) rest
                                      in ()# right ? left :? left <> dropANSI right


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
      = ()# rest' ? [(txt', escSeq)] :? (txt', escSeq) : helper rest'


-----


insertANSI :: [(T.Text, EscSeq)] -> [T.Text] -> [T.Text]
insertANSI [(_, "")] wrapped                                        = wrapped
insertANSI extracted (T.intercalate (T.singleton breakMarker) -> t) =
    T.split (== breakMarker) . loopOverExtractedList extracted $ t


loopOverExtractedList :: [(T.Text, EscSeq)] -> T.Text -> T.Text
loopOverExtractedList []                  ys = ys
loopOverExtractedList [("", escSeq)]      "" = escSeq
loopOverExtractedList ((xs, escSeq):rest) ys
  | ()# xs = escSeq <> loopOverExtractedList rest ys
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
