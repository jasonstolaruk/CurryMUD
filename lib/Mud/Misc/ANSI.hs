{-# LANGUAGE MultiWayIf, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Misc.ANSI ( abbrevColor
                     , adminBcastColor
                     , adminKillColor
                     , adminSetColor
                     , adminTagColor
                     , announceColor
                     , arrowColor
                     , asteriskColor
                     , black
                     , blink
                     , blinkANSI
                     , blue
                     , bonusColor
                     , bootMsgColor
                     , colorizeFileTxt
                     , colors
                     , colorWith
                     , cyan
                     , dfltColor
                     , dfltColor'
                     , dropANSI
                     , emoteTargetColor
                     , emphasisColor
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
                     , lvlUpColor
                     , magenta
                     , mkBgColorANSI
                     , mkColorANSI
                     , mkColorTxtForXps
                     , mkFgColorANSI
                     , motdColor
                     , newRecordColor
                     , nlColor
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
                     , tempDescColor
                     , toNpcColor
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
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI (BlinkSpeed(..), Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), Underlining(..), setSGRCode)


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Misc.ANSI"


-- ==================================================
-- Misc. definitions and helpers:


ansiCSI :: Text
ansiCSI = T.pack [ ansiEsc, ansiBracket ]


resetANSI :: Text
resetANSI = T.pack . setSGRCode . pure $ Reset


colors :: [Color]
colors = [Black .. White]


intensities :: [ColorIntensity]
intensities = [ Dull, Vivid ]


mkFgColorANSI :: (ColorIntensity, Color) -> Text
mkFgColorANSI fg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg ]


mkBgColorANSI :: (ColorIntensity, Color) -> Text
mkBgColorANSI bg = T.pack . setSGRCode $ [ uncurry (SetColor Background) bg ]


mkColorANSI :: (ColorIntensity, Color) -> (ColorIntensity, Color) -> Text
mkColorANSI fg bg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg, uncurry (SetColor Background) bg ]


mkColorTxtForXps :: (Int, Int) -> Text
mkColorTxtForXps (x, y) = if | x == y    -> green
                             | per > 67  -> cyan
                             | per > 33  -> yellow
                             | per > 10  -> red
                             | otherwise -> magenta
  where
    per = x `percent` y


colorWith :: Text -> Text -> Text
colorWith = quoteWith' . (, dfltColor)


blink :: Text -> Text
blink = quoteWith' (blinkANSI, noBlinkANSI)


underline :: Text -> Text
underline = quoteWith' (underlineANSI, noUnderlineANSI)


-- ==================================================
-- ANSI color codes by color name:


black, blue, cyan, green, magenta, red, white, yellow :: Text
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


abbrevColor :: Text
abbrevColor = cyan


adminBcastColor :: Text
adminBcastColor = yellow


adminKillColor :: Text
adminKillColor = red


adminSetColor :: Text
adminSetColor = magenta


adminTagColor :: Text
adminTagColor = magenta


announceColor :: Text
announceColor = magenta


arrowColor :: Text
arrowColor = magenta


asteriskColor :: Text
asteriskColor = magenta


blinkANSI :: Text
blinkANSI = T.pack . setSGRCode $ [ SetBlinkSpeed SlowBlink ]


bonusColor :: Text
bonusColor = magenta


bootMsgColor :: Text
bootMsgColor = red


dfltColor :: Text
dfltColor = resetANSI


dfltColor' :: Text
dfltColor' = ansiEsc `T.cons` ansiBracket `T.cons` "39;49" `T.snoc` ansiSGRDelimiter


emoteTargetColor :: Text
emoteTargetColor = green


emphasisColor :: Text
emphasisColor = yellow


envVarColor :: Text
envVarColor = cyan


exitsColor :: Text
exitsColor = magenta


fromPeepedColor :: Text
fromPeepedColor = mkColorANSI (Dull, White) (Dull, Blue)


greenBarColor :: Text
greenBarColor = mkColorANSI (Dull, White) (Dull, Green)


headerColor :: Text
headerColor = mkColorANSI (Dull, White) (Dull, Red)


hintANSI :: Text
hintANSI = blinkANSI <> underlineANSI


knownNameColor :: Text
knownNameColor = green


loggedInColor :: Text
loggedInColor = yellow


lvlUpColor :: Text
lvlUpColor = mkColorANSI (Dull, White) (Dull, Red)


motdColor :: Text
motdColor = yellow


newRecordColor :: Text
newRecordColor = magenta


nlColor :: Text
nlColor = yellow


noBlinkANSI :: Text
noBlinkANSI = T.pack . setSGRCode $ [ SetBlinkSpeed NoBlink ]


noHintANSI :: Text
noHintANSI = noUnderlineANSI <> noBlinkANSI


noUnderlineANSI :: Text
noUnderlineANSI = T.pack . setSGRCode $ [ SetUnderlining NoUnderline ]


pagerPromptColor :: Text
pagerPromptColor = mkColorANSI (Dull, Black) (Dull, White)


prefixColor :: Text
prefixColor = red


printConsoleColor :: Text
printConsoleColor = magenta


promoteDemoteColor :: Text
promoteDemoteColor = mkColorANSI (Dull, White) (Dull, Red)


promptIndentColor :: Text
promptIndentColor = mkColorANSI (Dull, White) (Dull, White)


questionArrivalColor :: Text
questionArrivalColor = yellow


quoteColor :: Text
quoteColor = green


redBarColor :: Text
redBarColor = mkColorANSI (Dull, White) (Dull, Red)


regexMatchColor :: Text
regexMatchColor = cyan


selectorColor :: Text
selectorColor = yellow


shutdownMsgColor :: Text
shutdownMsgColor = red


syntaxSymbolColor :: Text
syntaxSymbolColor = yellow


tempDescColor :: Text
tempDescColor = cyan


toNpcColor :: Text
toNpcColor = mkColorANSI (Dull, Cyan) (Dull, Cyan)


toPeepedColor :: Text
toPeepedColor = mkColorANSI (Dull, White) (Dull, Green)


tunedInColor :: Text
tunedInColor = loggedInColor


underlineANSI :: Text
underlineANSI = T.pack . setSGRCode $ [ SetUnderlining SingleUnderline ]


unknownNameColor :: Text
unknownNameColor = yellow


unlinkColor :: Text
unlinkColor = magenta


wiretapColor :: Text
wiretapColor = mkColorANSI (Dull, White) (Dull, Yellow)


wtfColor :: Text
wtfColor = magenta


zingColor :: Text
zingColor = red


-- ==================================================
-- Helpers for working with embedded ANSI codes:


colorizeFileTxt :: Text -> Text -> Text
colorizeFileTxt c t | T.last t == '\n' = nl . colorWith c . T.init $ t
                    | otherwise        = colorWith c t


-----


dropANSI :: Text -> Text
dropANSI t | ansiCSI `notInfixOf` t = t
           | otherwise              = let (left, rest)      = T.breakOn   (T.singleton ansiEsc) t
                                          (T.tail -> right) = T.dropWhile (/= ansiSGRDelimiter) rest
                                      in ()# right ? left :? left <> dropANSI right


-----


type EscSeq = Text


extractANSI :: Text -> [(Text, EscSeq)]
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


insertANSI :: [(Text, EscSeq)] -> [Text] -> [Text]
insertANSI [(_, "")] wrapped                                        = wrapped
insertANSI extracted (T.intercalate (T.singleton breakMarker) -> t) =
    T.split (== breakMarker) . loopOverExtractedList extracted $ t


loopOverExtractedList :: [(Text, EscSeq)] -> Text -> Text
loopOverExtractedList []                  ys = ys
loopOverExtractedList [("", escSeq)]      "" = escSeq
loopOverExtractedList ((xs, escSeq):rest) ys
  | ()# xs = escSeq <> loopOverExtractedList rest ys
  | left         <- loopOverExtractedTxt xs ys
  , (Just right) <- left `T.stripPrefix` ys = left <> escSeq <> loopOverExtractedList rest right
loopOverExtractedList xs _ = patternMatchFail "loopOverExtractedList" . showText $ xs


loopOverExtractedTxt :: Text -> Text -> Text
loopOverExtractedTxt a@(T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys))
  | x == y            = x            `T.cons` loopOverExtractedTxt xs ys
  | y == indentFiller = indentFiller `T.cons` loopOverExtractedTxt a  ys
  | y == breakMarker  = breakMarker  `T.cons` loopOverExtractedTxt a  ys
loopOverExtractedTxt "" _ = ""
loopOverExtractedTxt a  b = patternMatchFail "loopOverExtractedTxt" . showText $ (a, b)
