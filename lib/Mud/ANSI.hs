{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.ANSI ( abbrevColor
                , adminMsgColor
                , adminNoticeColor
                , adminTellColor
                , announceColor
                , asteriskColor
                , bootMsgColor
                , colors
                , dfltColor
                , dfltColor'
                , envVarColor
                , exitsColor
                , fromPeepedColor
                , headerColor
                , hintColor
                , intensities
                , knownNameColor
                , mkBgColorANSI
                , mkColorANSI
                , mkFgColorANSI
                , motdColor
                , newRecordColor
                , noUnderline
                , pagerPromptColor
                , printConsoleColor
                , promptColor
                , quoteColor
                , resetANSI
                , selfColor
                , shutdownMsgColor
                , toPeepedColor
                , underline
                , unknownNameColor
                , wtfColor
                , zingColor ) where

import Mud.TopLvlDefs.Chars

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), Underlining(..), setSGRCode)
import qualified Data.Text as T


resetANSI :: T.Text
resetANSI = T.pack . setSGRCode $ [Reset]


-----


intensities :: [ColorIntensity]
intensities = [ Dull, Vivid ]


colors :: [Color]
colors = [ Black .. White ]


-----


mkFgColorANSI :: (ColorIntensity, Color) -> T.Text
mkFgColorANSI fg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg ]


mkBgColorANSI :: (ColorIntensity, Color) -> T.Text
mkBgColorANSI bg = T.pack . setSGRCode $ [ uncurry (SetColor Background) bg ]


mkColorANSI :: (ColorIntensity, Color) -> (ColorIntensity, Color) -> T.Text
mkColorANSI fg bg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg, uncurry (SetColor Background) bg ]


-----


cyan, green, magenta, red, yellow :: T.Text
cyan    = mkFgColorANSI (Dull, Cyan)
green   = mkFgColorANSI (Dull, Green)
magenta = mkFgColorANSI (Dull, Magenta)
red     = mkFgColorANSI (Dull, Red)
yellow  = mkFgColorANSI (Dull, Yellow)


-----


abbrevColor :: T.Text
abbrevColor = cyan


adminMsgColor :: T.Text
adminMsgColor = magenta


adminNoticeColor :: T.Text
adminNoticeColor = yellow


adminTellColor :: T.Text
adminTellColor = magenta


announceColor :: T.Text
announceColor = magenta


asteriskColor :: T.Text
asteriskColor = magenta


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


hintColor :: T.Text
hintColor = magenta


knownNameColor :: T.Text
knownNameColor = green


motdColor :: T.Text
motdColor = yellow


newRecordColor :: T.Text
newRecordColor = magenta


noUnderline :: T.Text
noUnderline = T.pack . setSGRCode $ [ SetUnderlining NoUnderline ]


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


toPeepedColor :: T.Text
toPeepedColor = mkColorANSI (Vivid, White) (Dull, Green)


underline :: T.Text
underline = T.pack . setSGRCode $ [ SetUnderlining SingleUnderline ]


unknownNameColor :: T.Text
unknownNameColor = yellow


wtfColor :: T.Text
wtfColor = magenta


zingColor :: T.Text
zingColor = red
