{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.ANSI ( blue
                , colors
                , cyan
                , dfltColorANSI
                , green
                , intensities
                , magenta
                , mkBgColorANSI
                , mkColorANSI
                , mkFgColorANSI
                , noUnderlineANSI
                , promptColorANSI
                , quoteColorANSI
                , red
                , resetANSI
                , topicColorANSI
                , underlineANSI
                , white
                , yellow
                , zingColorANSI ) where


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


blue, cyan, green, magenta, red, white, yellow :: T.Text
blue    = mkFgColorANSI (Dull, Blue)
cyan    = mkFgColorANSI (Dull, Cyan)
green   = mkFgColorANSI (Dull, Green)
magenta = mkFgColorANSI (Dull, Magenta)
red     = mkFgColorANSI (Dull, Red)
white   = mkFgColorANSI (Dull, White)
yellow  = mkFgColorANSI (Dull, Yellow)


-----


dfltColorANSI :: T.Text
dfltColorANSI = ansiEsc `T.cons` ansiBracket `T.cons` "39;49" `T.snoc` ansiSGRDelimiter


noUnderlineANSI :: T.Text
noUnderlineANSI = T.pack . setSGRCode $ [ SetUnderlining NoUnderline ]


promptColorANSI :: T.Text
promptColorANSI = yellow


quoteColorANSI :: T.Text
quoteColorANSI = cyan


topicColorANSI :: T.Text
topicColorANSI = mkColorANSI (Dull, Black) (Dull, White)


underlineANSI :: T.Text
underlineANSI = T.pack . setSGRCode $ [ SetUnderlining SingleUnderline ]


zingColorANSI :: T.Text
zingColorANSI = red
