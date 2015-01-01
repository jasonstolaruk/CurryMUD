{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.ANSI ( colors
                , dfltColorANSI
                , intensities
                , mkBgColorANSI
                , mkColorANSI
                , mkFgColorANSI
                , noUnderlineANSI
                , resetANSI
                , topicColorANSI
                , underlineANSI ) where


import Mud.TopLvlDefs.Chars

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), Underlining(..), setSGRCode)
import qualified Data.Text as T


resetANSI :: T.Text
resetANSI = T.pack . setSGRCode $ [Reset]


dfltColorANSI :: T.Text
dfltColorANSI = T.pack $ ansiEsc : ansiBracket : "39;49" ++ [ansiSGRDelimiter]


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


topicColorANSI :: T.Text
topicColorANSI = mkColorANSI (Dull, Black) (Dull, White)


underlineANSI :: T.Text
underlineANSI = T.pack . setSGRCode $ [ SetUnderlining SingleUnderline ]


noUnderlineANSI :: T.Text
noUnderlineANSI = T.pack . setSGRCode $ [ SetUnderlining NoUnderline ]
