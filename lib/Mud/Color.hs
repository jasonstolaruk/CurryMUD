{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Color ( colors
                 , dfltColorANSI
                 , headingColorANSI
                 , intensities
                 , mkBgColorANSI
                 , mkColorANSI
                 , mkFgColorANSI
                 , resetANSI
                 , topicColorANSI ) where


import Mud.TopLvlDefs.Chars

import Data.Monoid ((<>))
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGRCode)
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
mkColorANSI fg bg = mkFgColorANSI fg <> mkBgColorANSI bg


-----


headingColorANSI :: T.Text
headingColorANSI = mkColorANSI (Dull, Black) (Dull, White)


topicColorANSI :: T.Text
topicColorANSI = mkColorANSI (Dull, White) (Dull, Cyan)
