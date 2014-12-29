{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Color where

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGRCode)
import qualified Data.Text as T


intensities :: [ColorIntensity]
intensities = [ Dull, Vivid ]


colors :: [Color]
colors = [ Black .. White ]


dfltFgColor :: (ColorIntensity, Color)
dfltFgColor = (Vivid, White)


dfltBgColor :: (ColorIntensity, Color)
dfltBgColor = (Dull, Black)


mkColorANSI :: (ColorIntensity, Color) -> (ColorIntensity, Color) -> T.Text
mkColorANSI fg bg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg, uncurry (SetColor Background) bg ]


mkFgColorANSI :: (ColorIntensity, Color) -> T.Text
mkFgColorANSI fg = mkColorANSI fg dfltBgColor


dfltColorANSI :: T.Text
dfltColorANSI = mkColorANSI dfltFgColor dfltBgColor
