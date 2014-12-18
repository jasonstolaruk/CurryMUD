{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.Color where

import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGRCode)
import qualified Data.Text as T


intensities :: [ColorIntensity]
intensities = [ Dull, Vivid ]


colors :: [Color]
colors = [ Black .. White ]


mkColorANSI :: (ColorIntensity, Color) -> (ColorIntensity, Color) -> T.Text
mkColorANSI fg bg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg, uncurry (SetColor Background) bg ]


dfltColorANSI :: T.Text
dfltColorANSI = mkColorANSI (Vivid, White) (Dull, Black)
