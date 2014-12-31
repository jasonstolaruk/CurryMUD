{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Color ( colors
                 , dfltColorANSI
                 , intensities
                 , mkColorANSI
                 , mkFgColorANSI
                 , parseColorCodes ) where


import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGRCode)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Color"


-- ==================================================


intensities :: [ColorIntensity]
intensities = [ Dull, Vivid ]


colors :: [Color]
colors = [ Black .. White ]


-- dfltFgColor :: (ColorIntensity, Color)
-- dfltFgColor = (Vivid, White)


dfltBgColor :: (ColorIntensity, Color)
dfltBgColor = (Dull, Black)


mkColorANSI :: (ColorIntensity, Color) -> (ColorIntensity, Color) -> T.Text
mkColorANSI fg bg = T.pack . setSGRCode $ [ uncurry (SetColor Foreground) fg, uncurry (SetColor Background) bg ]


mkFgColorANSI :: (ColorIntensity, Color) -> T.Text
mkFgColorANSI fg = mkColorANSI fg dfltBgColor


dfltColorANSI :: T.Text
dfltColorANSI = reset -- mkColorANSI dfltFgColor dfltBgColor


parseColorCodes :: T.Text -> T.Text
parseColorCodes t
  | T.singleton colorCodeDelimiter `notInfixOf` t = t
  | otherwise = let (left, T.tail -> rest)  = T.break (== colorCodeDelimiter) t
                    (code, T.tail -> right) = T.break (== colorCodeDelimiter) rest
                in left <> colorCodeToANSI code <> parseColorCodes right


-- TODO: Move?
colorCodeDelimiter :: Char
colorCodeDelimiter = '\\'


colorCodeToANSI :: T.Text -> T.Text
colorCodeToANSI (T.toLower -> code) = case code of
  "d"  -> dfltColorANSI
  "hh" -> helpHeading
  "ht" -> helpTopic
  "r"  -> reset
  x    -> patternMatchFail "colorCodeToANSI" [x]


helpHeading :: T.Text
helpHeading = mkColorANSI (Dull, Black) (Dull, White)


helpTopic :: T.Text
helpTopic = mkColorANSI (Dull, White) (Dull, Cyan)


reset :: T.Text
reset = T.pack . setSGRCode $ [Reset]
