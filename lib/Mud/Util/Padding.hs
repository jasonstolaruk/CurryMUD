{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Padding where

import Mud.Misc.ANSI
import Mud.TopLvlDefs.Padding
import Mud.Util.Quoting

import Data.Monoid ((<>))
import qualified Data.Text as T


quoteWithAndPad :: (T.Text, T.Text) -> Int -> T.Text -> T.Text
quoteWithAndPad q x t = quoteWith' q t' <> T.replicate (x - T.length t' - 2) " "
  where
    t' = T.take (pred $ x - l) t
    l  = sum . map T.length $ [ fst q, snd q ]


bracketPad :: Int -> T.Text -> T.Text
bracketPad = quoteWithAndPad ("[", "]")


pad :: Int -> T.Text -> T.Text
pad x t@(T.length . dropANSI -> l)
  | l >= x    = t
  | otherwise = t <> T.replicate (x - l) " "


padCmdName :: T.Text -> T.Text
padCmdName = pad cmdNamePadding


padColorName :: T.Text -> T.Text
padColorName = pad colorNamePadding


padEntName :: T.Text -> T.Text
padEntName = pad entNamePadding


padHelpTopic :: T.Text -> T.Text
padHelpTopic = pad helpTopicPadding


padName :: T.Text -> T.Text
padName = pad namePadding


padRace :: T.Text -> T.Text
padRace = pad racePadding


padSettingName :: T.Text -> T.Text
padSettingName = pad settingNamePadding


padSex :: T.Text -> T.Text
padSex = pad sexPadding


parensPad :: Int -> T.Text -> T.Text
parensPad = quoteWithAndPad ("(", ")")


padOrTrunc :: Int -> T.Text -> T.Text
padOrTrunc x _                 | x < 0 = ""
padOrTrunc x t@(T.length -> l) | l < x = t <> T.replicate (x - l) " "
                               | l > x = T.take x t
padOrTrunc _ t = t
