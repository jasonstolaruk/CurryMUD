{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.Padding where

import           Mud.Misc.ANSI
import           Mud.TopLvlDefs.Padding
import           Mud.Util.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

quoteWithAndPad :: (Text, Text) -> Int -> Text -> Text
quoteWithAndPad q x t = quoteWith' q t' <> T.replicate (x - T.length t' - 2) " "
  where
    t' = T.take (pred $ x - l) t
    l  = sum . map T.length $ [ fst q, snd q ]

bracketPad :: Int -> Text -> Text
bracketPad = quoteWithAndPad ("[", "]")

pad :: Int -> Text -> Text
pad x t@(T.length . dropANSI -> l) | l >= x    = t
                                   | otherwise = t <> T.replicate (x - l) " "

padBracketedEntName :: Text -> Text
padBracketedEntName = pad bracketedEntNamePadding

padChanName :: Text -> Text
padChanName = pad chanNamePadding

padCmdName :: Text -> Text
padCmdName = pad cmdNamePadding

padColorName :: Text -> Text
padColorName = pad colorNamePadding

padGodName :: Text -> Text
padGodName = pad godNamePadding

padHelpTopic :: Text -> Text
padHelpTopic = pad helpTopicPadding

padId :: Text -> Text
padId = pad idPadding

padName :: Text -> Text
padName = pad namePadding

padOrTrunc :: Int -> Text -> Text
padOrTrunc x _                 | x < 0 = ""
padOrTrunc x t@(T.length -> l) | l < x = t <> T.replicate (x - l) " "
                               | l > x = T.take x t
padOrTrunc _ t = t

padRace :: Text -> Text
padRace = pad racePadding

padSettingName :: Text -> Text
padSettingName = pad settingNamePadding

padSex :: Text -> Text
padSex = pad sexPadding

padTwoDigits :: Int -> Text
padTwoDigits x = onTrue (x < 10) ('0' `T.cons`) . showTxt $ x

parensPad :: Int -> Text -> Text
parensPad = quoteWithAndPad ("(", ")")
