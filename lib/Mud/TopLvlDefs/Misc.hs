{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Misc where

import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars

import Data.Monoid ((<>))
import System.Posix.Types (FileOffset)
import qualified Data.Text as T


aggregateCoinNames :: [T.Text]
aggregateCoinNames = [ "coin", "coins" ]


allCoinNames :: [T.Text]
allCoinNames = coinNames ++ aggregateCoinNames


ansiCSI :: T.Text
ansiCSI = T.pack [ ansiEsc, ansiBracket ]


coinNames :: [T.Text]
coinNames = [ "cp", "sp", "gp" ]


dfltPrompt :: T.Text
dfltPrompt = promptColor <> "->" <> dfltColor


isDebug :: Bool
isDebug = True


logRotationFlaggerDelay :: Int
logRotationFlaggerDelay = 5 * 60


maxCmdLen :: Int
maxCmdLen = 11


maxCols, minCols :: Int
maxCols = 200
minCols = 30


maxHelpTopicLen :: Int
maxHelpTopicLen = 12


maxInacSecs :: Integer
maxInacSecs = 10 * 60


maxPageLines, minPageLines :: Int
maxPageLines = 150
minPageLines = 8


maxLogSize :: FileOffset
maxLogSize = 100000


noOfTitles :: Int
noOfTitles = 33


port :: Int
port = 9696


stdLinkNames :: [T.Text]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


threadTblPurgerDelay :: Int
threadTblPurgerDelay = 60 * 60


ver :: T.Text
ver = "0.1.0.0 (in development since 2013-10)"
