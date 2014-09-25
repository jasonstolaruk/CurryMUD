{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Copyright 2014 Jason Stolaruk and Detroit Labs LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Mud.TopLvlDefs where

import Mud.StateDataTypes

import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T


ver :: T.Text
ver = "0.1.0.0 (in development since 2013-10)"


port :: Int
port = 9696


telnetIAC, telnetSB, telnetSE :: Char
telnetIAC = '\255'
telnetSB  = '\250'
telnetSE  = '\240'


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in (home ++) . T.unpack $ "/CurryMUD/"


logDir, resDir, helpDir, titleDir, miscDir :: FilePath
logDir   = mudDir ++ "logs/"
resDir   = mudDir ++ "res/"
helpDir  = resDir ++ "help/"
titleDir = resDir ++ "titles/"
miscDir  = resDir ++ "misc/"


noOfTitles :: Int
noOfTitles = 33


allChar, amountChar, indentTagChar, indexChar, rmChar, slotChar, wizCmdChar, debugCmdChar :: Char
allChar       = '\''
amountChar    = '/'
indentTagChar = '`'
indexChar     = '.'
rmChar        = '-'
slotChar      = ':'
wizCmdChar    = ':'
debugCmdChar  = '!'


minCols, maxCols :: Int
minCols = 30
maxCols = 200


stdLinkNames :: [LinkName]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


coinNames :: [T.Text]
coinNames = [ "cp", "sp", "gp" ]


aggregateCoinNames :: [T.Text]
aggregateCoinNames = [ "coin", "coins" ]


allCoinNames :: [T.Text]
allCoinNames = coinNames ++ aggregateCoinNames


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."
