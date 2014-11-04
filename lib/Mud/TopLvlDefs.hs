{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs where

import Mud.StateDataTypes

import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (FileOffset)
import qualified Data.Text as T


ver :: T.Text
ver = "0.1.0.0 (in development since 2013-10)"


isDebug :: Bool
isDebug = True


port :: Int
port = 9696


telnetIAC, telnetSB, telnetSE, telnetGA :: Char
telnetIAC = '\255'
telnetSB  = '\250'
telnetSE  = '\240'
telnetGA  = '\249'


stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter :: Char
stdDesigDelimiter    = '\128'
nonStdDesigDelimiter = '\129'
desigDelimiter       = '\130'


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in (home ++) . T.unpack $ "/CurryMUD/"


logDir, resDir, helpDir, titleDir, miscDir, uptimeFile :: FilePath
logDir     = mudDir ++ "logs/"
resDir     = mudDir ++ "res/"
helpDir    = resDir ++ "help/"
titleDir   = resDir ++ "titles/"
miscDir    = resDir ++ "misc/"
uptimeFile = mudDir ++ "uptime"


noOfTitles :: Int
noOfTitles = 33


maxLogSize :: FileOffset
maxLogSize = 100000


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
