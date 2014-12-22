{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs where

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


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in home ++ "/CurryMUD/"


helpDir, logDir, miscDir, resDir, titleDir, uptimeFile :: FilePath
helpDir    = resDir ++ "help/"
logDir     = mudDir ++ "logs/"
miscDir    = resDir ++ "misc/"
resDir     = mudDir ++ "res/"
titleDir   = resDir ++ "titles/"
uptimeFile = mudDir ++ "uptime"


allChar, amountChar, debugCmdChar, indentTagChar, indexChar, rmChar, slotChar, wizCmdChar :: Char
allChar       = '\''
amountChar    = '/'
debugCmdChar  = '!'
indentTagChar = '`'
indexChar     = '.'
rmChar        = '-'
slotChar      = ':'
wizCmdChar    = ':'


telnetGA, telnetIAC, telnetSB, telnetSE :: Char
telnetGA  = '\249'
telnetIAC = '\255'
telnetSB  = '\250'
telnetSE  = '\240'


desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
desigDelimiter       = '\132'
nonStdDesigDelimiter = '\131'
stdDesigDelimiter    = '\130'


indentFiller :: Char
indentFiller = '\128'


noOfTitles :: Int
noOfTitles = 33


maxInacSecs :: Integer -- TODO
maxInacSecs = 60 -- 60 * 10


maxLogSize :: FileOffset
maxLogSize = 100000


maxCmdLen :: Int
maxCmdLen = 9


minCols, maxCols :: Int
minCols = 30
maxCols = 200


stdLinkNames :: [T.Text]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


coinNames :: [T.Text]
coinNames = [ "cp", "sp", "gp" ]


aggregateCoinNames :: [T.Text]
aggregateCoinNames = [ "coin", "coins" ]


allCoinNames :: [T.Text]
allCoinNames = coinNames ++ aggregateCoinNames


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"
