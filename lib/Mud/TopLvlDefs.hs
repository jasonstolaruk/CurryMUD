{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs where

import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (FileOffset)
import qualified Data.Text as T


aggregateCoinNames :: [T.Text]
aggregateCoinNames = [ "coin", "coins" ]


allChar, amountChar, debugCmdChar, indentTagChar, indexChar, rmChar, slotChar, wizCmdChar :: Char
allChar       = '\''
amountChar    = '/'
debugCmdChar  = '!'
indentTagChar = '`'
indexChar     = '.'
rmChar        = '-'
slotChar      = ':'
wizCmdChar    = ':'


allCoinNames :: [T.Text]
allCoinNames = coinNames ++ aggregateCoinNames


coinNames :: [T.Text]
coinNames = [ "cp", "sp", "gp" ]


desigDelimiter, stdDesigDelimiter, nonStdDesigDelimiter :: Char
desigDelimiter       = '\132'
nonStdDesigDelimiter = '\131'
stdDesigDelimiter    = '\130'


dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


dfltShutdownMsg :: T.Text
dfltShutdownMsg = "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


helpDir, logDir, miscDir, resDir, titleDir, uptimeFile :: FilePath
helpDir    = resDir ++ "help/"
logDir     = mudDir ++ "logs/"
miscDir    = resDir ++ "misc/"
resDir     = mudDir ++ "res/"
titleDir   = resDir ++ "titles/"
uptimeFile = mudDir ++ "uptime"


indentFiller :: Char
indentFiller = '\128'


isDebug :: Bool
isDebug = True


logRotationFlaggerDelay :: Int
logRotationFlaggerDelay = 60 * 5


maxCmdLen :: Int
maxCmdLen = 9


maxCols, minCols :: Int
maxCols = 200
minCols = 30


maxInacSecs :: Integer
maxInacSecs = 60 * 10


maxLogSize :: FileOffset
maxLogSize = 100000


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in home ++ "/CurryMUD/"


noOfTitles :: Int
noOfTitles = 33


port :: Int
port = 9696


stdLinkNames :: [T.Text]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


telnetGA, telnetIAC, telnetSB, telnetSE :: Char
telnetGA  = '\249'
telnetIAC = '\255'
telnetSB  = '\250'
telnetSE  = '\240'


threadTblPurgerDelay :: Int
threadTblPurgerDelay = 60 * 5


ver :: T.Text
ver = "0.1.0.0 (in development since 2013-10)"
