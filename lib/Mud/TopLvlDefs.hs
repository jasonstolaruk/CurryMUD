{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs where

import Mud.StateDataTypes

import Control.Lens.Operators ((^.))
import Data.Text.Strict.Lens (unpacked)
import qualified Data.Text as T
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)


ver :: T.Text
ver = "0.1.0.0 (in development since 2013-10)"


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in home ++ "/CurryMUD/"^.unpacked


logDir, resDir, helpDir, titleDir, miscDir :: FilePath
logDir   = mudDir ++ "logs/"
resDir   = mudDir ++ "res/"
helpDir  = resDir ++ "help/"
titleDir = resDir ++ "titles/"
miscDir  = resDir ++ "misc/"


noOfTitles :: Int
noOfTitles = 35


nlt :: T.Text
nlt = "\n"


allChar, amountChar, indentTagChar, indexChar, rmChar, slotChar, wizChar :: Char
allChar       = '\''
amountChar    = '/'
indentTagChar = '`'
indexChar     = '.'
rmChar        = '-'
slotChar      = ':'
wizChar       = ':'



cols, minCols, maxCols :: Int -- TODO: Move to "Pla" data type?
cols    = 80
minCols = 30
maxCols = 200


stdLinkNames :: [LinkName]
stdLinkNames = ["n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d"]


coinNames :: [T.Text]
coinNames = ["cp", "sp", "gp"]


aggregateCoinNames :: [T.Text]
aggregateCoinNames = ["coin", "coins"]


allCoinNames :: [T.Text]
allCoinNames = coinNames ++ aggregateCoinNames
