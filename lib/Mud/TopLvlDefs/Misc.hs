{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Misc where

import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars

import Data.Monoid ((<>))
import System.Posix.Types (FileOffset)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


aggregateCoinNames :: [T.Text]
aggregateCoinNames = [ "coin", "coins" ]


allCoinNames :: [T.Text]
allCoinNames = coinNames ++ aggregateCoinNames


coinNames :: [T.Text]
coinNames = [ "cp", "sp", "gp" ]


coinFullNames :: [T.Text]
coinFullNames = [ "copper piece", "silver piece", "gold piece" ]


dbTblPurgerDelay :: Int
dbTblPurgerDelay = 24 * 60 * 60 -- one day


dfltPrompt :: T.Text
dfltPrompt = promptColor <> "->" <> dfltColor


acl, aop, enc, enc's, etc :: T.Text
acl   = T.singleton adverbCloseChar
aop   = T.singleton adverbOpenChar
enc   = T.singleton emoteNameChar
enc's = enc <> "'s"
etc   = T.singleton emoteTargetChar


isDebug :: Bool
isDebug = True


logRotationDelay :: Int
logRotationDelay = 60 * 60 -- one hr


maxChanNameLen :: Int
maxChanNameLen = 12


maxCmdLen :: Int
maxCmdLen = 11


maxCols, minCols :: Int
maxCols = 200
minCols = 30


maxDbTblRecs :: Int
maxDbTblRecs = 10000


maxHelpTopicLen :: Int
maxHelpTopicLen = 12


maxInacSecs :: Integer
maxInacSecs = 10 * 60 -- ten mins


maxNameLen,    minNameLen    :: Int
maxNameLenTxt, minNameLenTxt :: T.Text
maxNameLen    = 12
maxNameLenTxt = "twelve"
minNameLen    = 3
minNameLenTxt = "three"


maxPageLines, minPageLines :: Int
maxPageLines = 150
minPageLines = 8


maxLogSize :: FileOffset
maxLogSize = 100000


noOfDbTblRecsToPurge :: Int
noOfDbTblRecsToPurge = 1000


noOfLogFiles :: Int
noOfLogFiles = 10


noOfPersistedWorlds :: Int
noOfPersistedWorlds = 25


noOfTitles :: Int
noOfTitles = 33


port :: Int
port = 9696


rmDescIndentAmt :: Int
rmDescIndentAmt = 2


stdLinkNames :: [T.Text]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


threadTblPurgerDelay :: Int
threadTblPurgerDelay = 60 * 60 -- one hr


ver :: T.Text
ver = "0.1.0.0 (in development since 2013-10)"


worldPersisterDelay :: Int
worldPersisterDelay = 10 * 60 -- ten mins


yous :: [T.Text]
yous = [ "you"
       , "you'd"
       , "you'll"
       , "you're"
       , "you's"
       , "you've"
       , "your"
       , "yours"
       , "yourself"
       , "yourselves"
       , "yous" ]
