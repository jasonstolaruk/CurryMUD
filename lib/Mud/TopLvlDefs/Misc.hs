{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Misc where

import Mud.TopLvlDefs.Chars
import Paths_currymud

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Version (showVersion)
import qualified Data.Text as T
import System.Posix.Types (FileOffset)


default (Int)


-----


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


type Seconds = Int -- 10^6 microseconds (the unit used by "threadDelay").


type Milliseconds = Int -- 10^-3 or 1/1000 or 0.001 seconds.


aggregateCoinNames :: [Text]
aggregateCoinNames = [ "coin", "coins" ]


alertMsgTriggers :: [Text]
alertMsgTriggers = [ "molest", "rape" ]


allCoinNames :: [Text]
allCoinNames = coinNames ++ aggregateCoinNames


acl, aop, enc, enc's, etc :: Text
acl   = T.singleton adverbCloseChar
aop   = T.singleton adverbOpenChar
enc   = T.singleton emoteNameChar
enc's = enc <> "'s"
etc   = T.singleton emoteTargetChar


biodegDelay :: Seconds
biodegDelay = 5


biodegSecs :: Seconds
biodegSecs = 5 {- mins -} * 60 {- secs -} -- five mins


bonusDelay :: Seconds
bonusDelay = 24 {- hours -} * 60 {- mins -} * 60 {- secs -} -- twenty-four hours


coinNames :: [Text]
coinNames = [ "cp", "sp", "gp" ]


coinFullNames :: [Text]
coinFullNames = [ "copper piece", "silver piece", "gold piece" ]


dbTblPurgerDelay :: Seconds
dbTblPurgerDelay = 60 {- mins -} * 60 {- secs -} -- one hr


initPickPts :: Int
initPickPts = 200


isDebug :: Bool
isDebug = True


isEKGing :: Bool
isEKGing = True


isZBackDoor :: Bool
isZBackDoor = True


likewise :: Bool
likewise = not otherwise


logRotationDelay :: Seconds
logRotationDelay = 60 {- mins -} * 60 {- secs -} -- one hr


maxChanNameLen :: Int
maxChanNameLen = 12


maxSpiritSecs :: Seconds
maxSpiritSecs = 5 {- mins -} * 60 {- secs -} -- five mins


maxTempDescLen :: Int
maxTempDescLen = 200


maxCmdLen :: Int
maxCmdLen = 12


maxCols, minCols :: Int
maxCols = 200
minCols = 30


maxDbTblRecs :: Int
maxDbTblRecs = 10000


maxHelpTopicLen :: Int
maxHelpTopicLen = 13


maxInacSecs :: Integer
maxInacSecs = 10 {- mins -} * 60 {- secs -} -- ten mins


maxMobRmDescLen :: Int
maxMobRmDescLen = 80


maxNameLen,    minNameLen    :: Int
maxNameLenTxt, minNameLenTxt :: Text
maxNameLen    = 12
maxNameLenTxt = "twelve"
minNameLen    = 3
minNameLenTxt = "three"


maxPageLines, minPageLines :: Int
maxPageLines = 150
minPageLines = 8


maxPwLen, minPwLen :: Int
maxPwLen = 20
minPwLen = 6


maxLogSize :: FileOffset
maxLogSize = 1000000


noOfDbTblRecsToPurge :: Int
noOfDbTblRecsToPurge = 100000 -- 1/10th of "maxLogSize".


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


rndmVectorLen :: Int
rndmVectorLen = 5


stdLinkNames :: [Text]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


threadTblPurgerDelay :: Seconds
threadTblPurgerDelay = 10 {- mins -} * 60 {- secs -} -- ten mins


trashDumpPurgerDelay :: Seconds
trashDumpPurgerDelay = 6 {- hrs -} * 60 {- mins -} * 60 {- secs -} -- six hrs


ver :: Text
ver = T.pack . showVersion $ version


worldPersisterDelay :: Seconds
worldPersisterDelay = 10 {- mins -} * 60 {- secs -} -- ten mins


yous :: [Text]
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
