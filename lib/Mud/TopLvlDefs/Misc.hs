{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Misc ( acl
                           , aggregateCoinNames
                           , alertMsgTriggers
                           , allCoinNames
                           , aop
                           , biodegDelay
                           , biodegSecs
                           , bonusDelay
                           , coinFullNames
                           , coinNames
                           , corpsePlaceholder
                           , dbTblPurgerDelay
                           , dfltZoom
                           , ekgPort
                           , enc
                           , enc's
                           , etc
                           , foodWaterFeelDur
                           , initPickPts
                           , likewise
                           , logRotationDelay
                           , maxBonuses
                           , maxChanNameLen
                           , maxCmdLen
                           , maxCols
                           , maxDbTblRecs
                           , maxHelpTopicLen
                           , maxInacSecs
                           , maxInacSecsCompose
                           , maxLanternSecs
                           , maxLogSize
                           , maxMobRmDescLen
                           , maxNameLen
                           , maxNameLenTxt
                           , maxPageLines
                           , maxPwLen
                           , maxSpiritSecs
                           , maxTempDescLen
                           , minCols
                           , minNameLen
                           , minNameLenPlus1Txt
                           , minNameLenTxt
                           , minPageLines
                           , minPwLen
                           , noOfDbTblRecsToPurge
                           , noOfLogFiles
                           , noOfPersistedWorlds
                           , noOfTitles
                           , port
                           , potFeelDur
                           , restServicePort
                           , rmDescIndentAmt
                           , rndmVectorLen
                           , sacrificeBonusSecs
                           , sacrificeSecs
                           , stdLinkNames
                           , threadTblPurgerDelay
                           , torchSecs
                           , trashDumpPurgerDelay
                           , ver
                           , worldPersisterDelay
                           , yous ) where

import           Mud.TopLvlDefs.Chars
import           Mud.Util.Quoting
import           Paths_curry_mud

import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Version (showVersion)
import           Mud.TopLvlDefs.Seconds
import qualified Data.Text as T
import           System.Posix.Types (FileOffset)


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
biodegSecs = tenMinsInSecs


bonusDelay :: Seconds
bonusDelay = oneDayInSecs


coinNames :: [Text]
coinNames = [ "cp", "sp", "gp" ]


coinFullNames :: [Text]
coinFullNames = [ "copper piece", "silver piece", "gold piece" ]


corpsePlaceholder :: Text
corpsePlaceholder = parensQuote "corpse"


dbTblPurgerDelay :: Seconds
dbTblPurgerDelay = oneHrInSecs


dfltZoom :: Int
dfltZoom = 10


ekgPort :: Int
ekgPort = 8000


foodWaterFeelDur :: Seconds
foodWaterFeelDur = 20


initPickPts :: Int
initPickPts = 200


likewise :: Bool
likewise = not otherwise


logRotationDelay :: Seconds
logRotationDelay = oneHrInSecs


maxBonuses :: Int
maxBonuses = 10


maxChanNameLen :: Int
maxChanNameLen = 12


-- "lagomorphean" and "vulepnoidean" are both 12 characters long.
-- Admin and debug commands should be at most 11 characters long, leaving 1 character to account for the prefix.
-- "!coercepenny" is 12 characters long.
maxCmdLen :: Int
maxCmdLen = 12


maxCols, minCols :: Int
maxCols = 200
minCols = 30


maxDbTblRecs :: Int
maxDbTblRecs = 10000


maxHelpTopicLen :: Int
maxHelpTopicLen = 13


maxInacSecs :: Seconds
maxInacSecs = tenMinsInSecs


maxInacSecsCompose :: Seconds -- When a player is composing their PC description.
maxInacSecsCompose = oneHrInSecs


maxLanternSecs :: Seconds
maxLanternSecs = oneDayInSecs -- A lantern burns for 24 hours on a pint of fuel.


maxLogSize :: FileOffset
maxLogSize = 1000000


maxMobRmDescLen :: Int
maxMobRmDescLen = 80


maxNameLen,    minNameLen                        :: Int
maxNameLenTxt, minNameLenTxt, minNameLenPlus1Txt :: Text
maxNameLen         = 12
maxNameLenTxt      = "twelve"
minNameLen         = 3
minNameLenTxt      = "three"
minNameLenPlus1Txt = "four"


maxPageLines, minPageLines :: Int
maxPageLines = 150
minPageLines = 8


maxPwLen, minPwLen :: Int
maxPwLen = 20
minPwLen = 6


maxSpiritSecs :: Seconds
maxSpiritSecs = fiveMinsInSecs


maxTempDescLen :: Int
maxTempDescLen = 200


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


potFeelDur :: Seconds
potFeelDur = 30


restServicePort :: Int
restServicePort = 7249


rmDescIndentAmt :: Int
rmDescIndentAmt = 2


rndmVectorLen :: Int
rndmVectorLen = 5


sacrificeBonusSecs :: Int
sacrificeBonusSecs = threeHrsInSecs


sacrificeSecs :: Int
sacrificeSecs = 5


stdLinkNames :: [Text]
stdLinkNames = [ "n", "ne", "e", "se", "s", "sw", "w", "nw", "u", "d" ]


threadTblPurgerDelay :: Seconds
threadTblPurgerDelay = tenMinsInSecs


torchSecs :: Seconds
torchSecs = twoHrsInSecs


trashDumpPurgerDelay :: Seconds
trashDumpPurgerDelay = oneDayInSecs


ver :: Text
ver = T.pack . showVersion $ version


worldPersisterDelay :: Seconds
worldPersisterDelay = tenMinsInSecs


yous :: [Text]
yous = [ "you", "you'd", "you'll", "you're", "you's", "you've", "your", "yours", "yourself", "yourselves", "yous" ]
