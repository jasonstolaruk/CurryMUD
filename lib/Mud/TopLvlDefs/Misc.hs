{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Misc ( acl
                           , aggregateCoinNames
                           , alertMsgTriggers
                           , allCoinNames
                           , aop
                           , biodegDelay
                           , biodegSecs
                           , bonusDelay
                           , breadMouths
                           , coinFullNames
                           , coinNames
                           , corpsePlaceholder
                           , dbTblPurgerDelay
                           , dfltZoom
                           , enc
                           , enc's
                           , etc
                           , fruitMouths
                           , initPickPts
                           , isDebug
                           , isEKGing
                           , isZBackDoor
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
                           , minNameLenTxt
                           , minPageLines
                           , minPwLen
                           , noOfDbTblRecsToPurge
                           , noOfLogFiles
                           , noOfPersistedWorlds
                           , noOfTitles
                           , port
                           , rmDescIndentAmt
                           , rndmVectorLen
                           , sacrificeBonusSecs
                           , sacrificeSecs
                           , stdLinkNames
                           , threadTblPurgerDelay
                           , trashDumpPurgerDelay
                           , ver
                           , worldPersisterDelay
                           , yous ) where

import           Mud.Data.State.MudData
import           Mud.TopLvlDefs.Chars
import           Mud.Util.Quoting
import           Paths_curry_mud

import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Version (showVersion)
import           Mud.TopLvlDefs.Seconds
import qualified Data.Text as T
import           System.Posix.Types (FileOffset)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


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


breadMouths :: Mouthfuls
breadMouths = 60


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


fruitMouths :: Mouthfuls
fruitMouths = 5


initPickPts :: Int
initPickPts = 200


isDebug :: Bool
isDebug = True


isEKGing :: Bool
isEKGing = False


isZBackDoor :: Bool
isZBackDoor = True


likewise :: Bool
likewise = not otherwise


logRotationDelay :: Seconds
logRotationDelay = oneHrInSecs


maxBonuses :: Int
maxBonuses = 10


maxChanNameLen :: Int
maxChanNameLen = 12


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


maxLogSize :: FileOffset
maxLogSize = 1000000


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
threadTblPurgerDelay = fiveMinsInSecs


trashDumpPurgerDelay :: Seconds
trashDumpPurgerDelay = sixHrsInSecs


ver :: Text
ver = T.pack . showVersion $ version


worldPersisterDelay :: Seconds
worldPersisterDelay = tenMinsInSecs


yous :: [Text]
yous = [ "you", "you'd", "you'll", "you're", "you's", "you've", "your", "yours", "yourself", "yourselves", "yous" ]
