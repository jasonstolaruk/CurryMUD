{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.FilePaths where

import System.Directory (getHomeDirectory)
import System.FilePath ((<.>), (</>), pathSeparator)


type HomeDir     = FilePath
type FilePathFun = HomeDir -> FilePath


mkMudFilePath :: FilePathFun -> IO FilePath
mkMudFilePath = flip fmap getHomeDirectory


under :: FilePathFun -> FilePath -> HomeDir -> FilePath
under f fp hd = f hd </> fp


-- ==================================================
-- Directories:


mudDir :: FilePathFun
mudDir = (</> "CurryMUD")


dbDir, logDir, persistDir, resDir :: FilePathFun
dbDir      = under mudDir "db"
logDir     = under mudDir "logs"
persistDir = under mudDir "persist"
resDir     = under mudDir "res"


helpDir, miscDir, raceDir, titleDir :: FilePathFun
helpDir  = under resDir "help"
miscDir  = under resDir "misc"
raceDir  = under resDir "races"
titleDir = under resDir "titles"


adminHelpDir, plaHelpDir :: FilePathFun
adminHelpDir = under helpDir "admin"
plaHelpDir   = under helpDir "pla"


plaHelpCmdsDir, plaHelpTopicsDir :: FilePathFun
plaHelpCmdsDir   = under plaHelpDir "cmds"
plaHelpTopicsDir = under plaHelpDir "topics"


adminHelpCmdsDir, adminHelpTopicsDir :: FilePathFun
adminHelpCmdsDir   = under adminHelpDir "cmds"
adminHelpTopicsDir = under adminHelpDir "topics"


-- ==================================================
-- Log files:


bugLogFile, errorLogFile, loggingExLogFile, noticeLogFile, typoLogFile :: FilePathFun
bugLogFile       = under logDir $ "bug"                      <.> "log"
errorLogFile     = under logDir $ "error"                    <.> "log"
loggingExLogFile = under logDir $ "logging thread exception" <.> "log"
noticeLogFile    = under logDir $ "notice"                   <.> "log"
typoLogFile      = under logDir $ "typo"                     <.> "log"


-- ==================================================
-- Persistence files:


armTblFile, chanTblFile, clothTblFile, coinsTblFile, conTblFile, corpseTblFile, entTblFile, eqTblFile, foodTblFile, hostTblFile, invTblFile, mobTblFile, objTblFile, pausedEffectsTblFile, pcTblFile, plaTblFile, rmTblFile, rmTeleNameTblFile, rndmNamesMstrTblFile, teleLinkMstrTblFile, typeTblFile, vesselTblFile, wpnTblFile, writableTblFile :: FilePath
armTblFile           = "armTbl"           <.> "json"
chanTblFile          = "chanTbl"          <.> "json"
clothTblFile         = "clothTbl"         <.> "json"
coinsTblFile         = "coinsTbl"         <.> "json"
conTblFile           = "conTbl"           <.> "json"
corpseTblFile        = "corpseTbl"        <.> "json"
entTblFile           = "entTbl"           <.> "json"
eqTblFile            = "eqTbl"            <.> "json"
foodTblFile          = "foodTbl"          <.> "json"
hostTblFile          = "hostTbl"          <.> "json"
invTblFile           = "invTbl"           <.> "json"
mobTblFile           = "mobTbl"           <.> "json"
objTblFile           = "objTbl"           <.> "json"
pausedEffectsTblFile = "pausedEffectsTbl" <.> "json"
pcTblFile            = "pcTbl"            <.> "json"
plaTblFile           = "plaTbl"           <.> "json"
rmTblFile            = "rmTbl"            <.> "json"
rmTeleNameTblFile    = "rmTeleNameTbl"    <.> "json"
rndmNamesMstrTblFile = "rndmNamesMstrTbl" <.> "json"
teleLinkMstrTblFile  = "teleLinkMstrTbl"  <.> "json"
typeTblFile          = "typeTbl"          <.> "json"
vesselTblFile        = "vesselTbl"        <.> "json"
wpnTblFile           = "wpnTbl"           <.> "json"
writableTblFile      = "writableTbl"      <.> "json"


-- ==================================================
-- The database file:


dbFile :: FilePathFun
dbFile = under dbDir $ "CurryMud" <.> "sqlite3"


-- ==================================================
-- Misc. files:


aboutFile, cowbyeFile, descRulesFile, motdFile, rndmNamesFile, uptimeFile :: FilePathFun
aboutFile     = under miscDir "about"
cowbyeFile    = under miscDir "cowbye"
descRulesFile = under miscDir "descRules"
motdFile      = under miscDir "motd"
rndmNamesFile = under miscDir "randomnames"
uptimeFile    = under mudDir  "uptime"


-- ==================================================
-- Dictionaries:


propNamesFile :: HomeDir -> Maybe FilePath
propNamesFile = Just . under miscDir "propernames"


wordsFile :: Maybe FilePath
wordsFile = Just $ drive : "usr" </> "share" </> "dict" </> "words"
  where
    drive = pathSeparator


profanitiesFile :: FilePathFun
profanitiesFile = under miscDir "profanities"
