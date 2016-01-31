{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.FilePaths where

import System.Directory (getHomeDirectory)
import System.FilePath ((<.>), (</>), pathSeparator)
import System.IO.Unsafe (unsafePerformIO)


drive :: Char
drive = pathSeparator


-- ==================================================
-- Directories:


mudDir :: FilePath
mudDir = unsafePerformIO getHomeDirectory </> "CurryMUD"


dbDir, logDir, persistDir, resDir :: FilePath
dbDir      = mudDir </> "db"
logDir     = mudDir </> "logs"
persistDir = mudDir </> "persist"
resDir     = mudDir </> "res"


helpDir, miscDir, titleDir :: FilePath
helpDir  = resDir </> "help"
miscDir  = resDir </> "misc"
titleDir = resDir </> "titles"


adminHelpDir, plaHelpDir :: FilePath
adminHelpDir = helpDir </> "admin"
plaHelpDir   = helpDir </> "pla"


plaHelpCmdsDir, plaHelpTopicsDir :: FilePath
plaHelpCmdsDir   = plaHelpDir </> "cmds"
plaHelpTopicsDir = plaHelpDir </> "topics"


adminHelpCmdsDir, adminHelpTopicsDir :: FilePath
adminHelpCmdsDir   = adminHelpDir </> "cmds"
adminHelpTopicsDir = adminHelpDir </> "topics"


-- ==================================================
-- Log files:


bugLogFile, errorLogFile, loggingExLogFile, noticeLogFile, typoLogFile :: FilePath
bugLogFile       = logDir </> "bug"                      <.> "log"
errorLogFile     = logDir </> "error"                    <.> "log"
loggingExLogFile = logDir </> "logging thread exception" <.> "log"
noticeLogFile    = logDir </> "notice"                   <.> "log"
typoLogFile      = logDir </> "typo"                     <.> "log"


-- ==================================================
-- Persistence files:


armTblFile, chanTblFile, clothTblFile, coinsTblFile, conTblFile, entTblFile, eqTblFile, hostTblFile, invTblFile, mobTblFile, objTblFile, pcTblFile, plaTblFile, rmTblFile, rmTeleNameTblFile, rndmNamesMstrTblFile, teleLinkMstrTblFile, typeTblFile, wpnTblFile, writableTblFile :: FilePath
armTblFile           = "armTbl.json"
chanTblFile          = "chanTbl.json"
clothTblFile         = "clothTbl.json"
coinsTblFile         = "coinsTbl.json"
conTblFile           = "conTbl.json"
entTblFile           = "entTbl.json"
eqTblFile            = "eqTbl.json"
hostTblFile          = "hostTbl.json"
invTblFile           = "invTbl.json"
mobTblFile           = "mobTbl.json"
objTblFile           = "objTbl.json"
pcTblFile            = "pcTbl.json"
plaTblFile           = "plaTbl.json"
rmTblFile            = "rmTbl.json"
rmTeleNameTblFile    = "rmTeleNameTbl.json"
rndmNamesMstrTblFile = "rndmNamesMstrTbl.json"
teleLinkMstrTblFile  = "teleLinkMstrTbl.json"
typeTblFile          = "typeTbl.json"
wpnTblFile           = "wpnTbl.json"
writableTblFile      = "writableTbl.json"


-- ==================================================
-- The database file:


dbFile :: FilePath
dbFile = dbDir </> "CurryMud" <.> "sqlite3"


-- ==================================================
-- Misc. files:


aboutFile, cowbyeFile, motdFile, rndmNamesFile, uptimeFile :: FilePath
aboutFile     = miscDir </> "about"
cowbyeFile    = miscDir </> "cowbye"
motdFile      = miscDir </> "motd"
rndmNamesFile = miscDir </> "randomnames"
uptimeFile    = mudDir  </> "uptime"


-- ==================================================
-- Dictionaries:


propNamesFile, wordsFile :: Maybe FilePath
propNamesFile = Just $ miscDir </> "propernames"
wordsFile     = Just $ drive : "usr" </> "share" </> "dict" </> "words"


profanitiesFile :: FilePath
profanitiesFile = miscDir </> "profanities"
