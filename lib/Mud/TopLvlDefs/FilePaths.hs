module Mud.TopLvlDefs.FilePaths where

import System.Environment (getEnv)
import System.FilePath ((<.>), (</>), pathSeparator)
import System.IO.Unsafe (unsafePerformIO)


drive :: Char
drive = pathSeparator


-- ==================================================
-- Directories:


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME" in home </> "CurryMUD"


logDir, resDir :: FilePath
logDir = mudDir </> "logs"
resDir = mudDir </> "res"


helpDir, miscDir, persistDir, titleDir :: FilePath
helpDir    = resDir </> "help"
miscDir    = resDir </> "misc"
persistDir = resDir </> "persist"
titleDir   = resDir </> "titles"


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


bugLogFile, errorLogFile, loggingExLogFile, noticeLogFile, profanityLogFile, typoLogFile :: FilePath
bugLogFile       = logDir </> "bug"                      <.> "log"
errorLogFile     = logDir </> "error"                    <.> "log"
loggingExLogFile = logDir </> "logging thread exception" <.> "log"
noticeLogFile    = logDir </> "notice"                   <.> "log"
profanityLogFile = logDir </> "profanity"                <.> "log"
typoLogFile      = logDir </> "typo"                     <.> "log"


-- ==================================================
-- Persistence files:


armTblFile, clothTblFile, coinsTblFile, conTblFile, entTblFile, eqTblFile, hostTblFile, invTblFile, mobTblFile, objTblFile, pcTblFile, plaTblFile, rmTblFile, rmTeleNameTblFile, typeTblFile, wpnTblFile :: FilePath
armTblFile        = "armTbl.json"
clothTblFile      = "clothTbl.json"
coinsTblFile      = "coinsTbl.json"
conTblFile        = "conTbl.json"
entTblFile        = "entTbl.json"
eqTblFile         = "eqTbl.json"
hostTblFile       = "hostTbl.json"
invTblFile        = "invTbl.json"
mobTblFile        = "mobTbl.json"
objTblFile        = "objTbl.json"
pcTblFile         = "pcTbl.json"
plaTblFile        = "plaTbl.json"
rmTblFile         = "rmTbl.json"
rmTeleNameTblFile = "rmTeleNameTbl.json"
typeTblFile       = "typeTbl.json"
wpnTblFile        = "wpnTbl.json"


-- ==================================================
-- Misc. files:


aboutFile, cowbyeFile, motdFile, uptimeFile :: FilePath
aboutFile   = miscDir </> "about"
cowbyeFile  = miscDir </> "cowbye"
motdFile    = miscDir </> "motd"
uptimeFile  = mudDir  </> "uptime"


-- ==================================================
-- Dictionaries:


propNamesFile, wordsFile :: Maybe FilePath
propNamesFile = Just $ miscDir </> "propernames"
wordsFile     = Just $ drive : "usr" </> "share" </> "dict" </> "words"


profanitiesFile :: FilePath
profanitiesFile = miscDir </> "profanities"
