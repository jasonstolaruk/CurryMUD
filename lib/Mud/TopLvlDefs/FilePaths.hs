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


bugLogFile, errorLogFile, loggingExLogFile, noticeLogFile, profanityLogFile, typoLogFile :: FilePath
bugLogFile       = logDir </> "bug"                      <.> "log"
errorLogFile     = logDir </> "error"                    <.> "log"
loggingExLogFile = logDir </> "logging thread exception" <.> "log"
noticeLogFile    = logDir </> "notice"                   <.> "log"
profanityLogFile = logDir </> "profanity"                <.> "log"
typoLogFile      = logDir </> "typo"                     <.> "log"


-- ==================================================
-- Misc. files:


aboutFile, cowbyeFile, motdFile, uptimeFile :: FilePath
aboutFile  = miscDir </> "about"
cowbyeFile = miscDir </> "cowbye"
motdFile   = miscDir </> "motd"
uptimeFile = mudDir  </> "uptime"


-- ==================================================
-- Dictionaries:


wordsFile, propNamesFile :: Maybe FilePath
wordsFile     = Just $ drive : "usr" </> "share" </> "dict" </> "words"
propNamesFile = Just $ drive : "usr" </> "share" </> "dict" </> "propernames"


profanitiesFile :: FilePath
profanitiesFile = miscDir </> "profanities"
