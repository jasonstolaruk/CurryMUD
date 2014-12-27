{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}

module Mud.TopLvlDefs.FilePaths where

import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)


-- ==================================================
-- Directories:


mudDir, logDir, resDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME" in home ++ "/CurryMUD/"
logDir = mudDir ++ "logs/"
resDir = mudDir ++ "res/"


helpDir, miscDir, titleDir :: FilePath
helpDir  = resDir ++ "help/"
miscDir  = resDir ++ "misc/"
titleDir = resDir ++ "titles/"


-- ==================================================
-- Log files:


errorLogFile, loggingExLogFile, noticeLogFile, profanityLogFile :: FilePath
errorLogFile     = logDir ++ "error.log"
loggingExLogFile = logDir ++ "logging thread exception.log"
noticeLogFile    = logDir ++ "notice.log"
profanityLogFile = logDir ++ "profanity.log"


-- ==================================================
-- Misc. files:


aboutFile, cowbyeFile, motdFile, uptimeFile :: FilePath
aboutFile  = miscDir ++ "about"
cowbyeFile = miscDir ++ "cowbye"
motdFile   = miscDir ++ "motd"
uptimeFile = mudDir  ++ "uptime"


-- ==================================================
-- Dictionaries:


wordsFile, propNamesFile :: Maybe FilePath
wordsFile     = Just "/usr/share/dict/words"
propNamesFile = Just "/usr/share/dict/propernames"


profanitiesFile :: FilePath
profanitiesFile = miscDir ++ "profanities"
