{-# LANGUAGE OverloadedStrings #-}

{-
CurryMud - A Multi-User Dungeon by Jason Stolaruk.
Copyright (c) 2015, Jason Stolaruk and Detroit Labs LLC
currymud (`at` gmail) . com
@JasonStolaruk
https://github.com/jasonstolaruk/CurryMUD
kickButt <$> take maxBound names -- Welcome to the playground!
-}

module Main (main) where

import Mud.Data.Misc
import Mud.TheWorld.TheWorld
import Mud.Threads.Listen
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Monad.Reader (runReaderT)
import Network (withSocketsDo)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.Environment (getEnv, getProgName)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)


main :: IO ()
main = withSocketsDo $ do
    setCurrentDirectory mudDir
    mapM_ (createDirectoryIfMissing False) [ adminHelpCmdsDir
                                           , adminHelpTopicsDir
                                           , databaseDir
                                           , helpDir
                                           , logDir
                                           , persistDir
                                           , plaHelpCmdsDir
                                           , plaHelpTopicsDir ]
    welcome
    runReaderT threadListen =<< initMudData DoLog


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- what'sMyName
    T.putStrLn . nl . T.concat $ [ "Hello, ", T.pack un, ". Welcome to ", dblQuote mn, " ver ", ver, "." ]
  where
    what'sMyName = getProgName >>= \n ->
        return (n == "<interactive>" ? "Y U NO COMPILE ME?" :? T.pack n)
