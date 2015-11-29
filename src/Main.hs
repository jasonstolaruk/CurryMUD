{-# LANGUAGE OverloadedStrings #-}

{-
CurryMud - A Multi-User Dungeon by Jason Stolaruk.
Copyright (c) 2015, Jason Stolaruk and Detroit Labs LLC
currymud (`at` gmail) . com
https://github.com/jasonstolaruk/CurryMUD
In development since 2013-10.
-}

module Main (main) where

import Mud.Data.Misc
import Mud.TheWorld.TheWorld
import Mud.Threads.Listen
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Monad.Reader (runReaderT)
import Data.Monoid ((<>))
import Network (withSocketsDo)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, setCurrentDirectory)
import System.Environment (getEnv, getProgName)


main :: IO ()
main = withSocketsDo . mIf (not <$> doesDirectoryExist mudDir) stop $ go
  where
    stop = T.putStrLn $ "The " <> showText mudDir <> " directory does not exist; aborting."
    go   = do
        setCurrentDirectory mudDir
        mapM_ (createDirectoryIfMissing False) [ dbDir, logDir, persistDir ]
        welcome
        runReaderT threadListen =<< initMudData DoLog


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- what'sMyName
    T.putStrLn . nl . T.concat $ [ "Hello, ", T.pack un, ". Welcome to ", dblQuote mn, " ver ", ver, "." ]
  where
    what'sMyName = getProgName >>= \n -> return (n == "<interactive>" ? "Y U NO COMPILE ME?" :? T.pack n)
