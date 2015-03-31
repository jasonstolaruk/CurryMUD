{-# LANGUAGE OverloadedStrings #-}

{-
CurryMud - A Multi-User Dungeon by Jason Stolaruk.
Copyright (c) 2015, Jason Stolaruk and Detroit Labs LLC
currymud (`at` gmail) . com
@JasonStolaruk
https://github.com/jasonstolaruk/CurryMUD
kickButt <$> take maxBound names
-}

module Main (main) where

import Mud.Data.Misc
import Mud.Misc.Threads
import Mud.TheWorld.TheWorld
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
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
    mapM_ (createDirectoryIfMissing False) [ logDir, persistDir ]
    welcome
    runReaderT listenWrapper =<< initMudData DoLog


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . nl . T.concat $ [ "Hello, ", T.pack un, ". Welcome to ", dblQuote mn, " ver ", ver, "." ]
  where
    whatsMyName = getProgName >>= \mn ->
        return (mn == "<interactive>" ? "Y U NO COMPILE ME?" :? T.pack mn)
