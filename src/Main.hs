{-# LANGUAGE OverloadedStrings #-}

{-
CurryMud - A Multi-User Dungeon by Jason Stolaruk.
Copyright (c) 2013-2016, Jason Stolaruk and Detroit Labs LLC.
Version 0.1.0.0.
currymud (`at` gmail) . com
https://github.com/jasonstolaruk/CurryMUD
-}

module Main (main) where

import Mud.Data.Misc
import Mud.TheWorld.TheWorld
import Mud.Threads.Listen
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Monad ((<=<), forM_, void, when)
import Control.Monad.Reader (runReaderT)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, setCurrentDirectory)
import System.Environment (getEnv, getProgName)
import System.Remote.Monitoring (forkServer)


main :: IO ()
main = mkMudFilePath mudDirFun >>= \dir ->
    let stop = T.putStrLn $ "The " <> dblQuote (T.pack dir) <> " directory does not exist; aborting."
        go   = do
            when (isDebug && isEKGing) startEKG
            setCurrentDirectory dir
            forM_ [ dbDirFun, logDirFun, persistDirFun ] $ createDirectoryIfMissing False <=< mkMudFilePath
            welcome
            runReaderT threadListen =<< initMudData DoLog
        startEKG = do -- "curry +RTS -T" to enable GC statistics collection in the run-time system.
            void . forkServer "localhost" $ 8000
            T.putStrLn . prd $ "EKG server started " <> parensQuote "http://localhost:8000"
    in mIf (not <$> doesDirectoryExist dir) stop go


welcome :: IO ()
welcome = (,) <$> getEnv "USER" <*> what'sMyName >>= \(un, mn) ->
    T.putStrLn . T.concat $ [ "Hello, ", T.pack un, "! Welcome to ", mn, " ver ", ver, "." ]
  where
    what'sMyName = mIf ((== "<interactive>") <$> getProgName)
      (return . dblQuote $ "Y U NO COMPILE ME?")
      (return "CurryMUD")
