{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

{-
CurryMud - A Multi-User Dungeon by Jason Stolaruk.
Copyright (c) 2014, Jason Stolaruk and Detroit Labs LLC
jasonstolaruk (`at` gmail) . com
@JasonStolaruk
https://github.com/jasonstolaruk/CurryMUD
kickButt <$> take maxBound names
-}

module Main (main) where

import Mud.Data.State.StateInIORefT
import Mud.TheWorld.TheWorld
import Mud.Threads
import Mud.TopLvlDefs
import Mud.Util

import Control.Monad (void)
import Network (withSocketsDo)
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv, getProgName)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)


main :: IO ()
main = withSocketsDo $ do
    setCurrentDirectory mudDir
    welcome
    initMudState >>= void . runStateInIORefT listenWrapper


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . nl . T.concat $ [ "Hello, ", T.pack un, ". Welcome to ", dblQuote mn, " ver ", ver, "." ]
  where
    whatsMyName = getProgName >>= \mn ->
        return (if mn == "<interactive>" then "Y U NO COMPILE ME?" else T.pack mn)
