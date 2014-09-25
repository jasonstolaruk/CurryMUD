{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Copyright 2014 Jason Stolaruk and Detroit Labs LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-
CurryMud - A Multi-User Dungeon by Jason Stolaruk.
jasonstolaruk (`at` gmail) . com
@JasonStolaruk
https://github.com/jasonstolaruk/CurryMUD
kickButt <$> take maxBound names
-}

module Main (main) where

import Mud.Cmds
import Mud.StateInIORefT
import Mud.TheWorld
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
    initMudState >>= void . runStateInIORefT topLvlWrapper


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . T.concat $ [ "Hello, ", T.pack un, ". Welcome to ", dblQuote mn, " ver ", ver, ".\n" ]
  where
    whatsMyName = getProgName >>= \mn ->
        return (if mn == "<interactive>" then "Y U NO COMPILE ME?" else T.pack mn)
