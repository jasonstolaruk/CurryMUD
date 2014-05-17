{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  CurryMud - A Multi-User Dungeon written in Haskell.
  By Jason Stolaruk.
  jasonstolaruk `at` (gmail . com)
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

import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Data.Text.Strict.Lens (packed)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv, getProgName)


main :: IO ()
main = do
    setCurrentDirectory mudDir
    welcome
    void . runStateInIORefT gameWrapper $ initMudState


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . T.concat $ [ "\nHello, ", un^.packed, ". Welcome to ", dblQuote mn, " ver ", ver, ".\n" ]
  where
    whatsMyName = getProgName >>= \mn ->
        return (if mn == "<interactive>" then "Y U NO COMPILE ME?" else mn^.packed)
