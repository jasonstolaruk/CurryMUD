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
import Mud.StateDataTypes
import Mud.StateInIORefT
import Mud.TopLvlDefs
import Mud.Util

import Control.Concurrent.STM.TMVar (newTMVarIO)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Data.Text.Strict.Lens (packed)
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv, getProgName)
import qualified Data.IntMap.Lazy as IM (empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)


main :: IO ()
main = do
    setCurrentDirectory mudDir
    welcome
    initMudState >>= void . runStateInIORefT serverWrapper


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . T.concat $ [ "Hello, ", un^.packed, ". Welcome to ", dblQuote mn, " ver ", ver, ".", nlt ]
  where
    whatsMyName = getProgName >>= \mn ->
        return (if mn == "<interactive>" then "Y U NO COMPILE ME?" else mn^.packed)


initMudState :: IO MudState
initMudState = newTMVarIO ws >>= \wsTMVar -> return (MudState wsTMVar nws)
  where
    ws  = WorldState IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty
    nws = NonWorldState (LogServices Nothing Nothing) IM.empty IM.empty
