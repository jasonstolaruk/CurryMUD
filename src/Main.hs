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

import Control.Concurrent.STM.TVar (newTVarIO)
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
    initMudState >>= void . runStateInIORefT gameWrapper


welcome :: IO ()
welcome = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . T.concat $ [ "\nHello, ", un^.packed, ". Welcome to ", dblQuote mn, " ver ", ver, ".\n" ]
  where
    whatsMyName = getProgName >>= \mn ->
        return (if mn == "<interactive>" then "Y U NO COMPILE ME?" else mn^.packed)


initMudState :: IO MudState
initMudState = do
    a <- newTVarIO IM.empty
    b <- newTVarIO IM.empty
    c <- newTVarIO IM.empty
    d <- newTVarIO IM.empty
    e <- newTVarIO IM.empty
    f <- newTVarIO IM.empty
    g <- newTVarIO IM.empty
    h <- newTVarIO IM.empty
    i <- newTVarIO IM.empty
    j <- newTVarIO IM.empty
    k <- newTVarIO IM.empty
    l <- newTVarIO IM.empty
    m <- newTVarIO IM.empty
    n <- newTVarIO IM.empty
    return (MudState (WorldState a b c d e f g h i j k l m)  (NonWorldState (LogServices Nothing Nothing) n))
