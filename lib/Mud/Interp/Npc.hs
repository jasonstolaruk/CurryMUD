{-# LANGUAGE ViewPatterns #-}

module Mud.Interp.Npc (npcInterp) where

import           Mud.Cmds.Pla
import           Mud.Data.State.MudData
import           Mud.Interp.Dispatch

import qualified Data.Text as T


npcInterp :: Interp
npcInterp = dispatch findAction


findAction :: FindActionFun
findAction i ms (T.toLower -> cn) = findActionHelper i ms cn npcCmds
