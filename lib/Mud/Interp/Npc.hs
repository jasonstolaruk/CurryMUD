{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Npc (npcInterp) where

import Mud.Cmds.Pla
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Interp.Misc

import Data.List (sort)
import qualified Data.Text as T


npcInterp :: Interp
npcInterp = dispatch findAction


findAction :: FindActionFun
findAction i ms (T.toLower -> cn) = findActionHelper cn . sort $ npcCmds ++ (mkNonStdRmLinkCmds . getMobRm i $ ms)
