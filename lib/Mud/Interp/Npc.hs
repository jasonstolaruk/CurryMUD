{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Npc (npcInterp) where

import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Misc
import Mud.Util.Operators
import Mud.Util.Text

import Data.List (sort)
import qualified Data.Text as T


npcInterp :: Interp
npcInterp cn p@(ActionParams { plaId, plaMsgQueue }) = do
    getState >>= \ms -> maybe (send plaMsgQueue . nlnl $ "What?") (p |&|) =<< findAction plaId ms cn
    getState >>= \ms -> prompt plaMsgQueue . mkPrompt plaId $ ms


findAction :: Id -> MudState -> CmdName -> MudStack (Maybe Action)
findAction i ms (T.toLower -> cn) = findActionHelper cn . sort $ npcCmds ++ (mkNonStdRmLinkCmds . getNpcRm i $ ms)
