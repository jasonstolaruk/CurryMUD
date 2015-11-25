{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Admin
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Operators
import Mud.Util.Text

import Control.Monad (when)
import Data.List (sort)
import Data.Maybe (isNothing)
import qualified Data.Text as T


centralDispatch :: Interp
centralDispatch cn p@(ActionParams { myId, plaMsgQueue }) = do
    getState >>= \ms -> maybe (send plaMsgQueue . nlnl $ "What?") (p |&|) =<< findAction myId ms cn
    getState >>= \ms -> when (isNothing . getInterp myId $ ms) . prompt plaMsgQueue . mkPrompt myId $ ms


findAction :: Id -> MudState -> CmdName -> MudStack (Maybe Action)
findAction i ms (T.toLower -> cn) = findActionHelper cn $ let ia = getPlaFlag IsAdmin . getPla i $ ms
                                                          in sort . concat $ [ plaCmds
                                                                             , mkNonStdRmLinkCmds . getPCRm i $ ms
                                                                             , ia            |?| adminCmds
                                                                             , ia && isDebug |?| debugCmds ]
