{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Admin
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.TopLvlDefs.Misc
import Mud.Util.Operators
import Mud.Util.Text

import Control.Lens (view)
import Control.Monad (when)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (isNothing)
import qualified Data.Text as T


centralDispatch :: Interp
centralDispatch cn p@(ActionParams { plaId, plaMsgQueue }) = do
    getState >>= \ms -> maybe (send plaMsgQueue . nlnl $ "What?") (p |$|) =<< findAction plaId ms cn
    getState >>= \ms -> when (isNothing . getInterp plaId $ ms) . prompt plaMsgQueue $ dfltPrompt


findAction :: Id -> MudState -> CmdName -> MudStack (Maybe Action)
findAction i ms (T.toLower -> cn) = helper mkCmdList
  where
    helper cmds = return $ action . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds ]
    mkCmdList = let ia = getPlaFlag IsAdmin . getPla i $ ms
                in mkCmdListWithNonStdRmLinks (getPCRm i ms) ++ (ia |?| adminCmds) ++ (ia && isDebug |?| debugCmds)


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks (view rmLinks -> rls) = sort $ plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) =
    Cmd { cmdName = cn, cmdPriorityAbbrev = Nothing, cmdFullName = cn, action = go cn, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> T.Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _linkDir
                                               NonStdLink { .. } -> _linkName
