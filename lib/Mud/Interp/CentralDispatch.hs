{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Admin
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Output
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Text

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.List (sort)
import Data.Maybe (isNothing)
import qualified Data.Text as T


centralDispatch :: Interp
centralDispatch cn p@(ActionParams { plaId, plaMsgQueue }) = getState >>= \ms -> do
    maybe (send plaMsgQueue . nlnl $ "What?") (\act -> act p) =<< findAction ms cn
    when (isNothin . getInterp i $ ms) . prompt plaMsgQueue $ dfltPrompt


findAction :: Id -> PCTbl -> PlaTbl -> RmTbl -> CmdName -> MudStack (Maybe Action)
findAction i pcTbl plaTbl rt (T.toLower -> cn) = helper mkCmdList
  where
    helper cmds = maybe (return Nothing)
                        (\fn -> return . Just . findActionForFullName fn $ cmds)
                        (findFullNameForAbbrev cn [ cmdName cmd | cmd <- cmds ])
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)
    mkCmdList = let ri = (pcTbl ! i)^.rmId
                    r  = rt ! ri
                    ia = getPlaFlag IsAdmin $ plaTbl ! i
                in mkCmdListWithNonStdRmLinks r ++ (ia |?| adminCmds) ++ (ia && isDebug |?| debugCmds)


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks (view rmLinks -> rls) = sort $ plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) =
    Cmd { cmdName = cn, cmdPriorityAbbrev = Nothing, cmdFullName = cn, action = go cn, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> T.Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _linkDir
                                               NonStdLink { .. } -> _linkName
