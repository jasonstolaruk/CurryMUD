{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Mud.Interp.Misc where

import Mud.Cmds.Pla
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Util.Text

import Control.Monad (when)
import Data.List (sort)
import Data.Maybe (isNothing)


type FindActionFun = Id -> MudState -> CmdName -> MudStack (Maybe Action)


dispatch :: FindActionFun -> Interp
dispatch f cn p@ActionParams { myId, plaMsgQueue } = getState >>= \ms -> maybe notFound found =<< f myId ms cn
  where
    notFound                = send plaMsgQueue . nlnl $ "What?"
    found (Action actFun b) = do
        actFun p
        ms <- getState
        when (b && isNothing (getInterp myId ms)) . sendDfltPrompt plaMsgQueue $ myId


-----


-- TODO: Continue from here.
findActionHelper :: Id -> MudState -> CmdName -> [Cmd] -> MudStack (Maybe Action)
findActionHelper i ms cn cmds =
    let ri       = getRmId i ms
        cmds'    = sort $ cmds ++ mkNonStdRmLinkCmds (getRm ri ms)
        hookActs = case lookupHooks i ms cn of
          Nothing    -> Nothing
          Just hooks | cn `notElem` map cmdName cmds -> Just . mkActionForAdHocCmdHook i ri . head $ hooks
                     | otherwise                     -> Nothing
    in return $ case hookActs of
      Nothing  -> cmdAction . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds' ]
      Just act -> Just act


mkActionForAdHocCmdHook :: Id -> Id -> Hook -> Action
mkActionForAdHocCmdHook i ri Hook { hookName } = Action f True
  where
    f p = genericAction p helper hookName
    helper _ ms | getRmId i ms /= ri = (ms, (undefined, [], []))
                | otherwise = undefined
