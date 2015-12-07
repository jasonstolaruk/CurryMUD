{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Mud.Interp.Misc where

import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Util.Text

import Control.Monad (when)
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


findActionHelper :: CmdName -> [Cmd] -> MudStack (Maybe Action)
findActionHelper cn cmds = return $ cmdAction . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds ]
