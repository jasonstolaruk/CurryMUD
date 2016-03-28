{-# LANGUAGE MultiWayIf, NamedFieldPuns, OverloadedStrings, TupleSections #-}

module Mud.Interp.Dispatch where

import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Util.Text hiding (none)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (view)
import Control.Monad (when)
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Text (Text)


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Dispatch"


-- ==================================================


type FindActionFun = Id -> MudState -> CmdName -> MudStack (Maybe Action)


dispatch :: FindActionFun -> Interp
dispatch f cn p@ActionParams { myId, plaMsgQueue } = getState >>= \ms -> maybe notFound found =<< f myId ms cn
  where
    notFound                = send plaMsgQueue . nlnl $ sorryCmdNotFound
    found (Action actFun b) = do
        actFun p
        ms <- getState
        when (b && isNothing (getInterp myId ms)) . sendDfltPrompt plaMsgQueue $ myId


-----


findActionHelper :: Id -> MudState -> CmdName -> [Cmd] -> MudStack (Maybe Action)
findActionHelper i ms cn cmds =
    let r     = getMobRm i ms
        ras   = view rmActions r
        cmds' = sort $ cmds ++ mkNonStdRmLinkCmds r
    in return $ case [ ra | ra <- ras, cn == rmActionCmdName ra ] of
      []   -> cmdAction . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds' ]
      [ra] -> Just . Action (getRmActionFun (rmActionFunName ra) ms) $ True
      xs   -> patternMatchFail "findActionHelper" [ showText xs ]
