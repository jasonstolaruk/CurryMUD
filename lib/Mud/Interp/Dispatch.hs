{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}

module Mud.Interp.Dispatch where

import           Mud.Cmds.Pla
import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Util.Misc (PatternMatchFail)
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Text

import           Control.Lens (view)
import           Control.Monad (when)
import           Data.List (sort)
import           Data.Maybe (isNothing)
import           GHC.Stack (HasCallStack)


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Interp.Dispatch"


-- ==================================================


type FindActionFun = Id -> MudState -> CmdName -> MudStack (Maybe Action)


dispatch :: HasCallStack => FindActionFun -> Interp
dispatch f cn p@ActionParams { .. } = getState >>= \ms -> maybe notFound found =<< f myId ms cn
  where
    notFound                = sendCmdNotFound myId plaMsgQueue plaCols
    found (Action actFun b) = do actFun p
                                 ms <- getState
                                 when (b && isNothing (getInterp myId ms)) . sendDfltPrompt plaMsgQueue $ myId


-----


findActionHelper :: HasCallStack => Id -> MudState -> CmdName -> [Cmd] -> MudStack (Maybe Action)
findActionHelper i ms cn cmds =
    let r     = getMobRm i ms
        cmds' = sort . concat $ [ mkNonStdRmLinkCmds r -- Exit "in" should be prioritized over the "intro" cmd abbreviation.
                                , cmds
                                , mkRacialLangCmds i ms ]
    in return $ case [ ra | ra <- view rmActions r, cn == rmActionCmdName ra ] of
      []   -> cmdAction . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds' ]
      [ra] -> Just . Action (getRmActionFun (rmActionFunName ra) ms) $ True
      xs   -> pmf "findActionHelper" xs
