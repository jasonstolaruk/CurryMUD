{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}

module Mud.Interp.Misc where

import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Pla
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Util.Text hiding (none)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (none)
import Control.Monad (when)
import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Text (Text)


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Misc"


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
    let ri           = getRmId i ms
        cmds'        = sort $ cmds ++ mkNonStdRmLinkCmds (getRm ri ms)
        helper       = cmdAction . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds' ]
        maybeHookAct = maybe Nothing f . lookupHooks i ms $ cn
        f hooks      | cn `notElem` map cmdName cmds = Just . mkActionForAdHocCmdHook ri . head $ hooks
                     | otherwise                     = Nothing
    in return . onNothing helper $ maybeHookAct
  where
    onNothing x Nothing = x
    onNothing _ just    = just


mkActionForAdHocCmdHook :: Id -> Hook -> Action
mkActionForAdHocCmdHook ri h@Hook { .. } = Action f True
  where
    f p@(LowerNub' i as) = genericAction p helper hookName
      where
        helper v ms
          | getRmId i ms /= ri        = (ms, (pure sorryAlteredRm,   [], []))
          | none (`elem` triggers) as = (ms, (pure sorryCmdNotFound, [], []))
          | otherwise                 =
              let (_, (ms', toSelfs, bs, logMsgs)) = getHookFun hookName ms i h v (as, (ms, [], [], []))
              in (ms', (toSelfs, bs, logMsgs))
    f p = patternMatchFail hookName [ showText p ]
