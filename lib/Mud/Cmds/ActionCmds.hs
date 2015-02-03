{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-} -- TODO: Confirm.

module Mud.Cmds.ActionCmds ( actionCmdSet
                           , actionCmds ) where

import Mud.Data.Misc

import qualified Data.Set as S (Set, fromList, foldr)

{-
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Data.Monoid ((<>))
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.Data.State.Util.STM
import Mud.Logging hiding (logPlaOut)
import Mud.NameResolution
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Data.Text as T
import qualified Mud.Logging as L (logPlaOut)
import qualified Mud.Util.Misc as U (patternMatchFail)
-}


{-
patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.ActionCmds"


-----


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.ActionCmds"
-}


-- ==================================================


actionCmdSet :: S.Set ActionCmd
actionCmdSet = S.fromList [ ActionCmd { actionCmdName   = "admire"
                                      , actionCmdAction = actionCmdAdmire
                                      , actionCmdType   = HasTarget
                                      , actionCmdRes    = "You admire Hanako." }
                          , ActionCmd { actionCmdName   = "blink"
                                      , actionCmdAction = actionCmdBlink
                                      , actionCmdType   = NoTarget
                                      , actionCmdRes    = "You blink." } ]


actionCmds :: [Cmd]
actionCmds = S.foldr helper [] actionCmdSet
  where
    helper ac = (Cmd { cmdName = actionCmdName ac, action = actionCmdAction ac, cmdDesc = "" } :)


-----


actionCmdAdmire :: Action
actionCmdAdmire _ = return ()


-----


actionCmdBlink :: Action
actionCmdBlink _ = return ()
