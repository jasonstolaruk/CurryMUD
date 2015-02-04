{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-} -- TODO: Confirm.

module Mud.Cmds.ActionCmds ( actionCmdSet
                           , actionCmds ) where

import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.Util.Misc
import qualified Mud.Logging as L (logPlaOut)

import Data.List (delete)
import Data.Monoid ((<>))
import qualified Data.Set as S (Set, fromList, foldr)
import qualified Data.Text as T

{-
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Pla
import Mud.NameResolution
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)
-}


{-
patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.ActionCmds"
-}


-----


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.ActionCmds"


-- ==================================================


actionCmdSet :: S.Set ActionCmd
actionCmdSet = S.fromList [ ActionCmd { actionCmdName   = "admire"
                                      , actionCmdAction = actionCmdAdmire
                                      , actionCmdType   = HasTarget "You admire Hanako." }
                          , ActionCmd { actionCmdName   = "blink"
                                      , actionCmdAction = actionCmdBlink
                                      , actionCmdType   = Versatile "You blink." "You blink at Hanako." } ]


actionCmds :: [Cmd]
actionCmds = S.foldr helper [] actionCmdSet
  where
    helper ac = (Cmd { cmdName = actionCmdName ac, action = actionCmdAction ac, cmdDesc = "" } :)


-----


actionCmdAdmire :: Action
actionCmdAdmire _ = return ()


-----


actionCmdBlink :: Action
actionCmdBlink (NoArgs'' i) = readWSTMVar >>= \ws ->
    let (d, _, _, _, _) = mkCapStdDesig i ws
        toSelfMsg       = "You blink."
        toSelfBrdcst    = (nlnl toSelfMsg, [i])
        toOthersMsg     = serialize d <> " blinks."
        toOthersBrdcst  = (nlnl toOthersMsg, i `delete` pcIds d)
    in logPlaOut "actionCmdBlink" i [toSelfMsg] >> bcast (toSelfBrdcst : [toOthersBrdcst])
actionCmdBlink _ = return () -- TODO
