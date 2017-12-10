{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Mud.Interp.Pause (pause) where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Misc
import Mud.Misc.ANSI
import Mud.Util.Quoting

import Data.Maybe (fromMaybe)

-- ==================================================

pause :: Id -> MsgQueue -> Maybe Fun -> MudStack ()
pause i mq mf = sequence_ [ promptPause, setInterp i . Just . interpPause $ mf ]
  where
    promptPause = sendPrompt mq . colorWith pagerPromptColor . spaced . bracketQuote . spaced $ txt
    txt         = "Enter a blank line to continue..."

interpPause :: Maybe Fun -> Interp
interpPause mf _ ActionParams { myId, plaMsgQueue } =
    fromMaybe (sendDfltPrompt plaMsgQueue myId >> resetInterp myId) mf
