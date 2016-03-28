{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Interp.Misc ( neverMind
                       , promptRetryYesNo
                       , resetInterp
                       , yesNoHelper ) where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Data.Text as T

import Control.Lens.Operators ((.~))
import Control.Monad (guard)
import Data.Text (Text)


neverMind :: Id -> MsgQueue -> MudStack ()
neverMind i mq = send mq (nlnl "Never mind.") >> sendDfltPrompt mq i >> resetInterp i


-----


promptRetryYesNo :: MsgQueue -> Cols -> MudStack ()
promptRetryYesNo mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]


-----


resetInterp :: Id -> MudStack ()
resetInterp i = tweak (mobTbl.ind i.interp .~ Nothing)


-----


yesNoHelper :: Text -> Maybe Bool
yesNoHelper (T.toLower -> a) = guard (()!# a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = return True
           | a `T.isPrefixOf` "no"  = return False
           | otherwise              = Nothing
