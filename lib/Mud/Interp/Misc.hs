{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Interp.Misc where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Output
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Data.Text as T

import Control.Monad (guard)
import Data.Text (Text)


promptRetryYesNo :: MsgQueue -> Cols -> MudStack ()
promptRetryYesNo mq cols =
    wrapSendPrompt mq cols . T.concat $ [ "Please answer ", dblQuote "yes", " or ", dblQuote "no", "." ]


yesNoHelper :: Text -> Maybe Bool
yesNoHelper (T.toLower -> a) = guard (()!# a) >> helper
  where
    helper | a `T.isPrefixOf` "yes" = return True
           | a `T.isPrefixOf` "no"  = return False
           | otherwise              = Nothing
