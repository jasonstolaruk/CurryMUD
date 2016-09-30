{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Util.CmdPrefixes where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.Chars

import Data.Text (Text)
import qualified Data.Text as T


prefixAdminCmd :: Text -> CmdName
prefixAdminCmd = prefixCmd adminCmdChar


prefixDebugCmd :: Text -> CmdName
prefixDebugCmd = prefixCmd debugCmdChar


prefixCmd :: Char -> CmdName -> Text
prefixCmd prefix cn = T.cons prefix cn
