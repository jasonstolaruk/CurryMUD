{-# LANGUAGE ViewPatterns #-}

module Mud.Cmds.Util.CmdPrefixes where

import Mud.Data.State.MudData
import Mud.TopLvlDefs.Chars

import Data.Monoid ((<>))
import qualified Data.Text as T


prefixAdminCmd :: T.Text -> CmdName
prefixAdminCmd = prefixCmd adminCmdChar


prefixDebugCmd :: T.Text -> CmdName
prefixDebugCmd = prefixCmd debugCmdChar


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd (T.singleton -> prefix) cn = prefix <> cn
