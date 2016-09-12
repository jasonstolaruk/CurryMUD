{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Admin
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Interp.Dispatch
import Mud.TopLvlDefs.Misc
import Mud.Util.Operators

import qualified Data.Text as T


centralDispatch :: Interp
centralDispatch = dispatch findAction


findAction :: FindActionFun
findAction i ms (T.toLower -> cn) = findActionHelper i ms cn $ let p = getPla i ms in if isSpirit p
  then spiritCmds
  else let ia = isAdmin p
       in concat [ plaCmds
                 , ia            |?| adminCmds
                 , ia && isDebug |?| debugCmds ]
