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
findAction i ms (T.toLower -> cn) = findActionHelper i ms cn cmds
  where
    cmds = let (spirit, admin) = ((,) <$> isSpirit <*> isAdmin) . getPla i $ ms
           in spirit ? spiritCmds :? concat [ plaCmds
                                            , admin            |?| adminCmds
                                            , admin && isDebug |?| debugCmds ]
