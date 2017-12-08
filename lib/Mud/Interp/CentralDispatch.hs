{-# LANGUAGE ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import           Mud.Cmds.Admin
import           Mud.Cmds.Debug
import           Mud.Cmds.Pla
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Interp.Dispatch
import           Mud.Util.Operators

import           GHC.Stack (HasCallStack)
import qualified Data.Text as T

centralDispatch :: HasCallStack => Interp
centralDispatch = dispatch findAction

findAction :: HasCallStack => FindActionFun
findAction i ms (T.toLower -> cn) = findActionHelper i ms cn . mkCmds =<< getServerSettings
  where
    mkCmds s = let (spirit, admin) = ((,) <$> isSpirit <*> isAdmin) . getPla i $ ms
               in spirit ? spiritCmds :? concat [ plaCmds
                                                , admin                   |?| adminCmds
                                                , admin && settingDebug s |?| debugCmds ]
