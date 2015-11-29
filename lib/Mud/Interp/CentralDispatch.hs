{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Admin
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Interp.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Operators

import Data.List (sort)
import qualified Data.Text as T


centralDispatch :: Interp
centralDispatch = dispatch findAction


findAction :: Id -> MudState -> CmdName -> MudStack (Maybe Action)
findAction i ms (T.toLower -> cn) = findActionHelper cn $ let ia = getPlaFlag IsAdmin . getPla i $ ms
                                                          in sort . concat $ [ plaCmds
                                                                             , mkNonStdRmLinkCmds . getMobRm i $ ms
                                                                             , ia            |?| adminCmds
                                                                             , ia && isDebug |?| debugCmds ]
