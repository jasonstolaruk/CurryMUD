{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Cmds.Wiz
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.CentralDispatch"


-- ==================================================


centralDispatch :: Interp
centralDispatch cn p@(WithArgs i mq _ _) = do
    findAction i cn >>= maybe sorry (\act -> act p)
    prompt mq ">"
  where
    sorry = send mq . nlnl $ "What?"
centralDispatch cn p = patternMatchFail "centralDispatch" [ cn, showText p ]


findAction :: Id -> CmdName -> MudStack (Maybe Action)
findAction i (T.toLower -> cn) = do
    r  <- getPCRm     i
    iw <- getPlaIsWiz i
    let cmds = mkCmdListWithNonStdRmLinks r              ++
               (if iw            then wizCmds   else []) ++
               (if iw && isDebug then debugCmds else [])
    maybe (return Nothing)
          (\fn -> return . Just . findActionForFullName fn $ cmds)
          (findFullNameForAbbrev cn [ cmdName cmd | cmd <- cmds ])
  where
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)
