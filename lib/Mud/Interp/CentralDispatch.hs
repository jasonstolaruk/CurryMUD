{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Cmds.Wiz
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util
import Mud.TopLvlDefs.Misc
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.))
import Data.IntMap.Lazy ((!))
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
findAction i (T.toLower -> cn) = readWSTMVar >>= \ws ->
    readTMVarInNWS plaTblTMVar >>= \((! i) -> p) ->
        let (view rmId -> ri) = (ws^.pcTbl) ! i
            r                 = (ws^.rmTbl) ! ri
            cmds              = mkCmdListWithNonStdRmLinks r ++
                                (if p^.isWiz then wizCmds   else []) ++
                                (if isDebug  then debugCmds else [])
        in maybe (return Nothing)
                 (\fn -> return . Just . findActionForFullName fn $ cmds)
                 (findFullNameForAbbrev cn [ cmdName cmd | cmd <- cmds ])
  where
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)
