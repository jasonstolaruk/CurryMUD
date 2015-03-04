{-# LANGUAGE MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.CentralDispatch (centralDispatch) where

import Mud.Cmds.Admin
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Text

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Lens.Operators ((^.))
import Control.Monad ((>=>), when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.Maybe (isNothing)
import qualified Data.Text as T


centralDispatch :: Interp
centralDispatch cn p@(ActionParams { plaId, plaMsgQueue }) = ask >>= liftIO . atomically . helperSTM >>= \(pcTbl, plaTbl, rt) -> do
    cn |$| findAction plaId pcTbl plaTbl rt >=> maybe sorry (\act -> act p)
    when (plaIsDfltPrompt plaTbl) $ prompt plaMsgQueue dfltPrompt
  where
    helperSTM md = (,,) <$> readTVar (md^.pcTblTVar)
                        <*> readTVar (md^.plaTblTVar)
                        <*> readTVar (md^.rmTblTVar)
    sorry              = send plaMsgQueue . nlnl $ "What?"
    plaIsDfltPrompt pt = isNothing $ (pt ! plaId)^.interp


findAction :: Id -> PCTbl -> PlaTbl -> RmTbl -> CmdName -> MudStack (Maybe Action)
findAction i pcTbl plaTbl rt (T.toLower -> cn) = helper mkCmdList
  where
    helper cmds = maybe (return Nothing)
                        (\fn -> return . Just . findActionForFullName fn $ cmds)
                        (findFullNameForAbbrev cn [ cmdName cmd | cmd <- cmds ])
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)
    mkCmdList = let ri = (pcTbl ! i)^.rmId
                    r  = rt ! ri
                    ia = getPlaFlag IsAdmin $ plaTbl ! i
                in mkCmdListWithNonStdRmLinks r ++ (ia |?| adminCmds) ++ (ia && isDebug |?| debugCmds)
