{-# LANGUAGE LambdaCase, TupleSections #-}

module Mud.Threads.NpcServer ( runNpcServerAsync
                             , startNpcServers
                             , stopNpcServers
                             , threadNpcServer ) where

import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Interp.Npc
import Mud.Threads.Misc
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent.Async (wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception.Lifted (catch)
import Control.Lens (at, to)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.NpcServer"


-- ==================================================


runNpcServerAsync :: Id -> MudStack ()
runNpcServerAsync i = do
    npcMq <- liftIO newTQueueIO
    a     <- runAsync . threadNpcServer i $ npcMq
    modifyState $ (, ()) . (npcTbl.ind i .~ Npc npcMq a)


startNpcServers :: MudStack ()
startNpcServers =
    logNotice "startNpcServers" "starting NPC server threads." >> (mapM_ runNpcServerAsync  . findNpcIds =<< getState)


stopNpcServers :: MudStack ()
stopNpcServers =
    logNotice "stopNpcServers"  "stopping NPC server threads." >> (mapM_ stopWaitNpcServer  . findNpcIds =<< getState)


stopWaitNpcServer :: Id -> MudStack ()
stopWaitNpcServer i = helper |&| modifyState >=> \npc -> do
    npc^.npcMsgQueue   .to (liftIO . atomically . (`writeTQueue` StopNpcServer))
    npc^.npcServerAsync.to (liftIO . wait)
  where
    helper ms = let npc = ms^.npcTbl.ind i
                in (ms & npcTbl.at i .~ Nothing, npc)


-----


threadNpcServer :: Id -> NpcMsgQueue -> MudStack ()
threadNpcServer i npcMq = do
    setThreadType . NpcServer $ i
    loop `catch` threadExHandler "NPC server"
  where
    loop = npcMq |&| liftIO . atomically . readTQueue >=> \case
      ExternCmd mq cols msg -> handleExternCmd i mq cols msg >> loop
      StopNpcServer         -> unit -- TODO: Test.


handleExternCmd :: Id -> MsgQueue -> Cols -> T.Text -> MudStack ()
handleExternCmd i mq cols msg = msg |#| interpret . headTail . T.words
  where
    interpret (cn, as) = npcInterp cn . WithArgs i mq cols $ as
