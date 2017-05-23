{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.NpcServer ( runNpcServerAsync
                             , startNpcServers
                             , stopNpcServers
                             , stopWaitNpcServer
                             , threadNpcServer ) where

import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Interp.Npc
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Threads.Misc
import           Mud.Util.List
import           Mud.Util.Misc
import           Mud.Util.Operators

import           Control.Concurrent.Async (wait)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import           Control.Exception.Lifted (handle)
import           Control.Lens (at, to)
import           Control.Lens.Operators ((.~), (&), (^.))
import           Control.Monad ((>=>), unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import           Prelude hiding (pi)
import qualified Data.Text as T


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.NpcServer"


-- ==================================================


runNpcServerAsync :: HasCallStack => Id -> MudStack ()
runNpcServerAsync i = do npcMq <- liftIO newTQueueIO
                         a     <- runAsync . threadNpcServer i $ npcMq
                         tweak $ npcTbl.ind i .~ Npc npcMq a Nothing


startNpcServers :: HasCallStack => MudStack ()
startNpcServers =
    logNotice "startNpcServers" "starting NPC server threads." >> (mapM_ runNpcServerAsync  . findNpcIds =<< getState)


stopNpcServers :: HasCallStack => MudStack ()
stopNpcServers =
    logNotice "stopNpcServers"  "stopping NPC server threads." >> (mapM_ stopWaitNpcServer  . findNpcIds =<< getState)


stopWaitNpcServer :: HasCallStack => Id -> MudStack ()
stopWaitNpcServer i = helper |&| modifyState >=> \npc -> do
    npc^.npcMsgQueue   .to (liftIO . atomically . (`writeTQueue` StopNpcServer))
    npc^.npcServerAsync.to (liftIO . wait)
  where
    helper ms = let npc = ms^.npcTbl.ind i
                in (ms & npcTbl.at i .~ Nothing, npc)


-----


threadNpcServer :: HasCallStack => Id -> NpcMsgQueue -> MudStack ()
threadNpcServer i npcMq = handle (threadExHandler (Just i) "NPC server") $ setThreadType (NpcServer i) >> loop
  where
    loop = npcMq |&| liftIO . atomically . readTQueue >=> \case
      ExternCmd mq cols msg -> handleExternCmd i mq cols msg >> loop
      StopNpcServer         -> unit


handleExternCmd :: HasCallStack => Id -> MsgQueue -> Cols -> Text -> MudStack ()
handleExternCmd i mq cols msg = getState >>= \ms ->
    let (cn, as)      = ()# msg ? ("", []) :? (headTail . T.words $ msg)
        mkWithArgs i' = WithArgs i' mq cols as
        -----
        notPossessed  = helper toNpcInterp
        helper dflt   = maybe dflt (toOther i) . getInterp i $ ms
        toOther i'    = ((cn, mkWithArgs i') |&|) . uncurry
        toNpcInterp   = unless (()# msg) . npcInterp cn . mkWithArgs $ i
        -----
        possessed pi  = helper . maybe toNpcInterp (toOther pi) . getInterp pi $ ms
    in maybe notPossessed possessed . getPossessor i $ ms
