{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Server (threadServer) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.CentralDispatch
import Mud.Misc.ANSI
import Mud.Misc.Persist
import Mud.Threads.Misc
import Mud.TopLvlDefs.FilePaths
import Mud.Util.List
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text hiding (headTail)
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Async (wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (writeTMQueue)
import Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import Control.Exception.Lifted (catch)
import Control.Lens (view)
import Control.Lens.Operators ((^.))
import Control.Monad ((>=>), forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Monoid ((<>))
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, hPutStrLn, readFile)
import System.IO (Handle)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Server"


-- ==================================================


threadServer :: Handle -> Id -> MsgQueue -> TimerQueue -> MudStack ()
threadServer h i mq tq = sequence_ [ setThreadType . Server $ i, loop `catch` plaThreadExHandler "server" i ]
  where
    loop = mq |&| liftIO . atomically . readTQueue >=> \case
      Dropped        ->                                  sayonara
      FromClient msg -> handleFromClient i mq tq msg >> loop
      FromServer msg -> handleFromServer i h msg     >> loop
      InacBoot       -> sendInacBootMsg h            >> sayonara
      InacStop       -> stopTimerThread tq           >> loop
      MsgBoot msg    -> sendBootMsg h msg            >> sayonara
      Peeped  msg    -> (liftIO . T.hPutStr h $ msg) >> loop
      Prompt  p      -> sendPrompt i h p             >> loop
      Quit           -> cowbye h                     >> sayonara
      Shutdown       -> shutDown                     >> loop
      SilentBoot     ->                                 sayonara
    sayonara = sequence_ [ stopTimerThread tq, handleEgress i ]


handleFromClient :: Id -> MsgQueue -> TimerQueue -> T.Text -> MudStack ()
handleFromClient i mq tq (T.strip . stripControl . stripTelnet -> msg) = getState >>= \ms ->
    let p           = getPla i ms
        thruCentral = msg |#| uncurry (interpret p centralDispatch) . headTail . T.words
        thruOther f = uncurry (interpret p f) (()# msg ? ("", []) :? (headTail . T.words $ msg))
    in p^.interp |&| maybe thruCentral thruOther
  where
    interpret p f cn as = do
        forwardToPeepers i (p^.peepers) FromThePeeped msg
        liftIO . atomically . writeTMQueue tq $ ResetTimer
        f cn . WithArgs i mq (p^.columns) $ as


forwardToPeepers :: Id -> Inv -> ToOrFromThePeeped -> T.Text -> MudStack ()
forwardToPeepers i peeperIds toOrFrom msg = liftIO . atomically . helperSTM =<< getState
  where
    helperSTM ms = forM_ [ getMsgQueue peeperId ms | peeperId <- peeperIds ]
                         (`writeTQueue` (mkPeepedMsg . getSing i $ ms))
    mkPeepedMsg s = Peeped $ case toOrFrom of
      ToThePeeped   ->      T.concat $ toPeepedColor   : rest
      FromThePeeped -> nl . T.concat $ fromPeepedColor : rest
      where
        rest = [ " ", bracketQuote s, " ", dfltColor, " ", msg ]


handleFromServer :: Id -> Handle -> T.Text -> MudStack ()
handleFromServer i h msg = getState >>= \ms -> let peeps = getPeepers i ms in
    forwardToPeepers i peeps ToThePeeped msg >> (liftIO . T.hPutStr h $ msg)


sendInacBootMsg :: Handle -> MudStack ()
sendInacBootMsg h = liftIO . T.hPutStrLn h . nl . quoteWith' (bootMsgColor, dfltColor) $ inacBootMsg


sendBootMsg :: Handle -> T.Text -> MudStack ()
sendBootMsg h = liftIO . T.hPutStrLn h . nl . (<> dfltColor) . (bootMsgColor <>)


sendPrompt :: Id -> Handle -> T.Text -> MudStack ()
sendPrompt i h = handleFromServer i h . nl


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` fileIOExHandler "cowbye"
  where
    takeADump = T.hPutStrLn h =<< T.readFile cowbyeFile


shutDown :: MudStack ()
shutDown = do
    massMsg SilentBoot
    onEnv $ liftIO . void . forkIO . runReaderT commitSuicide
  where
    commitSuicide = do
        liftIO . mapM_ wait . M.elems . view talkAsyncTbl =<< getState
        logNotice "shutDown commitSuicide" "all players have been disconnected."
        persist
        logNotice "shutDown commitSuicide" "killing the listen thread."
        liftIO . killThread . getListenThreadId =<< getState
