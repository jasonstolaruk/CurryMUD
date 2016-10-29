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
import Mud.Misc.Database
import Mud.Data.State.Util.Output
import Mud.Interp.CentralDispatch
import Mud.Misc.ANSI
import Mud.Misc.Persist
import Mud.Threads.Act
import Mud.Threads.Biodegrader
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Threads.NpcServer
import Mud.Threads.Regen
import Mud.Threads.RmFuns
import Mud.TopLvlDefs.FilePaths
import Mud.Util.List
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text hiding (headTail)
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (killThread)
import Control.Concurrent.Async (wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (writeTMQueue)
import Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import Control.Exception.Lifted (catch)
import Control.Lens (view)
import Control.Lens.Operators ((^.))
import Control.Monad ((>=>), forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, hPutStrLn, readFile)
import System.IO (Handle, hFlush, hShow)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Server"


-- ==================================================


{-
CurryMUD doesn't send GA or EOR. Furthermore, prompts always end with a newline character.
This prompting and handle-flushing scheme (implemented below) produces an experience that looks the same on both Mudlet
and TinTin+++. Other options were tried (such as one-line prompts with GA), but this approach produces the most
consistency.
-}


data ToWhom = Plaに | Npcに


threadServer :: Handle -> Id -> MsgQueue -> TimerQueue -> MudStack ()
threadServer h i mq tq = sequence_ [ setThreadType . Server $ i
                                   , loop `catch` threadExHandler (Just i) "server" ]
  where
    loop = mq |&| liftIO . atomically . readTQueue >=> \case
      AsSelf     msg -> handleFromClient i mq tq True msg  >> loop
      BlankLine      -> handleBlankLine h                  >> loop
      Dropped        ->                                    sayonara
      FromClient msg -> handleFromClient i mq tq False msg >> loop
      FromServer msg -> handleFromServer i h Plaに msg     >> loop
      InacBoot       -> sendInacBootMsg h                  >> sayonara
      InacStop       -> stopTimer tq                       >> loop
      MsgBoot msg    -> sendBootMsg h msg                  >> sayonara
      Peeped  msg    -> (liftIO . T.hPutStr h $ msg)       >> loop
      Prompt p       -> promptHelper i h p                 >> loop
      Quit           -> cowbye h                           >> sayonara
      ShowHandle     -> handleShowHandle i h               >> loop
      Shutdown       -> shutDown                           >> loop
      SilentBoot     ->                                    sayonara
      ToNpc msg      -> handleFromServer i h Npcに msg     >> loop
    sayonara = sequence_ [ stopTimer tq, handleEgress i ]


handleBlankLine :: Handle -> MudStack ()
handleBlankLine h = liftIO $ T.hPutStr h theNl >> hFlush h


handleFromClient :: Id -> MsgQueue -> TimerQueue -> Bool -> Text -> MudStack ()
handleFromClient i mq tq isAsSelf msg | isTelnetTTypeResponse msg = go =<< tTypeHelper
                                      | otherwise                 = go msg
  where
    tTypeHelper :: MudStack Text
    tTypeHelper = let (ttype, msg') = parseTelnetTTypeResponse msg
                  in (,) <$> getState <*> liftIO mkTimestamp >>= \(ms, ts) -> do
                      let h = T.pack . getCurrHostName i $ ms
                      withDbExHandler_ "handleFromClient" . insertDbTblTType . TTypeRec ts h $ ttype
                      return msg'

    go :: Text -> MudStack ()
    go (T.strip . stripControl . stripTelnet -> msg') = getState >>= \ms ->
        let p                  = getPla i ms
            poss               = p^.possessing
            thruCentral        = msg' |#| interpret i p centralDispatch . headTail . T.words
            helper dflt        = maybe dflt thruOther . getInterp i $ ms
            thruOther f        = interpret (fromMaybe i poss) p f (()# msg' ? ("", []) :? (headTail . T.words $ msg'))
            forwardToNpc npcId = let npcMq = getNpcMsgQueue npcId ms
                                 in liftIO . atomically . writeTQueue npcMq . ExternCmd mq (p^.columns) $ msg'
        in isAsSelf ? thruCentral :? maybe (helper thruCentral) forwardToNpc poss
      where
        interpret asId p f (cn, as) = do
            forwardToPeepers i (p^.peepers) FromThePeeped msg'
            liftIO . atomically . writeTMQueue tq $ ResetTimer
            f cn . WithArgs asId mq (p^.columns) $ as


forwardToPeepers :: Id -> Inv -> ToOrFromThePeeped -> Text -> MudStack ()
forwardToPeepers i peeperIds toOrFrom msg = liftIO . atomically . helper =<< getState
  where
    helper ms     = forM_ [ getMsgQueue peeperId ms | peeperId <- peeperIds ] (`writeTQueue` (mkPeepedMsg . getSing i $ ms))
    mkPeepedMsg s = Peeped $ case toOrFrom of
      ToThePeeped   ->      T.concat $ toPeepedColor   : rest
      FromThePeeped -> nl . T.concat $ fromPeepedColor : rest
      where
        rest = [ spaced . bracketQuote $ s, dfltColor, " ", msg ]


handleFromServer :: Id -> Handle -> ToWhom -> Text -> MudStack ()
handleFromServer _ h Npcに msg = fromServerHelper h $ colorWith toNpcColor " " |<>| msg
handleFromServer i h Plaに msg = getState >>= \ms ->
    forwardToPeepers i (getPeepers i ms) ToThePeeped msg >> fromServerHelper h msg


fromServerHelper :: Handle -> Text -> MudStack ()
fromServerHelper h t = liftIO $ T.hPutStr h t >> hFlush h


sendInacBootMsg :: Handle -> MudStack ()
sendInacBootMsg h = liftIO . T.hPutStrLn h . nl . colorWith bootMsgColor $ inacBootMsg


sendBootMsg :: Handle -> Text -> MudStack ()
sendBootMsg h = liftIO . T.hPutStrLn h . nl . colorWith bootMsgColor


promptHelper :: Id -> Handle -> Text -> MudStack ()
promptHelper i h t = sequence_ [ handleFromServer i h Plaに . nl $ t, liftIO . hFlush $ h ]


handleShowHandle :: Id -> Handle -> MudStack ()
handleShowHandle i h = getMsgQueueColumns i <$> getState >>= \pair ->
    uncurry wrapSend1Nl pair . T.pack =<< liftIO (hShow h)


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` fileIOExHandler "cowbye"
  where
    takeADump = T.hPutStrLn h =<< T.readFile =<< mkMudFilePath cowbyeFileFun


shutDown :: MudStack ()
shutDown = do
    massMsg SilentBoot
    onNewThread commitSuicide
  where
    commitSuicide = do
        liftIO . mapM_ wait . M.elems . view talkAsyncTbl =<< getState
        logNotice "shutDown commitSuicide" "everyone has been disconnected."
        stopNpcActs
        stopBiodegraders
        stopRmFuns
        massPauseEffects
        stopNpcRegens
        stopNpcDigesters
        stopNpcServers
        persist
        logNotice "shutDown commitSuicide" "killing the listen thread."
        liftIO . killThread . getListenThreadId =<< getState
