{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Listen (threadListen) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.Database
import Mud.Misc.Logging hiding (logExMsg, logIOEx, logNotice)
import Mud.TheWorld.TheWorld
import Mud.Threads.Biodegrader
import Mud.Threads.CorpseDecomposer
import Mud.Threads.DbTblPurger
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Threads.NpcServer
import Mud.Threads.Regen
import Mud.Threads.RmFuns
import Mud.Threads.Talk
import Mud.Threads.ThreadTblPurger
import Mud.Threads.TrashDumpPurger
import Mud.Threads.WorldPersister
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logNotice)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (takeTMVar)
import Control.Exception (AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, finally, handle)
import Control.Lens (view)
import Control.Lens.Operators ((%~), (&))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Monoid ((<>), Any(..), getSum)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Network (PortID(..), accept, listenOn, sClose)
import qualified Data.IntMap.Lazy as IM (map)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, putStrLn)
import System.IO (hClose)
import System.Time.Utils (renderSecs)


logExMsg :: Text -> Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.Listen"


logIOEx :: Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Threads.Listen"


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Listen"


-- ==================================================


threadListen :: HasCallStack => MudStack ()
threadListen = a `finally` b
  where
    a = logNotice "threadListen" "server started." >> listen
    b = sequence_ [ getUptime >>= saveUptime, closeLogs, liftIO . T.putStrLn . nl $ "Goodbye!" ]


saveUptime :: HasCallStack => Int64 -> MudStack ()
saveUptime up@(T.pack . renderSecs . fromIntegral -> upTxt) =
    maybe (saveIt >> logIt) checkRecord =<< getSum `fmap2` getRecordUptime
  where
    saveIt            = liftIO saveHelper `catch` logIOEx "saveUptime saveIt"
    saveHelper        = flip writeFile (show up) =<< mkMudFilePath uptimeFileFun
    logIt             = logHelper "."
    checkRecord recUp = case up `compare` recUp of GT -> saveIt >> logRec
                                                   _  -> logIt
    logRec            = logHelper " - it's a new record!"
    logHelper         = logNotice "saveUptime logHelper" . ("CurryMUD was up for " <>) . (upTxt <>)


listen :: HasCallStack => MudStack ()
listen = handle listenExHandler $ setThreadType Listen >> mIf initWorld proceed halt
  where
    proceed = do initialize
                 logNotice "listen proceed" . prd $ "listening for incoming connections on port " <> showText port
                 sock      <- liftIO . listenOn . PortNumber . fromIntegral $ port
                 auxAsyncs <- mapM runAsync [ threadAdminChanTblPurger
                                            , threadAdminMsgTblPurger
                                            , threadChanTblPurger
                                            , threadQuestionChanTblPurger
                                            , threadTeleTblPurger
                                            , threadThreadTblPurger
                                            , threadTrashDumpPurger
                                            , threadWorldPersister ]
                 (forever . loop $ sock) `finally` cleanUp auxAsyncs sock
    initialize = do logNotice "listen initialize" "creating database tables."
                    liftIO createDbTbls `catch` dbExHandler "listen initialize"
                    startNpcServers
                    startNpcDigesters
                    startNpcRegens
                    restartCorpseDecomps
                    massRestartPausedEffects
                    startRmFuns
                    startBiodegraders
                    sortAllInvs
                    logInterfaces
    logInterfaces = liftIO mkInterfaceList >>= \ifList ->
        logNotice "listen listInterfaces" . prd $ "server network interfaces: " <> ifList
    loop sock = let fn = "listen loop" in (liftIO . accept $ sock) >>= \(h, host@(T.pack -> host'), localPort) -> do
        logNotice fn . T.concat $ [ "connected to ", showText host, " on local port ", showText localPort, "." ]
        (withDbExHandler "listen loop" . isHostBanned . T.toLower . T.pack $ host) >>= \case
          Just (Any False) -> runTalkAsync h host
          _                -> do
              liftIO . T.hPutStr h . nlnl $ bannedMsg
              liftIO . hClose $ h
              let msg = T.concat [ "Connection from ", dblQuote host', " refused ", parensQuote "host is banned", "." ]
              bcastAdmins msg
              logNotice fn . uncapitalize $ msg
    cleanUp auxAsyncs sock = do logNotice "listen cleanUp" "closing the socket."
                                liftIO . sClose $ sock
                                mapM_ throwWait auxAsyncs
                                onEnv $ liftIO . atomically . void . takeTMVar . view (locks.persistLock)
    halt = liftIO . T.putStrLn $ loadWorldErrorMsg


listenExHandler :: SomeException -> MudStack ()
listenExHandler e = case fromException e of
  Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
  Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
  _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e >> liftIO printPanicMsg


sortAllInvs :: HasCallStack => MudStack ()
sortAllInvs = logNotice "sortAllInvs" "sorting all inventories." >> tweak (\ms -> ms & invTbl %~ IM.map (sortInv ms))
