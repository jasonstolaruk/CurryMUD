{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Listen (threadListen) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Pla
import           Mud.Cmds.Util.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Locks
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Misc.Database
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logNotice)
import           Mud.Misc.Logging hiding (logExMsg, logIOEx, logNotice)
import           Mud.TheWorld.TheWorld
import           Mud.Threads.Biodegrader
import           Mud.Threads.CorpseDecomposer
import           Mud.Threads.CurryTime
import           Mud.Threads.DbTblPurger
import           Mud.Threads.Digester
import           Mud.Threads.Effect
import           Mud.Threads.LightTimer
import           Mud.Threads.Misc
import           Mud.Threads.NpcServer
import           Mud.Threads.Regen
import           Mud.Threads.RmFuns
import           Mud.Threads.Talk
import           Mud.Threads.ThreadTblPurger
import           Mud.Threads.TrashDumpPurger
import           Mud.Threads.WorldPersister
import           Mud.TopLvlDefs.FilePaths
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar (takeTMVar)
import           Control.Exception (AsyncException(..), IOException, SomeException, fromException)
import           Control.Exception.Lifted (catch, finally, handle)
import           Control.Lens.Operators ((&), (%~))
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Int (Int64)
import qualified Data.IntMap.Strict as IM (map)
import           Data.Monoid ((<>), Any(..), getSum)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, putStrLn)
import           GHC.Stack (HasCallStack)
import Network.Socket
    ( defaultHints,
      getAddrInfo,
      accept,
      close,
      socket,
      hostAddressToTuple,
      socketToHandle,
      AddrInfo(addrSocketType, addrFamily, addrProtocol, addrFlags),
      AddrInfoFlag(AI_NUMERICSERV, AI_NUMERICHOST),
      SocketType(Stream) )
import qualified Network.Socket as NS
import           System.IO (hClose)
import           System.Time.Utils (renderSecs)
import System.IO (IOMode(..))
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
    a = logNotice "threadListen" "server started." >> Mud.Threads.Listen.listen
    b = sequence_ [ saveUptime =<< getUptime, closeLogs, liftIO . T.putStrLn . nl $ "Goodbye!" ]

listen :: HasCallStack => MudStack ()
listen = handle listenExHandler $ setThreadType Listen >> mIf initWorld proceed halt -- Keep this exception handler here.
  where
    proceed = do initialize
                 let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
                 addr:_ <- liftIO $ getAddrInfo (Just hints) (Just "0.0.0.0") (Just $ show port)
                 sock' <- liftIO $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                 logNotice "listen proceed" . prd $ "listening for incoming connections on port " <> showTxt port
                 _      <- liftIO $ NS.listen sock' 1024
                 let sock = sock'
                 auxAsyncs <- mapM runAsync $ mkDbTblPurgerFuns ++ [ threadCurryTime
                                                                   , threadThreadTblPurger
                                                                   , threadTrashDumpPurger
                                                                   , threadWorldPersister ]
                 (forever . loop $ sock) `finally` cleanUp auxAsyncs sock
    initialize = do logNotice "listen initialize" "creating database tables."
                    withDbExHandler_ "listen initialize" createDbTbls
                    initPropNamesTbl
                    initWordsTbl
                    startNpcServers
                    startNpcDigesters
                    startNpcRegens
                    restartCorpseDecomps
                    massRestartPausedEffects
                    startRmFuns
                    startBiodegraders
                    massRestartNpcLightTimers
                    sortAllInvs
                    logInterfaces
    logInterfaces = liftIO mkInterfaceList >>= \ifList ->
        logNotice "listen listInterfaces" . prd $ "server network interfaces: " <> ifList
    loop sock = let fn = "listen loop" in liftIO (accept sock) >>= \(s@( (flip socketToHandle) ReadWriteMode -> h), (NS.SockAddrInet localPort host@(T.pack . (\(a,b,c,d) -> show a ++ show b ++ show c ++ show d) . hostAddressToTuple -> host'))) -> do
        logNotice fn . T.concat $ [ "connected to ", showTxt host, " on local port ", showTxt localPort, "." ]
        (withDbExHandler "listen loop" . isHostBanned . T.toLower $ host') >>= \case
          Just (Any False) -> do
            h'' <- liftIO h
            runTalkAsync h'' (T.unpack host')
          _                -> do
            h'' <- liftIO h
            liftIO . T.hPutStr h'' . nlnl $ bannedMsg
            liftIO . hClose $ h''
            let msg = "Connection from " <> dblQuote host' <> " refused (host is banned)."
            bcastAdmins msg
            logNotice fn . uncapitalize $ msg
    cleanUp auxAsyncs sock = do logNotice "listen cleanUp" "closing the socket."
                                liftIO . close $ sock
                                mapM_ throwDeathWait auxAsyncs
                                liftIO . atomically . void . takeTMVar =<< getLock persistLock
    halt = liftIO . T.putStrLn $ loadWorldErrorMsg

listenExHandler :: HasCallStack => SomeException -> MudStack ()
listenExHandler e = let fn = "listenExHandler" in case fromException e of
  Just UserInterrupt          -> logNotice fn "exiting on user interrupt."
  _ | isCancellingEx e -> logNotice fn "thread killed."
    | otherwise        -> logExMsg  fn "exception caught on listen thread" e >> liftIO printPanicMsg


sortAllInvs :: HasCallStack => MudStack ()
sortAllInvs = logNotice "sortAllInvs" "sorting all inventories." >> tweak (\ms -> ms & invTbl %~ IM.map (sortInv ms))

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
    logHelper         = logNotice "saveUptime" . ("CurryMUD was up for " <>) . (upTxt <>)
