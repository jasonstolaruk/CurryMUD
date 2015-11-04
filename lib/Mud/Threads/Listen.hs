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
import Mud.Threads.DbTblPurger
import Mud.Threads.Misc
import Mud.Threads.Regen
import Mud.Threads.Talk
import Mud.Threads.ThreadTblPurger
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
import Network (PortID(..), accept, listenOn, sClose)
import qualified Data.IntMap.Lazy as IM (map)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, putStrLn)
import System.IO (hClose)
import System.Time.Utils (renderSecs)


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.Listen"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Threads.Listen"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Listen"


-- ==================================================


threadListen :: MudStack ()
threadListen =
    (logNotice "threadListen" "server started." >> listen) `finally` (getUptime >>= saveUptime >> closeLogs >> bye)
  where
    bye = liftIO . T.putStrLn $ "Goodbye!\n"


saveUptime :: Int64 -> MudStack ()
saveUptime up@(T.pack . renderSecs . fromIntegral -> upTxt) =
    maybe (saveIt >> logIt) checkRecord =<< (fmap . fmap) getSum getRecordUptime
  where
    saveIt            = (liftIO . writeFile uptimeFile . show $ up) `catch` logIOEx "saveUptime saveIt"
    logIt             = logHelper "."
    checkRecord recUp = case up `compare` recUp of GT -> saveIt >> logRec
                                                   _  -> logIt
    logRec            = logHelper " - it's a new record!"
    logHelper         = logNotice "saveUptime logHelper" . ("CurryMUD was up for " <>) . (upTxt <>)


listen :: MudStack ()
listen = handle listenExHandler $ setThreadType Listen >> mIf initWorld proceed halt
  where
    proceed = do
        initialize
        logNotice "listen proceed" $ "listening for incoming connections on port " <> showText port <> "."
        sock <- liftIO . listenOn . PortNumber . fromIntegral $ port
        auxAsyncs <- mapM runAsync [ threadAdminChanTblPurger
                                   , threadAdminMsgTblPurger
                                   , threadChanTblPurger
                                   , threadQuestionChanTblPurger
                                   , threadTeleTblPurger
                                   , threadThreadTblPurger
                                   , threadWorldPersister ]
        (forever . loop $ sock) `finally` cleanUp auxAsyncs sock
    initialize = do
        startNpcRegens
        logNotice "listen initialize" "creating database tables."
        liftIO createDbTbls `catch` dbExHandler "listen initialize"
        sortAllInvs
        logInterfaces
    logInterfaces = liftIO mkInterfaceList >>= \ifList ->
        logNotice "listen listInterfaces" $ "server network interfaces: " <> ifList <> "."
    loop sock = let fn = "listen loop" in do
        (h, host@(T.pack -> host'), localPort) <- liftIO . accept $ sock
        logNotice fn . T.concat $ [ "connected to ", showText host, " on local port ", showText localPort, "." ]
        (withDbExHandler "listen loop" . isHostBanned . T.toLower . T.pack $ host) >>= \case
          Just (Any False) -> runTalkAsync h host
          _                -> do
              liftIO . T.hPutStr h . nlnl $ bannedMsg
              liftIO . hClose $ h
              let msg = T.concat [ "Connection from "
                                 , dblQuote host'
                                 , " refused "
                                 , parensQuote "host is banned"
                                 , "." ]
              bcastAdmins msg
              logNotice fn msg
    cleanUp auxAsyncs sock = do
        logNotice "listen cleanUp" "closing the socket."
        liftIO . sClose $ sock
        mapM_ throwWait auxAsyncs
        onEnv $ liftIO . atomically . void . takeTMVar . view (locks.persistLock)
    halt = liftIO . T.putStrLn $ loadWorldErrorMsg


listenExHandler :: SomeException -> MudStack ()
listenExHandler e = case fromException e of
  Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
  Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
  _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e


sortAllInvs :: MudStack ()
sortAllInvs = logNotice "sortAllInvs" "sorting all inventories." >> modifyState helper
  where
    helper ms = (ms & invTbl %~ IM.map (sortInv ms), ())
