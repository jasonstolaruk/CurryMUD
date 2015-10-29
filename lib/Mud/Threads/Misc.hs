{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, OverloadedStrings, ViewPatterns #-}

module Mud.Threads.Misc ( getUnusedId
                        , listenWrapper ) where

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
import Mud.Data.State.Util.Set
import Mud.Interp.CentralDispatch
import Mud.Interp.Login
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.Logging hiding (logExMsg, logIOEx, logNotice)
import Mud.Misc.Persist
import Mud.TheWorld.AdminZoneIds (iWelcome)
import Mud.TheWorld.TheWorld
import Mud.Threads.DbTblPurger
import Mud.Threads.InacTimer
import Mud.Threads.NewMisc
import Mud.Threads.Receive
import Mud.Threads.ThreadTblPurger
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text hiding (headTail)
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logNotice)

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, asyncThreadId, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (closeTMQueue, newTMQueueIO, writeTMQueue)
import Control.Concurrent.STM.TMVar (takeTMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, finally, handle, throwTo, try)
import Control.Lens (view, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad ((>=>), forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Bits (zeroBits)
import Data.Int (Int64)
import Data.List ((\\))
import Data.Monoid ((<>), Any(..), getSum)
import Data.Time (getCurrentTime)
import Network (HostName, PortID(..), accept, listenOn, sClose)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (keys, map)
import qualified Data.Map.Lazy as M (elems, empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, hPutStrLn, putStrLn, readFile)
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import System.Random (randomIO, randomRIO)
import System.Time.Utils (renderSecs)


-- TODO: Move everything out of this module, then rename "NewMisc" to "Misc".


default (Int)


-----


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Misc.Threads"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Misc.Threads"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Misc.Threads"


-- ==================================================
-- The "listen" thread:


listenWrapper :: MudStack ()
listenWrapper =
    let bye = liftIO . T.putStrLn $ "Goodbye!\n"
    in (logNotice "listenWrapper" "server started." >> listen) `finally` (getUptime >>= saveUptime >> closeLogs >> bye)


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
                                   , worldPersister ]
        (forever . loop $ sock) `finally` cleanUp auxAsyncs sock
    initialize = do
        logNotice "listen initialize" "creating database tables."
        liftIO createDbTbls `catch` dbExHandler "listen initialize"
        sortAllInvs
        logInterfaces
    logInterfaces = liftIO mkInterfaceList >>= \ifList ->
        logNotice "listen listInterfaces" $ "server network interfaces: " <> ifList <> "."
    runAsync f = onEnv $ liftIO . async . runReaderT f
    loop sock = let fn = "listen loop" in do
        (h, host@(T.pack -> host'), localPort) <- liftIO . accept $ sock
        logNotice fn . T.concat $ [ "connected to ", showText host, " on local port ", showText localPort, "." ]
        (withDbExHandler "listen loop" . isHostBanned . T.toLower . T.pack $ host) >>= \case
          Just (Any False) -> setTalkAsync =<< onEnv (liftIO . async . runReaderT (talk h host))
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
        mapM_ (liftIO . throwWait) auxAsyncs
        onEnv $ liftIO . atomically . void . takeTMVar . view (locks.persistLock)
    throwWait a = throwTo (asyncThreadId a) PlsDie >> (void . wait $ a)
    halt        = liftIO . T.putStrLn $ loadWorldErrorMsg


listenExHandler :: SomeException -> MudStack ()
listenExHandler e = case fromException e of
  Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
  Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
  _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e


sortAllInvs :: MudStack ()
sortAllInvs = logNotice "sortAllInvs" "sorting all inventories." >> modifyState helper
  where
    helper ms = (ms & invTbl %~ IM.map (sortInv ms), ())


-- ==================================================
-- The "world persister" thread:


worldPersister :: MudStack ()
worldPersister = handle (threadExHandler "world persister") $ do
    setThreadType WorldPersister
    logNotice "worldPersister" "world persister started."
    let loop = (liftIO . threadDelay $ worldPersisterDelay * 10 ^ 6) >> persist
    forever loop `catch` die "world persister"


-- ==================================================
-- "Talk" threads:


talk :: Handle -> HostName -> MudStack ()
talk h host = helper `finally` cleanUp
  where
    helper = do
        (mq, tq)           <- liftIO $ (,) <$> newTQueueIO <*> newTMQueueIO
        (i, dblQuote -> s) <- adHoc mq host
        setThreadType . Talk $ i
        handle (plaThreadExHandler "talk" i) $ onEnv $ \md -> do
            liftIO configBuffer
            dumpTitle mq
            prompt    mq "By what name are you known?"
            bcastAdmins $ "A new player has connected: " <> s <> "."
            logNotice "talk helper" $ "new PC name for incoming player: " <> s <> "."
            liftIO . void . forkIO . runReaderT (threadInacTimer i mq tq) $ md
            liftIO $ race_ (runReaderT (server        h i mq tq) md)
                           (runReaderT (threadReceive h i mq)    md)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = logNotice "talk cleanUp" ("closing the handle for " <> T.pack host <> ".") >> (liftIO . hClose $ h)


adHoc :: MsgQueue -> HostName -> MudStack (Id, Sing)
adHoc mq host = do
    (sexy, r) <- liftIO $ (,) <$> randomSex <*> randomRace
    ct        <- liftIO getCurrentTime
    modifyState $ \ms ->
        let i    = getUnusedId ms
            s    = showText r <> showText i
            e    = Ent { _entId    = i
                       , _entName  = Nothing
                       , _sing     = s
                       , _plur     = ""
                       , _entDesc  = capitalize $ mkThrPerPro sexy <> " is an ad-hoc player character."
                       , _entFlags = zeroBits }
            m    = Mob { _sex   = sexy
                       , _st    = 50
                       , _dx    = 50
                       , _iq    = 50
                       , _ht    = 50
                       , _maxHp = 100, _curHp = 100
                       , _maxMp = 100, _curMp = 100
                       , _maxPp = 100, _curPp = 100
                       , _maxFp = 100, _curFp = 100
                       , _xp    = 0
                       , _hand  = RHand }
            pc   = PC  { _rmId       = iWelcome
                       , _race       = r
                       , _introduced = []
                       , _linked     = [] }
            pla  = Pla { _currHostName = host
                       , _connectTime  = Just ct
                       , _plaFlags     = zeroBits
                       , _columns      = 80
                       , _pageLines    = 24
                       , _interp       = Just interpName
                       , _peepers      = []
                       , _peeping      = []
                       , _retainedMsgs = []
                       , _lastRmId     = Nothing }
            ms'  = ms  & coinsTbl        .ind i        .~ mempty
                       & entTbl          .ind i        .~ e
                       & eqTbl           .ind i        .~ M.empty
                       & invTbl          .ind i        .~ []
                       & invTbl          .ind iWelcome .~ getInv iWelcome ms ++ pure i
                       & mobTbl          .ind i        .~ m
                       & msgQueueTbl     .ind i        .~ mq
                       & pcTbl           .ind i        .~ pc
                       & plaTbl          .ind i        .~ pla
                       & rndmNamesMstrTbl.ind i        .~ M.empty
                       & teleLinkMstrTbl .ind i        .~ M.empty
                       & typeTbl         .ind i        .~ PCType
        in (ms', (i, s))


randomSex :: IO Sex
randomSex = ([ Male, Female ] !!) . fromEnum <$> (randomIO :: IO Bool)


randomRace :: IO Race
randomRace = randomIO


getUnusedId :: MudState -> Id
getUnusedId = views typeTbl (head . ([0..] \\) . IM.keys)


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO mkFilename >>= try . takeADump >>= eitherRet (fileIOExHandler "dumpTitle")
  where
    mkFilename   = ("title" ++) . show <$> randomRIO (1, noOfTitles)
    takeADump fn = send mq . nlPrefix . nl =<< (liftIO . T.readFile $ titleDir </> fn)


-- ==================================================
-- "Server" threads:


server :: Handle -> Id -> MsgQueue -> TimerQueue -> MudStack ()
server h i mq tq = sequence_ [ setThreadType . Server $ i, loop `catch` plaThreadExHandler "server" i ]
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


stopTimerThread :: TimerQueue -> MudStack ()
stopTimerThread = liftIO . atomically . closeTMQueue


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
