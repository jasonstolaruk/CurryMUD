{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, OverloadedStrings, ViewPatterns #-}

module Mud.Threads ( allKeys
                   , getUnusedId
                   , listenWrapper) where

import Mud.ANSI
import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.StateInIORefT
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.Data.State.Util.STM
import Mud.Interp.CentralDispatch
import Mud.Interp.Login
import Mud.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import Mud.TheWorld.Ids
import Mud.TheWorld.TheWorld
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import qualified Mud.Logging as L (logExMsg, logIOEx, logNotice, logPla)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent.Async (async, asyncThreadId, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, finally, handle, throwTo, try)
import Control.Lens (at)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.=), (?~), (^.))
import Control.Monad (forM_, forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Bits (zeroBits)
import Data.IntMap.Lazy ((!))
import Data.List ((\\))
import Data.Monoid ((<>), mempty)
import Network (HostName, PortID(..), accept, listenOn, sClose)
import Prelude hiding (pi)
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import System.Random (newStdGen, randomR) -- TODO: Use mwc-random or tf-random. QC uses tf-random.
import System.Time.Utils (renderSecs)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Map.Lazy as M (elems, empty)
import qualified Data.Set as S (Set, fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine, hPutStr, hPutStrLn, readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)


default (Int)


-----


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Threads"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Threads"


-- ==================================================
-- The "listen" thread:


listenWrapper :: MudStack ()
listenWrapper = flip finally graceful $ do
    initLogging
    logNotice "listenWrapper initAndStart" "server started."
    listen


graceful :: MudStack ()
graceful = getUptime >>= saveUptime >> closeLogs


saveUptime :: Int -> MudStack ()
saveUptime ut@(T.pack . renderSecs . toInteger -> utTxt) = getRecordUptime >>= maybe (saveIt >> logIt) checkRecord
  where
    saveIt          = (liftIO . writeFile uptimeFile . show $ ut) `catch` logIOEx "saveUptime saveIt"
    logIt           = logHelper "."
    checkRecord rut = case ut `compare` rut of GT -> saveIt >> logRec
                                               _  -> logIt
    logRec          = logHelper " - it's a new record!"
    logHelper       = logNotice "saveUptime" . ("CurryMUD was up for " <>) . (utTxt <>)


listen :: MudStack ()
listen = handle listenExHandler $ do
    registerThread Listen
    statefulFork_ threadTblPurger
    initWorld
    loadDictFiles
    listInterfaces
    logNotice "listen" $ "listening for incoming connections on port " <> showText port <> "."
    sock <- liftIO . listenOn . PortNumber . fromIntegral $ port
    (forever . loop $ sock) `finally` cleanUp sock
  where
    listInterfaces = liftIO NI.getNetworkInterfaces >>= \ns ->
        let ifList = T.intercalate ", " [ bracketQuote . T.concat $ [ showText . NI.name $ n
                                                                    , ": "
                                                                    , showText . NI.ipv4 $ n ] | n <- ns ]
        in logNotice "listen listInterfaces" $ "server network interfaces: " <> ifList <> "."
    loop sock = do
        (h, host, localPort) <- liftIO . accept $ sock
        logNotice "listen loop" . T.concat $ [ "connected to "
                                             , showText host
                                             , " on local port "
                                             , showText localPort
                                             , "." ]
        a@(asyncThreadId -> ti) <- liftIO . async . void . runStateInIORefT (talk h host) =<< get
        modifyNWS talkAsyncTblTMVar $ \tat -> tat & at ti ?~ a
    cleanUp sock = logNotice "listen cleanUp" "closing the socket." >> (liftIO . sClose $ sock)


listenExHandler :: SomeException -> MudStack ()
listenExHandler e = case fromException e of
  Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
  Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
  _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e


registerThread :: ThreadType -> MudStack ()
registerThread threadType = liftIO myThreadId >>= \ti ->
    modifyNWS threadTblTMVar $ \tt -> tt & at ti ?~ threadType


loadDictFiles :: MudStack ()
loadDictFiles = (nonWorldState.dicts .=) =<< [ Dicts mWSet mPnSet | mWSet  <- loadDictFile wordsFile
                                                                  , mPnSet <- loadDictFile propNamesFile ]


loadDictFile :: Maybe FilePath -> MudStack (Maybe (S.Set T.Text))
loadDictFile = maybe (return Nothing) loadIt
  where
    loadIt fn@(dblQuote . T.pack -> fn') = do
      logNotice "loadDictFile" $ "loading dictionary " <> fn' <> "."
      let helper = Just . S.fromList . T.lines . T.toLower <$> (liftIO . T.readFile $ fn)
      helper `catch` (\e -> fileIOExHandler "loadDictFile" e >> return Nothing)


-- ==================================================
-- The "thread table purger" thread:


threadTblPurger :: MudStack ()
threadTblPurger = do
    registerThread ThreadTblPurger
    logNotice "threadTblPurger" "thread table purger thread started."
    let loop = (liftIO . threadDelay $ 10 ^ 6 * threadTblPurgerDelay) >> purgeThreadTbls
    forever loop `catch` threadTblPurgerExHandler


threadTblPurgerExHandler :: SomeException -> MudStack ()
threadTblPurgerExHandler e = do
    logExMsg "threadTblPurgerExHandler" "exception caught on thread table purger thread; rethrowing to listen thread" e
    liftIO . flip throwTo e =<< getListenThreadId


getListenThreadId :: MudStack ThreadId
getListenThreadId = reverseLookup Listen <$> readTMVarInNWS threadTblTMVar


-- ==================================================
-- "Talk" threads:


talk :: Handle -> HostName -> MudStack ()
talk h host = helper `finally` cleanUp
  where
    helper = do
        (mq, itq)          <- (,) <$> liftIO newTQueueIO <*> liftIO newTQueueIO
        (i, dblQuote -> s) <- adHoc mq host
        registerThread . Talk $ i
        handle (plaThreadExHandler "talk" i) $ readTMVarInNWS plaTblTMVar >>= \pt -> do
            logNotice "talk helper" $ "new PC name for incoming player: " <> s <> "."
            bcastAdmins pt $ "A new player has connected: " <> s <> "."
            liftIO configBuffer
            dumpTitle mq
            prompt    mq "By what name are you known?"
            state <- statefulFork . inacTimer i mq $ itq
            liftIO $ race_ (runStateInIORefT (server  h i mq itq) state)
                           (runStateInIORefT (receive h i mq)     state)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = logNotice "talk cleanUp" ("closing the handle for " <> T.pack host <> ".") >> (liftIO . hClose $ h)


adHoc :: MsgQueue -> HostName -> MudStack (Id, Sing)
adHoc mq host = do
    (wsTMVar, mqtTMVar, ptTMVar) <- (,,) <$> getWSTMVar       <*> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    (sexy, r)                    <- (,)  <$> liftIO randomSex <*> liftIO randomRace
    liftIO . atomically $ do
        (ws, mqt, pt) <- (,,) <$> takeTMVar wsTMVar <*> takeTMVar mqtTMVar <*> takeTMVar ptTMVar
        let i    = getUnusedId ws
            s    = showText r <> showText i
            e    = Ent { _entId    = i
                       , _entName  = Nothing
                       , _sing     = s
                       , _plur     = ""
                       , _entDesc  = capitalize $ mkThrPerPro sexy <> " is an ad-hoc player character."
                       , _entFlags = zeroBits }
            m    = Mob { _sex  = sexy
                       , _st   = 10
                       , _dx   = 10
                       , _iq   = 10
                       , _ht   = 10
                       , _hp   = 10
                       , _fp   = 10
                       , _xp   = 0
                       , _hand = RHand }
            pc   = PC  { _rmId       = iWelcome
                       , _race       = r
                       , _introduced = []
                       , _linked     = [] }
            pla  = Pla { _hostName  = host
                       , _plaFlags  = zeroBits
                       , _columns   = 80
                       , _pageLines = 24
                       , _interp    = Just interpName
                       , _peepers   = []
                       , _peeping   = [] }
            ws'  = ws  & typeTbl.at  i ?~ PCType
                       & entTbl.at   i ?~ e
                       & invTbl.at   i ?~ []
                       & coinsTbl.at i ?~ mempty
                       & eqTbl.at    i ?~ M.empty
                       & mobTbl.at   i ?~ m
                       & pcTbl.at    i ?~ pc
            ris  = sortInv ws' $ (ws^.invTbl) ! iWelcome ++ [i]
            mqt' = mqt & at i ?~ mq
            pt'  = pt  & at i ?~ pla
        sequence_ [ putTMVar wsTMVar $ ws' & invTbl.at iWelcome ?~ ris, putTMVar mqtTMVar mqt', putTMVar ptTMVar pt' ]
        return (i, s)


randomSex :: IO Sex
randomSex = [ [ Male, Female ] !! x | g <- newStdGen, let (x, _) = randomR (0, 1) g ]


randomRace :: IO Race
randomRace = [ [ Dwarf .. Vulpenoid ] !! x | g <- newStdGen, let (x, _) = randomR (0, 7) g ]


getUnusedId :: WorldState -> Id
getUnusedId = head . (\\) [0..] . allKeys


allKeys :: WorldState -> Inv
allKeys = views typeTbl IM.keys


plaThreadExHandler :: T.Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler n i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = do
      logExMsg "plaThreadExHandler" ("exception caught on " <> n <> " thread; rethrowing to listen thread") e
      liftIO . flip throwTo e =<< getListenThreadId


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO mkFilename >>= try . takeADump >>= eitherRet (fileIOExHandler "dumpTitle")
  where
    mkFilename   = ("title" ++) . show . fst . randomR (1, noOfTitles) <$> newStdGen
    takeADump fn = send mq . nl' =<< nl `fmap` (liftIO . T.readFile . (titleDir ++) $ fn)


-- ==================================================
-- "Inactivity timer" threads:


type InacTimerQueue = TQueue InacTimerMsg


data InacTimerMsg = ResetTimer
                  | StopTimer


inacTimer :: Id -> MsgQueue -> InacTimerQueue -> MudStack ()
inacTimer i mq itq = (registerThread . InacTimer $ i) >> loop 0 `catch` plaThreadExHandler "inactivity timer" i
  where
    loop secs = do
        liftIO . threadDelay $ 10 ^ 6
        (liftIO . atomically . tryReadTQueue $ itq) >>= \case
          Nothing | secs >= maxInacSecs -> inacBoot secs
                  | otherwise           -> loop . succ $ secs
          Just itm                      -> case itm of StopTimer  -> return ()
                                                       ResetTimer -> loop 0
    inacBoot (parensQuote . T.pack . renderSecs -> secs) = do
        (parensQuote -> s) <- getEntSing i
        logNotice "inacTimer" . T.concat $ [ "booting player ", showText i, " ", s, " due to inactivity." ]
        logPla "inacTimer" i $ "booted due to inactivity " <> secs <>  "."
        liftIO . atomically . writeTQueue mq $ InacBoot


-- ==================================================
-- "Server" threads:


server :: Handle -> Id -> MsgQueue -> InacTimerQueue -> MudStack ()
server h i mq itq = sequence_ [ registerThread . Server $ i, loop (Just itq) `catch` plaThreadExHandler "server" i ]
  where
    loop mitq = (liftIO . atomically . readTQueue $ mq) >>= \case
      Dropped        ->                                   sayonara i mitq
      FromClient msg -> handleFromClient i mq mitq msg >> loop mitq
      FromServer msg -> handleFromServer i h msg       >> loop mitq
      InacBoot       -> sendInacBootMsg h              >> sayonara i mitq
      InacStop       -> stopInacThread mitq            >> loop Nothing
      MsgBoot msg    -> boot h msg                     >> sayonara i mitq
      Peeped  msg    -> (liftIO . T.hPutStr h $ msg)   >> loop mitq
      Prompt  p      -> sendPrompt i h p               >> loop mitq
      Quit           -> cowbye h                       >> sayonara i mitq
      Shutdown       -> shutDown                       >> loop mitq
      SilentBoot     ->                                   sayonara i mitq


sayonara :: Id -> Maybe InacTimerQueue -> MudStack ()
sayonara i (Just itq) = sequence_ [ liftIO . atomically . writeTQueue itq $ StopTimer, handleEgress i ]
sayonara i Nothing    = handleEgress i


handleFromClient :: Id -> MsgQueue -> Maybe InacTimerQueue -> T.Text -> MudStack ()
handleFromClient i mq mitq (T.strip . stripControl . stripTelnet -> msg) = getPla i >>= \p ->
    let thruCentral = unless (T.null msg) . uncurry (interpret p centralDispatch) . headTail . T.words $ msg
        thruOther f = uncurry (interpret p f) $ if T.null msg then ("", []) else headTail . T.words $ msg
    in maybe thruCentral thruOther $ p^.interp
  where
    interpret p f cn as = do
        forwardToPeepers i (p^.peepers) FromThePeeped msg
        maybeVoid (liftIO . atomically . flip writeTQueue ResetTimer) mitq
        f cn . WithArgs i mq (p^.columns) $ as


forwardToPeepers :: Id -> Inv -> ToOrFromThePeeped -> T.Text -> MudStack ()
forwardToPeepers i peeperIds toOrFrom msg = do
    peepMqs <- [ map (mqt !) peeperIds | mqt <- readTMVarInNWS msgQueueTblTMVar ]
    s       <- getEntSing i
    liftIO . atomically . forM_ peepMqs $ flip writeTQueue (mkPeepedMsg s)
  where
    mkPeepedMsg s = Peeped $ case toOrFrom of
      ToThePeeped   ->      T.concat   [ toPeepedColor,   " ", bracketQuote s, " ", dfltColor, " ", msg ]
      FromThePeeped -> nl . T.concat $ [ fromPeepedColor, " ", bracketQuote s, " ", dfltColor, " ", msg ]


handleFromServer :: Id -> Handle -> T.Text -> MudStack ()
handleFromServer i h msg = getPla i >>= \(view peepers -> peeperIds) -> do
    forwardToPeepers i peeperIds ToThePeeped msg
    liftIO . T.hPutStr h $ msg


sendInacBootMsg :: Handle -> MudStack ()
sendInacBootMsg h = liftIO . T.hPutStrLn h . nl $ bootMsgColor                                                  <>
                                                  "You are being disconnected from CurryMUD due to inactivity." <>
                                                  dfltColor


stopInacThread :: Maybe InacTimerQueue -> MudStack ()
stopInacThread = maybeVoid (liftIO . atomically . flip writeTQueue StopTimer)


boot :: Handle -> T.Text -> MudStack ()
boot h = liftIO . T.hPutStrLn h . nl . (<> dfltColor) . (bootMsgColor <>)


sendPrompt :: Id -> Handle -> T.Text -> MudStack ()
sendPrompt i h p = getPla i >>= \(view peepers -> peeperIds) -> do
    forwardToPeepers i peeperIds ToThePeeped . nl $ p
    liftIO . T.hPutStrLn h $ p


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` fileIOExHandler "cowbye"
  where
    takeADump = T.hPutStrLn h =<< T.readFile cowbyeFile


shutDown :: MudStack ()
shutDown = massMsg SilentBoot >> commitSuicide
  where
    commitSuicide = statefulFork_ $ do
        liftIO . mapM_ wait . M.elems =<< readTMVarInNWS talkAsyncTblTMVar
        logNotice "shutDown commitSuicide" "all players have been disconnected; killing the listen thread."
        liftIO . killThread =<< getListenThreadId


-- ==================================================
-- "Receive" threads:


receive :: Handle -> Id -> MsgQueue -> MudStack ()
receive h i mq = sequence_ [ registerThread . Receive $ i, loop `catch` plaThreadExHandler "receive" i ]
  where
    loop = mIf (liftIO . hIsEOF $ h)
               (sequence_ [ logPla "receive" i "connection dropped."
                          , liftIO . atomically . writeTQueue mq $ Dropped ])
               (sequence_ [ liftIO $ atomically . writeTQueue mq . FromClient . remDelimiters =<< T.hGetLine h
                          , loop ])
    remDelimiters = T.foldr helper ""
    helper c acc | T.singleton c `notInfixOf` delimiters = c `T.cons` acc
                 | otherwise                             = acc
    delimiters = T.pack [ stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter ]
