{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, OverloadedStrings, ViewPatterns #-}

module Mud.Threads ( getUnusedId
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
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.CentralDispatch
import Mud.Interp.Login
import Mud.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import Mud.TheWorld.Ids
import Mud.TheWorld.TheWorld
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List (headTail)
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text hiding (headTail)
import qualified Mud.Logging as L (logExMsg, logIOEx, logNotice, logPla)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, asyncThreadId, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, newTMQueueIO, tryReadTMQueue, writeTMQueue)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception (AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, finally, handle, throwTo, try)
import Control.Lens (at)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((&), (?~), (^.))
import Control.Monad ((>=>), forM_, forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Bits (zeroBits)
import Data.IntMap.Lazy ((!))
import Data.List ((\\))
import Data.Monoid ((<>), mempty)
import Network (HostName, PortID(..), accept, listenOn, sClose)
import Prelude hiding (pi)
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import System.Random (randomIO, randomRIO) -- TODO: Use mwc-random or tf-random. QC uses tf-random.
import System.Time.Utils (renderSecs)
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Map.Lazy as M (elems, empty)
-- import qualified Data.Set as S (Set, fromList) -- TODO
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
listenWrapper =
    (logNotice "listenWrapper" "server started." >> listen) `finally` (getUptime >>= saveUptime >> closeLogs)


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
    ask >>= liftIO . void . forkIO . runReaderT threadTblPurger
    initWorld
    logInterfaces
    logNotice "listen" $ "listening for incoming connections on port " <> showText port <> "."
    sock <- liftIO . listenOn . PortNumber . fromIntegral $ port
    (forever . loop $ sock) `finally` cleanUp sock
  where
    logInterfaces = liftIO NI.getNetworkInterfaces >>= \ns ->
        let ifList = T.intercalate ", " [ bracketQuote . T.concat $ [ showText . NI.name $ n
                                                                    , ": "
                                                                    , showText . NI.ipv4 $ n ] | n <- ns ]
        in logNotice "listen listInterfaces" $ "server network interfaces: " <> ifList <> "."
    loop sock = ask >>= \md -> do
        (h, host, localPort) <- liftIO . accept $ sock
        logNotice "listen loop" . T.concat $ [ "connected to "
                                             , showText host
                                             , " on local port "
                                             , showText localPort
                                             , "." ]
        a@(asyncThreadId -> ti) <- liftIO . async . runReaderT (talk h host) $ md
        liftIO . atomically . modifyTVar (md^.talkAsyncTblTVar) $ at ti ?~ a
    cleanUp sock = logNotice "listen cleanUp" "closing the socket." >> (liftIO . sClose $ sock)


listenExHandler :: SomeException -> MudStack ()
listenExHandler e = case fromException e of
  Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
  Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
  _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e


registerThread :: ThreadType -> MudStack ()
registerThread threadType = liftIO myThreadId >>= \ti ->
    ask >>= \md -> (liftIO . atomically . modifyTVar (md^.threadTblTVar) $ at ti ?~ threadType)


-- TODO: Figure out what to do with dictionaries.
{-
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
-}


-- ==================================================
-- The "thread table purger" thread:


threadTblPurger :: MudStack ()
threadTblPurger = do
    registerThread ThreadTblPurger
    logNotice "threadTblPurger" "thread table purger thread started."
    let loop = (liftIO . threadDelay $ threadTblPurgerDelay * 10 ^ 6) >> purgeThreadTbls
    forever loop `catch` threadTblPurgerExHandler


threadTblPurgerExHandler :: SomeException -> MudStack ()
threadTblPurgerExHandler e = do
    logExMsg "threadTblPurgerExHandler" "exception caught on thread table purger thread; rethrowing to listen thread" e
    liftIO . flip throwTo e =<< getListenThreadId


getListenThreadId :: MudStack ThreadId
getListenThreadId = ask >>= \md -> reverseLookup Listen <$> (liftIO . readTVarIO $ md^.threadTblTVar)


-- ==================================================
-- "Talk" threads:


talk :: Handle -> HostName -> MudStack ()
talk h host = helper `finally` cleanUp
  where
    helper = ask >>= \md -> do
        (mq, itq)          <- liftIO $ (,) <$> newTQueueIO <*> newTMQueueIO
        (i, dblQuote -> s) <- adHoc mq host
        registerThread . Talk $ i
        handle (plaThreadExHandler "talk" i) $ ask >>= liftIO . atomically . helperSTM >>= \(mt, mqt, pcTbl, plaTbl ) -> do
            logNotice "talk helper" $ "new PC name for incoming player: "    <> s <> "."
            bcastAdmins mt mqt pcTbl plaTbl $ "A new player has connected: " <> s <> "."
            liftIO configBuffer
            dumpTitle mq
            prompt    mq "By what name are you known?"
            liftIO . void . forkIO . runReaderT (inacTimer i mq itq) $ md
            liftIO $ race_ (runReaderT (server  h i mq itq) md)
                           (runReaderT (receive h i mq)     md)
    helperSTM md = (,,,) <$> readTVar (md^.mobTblTVar)
                         <*> readTVar (md^.msgQueueTblTVar)
                         <*> readTVar (md^.pcTblTVar)
                         <*> readTVar (md^.plaTblTVar)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = logNotice "talk cleanUp" ("closing the handle for " <> T.pack host <> ".") >> (liftIO . hClose $ h)


adHoc :: MsgQueue -> HostName -> MudStack (Id, Sing)
adHoc mq host = ask >>= \md -> do
    (sexy, r) <- liftIO $ (,) <$> randomSex <*> randomRace
    liftIO . atomically $ do
        (et, it, tt) <- (,,) <$> readTVar (md^.entTblTVar) <*> readTVar (md^.invTblTVar) <*> readTVar (md^.typeTblTVar)
        let i    = getUnusedId tt
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
            tt'  = tt & at i ?~ PCType
            et'  = et & at i ?~ e
            ris  = sortInv et' tt' $ it ! iWelcome ++ [i]
        writeTVar  (md^.typeTblTVar) tt'
        writeTVar  (md^.entTblTVar)  et'
        writeTVar  (md^.invTblTVar)      $ it & at i ?~ [] & at iWelcome ?~ ris
        modifyTVar (md^.coinsTblTVar)    $ at i ?~ mempty
        modifyTVar (md^.eqTblTVar)       $ at i ?~ M.empty
        modifyTVar (md^.mobTblTVar)      $ at i ?~ m
        modifyTVar (md^.msgQueueTblTVar) $ at i ?~ mq
        modifyTVar (md^.pcTblTVar)       $ at i ?~ pc
        modifyTVar (md^.plaTblTVar)      $ at i ?~ pla
        return (i, s)


randomSex :: IO Sex
randomSex = ([ Male, Female ] !!) . fromEnum <$> (randomIO :: IO Bool)


randomRace :: IO Race
randomRace = randomIO


getUnusedId :: TypeTbl -> Id
getUnusedId = head . ([0..] \\) . IM.keys


plaThreadExHandler :: T.Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler n i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = do
      logExMsg "plaThreadExHandler" ("exception caught on " <> n <> " thread; rethrowing to listen thread") e
      liftIO . flip throwTo e =<< getListenThreadId


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO mkFilename >>= try . takeADump >>= eitherRet (fileIOExHandler "dumpTitle")
  where
    mkFilename   = ("title" ++) . show <$> randomRIO (1, noOfTitles)
    takeADump fn = send mq . nlPrefix =<< nl `fmap` (liftIO . T.readFile $ titleDir </> fn)


-- ==================================================
-- "Inactivity timer" threads:


type InacTimerQueue = TMQueue InacTimerMsg


data InacTimerMsg = ResetTimer


inacTimer :: Id -> MsgQueue -> InacTimerQueue -> MudStack ()
inacTimer i mq itq = (registerThread . InacTimer $ i) >> loop 0 `catch` plaThreadExHandler "inactivity timer" i
  where
    loop secs = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        itq |$| liftIO . atomically . tryReadTMQueue >=> \case
          Just Nothing | secs >= maxInacSecs -> inacBoot secs
                       | otherwise           -> loop . succ $ secs
          Just (Just ResetTimer)             -> loop 0
          Nothing                            -> return ()
    inacBoot (parensQuote . T.pack . renderSecs -> secs) = do
        s <- ask >>= \md -> parensQuote . view sing . (! i) <$> (liftIO . readTVarIO $ (md^.entTblTVar))
        logNotice "inacTimer" . T.concat $ [ "booting player ", showText i, " ", s, " due to inactivity." ]
        logPla "inacTimer" i $ "booted due to inactivity " <> secs <>  "."
        liftIO . atomically . writeTQueue mq $ InacBoot


-- ==================================================
-- "Server" threads:


server :: Handle -> Id -> MsgQueue -> InacTimerQueue -> MudStack ()
server h i mq itq = sequence_ [ registerThread . Server $ i, loop `catch` plaThreadExHandler "server" i ]
  where
    loop = mq |$| liftIO . atomically . readTQueue >=> \case
      Dropped        ->                                  sayonara
      FromClient msg -> handleFromClient i mq itq msg >> loop
      FromServer msg -> handleFromServer i h msg      >> loop
      InacBoot       -> sendInacBootMsg h             >> sayonara
      InacStop       -> stopInacThread itq            >> loop
      MsgBoot msg    -> boot h msg                    >> sayonara
      Peeped  msg    -> (liftIO . T.hPutStr h $ msg)  >> loop
      Prompt  p      -> sendPrompt i h p              >> loop
      Quit           -> cowbye h                      >> sayonara
      Shutdown       -> shutDown                      >> loop
      SilentBoot     ->                                  sayonara
    sayonara = sequence_ [ liftIO . atomically . closeTMQueue $ itq, handleEgress i ]


handleFromClient :: Id -> MsgQueue -> InacTimerQueue -> T.Text -> MudStack ()
handleFromClient i mq itq (T.strip . stripControl . stripTelnet -> msg) =
    ask >>= liftIO . readTVarIO . view plaTblTVar >>= \((! i) -> p) ->
        let thruCentral = unless (T.null msg) . uncurry (interpret p centralDispatch) . headTail . T.words $ msg
            thruOther f = uncurry (interpret p f) (T.null msg ? ("", []) :? (headTail . T.words $ msg))
        in maybe thruCentral thruOther $ p^.interp
  where
    interpret p f cn as = do
        forwardToPeepers i (p^.peepers) FromThePeeped msg
        liftIO . atomically . writeTMQueue itq $ ResetTimer
        f cn . WithArgs i mq (p^.columns) $ as


forwardToPeepers :: Id -> Inv -> ToOrFromThePeeped -> T.Text -> MudStack ()
forwardToPeepers i peeperIds toOrFrom msg = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = do
        (view sing . (! i) -> s, mqt) <- (,) <$> readTVar (md^.entTblTVar) <*> readTVar (md^.msgQueueTblTVar)
        forM_ [ mqt ! pi | pi <- peeperIds ] $ flip writeTQueue (mkPeepedMsg s)
    mkPeepedMsg s = Peeped $ case toOrFrom of
      ToThePeeped   ->      T.concat   [ toPeepedColor,   " ", bracketQuote s, " ", dfltColor, " ", msg ]
      FromThePeeped -> nl . T.concat $ [ fromPeepedColor, " ", bracketQuote s, " ", dfltColor, " ", msg ]


handleFromServer :: Id -> Handle -> T.Text -> MudStack ()
handleFromServer i h msg = ask >>= liftIO . readTVarIO . view plaTblTVar >>= \((! i) -> p) -> do
    forwardToPeepers i (p^.peepers) ToThePeeped msg
    liftIO . T.hPutStr h $ msg


sendInacBootMsg :: Handle -> MudStack ()
sendInacBootMsg h = liftIO . T.hPutStrLn h . nl $ bootMsgColor                                                  <>
                                                  "You are being disconnected from CurryMUD due to inactivity." <>
                                                  dfltColor


stopInacThread :: InacTimerQueue -> MudStack ()
stopInacThread = liftIO . atomically . closeTMQueue


boot :: Handle -> T.Text -> MudStack ()
boot h = liftIO . T.hPutStrLn h . nl . (<> dfltColor) . (bootMsgColor <>)


sendPrompt :: Id -> Handle -> T.Text -> MudStack ()
sendPrompt i h = handleFromServer i h . nl


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` fileIOExHandler "cowbye"
  where
    takeADump = T.hPutStrLn h =<< T.readFile cowbyeFile


shutDown :: MudStack ()
shutDown = massMsg SilentBoot >> ask >>= liftIO . void . forkIO . runReaderT commitSuicide
  where
    commitSuicide = ask >>= liftIO . readTVarIO . view talkAsyncTblTVar >>= \tat -> do
        liftIO . mapM_ wait . M.elems $ tat
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
