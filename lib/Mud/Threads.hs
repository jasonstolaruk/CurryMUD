{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Threads (listenWrapper) where

import Mud.Cmds.Debug
import Mud.Cmds.Pla
import Mud.Color
import Mud.Data.State.State
import Mud.Data.State.StateInIORefT
import Mud.Data.State.Util
import Mud.Interp.Login
import Mud.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import Mud.TheWorld.Ids
import Mud.TheWorld.TheWorld
import Mud.TopLvlDefs
import Mud.Util
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
import Control.Lens.Operators ((&), (?~), (^.))
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.IntMap.Lazy ((!))
import Data.Monoid ((<>), mempty)
import Network (HostName, PortID(..), accept, listenOn, sClose)
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import System.Random (newStdGen, randomR) -- TODO: Use mwc-random or tf-random. QC uses tf-random.
import System.Time.Utils (renderSecs)
import qualified Data.Map.Lazy as M (elems, empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine, hPutStr, hPutStrLn, readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)


-- TODO: Here's the plan:
-- 1. Implement the broadcasting of messages.
-- 2. Review your coding guide, and undertake a refactoring of the entire codebase. Consider the following:
--   a. Code reduction.
--   b. Consistency in binding names.
-- 3. Write tests for NameResolution and the Cmds modules.


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


saveUptime :: Integer -> MudStack ()
saveUptime ut@(T.pack . renderSecs -> utTxt) = getRecordUptime >>= \case
  Nothing  -> saveIt >> logIt
  Just rut -> case ut `compare` rut of GT -> saveIt >> logRec
                                       _  -> logIt
  where
    saveIt    = (liftIO . writeFile uptimeFile . show $ ut) `catch` logIOEx "saveUptime saveIt"
    logIt     = logHelper "."
    logRec    = logHelper " - it's a new record!"
    logHelper = logNotice "saveUptime" . ("the MUD was up for " <>) . (utTxt <>)


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
        (h, host, port') <- liftIO . accept $ sock
        logNotice "listen loop" . T.concat $ [ "connected to ", showText host, " on local port ", showText port', "." ]
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
loadDictFiles = return ()


-- ==================================================
-- The "thread table purge" thread:


threadTblPurger :: MudStack ()
threadTblPurger = do
    registerThread ThreadTblPurger
    logNotice "threadTblPurger" "thread table purger thread started."
    forever loop `catch` threadTblPurgerExHandler
  where
    loop = (liftIO . threadDelay $ 10 ^ 6 * threadTblPurgerDelay) >> purgeThreadTbls


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
        (mq, itq) <- (,) <$> liftIO newTQueueIO <*> liftIO newTQueueIO
        i         <- adHoc mq host
        registerThread . Talk $ i
        handle (plaThreadExHandler "talk" i) $ do
            logNotice "talk helper" $ "new ID for incoming player: " <> showText i <> "."
            liftIO configBuffer
            setDfltColor mq
            dumpTitle    mq
            prompt       mq "By what name are you known?"
            s <- statefulFork . inacTimer i mq $ itq
            liftIO $ race_ (runStateInIORefT (server  h i mq itq) s)
                           (runStateInIORefT (receive h i mq)     s)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = logNotice "talk cleanUp" ("closing the handle for " <> T.pack host <> ".") >> (liftIO . hClose $ h)


adHoc :: MsgQueue -> HostName -> MudStack Id
adHoc mq host = do
    (wsTMVar, mqtTMVar, ptTMVar) <- (,,) <$> getWSTMVar       <*> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    (s, r)                       <- (,)  <$> liftIO randomSex <*> liftIO randomRace
    liftIO . atomically $ do
        (ws, mqt, pt) <- (,,) <$> takeTMVar wsTMVar <*> takeTMVar mqtTMVar <*> takeTMVar ptTMVar
        -----
        let i    = getUnusedId ws
        -----
        let desc = capitalize $ mkPronoun s <> " is an ad-hoc player character."
        let e    = Ent i Nothing (showText r <> showText i) "" desc 0
        let is   = []
        let co   = mempty
        let em   = M.empty
        let m    = Mob s 10 10 10 10 10 10 0 RHand
        let pc   = PC iWelcome r [] []
        let ris  = (ws^.invTbl) ! iWelcome ++ [i]
        -----
        let pla  = Pla True host 80 interpName
        -----
        let ws'  = ws  & typeTbl.at  i ?~ PCType
                       & entTbl.at   i ?~ e
                       & invTbl.at   i ?~ is
                       & coinsTbl.at i ?~ co
                       & eqTbl.at    i ?~ em
                       & mobTbl.at   i ?~ m
                       & pcTbl.at    i ?~ pc
        let mqt' = mqt & at i ?~ mq
        let pt'  = pt  & at i ?~ pla
        -----
        putTMVar wsTMVar $ ws' & invTbl.at iWelcome ?~ sortInv ws' ris
        putTMVar mqtTMVar mqt'
        putTMVar ptTMVar  pt'
        -----
        return i


randomSex :: IO Sex
randomSex = newStdGen >>= \g ->
    let (x, _) = randomR (0, 1) g in return $ [ Male, Female ] !! x


randomRace :: IO Race
randomRace = newStdGen >>= \g ->
    let (x, _) = randomR (0, 7) g in return $ [ Dwarf .. Vulpenoid ] !! x


plaThreadExHandler :: T.Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler n i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = do
      logExMsg "plaThreadExHandler" ("exception caught on " <> n <> " thread; rethrowing to listen thread") e
      liftIO . flip throwTo e =<< getListenThreadId


setDfltColor :: MsgQueue -> MudStack ()
setDfltColor = flip send dfltColorANSI


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO getFilename >>= try . takeADump >>= eitherRet (readFileExHandler "dumpTitle")
  where
    getFilename  = ("title" ++) . show . fst . randomR (1, noOfTitles) <$> newStdGen
    takeADump fn = send mq . nl' =<< (nl <$> (liftIO . T.readFile . (titleDir ++) $ fn))


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
          Nothing  -> if secs >= maxInacSecs
                        then inacBoot secs
                        else loop . succ $ secs
          Just itm -> case itm of StopTimer  -> return ()
                                  ResetTimer -> loop 0
    inacBoot (parensQuote . T.pack . renderSecs -> secs) = do
        logPla "inacTimer" i $ "booted due to inactivity " <> secs <>  "."
        liftIO . atomically . writeTQueue mq $ InacBoot


-- ==================================================
-- "Server" threads:


server :: Handle -> Id -> MsgQueue -> InacTimerQueue -> MudStack ()
server h i mq itq = (registerThread . Server $ i) >> loop `catch` plaThreadExHandler "server" i
  where
    loop = (liftIO . atomically . readTQueue $ mq) >>= \case
      Dropped        ->                                  sayonara i itq
      FromClient msg -> handleFromClient i mq itq msg >> loop
      FromServer msg -> (liftIO . T.hPutStr h $ msg)  >> loop
      InacBoot       -> sendInacBootMsg h             >> sayonara i itq
      MsgBoot msg    -> boot h msg                    >> sayonara i itq
      Prompt p       -> sendPrompt h p                >> loop
      Quit           -> cowbye h                      >> sayonara i itq
      Shutdown       -> shutDown                      >> loop
      SilentBoot     ->                                  sayonara i itq


sayonara :: Id -> InacTimerQueue -> MudStack ()
sayonara i itq = (liftIO . atomically . writeTQueue itq $ StopTimer) >> handleEgress i


handleFromClient :: Id -> MsgQueue -> InacTimerQueue -> T.Text -> MudStack ()
handleFromClient i mq itq (T.strip . stripControl . stripTelnet -> msg) = unless (T.null msg) $ do
    liftIO . atomically . writeTQueue itq $ ResetTimer
    handleInp i mq msg


handleInp :: Id -> MsgQueue -> T.Text -> MudStack ()
handleInp i mq (headTail . T.words -> (cn, as)) = getPla i >>= \p ->
    let cols = p^.columns
        f    = p^.interp
    in f cn . WithArgs i mq cols $ as


sendInacBootMsg :: Handle -> MudStack ()
sendInacBootMsg h = liftIO . T.hPutStrLn h . nl' . nl $ "You are being disconnected from CurryMUD due to inactivity."


boot :: Handle -> T.Text -> MudStack ()
boot h = liftIO . T.hPutStrLn h . nl' . nl


sendPrompt :: Handle -> T.Text -> MudStack ()
sendPrompt h = liftIO . T.hPutStrLn h


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` readFileExHandler "cowbye"
  where
    takeADump = T.hPutStrLn h =<< (T.readFile . (miscDir ++) $ "cowbye")


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
receive h i mq = (registerThread . Receive $ i) >> loop `catch` plaThreadExHandler "receive" i
  where
    loop = (liftIO . hIsEOF $ h) >>= \case
      True  -> do
          logPla "receive" i "connection dropped."
          liftIO . atomically . writeTQueue mq $ Dropped
      False -> do
          liftIO $ atomically . writeTQueue mq . FromClient . remDelimiters =<< T.hGetLine h
          loop
    remDelimiters = T.foldr helper ""
    helper c acc | T.singleton c `notInfixOf` delimiters = c `T.cons` acc
                 | otherwise                             = acc
    delimiters = T.pack [ stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter ]
