{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Cmds.Cmds (listenWrapper) where

import Mud.Cmds.DebugCmds
import Mud.Cmds.PlaCmds
import Mud.Cmds.WizCmds
import Mud.Color
import Mud.Ids
import Mud.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.StateInIORefT
import Mud.TheWorld
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logExMsg, logIOEx, logNotice, logPla)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId)
import Control.Concurrent.Async (async, asyncThreadId, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, finally, handle, throwTo, try)
import Control.Lens (at)
import Control.Lens.Getter (view)
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
import qualified Data.Text.IO as T (hGetLine, hPutStr, readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)


-- TODO: Here's the plan:
-- 1. Implement the broadcasting of messages.
-- 2. Review your coding guide, and undertake a refactoring of the entire codebase. Consider the following:
--   a. Code reduction.
--   b. Consistency in binding names.
-- 3. Write tests for NameResolution and Cmds.


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds"


-----


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Cmds"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds"


-- ==================================================


listenWrapper :: MudStack ()
listenWrapper = initAndStart `finally` graceful
  where
    initAndStart = do
        initLogging
        logNotice "listenWrapper initAndStart" "server started."
        initWorld
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
    listInterfaces
    logNotice "listen" $ "listening for incoming connections on port " <> showText port <> "."
    sock <- liftIO . listenOn . PortNumber . fromIntegral $ port
    (forever . loop $ sock) `finally` cleanUp sock
  where
    listInterfaces = liftIO NI.getNetworkInterfaces >>= \ns ->
        let ifList = T.intercalate ", " [ T.concat [ "["
                                                   , showText . NI.name $ n
                                                   , ": "
                                                   , showText . NI.ipv4 $ n
                                                   , "]" ] | n <- ns ]
        in logNotice "listen listInterfaces" $ "server network interfaces: " <> ifList <> "."
    loop sock = do
        (h, host, port') <- liftIO . accept $ sock
        logNotice "listen loop" . T.concat $ [ "connected to ", showText host, " on local port ", showText port', "." ]
        a@(asyncThreadId -> ti) <- liftIO . async . void . runStateInIORefT (talk h host) =<< get
        modifyNWS talkAsyncTblTMVar $ \tat -> tat & at ti ?~ a
    cleanUp sock = logNotice "listen cleanUp" "closing the socket." >> (liftIO . sClose $ sock)


listenExHandler :: SomeException -> MudStack ()
listenExHandler e =
    case fromException e of
      Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
      Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
      _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e


registerThread :: ThreadType -> MudStack ()
registerThread threadType = liftIO myThreadId >>= \ti ->
    modifyNWS threadTblTMVar $ \tt -> tt & at ti ?~ threadType


talk :: Handle -> HostName -> MudStack ()
talk h host = helper `finally` cleanUp
  where
    helper = do
        registerThread Talk
        mq <- liftIO newTQueueIO
        i  <- adHoc mq host
        handle (talkExHandler i) $ do
            logNotice "talk helper" $ "new ID for incoming player: " <> showText i <> "."
            liftIO configBuffer
            setDfltColor mq
            dumpTitle    mq
            prompt       mq "By what name are you known?"
            s <- get
            liftIO $ race_ (runStateInIORefT (server  h i mq) s)
                           (runStateInIORefT (receive h i mq) s)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = logNotice "talk cleanUp" ("closing the handle for " <> T.pack host <> ".") >> (liftIO . hClose $ h)


talkExHandler :: Id -> SomeException -> MudStack ()
talkExHandler = plaThreadExHandler "talk"


adHoc :: MsgQueue -> HostName -> MudStack Id
adHoc mq host = do
    (wsTMVar, mqtTMVar, ptTMVar) <- (,,) <$> getWSTMVar       <*> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    (s, r)                       <- (,)  <$> liftIO randomSex <*> liftIO randomRace
    liftIO . atomically $ do
        (ws, mqt, pt) <- (,,) <$> takeTMVar wsTMVar <*> takeTMVar mqtTMVar <*> takeTMVar ptTMVar
        -----
        let i   = getUnusedId ws
        -----
        let e   = Ent i Nothing (showText r <> showText i) "" "(Player-generated description here.)" 0
        let is  = []
        let co  = mempty
        let em  = M.empty
        let m   = Mob s 10 10 10 10 10 10 0 RHand
        let pc  = PC iHill r [] []
        let ris = (ws^.invTbl) ! iHill ++ [i]
        -----
        let pla = Pla True host 80 interpName
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
        putTMVar wsTMVar $ ws' & invTbl.at iHill ?~ sortInv ws' ris
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


setDfltColor :: MsgQueue -> MudStack ()
setDfltColor = flip send dfltColorANSI


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO getFilename >>= try . takeADump >>= eitherRet (readFileExHandler "dumpTitle")
  where
    getFilename  = ("title" ++) . show . fst . randomR (1, noOfTitles) <$> newStdGen
    takeADump fn = send mq . nl' =<< (nl <$> (liftIO . T.readFile . (titleDir ++) $ fn))


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


server :: Handle -> Id -> MsgQueue -> MudStack ()
server h i mq = (registerThread . Server $ i) >> loop `catch` serverExHandler i
  where
    loop = (liftIO . atomically . readTQueue $ mq) >>= \case
      FromServer msg -> (liftIO . T.hPutStr h $ msg)             >> loop
      FromClient (T.strip . stripControl . stripTelnet -> msg)
                     -> unless (T.null msg) (handleInp i mq msg) >> loop
      Prompt p       -> sendPrompt h p                           >> loop
      Quit           -> cowbye h                                 >> handleEgress i
      SilentBoot     ->                                             handleEgress i
      MsgBoot msg    -> boot h msg                               >> handleEgress i
      Dropped        ->                                             handleEgress i
      Shutdown       -> shutDown                                 >> loop
      StopThread     -> return ()


serverExHandler :: Id -> SomeException -> MudStack ()
serverExHandler = plaThreadExHandler "server"


plaThreadExHandler :: T.Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler n i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = do
      logExMsg (n <> "ExHandler") ("exception caught on " <> n <> " thread; rethrowing to listen thread") e
      liftIO . flip throwTo e =<< getListenThreadId


getListenThreadId :: MudStack ThreadId
getListenThreadId = reverseLookup Listen <$> readTMVarInNWS threadTblTMVar


handleInp :: Id -> MsgQueue -> T.Text -> MudStack ()
handleInp i mq (headTail . T.words -> (cn, as)) = getPla i >>= \p ->
    let cols = p^.columns
        f    = p^.interp
    in f cn . WithArgs i mq cols $ as


sendPrompt :: Handle -> T.Text -> MudStack ()
sendPrompt h = liftIO . T.hPutStr h . nl


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` readFileExHandler "cowbye"
  where
    takeADump = T.hPutStr h . nl =<< (T.readFile . (miscDir ++) $ "cowbye")


-- TODO: Make a wizard command that does this.
boot :: Handle -> T.Text -> MudStack ()
boot h = liftIO . T.hPutStr h . nl' . nlnl


shutDown :: MudStack ()
shutDown = massMsg StopThread >> commitSuicide
  where
    commitSuicide = do
        liftIO . void . forkIO . mapM_ wait . M.elems =<< readTMVarInNWS talkAsyncTblTMVar
        liftIO . killThread =<< getListenThreadId


receive :: Handle -> Id -> MsgQueue -> MudStack ()
receive h i mq = (registerThread . Receive $ i) >> loop `catch` receiveExHandler i
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


notInfixOf :: T.Text -> T.Text -> Bool
notInfixOf needle haystack = not $  needle `T.isInfixOf` haystack


receiveExHandler :: Id -> SomeException -> MudStack ()
receiveExHandler = plaThreadExHandler "receive"


-- TODO: Boot a player who tries too many times.
interpName :: Interp
interpName (capitalize . T.toLower -> cn) (NoArgs' i mq)
  | l <- T.length cn, l < 3 || l > 12 = sorryIllegalName mq "Your name must be between three and twelve characters \
                                                            \long."
  | T.any (`elem` illegalChars) cn    = sorryIllegalName mq "Your name cannot include any numbers or symbols."
  | otherwise                         = do
      prompt mq . nl' $ "Your name will be " <> dblQuote cn <> ", is that OK? [yes/no]"
      void . modifyPla i interp $ interpConfirmName
  where
    illegalChars    = [ '!' .. '@' ] ++ [ '[' .. '`' ] ++ [ '{' .. '~' ]
interpName _  (WithArgs _ mq _ _) = sorryIllegalName mq "Your name must be a single word."
interpName cn p                   = patternMatchFail "interpName" [ cn, showText p ]


sorryIllegalName :: MsgQueue -> T.Text -> MudStack ()
sorryIllegalName mq msg = do
    send mq . nl' . nl $ msg
    prompt mq "Let's try this again. By what name are you known?"


interpConfirmName :: Interp
interpConfirmName cn (NoArgs' i mq) = do
    void . modifyEnt i sing $ cn
    (T.pack . view hostName -> host) <- modifyPla i interp centralDispatch
    initPlaLog i cn
    logPla "interpName" i $ "(new player) logged on from " <> host <> "."
    notifyArrival i
    prompt mq . nl' $ ">"
interpConfirmName cn p = patternMatchFail "interpConfirmName" [ cn, showText p ]


-- TODO: Look for other places to use "view".
notifyArrival :: Id -> MudStack ()
notifyArrival i = readWSTMVar >>= \ws ->
    let (view sing -> s) = (ws^.entTbl) ! i
    in bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ws s A <> " has arrived in the game."


centralDispatch :: Interp
centralDispatch cn p@(WithArgs i mq _ _) = do
    findAction i cn >>= maybe sorry (\act -> act p)
    prompt mq ">"
  where
    sorry = send mq . nlnl $ "What?"
centralDispatch cn p = patternMatchFail "centralDispatch" [ cn, showText p ]


findAction :: Id -> CmdName -> MudStack (Maybe Action)
findAction i (T.toLower -> cn) = readWSTMVar >>= \ws ->
    readTMVarInNWS plaTblTMVar >>= \((! i) -> p) ->
        let (view rmId -> ri) = (ws^.pcTbl) ! i
            r                 = (ws^.rmTbl) ! ri
            cmds              = mkCmdListWithNonStdRmLinks r ++
                                (if p^.isWiz then wizCmds   else []) ++
                                (if isDebug  then debugCmds else [])
        in maybe (return Nothing)
                 (\fn -> return . Just . findActionForFullName fn $ cmds)
                 (findFullNameForAbbrev cn [ cmdName cmd | cmd <- cmds ])
  where
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)
