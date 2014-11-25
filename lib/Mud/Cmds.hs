{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

module Mud.Cmds (listenWrapper) where

import Mud.Ids
import Mud.Logging hiding (logAndDispIOEx, logExMsg, logIOEx, logIOExRethrow, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut, massLogPla)
import Mud.MiscDataTypes
import Mud.NameResolution
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.StateInIORefT
import Mud.TheWorld
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Logging as L (logAndDispIOEx, logExMsg, logIOEx, logIOExRethrow, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut, massLogPla)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (first)
import Control.Concurrent (forkIO, killThread, myThreadId, ThreadId)
import Control.Concurrent.Async (async, asyncThreadId, poll, race_, wait)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar, TMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), fromException, IOException, SomeException)
import Control.Exception.Lifted (catch, finally, throwIO, throwTo, try)
import Control.Lens (at, both, folded, over, to)
import Control.Lens.Operators ((&), (?~), (.~), (^.), (^..))
import Control.Monad (forever, forM_, guard, mplus, replicateM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Char (isSpace, toUpper)
import Data.IntMap.Lazy ((!))
import Data.List (delete, elemIndex, find, foldl', intercalate, intersperse, nub, nubBy, sort)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Monoid ((<>), mempty)
import Data.Text.Strict.Lens (packed)
import Data.Time (diffUTCTime, getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import GHC.Conc (threadStatus, ThreadStatus(..))
import Network (accept, HostName, listenOn, PortID(..), sClose)
import Prelude hiding (pi)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, getDirectoryContents, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (BufferMode(..), Handle, hClose, hGetBuffering, hGetLine, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1, Newline(..), NewlineMode(..), openTempFile)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import System.Random (newStdGen, randomR) -- TODO: Use mwc-random or tf-random. QC uses tf-random.
import System.Time.Utils (renderSecs)
import qualified Data.IntMap.Lazy as IM (assocs, delete, elems, keys)
import qualified Data.Map.Lazy as M (assocs, delete, elems, empty, filter, keys, null, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr, readFile)
import qualified Network.Info as NI (getNetworkInterfaces, ipv4, name)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- TODO: Here's the plan:
-- [DONE] 1. Consider using conduit.
-- [DONE] 2. Go into server mode. Accept incoming connections.
-- [DONE] 3. Implement client-based routing of output.
-- 4. Implement the broadcasting of messages.
-- [DONE] 5. Log commands.
-- 6. Review your coding guide, and undertake a refactoring of the entire codebase. Consider the following:
--   a. Code reduction.
--   b. Consistency in binding names.
--   c. Stylistic issues:
--      [DONE] Use "++" instead of "<>" where applicable.
--      [DONE] Concat lists of text instead of using "<>".
--      [DONE] ">>=" vs. "=<<".
--      [DONE] "(..)" instead of "(blah)" in import statements.
--   d. [DONE] Check for superfluous exports.
-- 7. Write tests for NameResolution and Cmds.
-- 8. Refactor for ViewPatterns and pattern guards.


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Cmds"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds"


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols = L.logAndDispIOEx mq cols "Mud.Cmds"


logIOExRethrow :: T.Text -> IOException -> MudStack ()
logIOExRethrow = L.logIOExRethrow "Mud.Cmds"


logExMsg :: T.Text -> T.Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Cmds"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds"


logPlaExecArgs :: CmdName -> Rest -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds"


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds"


massLogPla :: T.Text -> T.Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds"


-- ==================================================


wizCmds :: [Cmd]
wizCmds =
    [ Cmd { cmdName = prefixWizCmd "?", action = wizDispCmdList, cmdDesc = "Display this command list." }
    , Cmd { cmdName = prefixWizCmd "date", action = wizDate, cmdDesc = "Display the date." }
    , Cmd { cmdName = prefixWizCmd "name", action = wizName, cmdDesc = "Verify your PC name." }
    , Cmd { cmdName = prefixWizCmd "shutdown", action = wizShutdown, cmdDesc = "Shut down the MUD." }
    , Cmd { cmdName = prefixWizCmd "start", action = wizStart, cmdDesc = "Display the MUD start time." }
    , Cmd { cmdName = prefixWizCmd "time", action = wizTime, cmdDesc = "Display the current system time." }
    , Cmd { cmdName = prefixWizCmd "uptime", action = wizUptime, cmdDesc = "Display the server uptime." } ]


debugCmds :: [Cmd]
debugCmds =
    [ Cmd { cmdName = prefixDebugCmd "?", action = debugDispCmdList, cmdDesc = "Display this command list." }
    , Cmd { cmdName = prefixDebugCmd "boot", action = debugBoot, cmdDesc = "Boot all players." }
    , Cmd { cmdName = prefixDebugCmd "broad", action = debugBroad, cmdDesc = "Broadcast (to yourself) a multi-line message." }
    , Cmd { cmdName = prefixDebugCmd "buffer", action = debugBuffCheck, cmdDesc = "Confirm the default buffering mode." }
    , Cmd { cmdName = prefixDebugCmd "cpu", action = debugCPU, cmdDesc = "Display the CPU time." }
    , Cmd { cmdName = prefixDebugCmd "env", action = debugDispEnv, cmdDesc = "Display system environment variables." }
    , Cmd { cmdName = prefixDebugCmd "log", action = debugLog, cmdDesc = "Put the logging service under heavy load." }
    , Cmd { cmdName = prefixDebugCmd "purge", action = debugPurge, cmdDesc = "Purge the thread tables." }
    , Cmd { cmdName = prefixDebugCmd "remput", action = debugRemPut, cmdDesc = "In quick succession, remove from and put into \
                                                                               \a sack on the ground." }
    , Cmd { cmdName = prefixDebugCmd "stop", action = debugStop, cmdDesc = "Stop all server threads." }
    , Cmd { cmdName = prefixDebugCmd "talk", action = debugTalk, cmdDesc = "Dump the talk async table." }
    , Cmd { cmdName = prefixDebugCmd "thread", action = debugThread, cmdDesc = "Dump the thread table." }
    , Cmd { cmdName = prefixDebugCmd "throw", action = debugThrow, cmdDesc = "Throw an exception." } ]


plaCmds :: [Cmd]
plaCmds =
    [ Cmd { cmdName = "?", action = plaDispCmdList, cmdDesc = "Display this command list." }
    , Cmd { cmdName = "about", action = about, cmdDesc = "About this MUD." }
    , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
    , Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop items on the ground." }
    , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
    , Cmd { cmdName = "equip", action = equip, cmdDesc = "Display readied equipment." }
    , Cmd { cmdName = "exits", action = exits, cmdDesc = "Display obvious exits." }
    , Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick items up off the ground." }
    , Cmd { cmdName = "help", action = help, cmdDesc = "Get help on topics or commands." }
    , Cmd { cmdName = "i", action = inv, cmdDesc = "Inventory." }
    , Cmd { cmdName = "intro", action = intro, cmdDesc = "Introduce yourself." }
    , Cmd { cmdName = "look", action = look, cmdDesc = "Look." }
    , Cmd { cmdName = "motd", action = motd, cmdDesc = "Display the message of the day." }
    , Cmd { cmdName = "n", action = go "n", cmdDesc = "Go north." }
    , Cmd { cmdName = "ne", action = go "ne", cmdDesc = "Go northeast." }
    , Cmd { cmdName = "nw", action = go "nw", cmdDesc = "Go northwest." }
    , Cmd { cmdName = "put", action = putAction, cmdDesc = "Put items in a container." }
    , Cmd { cmdName = "quit", action = quit, cmdDesc = "Quit." }
    , Cmd { cmdName = "ready", action = ready, cmdDesc = "Ready items." }
    , Cmd { cmdName = "remove", action = remove, cmdDesc = "Remove items from a container." }
    , Cmd { cmdName = "s", action = go "s", cmdDesc = "Go south." }
    , Cmd { cmdName = "se", action = go "se", cmdDesc = "Go southeast." }
    , Cmd { cmdName = "sw", action = go "sw", cmdDesc = "Go southwest." }
    , Cmd { cmdName = "u", action = go "u", cmdDesc = "Go up." }
    , Cmd { cmdName = "unready", action = unready, cmdDesc = "Unready items." }
    , Cmd { cmdName = "uptime", action = uptime, cmdDesc = "Display how long the MUD has been running." }
    , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." }
    , Cmd { cmdName = "what", action = what, cmdDesc = "Disambiguate abbreviations." } ]


allCmds :: [Cmd]
allCmds = wizCmds ++ debugCmds ++ plaCmds


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd (T.pack . pure -> prefix) cn = prefix <> cn


prefixWizCmd :: CmdName -> T.Text
prefixWizCmd = prefixCmd wizCmdChar


prefixDebugCmd :: CmdName -> T.Text
prefixDebugCmd = prefixCmd debugCmdChar


listenWrapper :: MudStack ()
listenWrapper = (initAndStart `catch` listenExHandler) `finally` (getUptime >>= saveUptime >> closeLogs)
  where
    initAndStart = do
        initLogging
        logNotice "listenWrapper initAndStart" "server started."
        initWorld
        listen


listenExHandler :: SomeException -> MudStack ()
listenExHandler e =
    case fromException e of
      Just UserInterrupt -> logNotice "listenExHandler" "exiting on user interrupt."
      Just ThreadKilled  -> logNotice "listenExHandler" "thread killed."
      _                  -> logExMsg  "listenExHandler" "exception caught on listen thread" e


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


getRecordUptime :: MudStack (Maybe Integer)
getRecordUptime = (liftIO . doesFileExist $ uptimeFile) >>= \case
  True  -> readUptime `catch` (\e -> readFileExHandler "getRecordUptime" e >> return Nothing)
  False -> return Nothing
  where
    readUptime = return . Just . read =<< (liftIO . readFile $ uptimeFile)


listen :: MudStack ()
listen = do
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


registerThread :: ThreadType -> MudStack ()
registerThread threadType = liftIO myThreadId >>= \ti ->
    modifyNWS threadTblTMVar $ \tt -> tt & at ti ?~ threadType


talk :: Handle -> HostName -> MudStack ()
talk h host@(T.pack -> host') = helper `finally` cleanUp
  where
    helper = do
        registerThread Talk
        liftIO configBuffer
        mq     <- liftIO newTQueueIO
        (i, n) <- adHoc mq host
        logNotice "talk" . T.concat $ [ n
                                      , " has logged on "
                                      , parensQuote $ "new ID for incoming player: " <> showText i
                                      , "." ]
        initPlaLog i n
        logPla "talk" i $ "logged on from " <> host' <> "."
        dumpTitle mq
        prompt mq "> "
        notifyArrival i
        s <- get
        liftIO $ race_ (runStateInIORefT (server  h i mq) s)
                       (runStateInIORefT (receive h i mq) s)
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = logNotice "talk cleanUp" ("closing the handle for " <> host' <> ".") >> (liftIO . hClose $ h)


adHoc :: MsgQueue -> HostName -> MudStack (Id, Sing)
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
        let pla = Pla True host 80
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
        return (i, e^.sing)


randomSex :: IO Sex
randomSex = newStdGen >>= \g ->
    let (x, _) = randomR (0, 1) g in return $ [ Male, Female ] !! x


randomRace :: IO Race
randomRace = newStdGen >>= \g ->
    let (x, _) = randomR (0, 7) g
    in return $ [ Dwarf .. Vulpenoid ] !! x


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO newStdGen >>= \g ->
    let range                                     = (1, noOfTitles)
        ((T.unpack "title" ++) . show -> fn, _) = randomR range g
    in (try . takeADump $ fn) >>= eitherRet (readFileExHandler "dumpTitle")
  where
    takeADump fn = send mq . nl' =<< (nl <$> (liftIO . T.readFile . (titleDir ++) $ fn))


readFileExHandler :: T.Text -> IOException -> MudStack ()
readFileExHandler fn e
  | isDoesNotExistError e = logIOEx        fn e
  | isPermissionError   e = logIOEx        fn e
  | otherwise             = logIOExRethrow fn e


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


notifyArrival :: Id -> MudStack ()
notifyArrival i = readWSTMVar >>= \ws ->
    let ((^.sing) -> s) = (ws^.entTbl) ! i
    in bcastOthersInRm i . nlnl $ mkSerializedNonStdDesig i ws s A <> " has arrived in the game."


mkSerializedNonStdDesig :: Id -> WorldState -> Sing -> AOrThe -> T.Text
mkSerializedNonStdDesig i ws s (capitalize . pp -> aot) | (pp -> s', pp -> r) <- getSexRace i ws =
    serialize NonStdDesig { nonStdPCEntSing = s
                          , nonStdDesc      = T.concat [ aot, " ", s', " ", r ] }


server :: Handle -> Id -> MsgQueue -> MudStack ()
server h i mq = (registerThread . Server $ i) >> loop `catch` serverExHandler i
  where
    loop = (liftIO . atomically . readTQueue $ mq) >>= \case
      FromServer msg -> (liftIO . T.hPutStr h $ msg) >> loop
      FromClient msg -> let msg' = T.strip . T.pack . stripTelnet . T.unpack $ msg
                        in unless (T.null msg') (handleInp i mq msg') >> loop
      Prompt     p   -> sendPrompt h p >> loop
      Quit           -> cowbye h       >> handleEgress i
      Boot           -> boot   h       >> handleEgress i
      Dropped        ->                   handleEgress i
      Shutdown       -> shutDown       >> loop
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


sendPrompt :: Handle -> T.Text -> MudStack ()
sendPrompt h = liftIO . T.hPutStr h . nl


cowbye :: Handle -> MudStack ()
cowbye h = takeADump `catch` readFileExHandler "cowbye"
  where
    takeADump = liftIO . T.hPutStr h =<< nl <$> (liftIO . T.readFile . (miscDir ++) $ "cowbye")


-- TODO: Make a wizard command that does this.
boot :: Handle -> MudStack ()
boot h = liftIO . T.hPutStr h . nl $ "You have been booted from CurryMUD. Goodbye!"


shutDown :: MudStack ()
shutDown = massMsg StopThread >> commitSuicide
  where
    commitSuicide = do
        liftIO . void . forkIO . mapM_ wait =<< M.elems <$> readTMVarInNWS talkAsyncTblTMVar
        liftIO . killThread =<< getListenThreadId


receive :: Handle -> Id -> MsgQueue -> MudStack ()
receive h i mq = (registerThread . Receive $ i) >> loop `catch` receiveExHandler i
  where
    loop = (liftIO . hIsEOF $ h) >>= \case
      True  -> do
          logPla "receive" i "connection dropped."
          liftIO . atomically . writeTQueue mq $ Dropped
      False -> do
          liftIO $ atomically . writeTQueue mq . FromClient . T.pack . remDelimiters =<< hGetLine h
          loop
    remDelimiters                         = foldr helper ""
    helper c acc | c `notElem` delimiters = c : acc
                 | otherwise              = acc
    delimiters                            = [ stdDesigDelimiter, nonStdDesigDelimiter, desigDelimiter ]


receiveExHandler :: Id -> SomeException -> MudStack ()
receiveExHandler = plaThreadExHandler "receive"


handleInp :: Id -> MsgQueue -> T.Text -> MudStack ()
handleInp i mq = maybeVoid (dispatch i mq) . splitInp


type Input = (CmdName, Rest)


splitInp :: T.Text -> Maybe Input
splitInp = splitIt . T.words
  where
    splitIt [] = Nothing
    splitIt xs = Just . headTail $ xs


dispatch :: Id -> MsgQueue -> Input -> MudStack ()
dispatch i mq (cn, rest) = do
    cols <- getPlaColumns i
    findAction i cn >>= maybe sorry (\act -> act (i, mq, cols) rest)
    prompt mq "> "
  where
    sorry = send mq . nlnl $ "What?"


findAction :: Id -> CmdName -> MudStack (Maybe Action)
findAction i (T.toLower -> cn) = readWSTMVar >>= \ws ->
    readTMVarInNWS plaTblTMVar >>= \((! i) -> p) ->
        let ((^.rmId) -> ri) = (ws^.pcTbl) ! i
            r                = (ws^.rmTbl) ! ri
            cmds             = mkCmdListWithNonStdRmLinks r ++
                               (if p^.isWiz then wizCmds   else []) ++
                               (if isDebug  then debugCmds else [])
        in maybe (return Nothing)
                 (\fn -> return . Just . findActionForFullName fn $ cmds)
                 (findFullNameForAbbrev cn [ cmdName cmd | cmd <- cmds ])
  where
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd] -- TODO: Should these be sorted?
mkCmdListWithNonStdRmLinks ((^.rmLinks) -> rls) = plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


isNonStdLink :: RmLink -> Bool
isNonStdLink (NonStdLink {}) = True
isNonStdLink _               = False


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) = Cmd { cmdName = cn, action = go cn, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> T.Text
mkCmdNameForRmLink rl = T.toLower $ case rl of (StdLink    dir _    ) -> linkDirToCmdName dir
                                               (NonStdLink ln  _ _ _) -> ln


linkDirToCmdName :: LinkDir -> CmdName
linkDirToCmdName North     = "n"
linkDirToCmdName Northeast = "ne"
linkDirToCmdName East      = "e"
linkDirToCmdName Southeast = "se"
linkDirToCmdName South     = "s"
linkDirToCmdName Southwest = "sw"
linkDirToCmdName West      = "w"
linkDirToCmdName Northwest = "nw"
linkDirToCmdName Up        = "u"
linkDirToCmdName Down      = "d"


-- ==================================================
-- Player commands:


about :: Action
about (i, mq, cols) [] = do
    logPlaExec "about" i
    try helper >>= eitherRet (\e -> readFileExHandler "about" e >> sendGenericErrorMsg mq cols)
  where
    helper = send mq . nl . T.unlines . concatMap (wordWrap cols) . T.lines =<< (liftIO . T.readFile . (miscDir ++) $ "about")
about mic@(_, mq, cols) rs = ignore mq cols rs >> about mic []


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = send mq . nl . T.unlines . wordWrap cols $ genericErrorMsg


ignore :: MsgQueue -> Cols -> Rest -> MudStack ()
ignore mq cols (dblQuote . T.unwords -> ignored) =
    send mq . T.unlines . wordWrap cols . parensQuote $ "Ignoring " <> ignored <> "..."


-----


-- TODO: Automatically execute this cmd after a user authenticates. (We won't want to log anything in that case.)
motd :: Action
motd     (i, mq, cols) [] = logPlaExec "motd" i >> (send mq =<< getMotdTxt cols)
motd mic@(_, mq, cols) rs = ignore mq cols rs >> motd mic []


getMotdTxt :: Cols -> MudStack T.Text
getMotdTxt cols = (try . liftIO $ helper) >>= eitherRet (\e -> readFileExHandler "getMotdTxt" e >> (return . nl . T.unlines . wordWrap cols $ "Unfortunately, the message of the day could not be retrieved."))
  where
    helper  = return . frame cols . T.unlines . concatMap (wordWrap cols) . T.lines =<< (T.readFile . (miscDir ++) $ "motd")


-----


plaDispCmdList :: Action
plaDispCmdList imc@(i, _, _) rs = logPlaExecArgs "?" rs i >> dispCmdList (cmdPred Nothing) imc rs


dispCmdList :: (Cmd -> Bool) -> Action
dispCmdList p (_, mq, cols) [] = send mq . nl . T.unlines . concatMap (wordWrapIndent 10 cols) . cmdListText $ p
dispCmdList p (_, mq, cols) (nub . map T.toLower -> rs) | matches <- [ grepTextList r . cmdListText $ p | r <- rs ] =
    send mq . nl . T.unlines . concatMap (wordWrapIndent 10 cols) . intercalate [""] $ matches


cmdListText :: (Cmd -> Bool) -> [T.Text]
cmdListText p = sort . T.lines . T.concat . foldl' helper [] . filter p $ allCmds
  where
    helper acc c | cmdTxt <- nl $ (padOrTrunc 10 . cmdName $ c) <> cmdDesc c = cmdTxt : acc


cmdPred :: Maybe Char -> Cmd -> Bool
cmdPred mc (T.head . cmdName -> p) = case mc of (Just c) -> c == p
                                                Nothing  -> p `notElem` [ wizCmdChar, debugCmdChar ]


-----


help :: Action
help (i, mq, cols) [] = do
    try helper >>= eitherRet (\e -> readFileExHandler "help" e >> sendGenericErrorMsg mq cols)
    logPla "help" i "read the root help file."
  where
    helper   = send mq . nl . T.unlines . concat . wordWrapLines cols . T.lines =<< readRoot
    readRoot = liftIO . T.readFile . (helpDir ++) $ "root"
help (i, mq, cols) (nub . map T.toLower -> rs) =
    send mq . nl . T.unlines . intercalate [ "", mkDividerTxt cols, "" ] =<< getTopics
  where
    getTopics = mapM (\r -> concat . wordWrapLines cols . T.lines <$> getHelpTopicByName i cols r) rs


type HelpTopic = T.Text


getHelpTopicByName :: Id -> Cols -> HelpTopic -> MudStack T.Text
getHelpTopicByName i cols r = (liftIO . getDirectoryContents $ helpDir) >>= \(getTopics -> topics) ->
    maybe sorry
          (\t -> logPla "getHelpTopicByName" i ("read help on " <> dblQuote t <> ".") >> getHelpTopic t)
          (findFullNameForAbbrev r topics)
  where
    getTopics       = (^..folded.packed) . drop 2 . sort . delete "root"
    sorry           = return $ "No help is available on " <> dblQuote r <> "."
    getHelpTopic t  = (try . helper $ t) >>= eitherRet (\e -> readFileExHandler "getHelpTopicByName" e >> (return . T.unlines . wordWrap cols $ "Unfortunately, the " <> dblQuote t <> " help file could not be retrieved."))
    helper          = liftIO . T.readFile . (helpDir ++) . T.unpack


-----


go :: T.Text -> Action
go dir imc [] = goDispatcher imc [dir]
go dir imc rs = goDispatcher imc . (dir :) $ rs


goDispatcher :: Action
goDispatcher _   [] = return ()
goDispatcher imc rs = mapM_ (tryMove imc) rs


tryMove :: IdMsgQueueCols -> T.Text -> MudStack ()
tryMove imc@(i, mq, cols) (T.toLower -> dir) = helper >>= \case
  Left  msg          -> send mq . nl . T.unlines . wordWrap cols $ msg
  Right (logMsg, bs) -> bcast bs >> logPla "tryMove" i logMsg >> look imc []
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s) = (ws^.entTbl) ! i
            p               = (ws^.pcTbl)  ! i
            ri              = p^.rmId
            r               = (ws^.rmTbl)  ! ri
            ris             = (ws^.invTbl) ! ri
        in case findExit r dir of
          Nothing                       -> putTMVar t ws >> (return . Left $ sorry)
          Just (linkTxt, ri', mom, mdm) ->
              let p'          = p & rmId .~ ri'
                  r'          = (ws^.rmTbl)  ! ri'
                  originIs    = i `delete` ris
                  destIs      = (ws^.invTbl) ! ri'
                  destIs'     = sortInv ws $ destIs ++ [i]
                  originPis   = findPCIds ws originIs
                  destPis     = findPCIds ws destIs
                  msgAtOrigin = let d = serialize StdDesig { stdPCEntSing = Just s
                                                           , isCap        = True
                                                           , pcEntName    = mkUnknownPCEntName i ws
                                                           , pcId         = i
                                                           , pcIds        = findPCIds ws ris }
                                in nlnl $ case mom of
                                  Nothing -> T.concat [ d, " ", verb, " ", expandLinkName dir, "." ]
                                  Just f  -> f d
                  msgAtDest   = let d = mkSerializedNonStdDesig i ws s A
                                in nlnl $ case mdm of
                                  Nothing -> T.concat [ d, " arrives from ", expandOppLinkName dir, "." ]
                                  Just f  -> f d
                  logMsg      = T.concat [ "moved "
                                         , linkTxt
                                         , " from room "
                                         , showRm ri r
                                         , " to room "
                                         , showRm ri' r'
                                         , "." ]
              in do
                  putTMVar t (ws & pcTbl.at  i   ?~ p'
                                 & invTbl.at ri  ?~ originIs
                                 & invTbl.at ri' ?~ destIs')
                  return . Right $ (logMsg, [ (msgAtOrigin, originPis), (msgAtDest, destPis) ])
    sorry | dir `elem` stdLinkNames = "You can't go that way."
          | otherwise               = dblQuote dir <> " is not a valid exit."
    verb
      | dir == "u"              = "goes"
      | dir == "d"              = "heads"
      | dir `elem` stdLinkNames = "leaves"
      | otherwise               = "enters"
    showRm ri (parensQuote . (^.rmName) -> rn) = showText ri <> " " <> rn


findExit :: Rm -> LinkName -> Maybe (T.Text, Id, Maybe (T.Text -> T.Text), Maybe (T.Text -> T.Text))
findExit ((^.rmLinks) -> rls) ln =
    case [ (showLink rl, getDestId rl, getOriginMsg rl, getDestMsg rl) | rl <- rls, isValid rl ] of
      [] -> Nothing
      xs -> Just . head $ xs
  where
    isValid      (StdLink    dir _    ) = ln == linkDirToCmdName dir
    isValid      (NonStdLink ln' _ _ _) = ln `T.isPrefixOf` ln'
    showLink     (StdLink    dir _    ) = showText dir
    showLink     (NonStdLink ln' _ _ _) = ln'
    getDestId    (StdLink    _   i    ) = i
    getDestId    (NonStdLink _   i _ _) = i
    getOriginMsg (NonStdLink _   _ f _) = Just f
    getOriginMsg _                      = Nothing
    getDestMsg   (NonStdLink _   _ _ f) = Just f
    getDestMsg   _                      = Nothing


expandLinkName :: T.Text -> T.Text
expandLinkName "n"  = "north"
expandLinkName "ne" = "northeast"
expandLinkName "e"  = "east"
expandLinkName "se" = "southeast"
expandLinkName "s"  = "south"
expandLinkName "sw" = "southwest"
expandLinkName "w"  = "west"
expandLinkName "nw" = "northwest"
expandLinkName "u"  = "up"
expandLinkName "d"  = "down"
expandLinkName x    = patternMatchFail "expandLinkName" [x]


expandOppLinkName :: T.Text -> T.Text
expandOppLinkName "n"  = "the south"
expandOppLinkName "ne" = "the southwest"
expandOppLinkName "e"  = "the west"
expandOppLinkName "se" = "the northwest"
expandOppLinkName "s"  = "the north"
expandOppLinkName "sw" = "the northeast"
expandOppLinkName "w"  = "the east"
expandOppLinkName "nw" = "the southeast"
expandOppLinkName "u"  = "below"
expandOppLinkName "d"  = "above"
expandOppLinkName x    = patternMatchFail "expandOppLinkName" [x]


-----


look :: Action
look (i, mq, cols) [] = readWSTMVar >>= \ws ->
    let ((^.rmId) -> ri) = (ws^.pcTbl) ! i
        r                = (ws^.rmTbl) ! ri
        primary          = T.unlines . concatMap (wordWrap cols) $ [ r^.rmName, r^.rmDesc ]
        suppl            = mkExitsSummary cols r <>  mkRmInvCoinsDesc i cols ws ri
    in send mq . nl $ primary <> suppl
look (i, mq, cols) (nub . map T.toLower -> rs) = helper >>= \case
  (Left  msg, _           ) -> send mq msg
  (Right msg, Nothing     ) -> send mq msg
  (Right msg, Just (d, ds)) ->
      let pis = i `delete` pcIds d
          d'  = serialize d
          f targetDesig acc | targetId <- pcId targetDesig =
              (nlnl $ d' <> " looks at you.", [targetId]) :
              (nlnl . T.concat $ [ d', " looks at ", serialize targetDesig, "." ], targetId `delete` pis) :
              acc
      in do
          bcast . foldr f [] $ ds
          send mq msg
          forM_ [ fromJust . stdPCEntSing $ targetDesig | targetDesig <- ds ] $ \es ->
              logPla "look" i ("looked at " <> es <> ".")
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s ) = (ws^.entTbl)   ! i
            ((^.rmId) -> ri) = (ws^.pcTbl)    ! i
            ris              = (ws^.invTbl)   ! ri
            ris'             = i `delete` ris
            pis              = findPCIds ws ris
            c                = (ws^.coinsTbl) ! ri
            d                = StdDesig { stdPCEntSing = Just s
                                        , isCap        = True
                                        , pcEntName    = mkUnknownPCEntName i ws
                                        , pcId         = i
                                        , pcIds        = pis }
        in if (not . null $ ris') || (c /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames i ws rs ris' c
                   eiss               = zipWith (curry procGecrMisRm) gecrs miss
                   ecs                = map procReconciledCoinsRm rcs
                   invDesc            = foldl' (helperLookEitherInv ws) "" eiss
                   coinsDesc          = foldl' helperLookEitherCoins    "" ecs
                   ds                 = [ let ((^.sing) -> s') = (ws^.entTbl) ! pi
                                          in StdDesig { stdPCEntSing = Just s'
                                                      , isCap        = False
                                                      , pcEntName    = mkUnknownPCEntName pi ws
                                                      , pcId         = pi
                                                      , pcIds        = pis } | pi <- extractPCIdsFromEiss ws eiss ]
               in putTMVar t ws >> return (Right $ invDesc <> coinsDesc, Just (d, ds))
          else    putTMVar t ws >> return ( Left . nl . T.unlines . wordWrap cols $ "You don't see anything here to \
                                                                                    \look at."
                                          , Nothing )
    helperLookEitherInv _  acc (Left  msg ) = (acc <>) . nl . T.unlines . wordWrap cols $ msg
    helperLookEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . nl . T.unlines . concatMap (wordWrap cols) . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c


-- TODO: Consider implementing a color scheme for lists like these such that the least significant characters of each name are highlighted or bolded somehow.
mkRmInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> T.Text
mkRmInvCoinsDesc i cols ws ri =
    let (splitRmInv ws -> ((i `delete`) -> pis, ois)) = (ws^.invTbl) ! ri
        pcDescs    = T.unlines . concatMap (wordWrapIndent 2 cols . mkPCDesc   ) . mkNameCountBothList i ws $ pis
        otherDescs = T.unlines . concatMap (wordWrapIndent 2 cols . mkOtherDesc) . mkNameCountBothList i ws $ ois
        c          = (ws^.coinsTbl) ! ri
    in (if not . null $ pis then pcDescs               else "") <>
       (if not . null $ ois then otherDescs            else "") <>
       (if c /= mempty      then mkCoinsSummary cols c else "")
  where
    mkPCDesc    (bracketQuote -> en, c, (s, _)) | c == 1 = (<> en) . (<> " ") $ if isKnownPCSing s then s else aOrAn s
    mkPCDesc    a                                        = mkOtherDesc a
    mkOtherDesc (bracketQuote -> en, c, (s, _)) | c == 1 = aOrAn s <> " " <> en
    mkOtherDesc (bracketQuote -> en, c, b     )          = T.concat [ showText c, " ", mkPlurFromBoth b, " ", en ]


isKnownPCSing :: Sing -> Bool
isKnownPCSing (T.words -> ss) = case ss of [ "male",   _ ] -> False
                                           [ "female", _ ] -> False
                                           _               -> True


mkNameCountBothList :: Id -> WorldState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList i ws is = let ens   = [ getEffName        i ws i' | i' <- is ]
                                  ebgns = [ getEffBothGramNos i ws i' | i' <- is ]
                                  cs    = mkCountList ebgns
                              in nub . zip3 ens cs $ ebgns


extractPCIdsFromEiss :: WorldState -> [Either T.Text Inv] -> [Id]
extractPCIdsFromEiss ws = foldl' helper []
  where
    helper acc (Left  _ )  = acc
    helper acc (Right is)  = acc ++ findPCIds ws is


mkEntDescs :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntDescs i cols ws is = T.intercalate "\n" . map (mkEntDesc i cols ws) $ [ (ei, (ws^.entTbl) ! ei) | ei <- is ]


mkEntDesc :: Id -> Cols -> WorldState -> (Id, Ent) -> T.Text
mkEntDesc i cols ws (ei@(((ws^.typeTbl) !) -> t), e@(T.unlines . wordWrap cols . (^.entDesc) -> ed)) =
    case t of ConType ->                 (ed <>) . mkInvCoinsDesc i cols ws ei $ e
              MobType ->                 (ed <>) . mkEqDesc       i cols ws ei   e $ t
              PCType  -> (pcHeader <>) . (ed <>) . mkEqDesc       i cols ws ei   e $ t
              _       -> ed
  where
    pcHeader = T.unlines . wordWrap cols . mkPCDescHeader ei $ ws


mkPCDescHeader :: Id -> WorldState -> T.Text
mkPCDescHeader i ws | (pp -> s, pp -> r) <- getSexRace i ws = T.concat [ "You see a ", s, " ", r, "." ]


mkInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> Ent -> T.Text
mkInvCoinsDesc i cols ws ei ((^.sing) -> s) | is <- (ws^.invTbl)   ! ei
                                            , c  <- (ws^.coinsTbl) ! ei = case (not . null $ is, c /= mempty) of
  (False, False) -> T.unlines . wordWrap cols $ if ei == i then dudeYourHandsAreEmpty else "The " <> s <> " is empty."
  (True,  False) -> header <> mkEntsInInvDesc i cols ws is
  (False, True ) -> header <>                                 mkCoinsSummary cols c
  (True,  True ) -> header <> mkEntsInInvDesc i cols ws is <> mkCoinsSummary cols c
  where
    header | ei == i   = nl "You are carrying:"
           | otherwise = T.unlines . wordWrap cols $ "The " <> s <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntsInInvDesc i cols ws = T.unlines . concatMap (wordWrapIndent ind cols . helper) . mkNameCountBothList i ws
  where
    helper (bracketPad ind -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (bracketPad ind -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]
    ind = 11


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols (zip coinNames . mkListFromCoins -> coinsWithNames) = helper coinsWithNames
  where
    helper = T.unlines . wordWrapIndent 2 cols . T.intercalate ", " . filter (not . T.null) . map mkNameAmt
    mkNameAmt (bracketQuote -> cn, a) = if a == 0 then "" else showText a <> " " <> cn


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) =
    T.unlines . intercalate [""] . map (wordWrap cols) . filter (not . T.null) $ [ copDesc, silDesc, golDesc ]
  where -- TODO: Come up with good descriptions.
    copDesc = if cop /= 0 then "The copper piece is round and shiny." else ""
    silDesc = if sil /= 0 then "The silver piece is round and shiny." else ""
    golDesc = if gol /= 0 then "The gold piece is round and shiny."   else ""


-----


exits :: Action
exits (i, mq, cols) [] = readWSTMVar >>= \ws ->
    let ((^.rmId) -> ri) = (ws^.pcTbl) ! i
        r                = (ws^.rmTbl) ! ri
    in logPlaExec "exits" i >> (send mq . nl . mkExitsSummary cols $ r)
exits imc@(_, mq, cols) rs = ignore mq cols rs >> exits imc []


mkExitsSummary :: Cols -> Rm -> T.Text
mkExitsSummary cols ((^.rmLinks) -> rls)
  | stdNames    <- [ rl^.linkDir.to linkDirToCmdName | rl <- rls, not . isNonStdLink $ rl ]
  , customNames <- [ rl^.linkName                    | rl <- rls,       isNonStdLink   rl ]
  = T.unlines . wordWrapIndent 2 cols . ("Obvious exits: " <>) . summarize stdNames $ customNames
  where
    summarize []  []  = "None!"
    summarize std cus = T.intercalate ", " . (std ++) $ cus


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (i, mq, cols) [] = readWSTMVar >>= \ws ->
    send mq . nl . mkInvCoinsDesc i cols ws i $ (ws^.entTbl) ! i
inv (i, mq, cols) (nub . map T.toLower -> rs) = readWSTMVar >>= \ws ->
    let is = (ws^.invTbl)   ! i
        c  = (ws^.coinsTbl) ! i
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames i ws rs is c
               eiss               = zipWith (curry procGecrMisPCInv) gecrs miss
               ecs                = map procReconciledCoinsPCInv rcs
               invDesc            = foldl' (helperEitherInv ws) "" eiss
               coinsDesc          = foldl' helperEitherCoins    "" ecs
           in invDesc <> coinsDesc
      else nl . T.unlines . wordWrap cols $ dudeYourHandsAreEmpty
  where
    helperEitherInv _  acc (Left  msg ) = (acc <>) . nl . T.unlines . wordWrap cols $ msg
    helperEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperEitherCoins  acc (Left  msgs) = (acc <>) . nl . T.unlines . concatMap (wordWrap cols) . intersperse "" $ msgs
    helperEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c


-----


equip :: Action
equip (i, mq, cols) [] = readWSTMVar >>= \ws ->
    let e = (ws^.entTbl) ! i
    in send mq . nl . mkEqDesc i cols ws i e $ PCType
equip (i, mq, cols) (nub . map T.toLower -> rs) = readWSTMVar >>= \ws ->
    let em@(M.elems -> is) = (ws^.eqTbl) ! i
    in send mq $ if not . M.null $ em
      then let (gecrs, miss, rcs)           = resolveEntCoinNames i ws rs is mempty
               eiss                         = zipWith (curry procGecrMisPCEq) gecrs miss
               invDesc                      = foldl' (helperEitherInv ws) "" eiss
               coinsDesc | not . null $ rcs = nl . T.unlines . wordWrap cols $ "You don't have any coins among your \
                                                                               \readied equipment."
                         | otherwise        = ""
           in invDesc <> coinsDesc
      else nl . T.unlines . wordWrap cols $ dudeYou'reNaked
  where
    helperEitherInv _  acc (Left  msg) = (acc <>) . nl . T.unlines . wordWrap cols $ msg
    helperEitherInv ws acc (Right is ) = nl $ acc <> mkEntDescs i cols ws is


mkEqDesc :: Id -> Cols -> WorldState -> Id -> Ent -> Type -> T.Text
mkEqDesc i cols ws ei ((^.sing) -> s) t | descs <- map mkDesc . mkSlotNameIdList . M.toList $ (ws^.eqTbl) ! ei =
    case descs of [] -> none
                  _  -> (header <>) . T.unlines . concatMap (wordWrapIndent 15 cols) $ descs
  where
    mkSlotNameIdList = map (first pp)
    mkDesc (T.breakOn " finger" -> (sn, _), i')
      | e' <- (ws^.entTbl) ! i'
      , en <- if ei == i then (" " <>) . bracketQuote . fromJust $ e'^.entName else ""
      = parensPad 15 sn <> e'^.sing <> en
    none = T.unlines . wordWrap cols $ if
      | ei == i      -> dudeYou'reNaked
      | t  == PCType -> parsePCDesig i ws $ d <> " doesn't have anything readied."
      | otherwise    -> "The " <> s <> " doesn't have anything readied."
    header = T.unlines . wordWrap cols $ if
      | ei == i      -> "You have readied the following equipment:"
      | t  == PCType -> parsePCDesig i ws $ d <> " has readied the following equipment:"
      | otherwise    -> "The " <> s <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig ei ws s The


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


-----


-- TODO: Continue refactoring from here.
getAction :: Action
getAction (_, mq, cols) [] = advise mq cols ["get"] $ "Please specify one or more items to pick up, as \
                                                      \in " <> dblQuote "get sword" <> "."
getAction (i, _, _) rs = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "get" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s) = (ws^.entTbl)   ! i
            p               = (ws^.pcTbl)    ! i
            ri              = p^.rmId
            ris             = (ws^.invTbl)   ! ri
            ris'            = i `delete` ris
            rc              = (ws^.coinsTbl) ! ri
            d               = StdDesig { stdPCEntSing = Just s
                                       , isCap        = True
                                       , pcEntName    = mkUnknownPCEntName i ws
                                       , pcId         = i
                                       , pcIds        = findPCIds ws ris }
        in if (not . null $ ris') || (rc /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws (nub . map T.toLower $ rs) ris' rc
                   eiss                  = zipWith (curry procGecrMisRm) gecrs miss
                   ecs                   = map procReconciledCoinsRm rcs
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Get ri i) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Get ri i) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i "You don't see anything here to pick up.", [])


advise :: MsgQueue -> Cols -> [HelpTopic] -> T.Text -> MudStack ()
advise mq cols []  msg = send mq . nl . T.unlines . wordWrap cols $ msg
advise mq cols [h] msg = send mq . nl . T.unlines . concatMap (wordWrap cols) $ [ msg, "For more information, type " <> (dblQuote . ("help " <>) $ h) <> "." ]
advise mq cols hs  msg = send mq . nl . T.unlines . concatMap (wordWrap cols) $ [ msg, "See also the following help topics: " <> helpTopics <> "." ]
  where
    helpTopics = dblQuote . T.intercalate (dblQuote ", ") $ hs


type FromId = Id
type ToId   = Id


helperGetDropEitherInv :: Id                                  ->
                          PCDesig                             ->
                          GetOrDrop                           ->
                          FromId                              ->
                          ToId                                ->
                          (WorldState, [Broadcast], [T.Text]) ->
                          Either T.Text Inv                   ->
                          (WorldState, [Broadcast], [T.Text])
helperGetDropEitherInv i d god fi ti (ws, bs, logMsgs) = \case
  Left  msg -> (ws, bs ++ mkBroadcast i msg, logMsgs)
  Right is  -> let fis             = (ws^.invTbl) ! fi
                   tis             = (ws^.invTbl) ! ti
                   ws'             = ws & invTbl.at fi ?~ deleteFirstOfEach is fis
                                        & invTbl.at ti ?~ sortInv ws (tis ++ is)
                   (bs', logMsgs') = mkGetDropInvDesc i ws' d god is
               in (ws', bs ++ bs', logMsgs ++ logMsgs')


mkGetDropInvDesc :: Id -> WorldState -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i ws d god is = let bs = concatMap helper . mkNameCountBothList i ws $ is
                                 in (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _))
      | c == 1 = [ (T.concat [ "You ",           mkGodVerb god SndPer, " the ", s, "." ], [i])
                 , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " a ",   s, "." ], otherPCIds) ]
    helper (_, c, b) =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, rest ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, rest ], otherPCIds) ]
      where
        rest = T.concat [ " ", showText c, " ", mkPlurFromBoth b, "." ]
    otherPCIds = i `delete` pcIds d


mkGodVerb :: GetOrDrop -> Verb -> T.Text
mkGodVerb Get  SndPer = "pick up"
mkGodVerb Get  ThrPer = "picks up"
mkGodVerb Drop SndPer = "drop"
mkGodVerb Drop ThrPer = "drops"


extractLogMsgs :: Id -> [Broadcast] -> [T.Text]
extractLogMsgs i bs = [ fst b | b <- bs, snd b == [i] ]


helperGetDropEitherCoins :: Id                                  ->
                            PCDesig                             ->
                            GetOrDrop                           ->
                            FromId                              ->
                            ToId                                ->
                            (WorldState, [Broadcast], [T.Text]) ->
                            Either [T.Text] Coins               ->
                            (WorldState, [Broadcast], [T.Text])
helperGetDropEitherCoins i d god fi ti (ws, bs, logMsgs) = \case
  Left  msgs -> (ws, bs ++ [ (msg, [i]) | msg <- msgs ], logMsgs)
  Right c    -> let fc              = (ws^.coinsTbl) ! fi
                    tc              = (ws^.coinsTbl) ! ti
                    ws'             = ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                         & coinsTbl.at ti ?~ tc <> c
                    (bs', logMsgs') = mkGetDropCoinsDesc i d god c
                in (ws', bs ++ bs', logMsgs ++ logMsgs')


mkGetDropCoinsDesc :: Id -> PCDesig -> GetOrDrop -> Coins -> ([Broadcast], [T.Text])
mkGetDropCoinsDesc i d god (Coins (cop, sil, gol)) = let bs = concat . catMaybes $ [ c, s, g ]
                                                     in (bs, extractLogMsgs i bs)
  where
    c = if cop /= 0 then Just . helper cop $ "copper piece" else Nothing
    s = if sil /= 0 then Just . helper sil $ "silver piece" else Nothing
    g = if gol /= 0 then Just . helper gol $ "gold piece"   else Nothing
    helper a cn | a == 1 = [ (T.concat [ "You ",           mkGodVerb god SndPer, " a ", cn, "." ], [i])
                           , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " a ", cn, "." ], otherPCIds) ]
    helper a cn          = [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", showText a, " ", cn, "s." ], [i])
                           , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", showText a, " ", cn, "s." ], otherPCIds) ]
    otherPCIds           = i `delete` pcIds d


-----


dropAction :: Action
dropAction (_, mq, cols) [] = advise mq cols ["drop"] $ "Please specify one or more things to drop, as \
                                                        \in " <> dblQuote "drop sword" <> "."
dropAction (i, _, _) rs = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "drop" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s) = (ws^.entTbl)   ! i
            p               = (ws^.pcTbl)    ! i
            pis             = (ws^.invTbl)   ! i
            pc              = (ws^.coinsTbl) ! i
            ri              = p^.rmId
            ris             = (ws^.invTbl)   ! ri
            d               = StdDesig { stdPCEntSing = Just s
                                       , isCap        = True
                                       , pcEntName    = mkUnknownPCEntName i ws
                                       , pcId         = i
                                       , pcIds        = findPCIds ws ris }
        in if (not . null $ pis) || (pc /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws (nub . map T.toLower $ rs) pis pc
                   eiss                  = zipWith (curry procGecrMisPCInv) gecrs miss
                   ecs                   = map procReconciledCoinsPCInv rcs
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Drop i ri) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Drop i ri) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else do
              putTMVar t ws
              return (mkBroadcast i dudeYourHandsAreEmpty, [])


-----


putAction :: Action
putAction (_, mq, cols) []  = advise mq cols ["put"] $ "Please specify one or more things you want to put, followed by where \
                                                       \you want to put them, as in " <> dblQuote "put doll sack" <> "."
putAction (_, mq, cols) [r] = advise mq cols ["put"] $ "Please also specify where you want to put it, as \
                                                       \in " <> dblQuote ("put " <> r <> " sack") <> "."
putAction (i, _, _) rs  = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "put" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let ((^.sing) -> s) = (ws^.entTbl)   ! i
          p               = (ws^.pcTbl)    ! i
          pis             = (ws^.invTbl)   ! i
          pc              = (ws^.coinsTbl) ! i
          ri              = p^.rmId
          ris             = (ws^.invTbl)   ! ri
          ris'            = i `delete` ris
          rc              = (ws^.coinsTbl) ! ri
          rs'             = map T.toLower rs
          cn              = last rs'
          rs''            = case rs' of [_, _] -> rs'
                                        _      -> (++ [cn]) . nub . init $ rs'
          restWithoutCon  = init rs''
          d               = StdDesig { stdPCEntSing = Just s
                                     , isCap        = True
                                     , pcEntName    = mkUnknownPCEntName i ws
                                     , pcId         = i
                                     , pcIds        = findPCIds ws ris }
      in if (not . null $ pis) || (pc /= mempty)
        then if T.head cn == rmChar && cn /= T.pack [rmChar]
          then if not . null $ ris'
            then shufflePut i (t, ws) d (T.tail cn) True restWithoutCon ris' rc pis pc procGecrMisRm
            else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
          else shufflePut i (t, ws) d cn False restWithoutCon pis pc pis pc procGecrMisPCInv
      else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])


type IsConInRm    = Bool
type InvWithCon   = Inv
type CoinsWithCon = Coins
type PCInv        = Inv
type PCCoins      = Coins


shufflePut :: Id                                                  ->
              (TMVar WorldState, WorldState)                      ->
              PCDesig                                             ->
              ConName                                             ->
              IsConInRm                                           ->
              Rest                                                ->
              InvWithCon                                          ->
              CoinsWithCon                                        ->
              PCInv                                               ->
              PCCoins                                             ->
              ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) ->
              STM ([Broadcast], [T.Text])
shufflePut i (t, ws) d cn icir rs is c pis pc f =
    let (gecrs, miss, rcs) = resolveEntCoinNames i ws [cn] is c
    in if null miss && (not . null $ rcs)
      then putTMVar t ws >> return (mkBroadcast i "You can't put something inside a coin.", [])
      else case f . head . zip gecrs $ miss of
        Left  msg  -> putTMVar t ws >> return (mkBroadcast i msg, [])
        Right [ci] ->
            let e  = (ws^.entTbl)  ! ci
                t' = (ws^.typeTbl) ! ci
            in if t' /= ConType
              then putTMVar t ws >> return (mkBroadcast i $ "The " <> e^.sing <> " isn't a container.", [])
              else let (gecrs', miss', rcs') = resolveEntCoinNames i ws rs pis pc
                       eiss                  = zipWith (curry procGecrMisPCInv) gecrs' miss'
                       ecs                   = map procReconciledCoinsPCInv rcs'
                       mnom                  = mkMaybeNthOfM icir ws ci e is
                       (ws',  bs,  logMsgs ) = foldl' (helperPutRemEitherInv   i d Put mnom i ci e) (ws,  [], []     ) eiss
                       (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Put mnom i ci e) (ws', bs, logMsgs) ecs
                   in putTMVar t ws'' >> return (bs', logMsgs')
        Right _    -> putTMVar t ws   >> return (mkBroadcast i "You can only put things into one container at a time.", [])


type NthOfM = (Int, Int)


mkMaybeNthOfM :: IsConInRm -> WorldState -> Id -> Ent -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM False _  _ _ _  = Nothing
mkMaybeNthOfM True  ws i ((^.sing) -> s) is = Just ((+ 1) . fromJust . elemIndex i $ matches, length matches)
  where
    matches = filter (\i' -> let ((^.sing) -> s') = (ws^.entTbl) ! i' in s' == s) is


type ToEnt  = Ent


helperPutRemEitherInv :: Id                                  ->
                         PCDesig                             ->
                         PutOrRem                            ->
                         Maybe NthOfM                        ->
                         FromId                              ->
                         ToId                                ->
                         ToEnt                               ->
                         (WorldState, [Broadcast], [T.Text]) ->
                         Either T.Text Inv                   ->
                         (WorldState, [Broadcast], [T.Text])
helperPutRemEitherInv i d por mnom fi ti te (ws, bs, logMsgs) = \case
  Left  msg -> (ws, bs ++ mkBroadcast i msg, logMsgs)
  Right is  -> let (is', bs')       = if ti `elem` is
                                        then (filter (/= ti) is, bs ++ [sorry])
                                        else (is, bs)
                   fis              = (ws^.invTbl) ! fi
                   tis              = (ws^.invTbl) ! ti
                   ws'              = ws & invTbl.at fi ?~ deleteFirstOfEach is' fis
                                         & invTbl.at ti ?~ (sortInv ws . (tis ++) $ is')
                   (bs'', logMsgs') = mkPutRemInvDesc i ws' d por mnom is' te
               in (ws', bs' ++ bs'', logMsgs ++ logMsgs')
  where
    sorry = ("You can't put the " <> te^.sing <> " inside itself.", [i])


mkPutRemInvDesc :: Id -> WorldState -> PCDesig -> PutOrRem -> Maybe NthOfM -> Inv -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemInvDesc i ws d por mnom is ((^.sing) -> ts) = let bs = concatMap helper . mkNameCountBothList i ws $ is
                                                      in (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _)) | c == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , mkArticle
                    , s
                    , " "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " a "
                    , s
                    , " "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
      where
        mkArticle | por == Put = " the "
                  | otherwise  = " a "
    helper (_, c, b) =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
    rest       = T.concat [ " ", ts, onTheGround mnom, "."  ]
    otherPCIds = i `delete` pcIds d


mkPorVerb :: PutOrRem -> Verb -> T.Text
mkPorVerb Put SndPer = "put"
mkPorVerb Put ThrPer = "puts"
mkPorVerb Rem SndPer = "remove"
mkPorVerb Rem ThrPer = "removes"


mkPorPrep :: PutOrRem -> Verb -> Maybe NthOfM -> T.Text
mkPorPrep Put SndPer Nothing       = "in the"
mkPorPrep Put SndPer (Just (n, m)) = "in the"   <> descNthOfM n m
mkPorPrep Rem SndPer Nothing       = "from the"
mkPorPrep Rem SndPer (Just (n, m)) = "from the" <> descNthOfM n m
mkPorPrep Put ThrPer Nothing       = "in a"
mkPorPrep Put ThrPer (Just (n, m)) = "in the"   <> descNthOfM n m
mkPorPrep Rem ThrPer Nothing       = "from a"
mkPorPrep Rem ThrPer (Just (n, m)) = "from the" <> descNthOfM n m


descNthOfM :: Int -> Int -> T.Text
descNthOfM 1 1 = ""
descNthOfM n _ = " " <> mkOrdinal n


onTheGround :: Maybe NthOfM -> T.Text
onTheGround Nothing = ""
onTheGround _       = " on the ground"


helperPutRemEitherCoins :: Id                                  ->
                           PCDesig                             ->
                           PutOrRem                            ->
                           Maybe NthOfM                        ->
                           FromId                              ->
                           ToId                                ->
                           ToEnt                               ->
                           (WorldState, [Broadcast], [T.Text]) ->
                           Either [T.Text] Coins               ->
                           (WorldState, [Broadcast], [T.Text])
helperPutRemEitherCoins i d por mnom fi ti te (ws, bs, logMsgs) = \case
  Left  msgs -> (ws, bs ++ [ (msg, [i]) | msg <- msgs ], logMsgs)
  Right c    -> let fc              = (ws^.coinsTbl) ! fi
                    tc              = (ws^.coinsTbl) ! ti
                    ws'             = ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                         & coinsTbl.at ti ?~ tc <> c
                    (bs', logMsgs') = mkPutRemCoinsDescs i d por mnom c te
                in (ws', bs ++ bs', logMsgs ++ logMsgs')


mkPutRemCoinsDescs :: Id -> PCDesig -> PutOrRem -> Maybe NthOfM -> Coins -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemCoinsDescs i d por mnom (Coins (cop, sil, gol)) ((^.sing) -> ts) = let bs = concat . catMaybes $ [ c, s, g ]
                                                                           in (bs, extractLogMsgs i bs)
  where
    c = if cop /= 0 then Just . helper cop $ "copper piece" else Nothing
    s = if sil /= 0 then Just . helper sil $ "silver piece" else Nothing
    g = if gol /= 0 then Just . helper gol $ "gold piece"   else Nothing
    helper a cn | a == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " a "
                    , cn
                    , " "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " a "
                    , cn
                    , " "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText a
                    , " "
                    , cn
                    , "s "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , showText a
                    , " "
                    , cn
                    , "s "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
    rest       = T.concat [ " ", ts, onTheGround mnom, "." ]
    otherPCIds = i `delete` pcIds d


-----


remove :: Action
remove (_, mq, cols) []  = advise mq cols ["remove"] $ "Please specify one or more things to remove, followed by the \
                                                       \container you want to remove them from, as in " <> dblQuote "remove \
                                                       \doll sack" <> "."
remove (_, mq, cols) [r] = advise mq cols ["remove"] $ "Please also specify the container you want to remove it from, as \
                                                       \in " <> dblQuote ("remove " <> r <> " sack") <> "."
remove (i, _, _) rs  = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "remove" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let ((^.sing) -> s) = (ws^.entTbl)   ! i
          p               = (ws^.pcTbl)    ! i
          pis             = (ws^.invTbl)   ! i
          pc              = (ws^.coinsTbl) ! i
          ri              = p^.rmId
          ris             = (ws^.invTbl)   ! ri
          ris'            = i `delete` ris
          rc              = (ws^.coinsTbl) ! ri
          rs'             = map T.toLower rs
          cn              = last rs'
          rs''            = case rs' of [_, _] -> rs'
                                        _      -> (++ [cn]) . nub . init $ rs'
          restWithoutCon  = init rs''
          d               = StdDesig { stdPCEntSing = Just s
                                     , isCap        = True
                                     , pcEntName    = mkUnknownPCEntName i ws
                                     , pcId         = i
                                     , pcIds        = findPCIds ws ris }
      in if T.head cn == rmChar && cn /= T.pack [rmChar]
          then if not . null $ ris'
            then shuffleRem i (t, ws) d (T.tail cn) True restWithoutCon ris' rc procGecrMisRm
            else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
          else shuffleRem i (t, ws) d cn False restWithoutCon pis pc procGecrMisPCInv


shuffleRem :: Id                                                  ->
              (TMVar WorldState, WorldState)                      ->
              PCDesig                                             ->
              ConName                                             ->
              IsConInRm                                           ->
              Rest                                                ->
              InvWithCon                                          ->
              CoinsWithCon                                        ->
              ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) ->
              STM ([Broadcast], [T.Text])
shuffleRem i (t, ws) d cn icir rs is c f =
    let (gecrs, miss, rcs) = resolveEntCoinNames i ws [cn] is c
    in if null miss && (not . null $ rcs)
      then putTMVar t ws >> return (mkBroadcast i "You can't remove something from a coin.", [])
      else case f . head . zip gecrs $ miss of
        Left  msg  -> putTMVar t ws >> return (mkBroadcast i msg, [])
        Right [ci] ->
            let e@((^.sing) -> s)  = (ws^.entTbl)  ! ci
                t'                 = (ws^.typeTbl) ! ci
            in if t' /= ConType
              then putTMVar t ws >> return (mkBroadcast i $ "The " <> s <> " isn't a container.", [])
              else let cis                   = (ws^.invTbl)   ! ci
                       cc                    = (ws^.coinsTbl) ! ci
                       (gecrs', miss', rcs') = resolveEntCoinNames i ws rs cis cc
                       eiss                  = map (procGecrMisCon s) . zip gecrs' $ miss'
                       ecs                   = map (procReconciledCoinsCon s) rcs'
                       mnom                  = mkMaybeNthOfM icir ws ci e is
                       (ws',  bs,  logMsgs)  = foldl' (helperPutRemEitherInv   i d Rem mnom ci i e) (ws,  [], []     ) eiss
                       (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Rem mnom ci i e) (ws', bs, logMsgs) ecs
                   in putTMVar t ws'' >> return (bs', logMsgs')
        Right _    -> putTMVar t ws   >> return (mkBroadcast i "You can only remove things from one container at a time.", [])


-----


ready :: Action
ready (_, mq, cols) [] = advise mq cols ["ready"] $ "Please specify one or more things to ready, as in " <> dblQuote "ready \
                                                    \sword" <> "."
ready (i, mq, cols) rs = do
    (msg, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "ready" i logMsgs
    send mq . nl $ msg
  where
    helper = onWS $ \(t, ws) ->
        let is = (ws^.invTbl)   ! i
            c  = (ws^.coinsTbl) ! i
        in if (not . null $ is) || (c /= mempty)
          then let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols i ws (nub . map T.toLower $ rs) is mempty
                   eiss                      = zipWith (curry procGecrMisReady) gecrs miss
                   msg                       = if null rcs then "" else nl "You can't ready coins."
                   (ws',  msg', logMsgs)     = foldl' (helperReady i cols) (ws, msg, []) . zip eiss $ mrols
               in putTMVar t ws' >> return (msg', logMsgs)
          else putTMVar t ws >> return (T.unlines . wordWrap cols $ dudeYourHandsAreEmpty, [])


helperReady :: Id                                     ->
               Cols                                   ->
               (WorldState, T.Text, [T.Text])         ->
               (Either T.Text Inv, Maybe RightOrLeft) ->
               (WorldState, T.Text, [T.Text])
helperReady i cols (ws, msg, logMsgs) (eis, mrol) = case eis of
  Left  msg' -> (ws, (msg <>) . T.unlines . wordWrap cols $ msg', logMsgs)
  Right is   -> foldl' (readyDispatcher i cols mrol) (ws, msg, logMsgs) is


readyDispatcher :: Id -> Cols -> Maybe RightOrLeft -> (WorldState, T.Text, [T.Text]) -> Id -> (WorldState, T.Text, [T.Text])
readyDispatcher i cols mrol (ws, msg, logMsgs) ei =
    let e = (ws^.entTbl)  ! ei
        t = (ws^.typeTbl) ! ei
    in case t of
      ClothType -> readyCloth i cols mrol (ws, msg, logMsgs) ei e
      WpnType   -> readyWpn   i cols mrol (ws, msg, logMsgs) ei e
      _         -> (ws, (msg <>) . T.unlines . wordWrap cols $ "You can't ready a " <> e^.sing <> ".", logMsgs)


moveReadiedItem :: Id                             ->
                   Cols                           ->
                   (WorldState, T.Text, [T.Text]) ->
                   EqMap                          ->
                   Slot                           ->
                   Id                             ->
                   T.Text                         ->
                   (WorldState, T.Text, [T.Text])
moveReadiedItem i cols (ws, msg, logMsgs) em s ei msg' =
    let is  = (ws^.invTbl) ! i
        ws' = ws & invTbl.at i ?~ filter (/= ei) is
                 & eqTbl.at  i ?~ (em & at s ?~ ei)
    in (ws', (msg <>) . T.unlines . wordWrap cols $ msg', logMsgs ++ [msg'])


-- Helpers for the entity type-specific ready functions:


otherSex :: Sex -> Sex
otherSex Male   = Female
otherSex Female = Male
otherSex NoSex  = NoSex


otherHand :: Hand -> Hand
otherHand RHand  = LHand
otherHand LHand  = RHand
otherHand NoHand = NoHand


isRingRol :: RightOrLeft -> Bool
isRingRol = \case R -> False
                  L -> False
                  _ -> True


rEarSlots, lEarSlots, noseSlots, neckSlots, rWristSlots, lWristSlots :: [Slot]
rEarSlots   = [ REar1S, REar2S       ]
lEarSlots   = [ LEar1S, LEar2S       ]
noseSlots   = [ Nose1S, Nose2S       ]
neckSlots   = [ Neck1S   .. Neck3S   ]
rWristSlots = [ RWrist1S .. RWrist3S ]
lWristSlots = [ LWrist1S .. LWrist3S ]


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = em^.at s.to isNothing


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


sorryFullClothSlots :: Cloth -> T.Text
sorryFullClothSlots c = "You can't wear any more " <> whatWhere c
  where
    whatWhere = \case
      EarC      -> aoy <> "ears."
      NoseC     -> "rings on your nose."
      NeckC     -> aoy <> "neck."
      WristC    -> aoy <> "wrists."
      FingerC   -> aoy <> "fingers."
      UpBodyC   -> coy <> "torso."
      LowBodyC  -> coy <> "legs."
      FullBodyC -> "clothing about your body."
      BackC     -> "on your back."
      FeetC     -> "footwear on your feet."
    aoy = "accessories on your "
    coy = "clothing on your "


sorryFullClothSlotsOneSide :: Slot -> T.Text
sorryFullClothSlotsOneSide s = "You can't wear any more on your " <> pp s <> "."


-- Ready clothing:


readyCloth :: Id -> Cols -> Maybe RightOrLeft -> (WorldState, T.Text, [T.Text]) -> Id -> Ent -> (WorldState, T.Text, [T.Text])
readyCloth i cols mrol (ws, msg, logMsgs) ei e@((^.sing) -> s) =
    let em = (ws^.eqTbl)    ! i
        c  = (ws^.clothTbl) ! ei
    in case maybe (getAvailClothSlot cols ws i c em) (getDesigClothSlot cols ws e c em) mrol of
      Left  msg' -> (ws, msg <> msg', logMsgs)
      Right slot -> moveReadiedItem i cols (ws, msg, logMsgs) em slot ei . mkReadyMsg slot $ c
  where
    mkReadyMsg (pp -> slot) = \case NoseC   -> putOnMsg
                                    NeckC   -> putOnMsg
                                    FingerC -> T.concat [ "You slide the ", s, " on your ", slot, "." ]
                                    _       -> wearMsg
      where
        putOnMsg = "You put on the " <> s <> "."
        wearMsg  = T.concat [ "You wear the ", s, " on your ", slot, "." ]


getAvailClothSlot :: Cols -> WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot cols ws i c em = let m = (ws^.mobTbl) ! i
                                       s = m^.sex
                                       h = m^.hand
                                   in procMaybe $ case c of
                                     EarC    -> getEarSlotForSex s `mplus` (getEarSlotForSex . otherSex $ s)
                                     NoseC   -> findAvailSlot em noseSlots
                                     NeckC   -> findAvailSlot em neckSlots
                                     WristC  -> getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
                                     FingerC -> getRingSlot s h
                                     _       -> undefined -- TODO
  where
    procMaybe             = maybe (Left . T.unlines . wordWrap cols . sorryFullClothSlots $ c) Right
    getEarSlotForSex s    =
        findAvailSlot em $ case s of Male   -> lEarSlots
                                     Female -> rEarSlots
                                     _      -> patternMatchFail "getAvailClothSlot getEarSlotForSex" [ showText s ]
    getWristSlotForHand h =
        findAvailSlot em $ case h of RHand  -> lWristSlots
                                     LHand  -> rWristSlots
                                     _      -> patternMatchFail "getAvailClothSlot getWristSlotForHand" [ showText h ]
    getRingSlot s h       =
        findAvailSlot em $ case s of Male    -> case h of
                                       RHand -> [ LRingFS, LIndexFS, RRingFS, RIndexFS, LMidFS, RMidFS, LPinkyFS, RPinkyFS ]
                                       LHand -> [ RRingFS, RIndexFS, LRingFS, LIndexFS, RMidFS, LMidFS, RPinkyFS, LPinkyFS ]
                                       _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
                                     Female  -> case h of
                                       RHand -> [ LRingFS, LIndexFS, RRingFS, RIndexFS, LPinkyFS, RPinkyFS, LMidFS, RMidFS ]
                                       LHand -> [ RRingFS, RIndexFS, LRingFS, LIndexFS, RPinkyFS, LPinkyFS, RMidFS, LMidFS ]
                                       _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
                                     _       -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText s ]


getDesigClothSlot :: Cols -> WorldState -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot cols ws ((^.sing) -> s) c em rol
  | c `elem` [ NoseC, NeckC, UpBodyC, LowBodyC, FullBodyC, BackC, FeetC ] = Left sorryCantWearThere
  | isRingRol rol, c /= FingerC         = Left sorryCantWearThere
  | c == FingerC, not . isRingRol $ rol = Left . T.unlines . wordWrap cols $ ringHelp
  | otherwise = case c of EarC    -> maybe (Left sorryFullEar)   Right (findSlotFromList rEarSlots   lEarSlots)
                          WristC  -> maybe (Left sorryFullWrist) Right (findSlotFromList rWristSlots lWristSlots)
                          FingerC -> maybe (Right slotFromRol)
                                           (\i -> let e = (ws^.entTbl) ! i in Left . sorry slotFromRol $ e)
                                           (em^.at slotFromRol)
                          _       -> undefined -- TODO
  where
    sorryCantWearThere     = T.unlines . wordWrap cols . T.concat $ [ "You can't wear a ", s, " on your ", pp rol, "." ]
    findSlotFromList rs ls =
        findAvailSlot em $ case rol of R -> rs
                                       L -> ls
                                       _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls =
        head $ case rol of R -> rs
                           L -> ls
                           _ -> patternMatchFail "getDesigClothSlot getSlotFromList" [ showText rol ]
    sorryFullEar   = T.unlines . wordWrap cols . sorryFullClothSlotsOneSide . getSlotFromList rEarSlots   $ lEarSlots
    sorryFullWrist = T.unlines . wordWrap cols . sorryFullClothSlotsOneSide . getSlotFromList rWristSlots $ lWristSlots
    slotFromRol    = fromRol rol :: Slot
    sorry (pp -> slot) ((^.sing) -> s') = T.unlines . wordWrap cols . T.concat $ [ "You're already wearing a ", s', " on your ", slot, "." ]


-- Ready weapons:


readyWpn :: Id -> Cols -> Maybe RightOrLeft -> (WorldState, T.Text, [T.Text]) -> Id -> Ent -> (WorldState, T.Text, [T.Text])
readyWpn i cols mrol (ws, msg, logMsgs) ei e@((^.sing) -> s) =
    let em  = (ws^.eqTbl)  ! i
        w   = (ws^.wpnTbl) ! ei
        sub = w^.wpnSub
    in if not . isSlotAvail em $ BothHandsS
      then (ws, (msg <>) . T.unlines . wordWrap cols $ "You're already wielding a two-handed weapon.", logMsgs)
      else case maybe (getAvailWpnSlot cols ws i em) (getDesigWpnSlot cols ws e em) mrol of
        Left  msg'  -> (ws, msg <> msg', logMsgs)
        Right slot  -> case sub of
          OneHanded -> moveReadiedItem i cols (ws, msg, logMsgs) em slot ei . T.concat $ [ "You wield the "
                                                                                         , s
                                                                                         , " with your "
                                                                                         , pp slot
                                                                                         , "." ]
          TwoHanded -> if all (isSlotAvail em) [ RHandS, LHandS ]
            then moveReadiedItem i cols (ws, msg, logMsgs) em BothHandsS ei $ "You wield the " <> s <> " with both hands."
            else (ws, (msg <>) . T.unlines . wordWrap cols $ "Both hands are required to wield the " <> s <> ".", logMsgs)


getAvailWpnSlot :: Cols -> WorldState -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot cols ws i em = let m = (ws^.mobTbl) ! i
                                   h = m^.hand
                               in maybe (Left . T.unlines . wordWrap cols $ "You're already wielding two weapons.")
                                        Right
                                        (findAvailSlot em . map getSlotForHand $ [ h, otherHand h ])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> patternMatchFail "getAvailWpnSlot getSlotForHand" [ showText h ]


getDesigWpnSlot :: Cols -> WorldState -> Ent -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigWpnSlot cols ws ((^.sing) -> s) em rol
  | isRingRol rol = Left sorryNotRing
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e = (ws^.entTbl) ! i in Left . sorry $ e)
                          (em^.at desigSlot)
  where
    sorryNotRing = T.unlines . wordWrap cols $ "You can't wield a " <> s <> " with your finger!"
    sorry ((^.sing) -> s') = T.unlines . wordWrap cols . T.concat $ [ "You're already wielding a "
                                                                    , s'
                                                                    , " with your "
                                                                    , pp desigSlot
                                                                    , "." ]
    desigSlot    = case rol of R -> RHandS
                               L -> LHandS
                               _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-- Ready armor:


-----


unready :: Action
unready (_, mq, cols) [] = advise mq cols ["unready"] $ "Please specify one or more things to unready, as \
                                                        \in " <> dblQuote "unready sword" <> "."
unready (i, mq, cols) rs = do
    (msg, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "unready" i logMsgs
    send mq . nl $ msg
  where
    helper = onWS $ \(t, ws) ->
        let em = (ws^.eqTbl) ! i
            is = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws (nub . map T.toLower $ rs) is mempty
                   eiss                  = zipWith (curry procGecrMisPCEq) gecrs miss
                   msg                   = if null rcs then "" else nl "You can't unready coins."
                   (ws',  msg', logMsgs) = foldl' (helperUnready i cols) (ws, msg, []) eiss
               in putTMVar t ws' >> return (msg', logMsgs)
          else putTMVar t ws >> return (T.unlines . wordWrap cols $ dudeYou'reNaked, [])


helperUnready :: Id -> Cols -> (WorldState, T.Text, [T.Text]) -> Either T.Text Inv -> (WorldState, T.Text, [T.Text])
helperUnready i cols (ws, msg, logMsgs) = \case
  Left  msg' -> (ws, (msg <>) . T.unlines . wordWrap cols $ msg', logMsgs)
  Right is   -> let em   = (ws^.eqTbl)  ! i
                    pis  = (ws^.invTbl) ! i
                    ws'  = ws & eqTbl.at  i ?~ M.filter (`notElem` is) em
                              & invTbl.at i ?~ (sortInv ws . (pis ++) $ is)
                    msgs = mkUnreadyDescs i ws' is
                in (ws', (msg <>) . T.concat . map (T.unlines . wordWrap cols) $ msgs, logMsgs ++ msgs)


mkUnreadyDescs :: Id -> WorldState -> Inv -> [T.Text]
mkUnreadyDescs i ws is = [ helper icb | icb <- mkIdCountBothList i ws is ]
  where
    helper (i', c, b@(s, _)) = let v = verb i' in T.concat $ if c == 1
      then [ "You ", v, " the ", s, "." ]
      else [ "You ", v, " ", showText c, " ", mkPlurFromBoth b, "." ]
    verb i' = case (ws^.typeTbl) ! i' of
      ClothType -> unwearGenericVerb -- TODO
      WpnType   -> "stop wielding"
      _         -> undefined -- TODO
    unwearGenericVerb = "take off"


mkIdCountBothList :: Id -> WorldState -> Inv -> [(Id, Int, BothGramNos)]
mkIdCountBothList i ws is = let ebgns = [ getEffBothGramNos i ws i' | i' <- is ]
                                cs    = mkCountList ebgns
                            in nubBy equalCountsAndBoths . zip3 is cs $ ebgns
  where
    equalCountsAndBoths (_, c, b) (_, c', b') = c == c' && b == b'


-----


intro :: Action
intro (i, mq, cols) [] = readWSTMVar >>= \ws ->
    let p      = (ws^.pcTbl) ! i
        intros = p^.introduced
    in if null intros
      then do
          let introsTxt = "No one has introduced themselves to you yet."
          send mq . nl . T.unlines . wordWrap cols $ introsTxt
          logPlaOut "intro" i [introsTxt]
      else do
          let introsTxt = T.intercalate ", " intros
          send mq . nl . T.unlines . concatMap (wordWrap cols) $ [ "You know the following names:", introsTxt ]
          logPlaOut "intro" i [introsTxt]
intro (i, _, _) rs = do
    (cbs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "intro" i logMsgs
    bcast . map fromClassifiedBroadcast . sort $ cbs
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s) = (ws^.entTbl)   ! i
            p               = (ws^.pcTbl)    ! i
            ri              = p^.rmId
            is              = (ws^.invTbl)   ! ri
            is'             = i `delete` is
            c               = (ws^.coinsTbl) ! ri
        in if (not . null $ is') || (c /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws (nub . map T.toLower $ rs) is' c
                   eiss                  = zipWith (curry procGecrMisRm) gecrs miss
                   ecs                   = map procReconciledCoinsRm rcs
                   (ws', cbs,  logMsgs ) = foldl' (helperIntroEitherInv s is) (ws, [],  []     ) eiss
                   (     cbs', logMsgs') = foldl' helperIntroEitherCoins      (    cbs, logMsgs) ecs
               in putTMVar t ws' >> return (cbs', logMsgs')
          else do
              putTMVar t ws
              return (mkNTBroadcast i . nlnl $ "You don't see anyone here to introduce yourself to.", [])
    helperIntroEitherInv _ _   a@(ws, cbs, logMsgs) (Left msg)
      | T.null msg = a
      | otherwise  = (ws, (cbs ++) . mkNTBroadcast i . nlnl $ msg, logMsgs)
    helperIntroEitherInv s ris a (Right is) = foldl' tryIntro a is
      where
        tryIntro (ws, cbs, logMsgs) targetId =
            let targetType = (ws^.typeTbl) ! targetId
                targetEnt  = (ws^.entTbl)  ! targetId
                targetSing = targetEnt^.sing
            in case targetType of
              PCType -> let targetPC    = (ws^.pcTbl) ! targetId
                            intros      = targetPC^.introduced
                            pis         = findPCIds ws ris
                            targetDesig = serialize StdDesig { stdPCEntSing = Just targetSing
                                                             , isCap        = False
                                                             , pcEntName    = mkUnknownPCEntName targetId ws
                                                             , pcId         = targetId
                                                             , pcIds        = pis }
                            (mkReflexive . (^.sex) -> himHerself) = (ws^.mobTbl) ! i
                        in if s `elem` intros
                          then let msg = nlnl $ "You've already introduced yourself to " <> targetDesig <> "."
                               in (ws, (cbs ++) . mkNTBroadcast i $ msg, logMsgs)
                          else let p         = targetPC & introduced .~ sort (s : intros)
                                   ws'       = ws & pcTbl.at targetId ?~ p
                                   srcMsg    = nlnl $ "You introduce yourself to " <> targetDesig <> "."
                                   srcDesig  = StdDesig { stdPCEntSing = Nothing
                                                        , isCap        = True
                                                        , pcEntName    = mkUnknownPCEntName i ws
                                                        , pcId         = i
                                                        , pcIds        = pis }
                                   targetMsg = nlnl . T.concat $ [ serialize srcDesig
                                                                 , " introduces "
                                                                 , himHerself
                                                                 , " to you as "
                                                                 , s
                                                                 , "." ]
                                   srcDesig' = srcDesig { stdPCEntSing = Just s }
                                   othersMsg = nlnl . T.concat $ [ serialize srcDesig'
                                                                 , " introduces "
                                                                 , himHerself
                                                                 , " to "
                                                                 , targetDesig
                                                                 , "." ]
                               in ( ws'
                                  , cbs ++ [ NonTargetBroadcast (srcMsg,    [i])
                                           , TargetBroadcast    (targetMsg, [targetId])
                                           , NonTargetBroadcast (othersMsg, deleteFirstOfEach [ i, targetId ] pis) ]
                                  , logMsgs ++ [srcMsg] )
              _      -> let b = NonTargetBroadcast (nlnl $ "You can't introduce yourself to a " <> targetSing <> ".", [i])
                        in (ws, cbs `appendIfUnique` b, logMsgs)
    helperIntroEitherCoins (cbs, logMsgs) (Left  msgs) = ( (cbs ++) . concat $ [ mkNTBroadcast i . nlnl $ msg | msg <- msgs ]
                                                         , logMsgs )
    helperIntroEitherCoins (cbs, logMsgs) (Right _   ) =
        let b = NonTargetBroadcast (nlnl "You can't introduce yourself to a coin.", [i])
        in (cbs `appendIfUnique` b, logMsgs)
    fromClassifiedBroadcast (TargetBroadcast    b) = b
    fromClassifiedBroadcast (NonTargetBroadcast b) = b


mkReflexive :: Sex -> T.Text
mkReflexive Male   = "himself"
mkReflexive Female = "herself"
mkReflexive s      = patternMatchFail "mkReflexive" [ showText s ]


-----


-- TODO: Disambiguate player names.
what :: Action
what (_, mq, cols) [] = advise mq cols ["what"] $ "Please specify one or more abbreviations to disambiguate, as \
                                                  \in " <> dblQuote "what up" <> "."
what (i, mq, cols) rs = readWSTMVar >>= \ws ->
    let p  = (ws^.pcTbl) ! i
        r  = (ws^.rmTbl) ! (p^.rmId)
    in logPlaExecArgs "what" rs i >> (send mq . T.concat . map (helper ws r) . nub . map T.toLower $ rs)
  where
    helper ws r n = nl . T.concat $ [ whatCmd   cols r        n
                                    , whatInv i cols ws PCInv n
                                    , whatInv i cols ws PCEq  n
                                    , whatInv i cols ws RmInv n ]


whatCmd :: Cols -> Rm -> T.Text -> T.Text
whatCmd cols r n = maybe notFound found . findFullNameForAbbrev (T.toLower n) $ cs
  where
    cs       = filter isPlaCmd . map cmdName . mkCmdListWithNonStdRmLinks $ r
    isPlaCmd = (`notElem` [ wizCmdChar, debugCmdChar ]) . T.head
    notFound = T.unlines . wordWrap cols $ dblQuote n <> " doesn't refer to any commands."
    found cn = T.unlines . wordWrap cols . T.concat $ [ dblQuote n, " may refer to the ", dblQuote cn, " command." ]


whatInv :: Id -> Cols -> WorldState -> InvType -> T.Text -> T.Text
whatInv i cols ws it n = let (is, gecrs, rcs) = resolveName
                         in if not . null $ gecrs
                           then whatInvEnts i cols ws it n (head gecrs) is
                           else T.concat . map (whatInvCoins cols it n) $ rcs
  where
    resolveName = let (is, c)         = getLocInvCoins
                      (gecrs, _, rcs) = resolveEntCoinNames i ws [n] is c
                  in (is, gecrs, rcs)
    getLocInvCoins = case it of PCInv -> ((ws^.invTbl) ! i,          (ws^.coinsTbl) ! i )
                                PCEq  -> (M.elems $ (ws^.eqTbl) ! i, mempty             )
                                RmInv -> ((ws^.invTbl) ! ri,         (ws^.coinsTbl) ! ri)
    p  = (ws^.pcTbl) ! i
    ri = p^.rmId


whatInvEnts :: Id -> Cols -> WorldState -> InvType -> T.Text -> GetEntsCoinsRes -> Inv -> T.Text
whatInvEnts i cols ws it r gecr is = case gecr of
  Mult _ n (Just es) _
    | n == acp  -> T.unlines . wordWrap cols . T.concat $ [ dblQuote acp
                                                          , " may refer to everything "
                                                          , getLocTxtForInvType it
                                                          , supplement
                                                          , "." ]
    | otherwise ->
        let e   = head es
            len = length es
        in if len > 1
          then let ebgns  = take len [ getEffBothGramNos i ws (e'^.entId) | e' <- es ]
                   h      = head ebgns
                   target = if all (== h) ebgns then mkPlurFromBoth h else bracketQuote . (<> "s") . getEffName i ws $ e^.entId
               in T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                         , " may refer to the "
                                                         , showText len
                                                         , " "
                                                         , target
                                                         , " "
                                                         , getLocTxtForInvType it
                                                         , "." ]
          else let ens = [ getEffName i ws i' | i' <- is ]
               in T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                         , " may refer to the "
                                                         , T.pack . checkFirst e $ ens
                                                         , e^.sing
                                                         , " "
                                                         , getLocTxtForInvType it
                                                         , "." ]
  Indexed x _ (Right e) -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                                  , " may refer to the "
                                                                  , mkOrdinal x
                                                                  , " "
                                                                  , bracketQuote . getEffName i ws $ e^.entId
                                                                  , " "
                                                                  , e^.sing.to parensQuote
                                                                  , " "
                                                                  , getLocTxtForInvType it
                                                                  , "." ]
  _                     -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                                  , " doesn't refer to anything "
                                                                  , getLocTxtForInvType it
                                                                  , "." ]
  where
    acp                                     = T.pack [allChar]
    supplement | it `elem` [ PCInv, RmInv ] = " " <> parensQuote "including any coins"
               | otherwise                  = ""
    checkFirst e ens                        = let en      = getEffName i ws $ e^.entId
                                                  matches = filter (== en) ens
                                              in guard (length matches > 1) >> T.unpack "first "


getLocTxtForInvType :: InvType -> T.Text
getLocTxtForInvType = \case PCInv -> "in your inventory"
                            PCEq  -> "in your readied equipment"
                            RmInv -> "in this room"


whatInvCoins :: Cols -> InvType -> T.Text -> ReconciledCoins -> T.Text
whatInvCoins cols it r rc
  | it == PCEq = ""
  | otherwise = case rc of
    Left  Empty      -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                               , " doesn't refer to any coins "
                                                               , getLocTxtForInvType it
                                                               , " "
                                                               , supplementNone "coins" it
                                                               , "." ]
    Left  (NoneOf c) -> let cn = mkTxtForCoins c in T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                                                           , " doesn't refer to any "
                                                                                           , cn
                                                                                           , " "
                                                                                           , getLocTxtForInvType it
                                                                                           , " "
                                                                                           , supplementNone cn it
                                                                                           , "." ]
    Left  (SomeOf c) -> let cn = mkTxtForCoins c in T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                                                           , " doesn't refer to any "
                                                                                           , cn
                                                                                           , " "
                                                                                           , getLocTxtForInvType it
                                                                                           , " "
                                                                                           , supplementNotEnough cn it
                                                                                           , "." ]
    Right (SomeOf c) -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r
                                                               , " may refer to the "
                                                               , mkTxtForCoinsWithAmt c
                                                               , " "
                                                               , getLocTxtForInvType it
                                                               , "." ]
    _                -> patternMatchFail "whatInvCoins" [ showText rc ]
  where
    supplementNone cn      = \case PCInv -> parensQuote $ "you don't have any "       <> cn
                                   RmInv -> parensQuote $ "there aren't any "         <> cn <> " here"
                                   PCEq  -> oops "supplementNone"
    supplementNotEnough cn = \case PCInv -> parensQuote $ "you don't have that many " <> cn
                                   RmInv -> parensQuote $ "there aren't that many "   <> cn <> " here"
                                   PCEq  -> oops "supplementNotEnough"
    oops fn                = blowUp ("whatInvCoins " <> fn) "called for InvType of PCEq" []
    mkTxtForCoins c@(Coins (cop, sil, gol))
      | cop /= 0  = "copper pieces"
      | sil /= 0  = "silver pieces"
      | gol /= 0  = "gold pieces"
      | otherwise = blowUp "whatInvCoins mkTxtForCoins" "attempted to make text for empty coins" [ showText c ]
    mkTxtForCoinsWithAmt c@(Coins (cop, sil, gol))
      | cop == 1  = "1 copper piece"
      | cop /= 0  = showText cop <> " copper pieces"
      | sil == 1  = "1 silver piece"
      | sil /= 0  = showText sil <> " silver pieces"
      | gol == 1  = "1 gold piece"
      | gol /= 0  = showText gol <> " gold pieces"
      | otherwise = blowUp "whatInvCoins mkTxtForCoinsWithAmt" "attempted to make text for empty coins" [ showText c ]


-----


uptime :: Action
uptime (i, mq, cols) [] = do
    logPlaExec "uptime" i
    send mq . nl . T.unlines . wordWrap cols =<< uptimeHelper =<< getUptime
uptime imc@(_, mq, cols) rs = ignore mq cols rs >> uptime imc []


uptimeHelper :: Integer -> MudStack T.Text
uptimeHelper ut = helper <$> getRecordUptime
  where
    helper = \case
      Nothing  -> mkUptimeTxt
      Just rut -> case ut `compare` rut of GT -> mkNewRecTxt
                                           _  -> mkRecTxt rut
    mkUptimeTxt  = mkTxtHelper "."
    mkNewRecTxt  = mkTxtHelper " - it's a new record!"
    mkRecTxt rut = mkTxtHelper $ " (record uptime: " <> renderIt rut <> ")."
    mkTxtHelper  = ("Up " <>) . (renderIt ut <>)
    renderIt     = T.pack . renderSecs


getUptime :: MudStack Integer
getUptime = do
    start <- getNWSRec startTime
    now   <- liftIO getCurrentTime
    return . round $ now `diffUTCTime` start


-----


quit :: Action
quit (i, mq, _   ) [] = (liftIO . atomically . writeTQueue mq $ Quit) >> logPlaExec "quit" i
quit (_, mq, cols) _  =
    send mq . nl . T.unlines . wordWrap cols $ "Type " <> dblQuote "quit" <> " with no arguments to quit the game."


handleEgress :: Id -> MudStack ()
handleEgress i = do
    notifyEgress i
    wsTMVar  <- getWSTMVar
    mqtTMVar <- getNWSRec msgQueueTblTMVar
    ptTMVar  <- getNWSRec plaTblTMVar
    n <- liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        mqt <- takeTMVar mqtTMVar
        pt  <- takeTMVar ptTMVar
        -----
        let ((^.sing) -> s) = (ws^.entTbl) ! i
        let p               = (ws^.pcTbl)  ! i
        let ri              = p^.rmId
        let ris             = (ws^.invTbl) ! ri
        let ws'             = ws  & typeTbl.at  i  .~ Nothing
                                  & entTbl.at   i  .~ Nothing
                                  & invTbl.at   i  .~ Nothing
                                  & coinsTbl.at i  .~ Nothing
                                  & eqTbl.at    i  .~ Nothing
                                  & mobTbl.at   i  .~ Nothing
                                  & pcTbl.at    i  .~ Nothing
                                  & invTbl.at   ri ?~ i `delete` ris
        let mqt' = mqt & at i .~ Nothing
        let pt'  = pt  & at i .~ Nothing
        -----
        putTMVar wsTMVar  ws'
        putTMVar mqtTMVar mqt'
        putTMVar ptTMVar  pt'
        return s
    logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", parensQuote n, " has left the game." ]
    closePlaLog i


notifyEgress :: Id -> MudStack ()
notifyEgress i = readWSTMVar >>= \ws ->
    let ((^.sing) -> s) = (ws^.entTbl) ! i
        p               = (ws^.pcTbl)  ! i
        ri              = p^.rmId
        is              = (ws^.invTbl) ! ri
        pis             = findPCIds ws is
        d               = serialize StdDesig { stdPCEntSing = Just s
                                             , isCap        = True
                                             , pcEntName    = mkUnknownPCEntName i ws
                                             , pcId         = i
                                             , pcIds        = pis }
        msg = nlnl $ d <> " has left the game."
    in bcast [(msg, i `delete` pis)]


-- ==================================================
-- Wizard commands:


wizDispCmdList :: Action
wizDispCmdList imc@(i, _, _) rs = logPlaExecArgs (prefixWizCmd "?") rs i >> dispCmdList (cmdPred . Just $ wizCmdChar) imc rs


-----


wizShutdown :: Action
wizShutdown (i, mq, _) [] = readWSTMVar >>= \ws ->
    let ((^.sing) -> s) = (ws^.entTbl) ! i
    in do
        massSend "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"
        logPlaExecArgs (prefixWizCmd "shutdown") [] i
        massLogPla "wizShutDown" $ T.concat [ "closing connection due to server shutdown initiated by "
                                            , s
                                            , " "
                                            , parensQuote "no message given"
                                            , "." ]
        logNotice  "wizShutdown" $ T.concat [ "server shutdown initiated by "
                                            , s
                                            , " "
                                            , parensQuote "no message given"
                                            , "." ]
        liftIO . atomically . writeTQueue mq $ Shutdown
wizShutdown (i, mq, _) rs = readWSTMVar >>= \ws ->
    let ((^.sing) -> s)   = (ws^.entTbl) ! i
        msg               = T.intercalate " " rs
    in do
        massSend msg
        logPlaExecArgs (prefixWizCmd "shutdown") rs i
        massLogPla "wizShutDown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                              , s
                                              , "; reason: "
                                              , msg
                                              , "." ]
        logNotice  "wizShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; reason: ", msg, "." ]
        liftIO . atomically . writeTQueue mq $ Shutdown


-----


wizTime :: Action
wizTime (i, mq, cols) [] = do
    logPlaExec (prefixWizCmd "time") i
    ct <- liftIO getCurrentTime
    zt <- liftIO getZonedTime
    send mq . T.unlines . concatMap (wordWrap cols) $ [ "At the tone, the time will be...", formatThat ct, formatThat zt, "" ]
  where
    formatThat t = let wordy = T.words . showText $ t
                       zone  = last wordy
                       date  = head wordy
                       time  = T.init . T.reverse . T.dropWhile (/= '.') . T.reverse . head . tail $ wordy
                   in T.concat [ zone, ": ", date, " ", time, "\b" ]
wizTime imc@(_, mq, cols) rs = ignore mq cols rs >> wizTime imc []


-----


wizDate :: Action
wizDate (i, mq, _)    [] = do
    logPlaExec (prefixWizCmd "date") i
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
wizDate imc@(_, mq, cols) rs = ignore mq cols rs >> wizDate imc []


-----


wizUptime :: Action
wizUptime (i, mq, cols) [] = do
    logPlaExec (prefixWizCmd "uptime") i
    (try . send mq . parse =<< runUptime) >>= eitherRet (\e -> logIOEx "wizUptime" e >> sendGenericErrorMsg mq cols)
  where
    runUptime = liftIO . readProcess "uptime" [] $ ""
    parse ut  = let (a, b) = span (/= ',') ut
                    a'     = unwords . tail . words $ a
                    b'     = dropWhile isSpace . takeWhile (/= ',') . tail $ b
                    c      = (toUpper . head $ a') : tail a'
                in nlnl . T.concat $ [ T.pack c, " ", T.pack b', "." ]
wizUptime imc@(_, mq, cols) rs = ignore mq cols rs >> wizUptime imc []


-----


wizStart :: Action
wizStart (i, mq, cols) [] = do
    logPlaExec (prefixWizCmd "start") i
    start <- getNWSRec startTime
    send mq . nl . T.unlines . wordWrap cols . showText $ start
wizStart imc@(_, mq, cols) rs = ignore mq cols rs >> wizStart imc []


-----


wizName :: Action
wizName (i, mq, cols) [] = do
    logPlaExec (prefixWizCmd "name") i
    readWSTMVar >>= \ws ->
        let ((^.sing) -> s)     = (ws^.entTbl) ! i
            (pp -> s', pp -> r) = getSexRace i ws
        in send mq . nl . T.unlines . wordWrap cols . T.concat $ [ "You are ", s, " (a ", s', " ", r, ")." ]
wizName imc@(_, mq, cols) rs = ignore mq cols rs >> wizName imc []


-- ==================================================
-- Debug commands:


debugDispCmdList :: Action
debugDispCmdList imc@(i, _, _) rs = do
    logPlaExecArgs (prefixDebugCmd "?") rs i
    dispCmdList (cmdPred . Just $ debugCmdChar) imc rs


-----


debugBuffCheck :: Action
debugBuffCheck (i, mq, cols) [] = do
    logPlaExec (prefixDebugCmd "buffer") i
    try helper >>= eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = do
        td      <- liftIO getTemporaryDirectory
        (fn, h) <- liftIO . openTempFile td $ "temp"
        bm      <- liftIO . hGetBuffering $ h
        liftIO $ hClose h >> removeFile fn
        send mq . nl . T.unlines . wordWrapIndent 2 cols . T.concat $ [ parensQuote "Default"
                                                                      , " buffering mode for temp file "
                                                                      , dblQuote . T.pack $ fn
                                                                      , " is "
                                                                      , dblQuote . showText $ bm
                                                                      , "." ]
debugBuffCheck imc@(_, mq, cols) rs = ignore mq cols rs >> debugBuffCheck imc []


-----


debugDispEnv :: Action
debugDispEnv (i, mq, cols) [] = do
    logPlaExecArgs (prefixDebugCmd "env") [] i
    send mq . nl =<< (mkAssocListTxt cols <$> liftIO getEnvironment)
debugDispEnv (i, mq, cols) rs = do
    logPlaExecArgs (prefixDebugCmd "env") rs i
    env <- liftIO getEnvironment
    send mq . T.unlines . map (helper env) . nub $ rs
  where
    helper env r = mkAssocListTxt cols . filter grepPair $ env
      where
        grepPair = uncurry (||) . over both (grep . T.pack)
        grep     = (r `T.isInfixOf`)


-----


debugLog :: Action
debugLog (i, mq, _) [] = logPlaExec (prefixDebugCmd "log") i >> helper >> ok mq
  where
    helper       = replicateM_ 100 . liftIO . forkIO . void . runStateInIORefT heavyLogging =<< get
    heavyLogging = replicateM_ 100 . logNotice "debugLog" . (<> ".") . ("Logging from " <>) . showText =<< liftIO myThreadId
debugLog imc@(_, mq, cols) rs = ignore mq cols rs >> debugLog imc []


------


debugThrow :: Action
debugThrow     (i, _,  _   ) [] = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow imc@(_, mq, cols) rs = ignore mq cols rs >> debugThrow imc []


-----


debugThread :: Action
debugThread (i, mq, cols) [] = do
    logPlaExec (prefixDebugCmd "thread") i
    (nli, eli) <- over both asyncThreadId <$> getLogAsyncs
    kvs <- M.assocs <$> readTMVarInNWS threadTblTMVar
    plt <- readTMVarInNWS plaLogTblTMVar
    let zipped = zip (map (asyncThreadId . fst) . IM.elems $ plt) (map PlaLog . IM.keys $ plt)
    ds  <- mapM mkDesc $ head kvs      :
                         (nli, Notice) :
                         (eli, Error)  :
                         tail kvs ++ zipped
    let msg = T.unlines . concatMap (wordWrap cols) $ ds
    send mq . frame cols $ msg
  where
    mkDesc (k, v) = (liftIO . threadStatus $ k) >>= \s ->
        return . T.concat $ [ showText k, " ", bracketPad 15 . mkTypeName $ v, showText s ]
    mkTypeName (Server ti) = padOrTrunc 8 "Server" <> showText ti
    mkTypeName (PlaLog ti) = padOrTrunc 8 "PlaLog" <> showText ti
    mkTypeName t           = showText t
debugThread imc@(_, mq, cols) rs = ignore mq cols rs >> debugThread imc []


-----


debugTalk :: Action
debugTalk (i, mq, cols) [] = do
    logPlaExec (prefixDebugCmd "talk") i
    ds <- readTMVarInNWS talkAsyncTblTMVar >>= mapM mkDesc . M.elems
    let msg = T.unlines . concatMap (wordWrap cols) $ ds
    send mq . frame cols $ msg
  where
    mkDesc a = (liftIO . poll $ a) >>= \status ->
        let statusTxt = case status of Nothing         -> "running"
                                       Just (Left  e ) -> ("exception " <>) . parensQuote . showText $ e
                                       Just (Right ()) -> "finished"
        in return . T.concat $ [ "Talk async ", showText . asyncThreadId $ a, ": ", statusTxt, "." ]
debugTalk imc@(_, mq, cols) rs = ignore mq cols rs >> debugTalk imc []


-----


debugPurge :: Action
debugPurge     (i, mq, _   ) [] = logPlaExec (prefixDebugCmd "purge") i >> purge >> ok mq
debugPurge imc@(_, mq, cols) rs = ignore mq cols rs >> debugPurge imc []


-- TODO: This function could be automatically run at certain intervals.
purge :: MudStack ()
purge = logNotice "purge" "purging the thread tables." >> purgePlaLogTbl >> purgeThreadTbl >> purgeTalkAsyncTbl


purgePlaLogTbl :: MudStack ()
purgePlaLogTbl = do
    kvs <- IM.assocs <$> readTMVarInNWS plaLogTblTMVar
    let is     = [ fst kv         | kv <- kvs ]
    let asyncs = [ fst . snd $ kv | kv <- kvs ]
    ss <- liftIO . mapM poll $ asyncs
    let zipped = zip is ss
    modifyNWS plaLogTblTMVar $ flip (foldl' helper) zipped
  where
    helper m (_, Nothing) = m
    helper m (i, _      ) = IM.delete i m


purgeThreadTbl :: MudStack ()
purgeThreadTbl = do
    tis <- M.keys <$> readTMVarInNWS threadTblTMVar
    ss  <- liftIO . mapM threadStatus $ tis
    let zipped = zip tis ss
    modifyNWS threadTblTMVar $ flip (foldl' helper) zipped
  where
    helper m (ti, s)
      | s == ThreadFinished = M.delete ti m
      | otherwise           = m


purgeTalkAsyncTbl :: MudStack ()
purgeTalkAsyncTbl = do
    asyncs <- M.elems <$> readTMVarInNWS talkAsyncTblTMVar
    ss     <- liftIO . mapM poll $ asyncs
    let zipped = zip asyncs ss
    modifyNWS talkAsyncTblTMVar $ flip (foldl' helper) zipped
  where
    helper m (_, Nothing) = m
    helper m (a, _      ) = M.delete (asyncThreadId a) m


-----


debugBoot :: Action
debugBoot     (i, mq, _   ) [] = logPlaExec (prefixDebugCmd "boot") i >> ok mq >> massMsg Boot
debugBoot imc@(_, mq, cols) rs = ignore mq cols rs >> debugBoot imc []


-----


debugStop :: Action
debugStop     (i, mq, _   ) [] = logPlaExec (prefixDebugCmd "stop") i >> ok mq >> massMsg StopThread
debugStop imc@(_, mq, cols) rs = ignore mq cols rs >> debugStop imc []


-----


debugCPU :: Action
debugCPU (i, mq, cols) [] = do
    logPlaExec (prefixDebugCmd "cpu") i
    t <- liftIO getCPUTime
    let secs = fromIntegral t / fromIntegral (10 ^ 12)
    send mq . nl . T.unlines . wordWrap cols $ "CPU time: " <> showText secs
debugCPU imc@(_, mq, cols) rs = ignore mq cols rs >> debugCPU imc []


-----


debugBroad :: Action
debugBroad (i, _, _) [] = do
    logPlaExec (prefixDebugCmd "broad") i
    bcast . mkBroadcast i $ msg
  where
    msg = "[1] abcdefghij\n\
          \[2] abcdefghij abcdefghij\n\
          \[3] abcdefghij abcdefghij abcdefghij\n\
          \[4] abcdefghij abcdefghij abcdefghij abcdefghij\n\
          \[5] abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij\n\
          \[6] abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij\n\
          \[7] abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij\n\
          \[8] abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij\n\
          \[9] abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij\n\
          \[0] abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij"
debugBroad imc@(_, mq, cols) rs = ignore mq cols rs >> debugBroad imc []


-----


debugRemPut :: Action
debugRemPut (i, mq, _) [] = do
    logPlaExec (prefixDebugCmd "remput") i
    mapM_ (fakeClientInput mq) . take 10 . cycle $ [ remCmd, putCmd ]
  where
    remCmd = "remove" <> rest
    putCmd = "put"    <> rest
    rest   = T.concat [ " ", T.pack [allChar], " ", T.pack [rmChar], "sack" ]
debugRemPut imc@(_, mq, cols) rs = ignore mq cols rs >> debugRemPut imc []


fakeClientInput :: MsgQueue -> T.Text -> MudStack ()
fakeClientInput mq = liftIO . atomically . writeTQueue mq . FromClient . nl
