{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, ScopedTypeVariables, TupleSections, ViewPatterns #-}

module Mud.Cmds (listenWrapper) where

import Mud.Color
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
import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId)
import Control.Concurrent.Async (async, asyncThreadId, poll, race_, wait)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), IOException, SomeException, fromException)
import Control.Exception.Lifted (catch, finally, throwIO, throwTo, try)
import Control.Lens (_1, _2, _3, at, both, folded, over, to)
import Control.Lens.Operators ((&), (?~), (.~), (^.), (^..))
import Control.Lens.Setter (set)
import Control.Monad (forM_, forever, guard, mplus, replicateM, replicateM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Char (isSpace, ord)
import Data.IntMap.Lazy ((!))
import Data.List (delete, elemIndex, find, foldl', intercalate, intersperse, nub, nubBy, sort, sortBy)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Monoid ((<>), mempty)
import Data.Text.Strict.Lens (packed)
import Data.Time (diffUTCTime, getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import GHC.Conc (ThreadStatus(..), threadStatus)
import Network (HostName, PortID(..), accept, listenOn, sClose)
import Prelude hiding (pi)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, getDirectoryContents, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (BufferMode(..), Handle, Newline(..), NewlineMode(..), hClose, hGetBuffering, hGetLine, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1, openTempFile)
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
-- [DONE] 8. Refactor for ViewPatterns and pattern guards.
-- [DONE] 9. Refactor for NamedFieldPuns and RecordWildCards.
-- [DONE] 10. See if you can keep your lines at 120 characters or less.
-- [DONE] 11. Are there places where I can use IO as a Functor or Applicative?
-- [DONE] 12. Make sure you are using "as" and "a" for "args" instead of "rs" and "r".
-- [DONE] 13. Make sure that all your export lists are properly sorted.
-- [DONE] 14. "forall"?
-- [DONE] 15. Lists of imported functions should be sorted.
-- [DONE] 16. Loggers shouldn't print to screen.
-- [DONE] 17. Use "T.singleton".


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


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
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
    , Cmd { cmdName = prefixDebugCmd "broad", action = debugBroad, cmdDesc = "Broadcast (to yourself) a multi-line \
                                                                             \message." }
    , Cmd { cmdName = prefixDebugCmd "buffer", action = debugBuffCheck, cmdDesc = "Confirm the default buffering \
                                                                                  \mode." }
    , Cmd { cmdName = prefixDebugCmd "color", action = debugColor, cmdDesc = "Perform a color test." }
    , Cmd { cmdName = prefixDebugCmd "cpu", action = debugCPU, cmdDesc = "Display the CPU time." }
    , Cmd { cmdName = prefixDebugCmd "env", action = debugDispEnv, cmdDesc = "Display system environment variables." }
    , Cmd { cmdName = prefixDebugCmd "log", action = debugLog, cmdDesc = "Put the logging service under heavy load." }
    , Cmd { cmdName = prefixDebugCmd "purge", action = debugPurge, cmdDesc = "Purge the thread tables." }
    , Cmd { cmdName = prefixDebugCmd "remput", action = debugRemPut, cmdDesc = "In quick succession, remove from and \
                                                                               \put into a sack on the ground." }
    , Cmd { cmdName = prefixDebugCmd "params", action = debugParams, cmdDesc = "Show \"ActionParams\"." }
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
listenWrapper = (initAndStart `catch` listenExHandler) `finally` graceful
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


getRecordUptime :: MudStack (Maybe Integer)
getRecordUptime = (liftIO . doesFileExist $ uptimeFile) >>= \case
  True  -> liftIO readUptime `catch` (\e -> readFileExHandler "getRecordUptime" e >> return Nothing)
  False -> return Nothing
  where
    readUptime = Just . read <$> readFile uptimeFile


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
        setDfltColor mq
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
    let (x, _) = randomR (0, 7) g in return $ [ Dwarf .. Vulpenoid ] !! x


setDfltColor :: MsgQueue -> MudStack ()
setDfltColor = flip send dfltColorANSI


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO getFilename >>= try . takeADump >>= eitherRet (readFileExHandler "dumpTitle")
  where
    getFilename  = (T.unpack "title" ++) . show . fst . randomR (1, noOfTitles) <$> newStdGen
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
      FromServer msg -> (liftIO . T.hPutStr h $ msg)             >> loop
      FromClient (T.strip . T.pack . stripTelnet . T.unpack -> msg)
                     -> unless (T.null msg) (handleInp i mq msg) >> loop
      Prompt p       -> sendPrompt h p                           >> loop
      Quit           -> cowbye h                                 >> handleEgress i
      Boot           -> boot   h                                 >> handleEgress i
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


sendPrompt :: Handle -> T.Text -> MudStack ()
sendPrompt h = liftIO . T.hPutStr h . nl


cowbye :: Handle -> MudStack ()
cowbye h = liftIO takeADump `catch` readFileExHandler "cowbye"
  where
    takeADump = T.hPutStr h . nl =<< (T.readFile . (miscDir ++) $ "cowbye")


-- TODO: Make a wizard command that does this.
boot :: Handle -> MudStack ()
boot h = liftIO . T.hPutStr h . nl $ "You have been booted from CurryMUD. Goodbye!"


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


type Input = (CmdName, Args)


splitInp :: T.Text -> Maybe Input
splitInp = splitIt . T.words
  where
    splitIt [] = Nothing
    splitIt xs = Just . headTail $ xs


dispatch :: Id -> MsgQueue -> Input -> MudStack ()
dispatch i mq (cn, as) = do
    cols <- getPlaColumns i
    findAction i cn >>= maybe sorry (\act -> act (WithArgs i mq cols as))
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


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks ((^.rmLinks) -> rls) =
    sortBy sorter $ plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]
  where
    sorter c = uncurry compare . over both cmdName . (c,)


isNonStdLink :: RmLink -> Bool
isNonStdLink (NonStdLink {}) = True
isNonStdLink _               = False


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) = Cmd { cmdName = cn, action = go cn, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> T.Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _linkDir
                                               NonStdLink { .. } -> _linkName


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
-- Patterns matching type "ActionParams":


pattern WithArgs i mq cols as = ActionParams { plaId       = i
                                             , plaMsgQueue = mq
                                             , plaCols     = cols
                                             , args        = as }


pattern NoArgs i mq cols = WithArgs i mq cols []


pattern NoArgs' i mq <- NoArgs i mq _


pattern NoArgs'' i <- NoArgs' i _


pattern Lower i mq cols as <- WithArgs i mq cols (map T.toLower -> as)


pattern Lower' i as <- Lower i _ _ as


pattern LowerNub i mq cols as <- WithArgs i mq cols (nub . map T.toLower -> as)


pattern LowerNub' i as <- LowerNub i _ _ as


pattern Ignoring mq cols as <- WithArgs _ mq cols (dblQuote . T.unwords -> as)


pattern AdviseNoArgs <- NoArgs' _ _


pattern AdviseOneArg a <- WithArgs _ _ _ [a]


pattern Advising mq cols <- WithArgs _ mq cols _


-- ==================================================
-- Player commands:

about :: Action
about (NoArgs i mq cols) = do
    logPlaExec "about" i
    try helper >>= eitherRet (\e -> readFileExHandler "about" e >> sendGenericErrorMsg mq cols)
  where
    helper = multiWrapSend mq cols . T.lines =<< (liftIO . T.readFile . (miscDir ++) $ "about")
about p = withoutArgs about p


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = wrapSend mq cols genericErrorMsg


withoutArgs :: Action -> ActionParams -> MudStack ()
withoutArgs act p = ignore p >> act p { args = [] }


ignore :: Action
ignore (Ignoring mq cols as) = send mq . wrapUnlines cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" [ showText p ]


-----


-- TODO: Automatically execute this cmd after a user authenticates. (We won't want to log anything in that case.)
motd :: Action
motd (NoArgs i mq cols) = logPlaExec "motd" i >> (send mq =<< getMotdTxt cols)
motd p                  = withoutArgs motd p


getMotdTxt :: Cols -> MudStack T.Text
getMotdTxt cols = (try . liftIO $ helper) >>= eitherRet handler
  where
    helper    = return . frame cols . multiWrap cols . T.lines =<< (T.readFile . (miscDir ++) $ "motd")
    handler e = do
        readFileExHandler "getMotdTxt" e
        return . wrapUnlinesNl cols $ "Unfortunately, the message of the day could not be retrieved."


-----


plaDispCmdList :: Action
plaDispCmdList p@(LowerNub' i as) = logPlaExecArgs "?" as i >> dispCmdList (mkCmdPred Nothing) p
plaDispCmdList p                  = patternMatchFail "plaDispCmdList" [ showText p ]


dispCmdList :: (Cmd -> Bool) -> Action
dispCmdList p (NoArgs   _ mq cols   ) =
    send mq . nl . T.unlines . concatMap (wordWrapIndent (maxCmdLen + 1) cols) . cmdListText $ p
dispCmdList p (LowerNub _ mq cols as) | matches <- [ grepTextList a . cmdListText $ p | a <- as ] =
    send mq . nl . T.unlines . concatMap (wordWrapIndent (maxCmdLen + 1) cols) . intercalate [""] $ matches
dispCmdList _ p = patternMatchFail "dispCmdList" [ showText p ]


cmdListText :: (Cmd -> Bool) -> [T.Text]
cmdListText p = sort . T.lines . T.concat . foldl' helper [] . filter p $ allCmds
  where
    helper acc Cmd { .. } | cmdTxt <- nl $ padOrTrunc (maxCmdLen + 1) cmdName <> cmdDesc = cmdTxt : acc


mkCmdPred :: Maybe Char -> Cmd -> Bool
mkCmdPred mc (T.head . cmdName -> p) = case mc of (Just c) -> c == p
                                                  Nothing  -> p `notElem` [ wizCmdChar, debugCmdChar ]


-----


help :: Action
help (NoArgs i mq cols) = do
    try helper >>= eitherRet (\e -> readFileExHandler "help" e >> sendGenericErrorMsg mq cols)
    logPla "help" i "read the root help file."
  where
    helper   = send mq . nl . T.unlines . concat . wordWrapLines cols . T.lines =<< readRoot
    readRoot = liftIO . T.readFile . (helpDir ++) $ "root"
help (LowerNub i mq cols as) =
    send mq . nl . T.unlines . intercalate [ "", mkDividerTxt cols, "" ] =<< getTopics
  where
    getTopics = mapM (\a -> concat . wordWrapLines cols . T.lines <$> getHelpTopicByName i cols a) as
help p = patternMatchFail "help" [ showText p ]


type HelpTopic = T.Text


getHelpTopicByName :: Id -> Cols -> HelpTopic -> MudStack T.Text
getHelpTopicByName i cols r = (liftIO . getDirectoryContents $ helpDir) >>= \(getTopics -> topics) ->
    maybe sorry
          (\t -> logPla "getHelpTopicByName" i ("read help on " <> dblQuote t <> ".") >> getHelpTopic t)
          (findFullNameForAbbrev r topics)
  where
    getTopics       = (^..folded.packed) . drop 2 . sort . delete "root"
    sorry           = return $ "No help is available on " <> dblQuote r <> "."
    helper          = liftIO . T.readFile . (helpDir ++) . T.unpack
    getHelpTopic t  = (try . helper $ t) >>= eitherRet handler
      where
        handler e = do
            readFileExHandler "getHelpTopicByName" e
            return . wrapUnlines cols $ "Unfortunately, the " <> dblQuote t <> " help file could not be retrieved."


-----


go :: T.Text -> Action
go dir p@(ActionParams { args = [], .. }) = goDispatcher p { args = [dir] }
go dir p@(ActionParams { args,      .. }) = goDispatcher p { args = dir : args }


goDispatcher :: Action
goDispatcher (ActionParams { args = [], .. }) = return ()
goDispatcher (Lower i mq cols as)             = mapM_ (tryMove i mq cols) as
goDispatcher p                                = patternMatchFail "goDispatcher" [ showText p ]


tryMove :: Id -> MsgQueue -> Cols -> T.Text -> MudStack ()
tryMove i mq cols dir = helper >>= \case
  Left  msg          -> wrapSend mq cols msg
  Right (logMsg, bs) -> bcast bs >> logPla "tryMove" i logMsg >> look ActionParams { plaId       = i
                                                                                   , plaMsgQueue = mq
                                                                                   , plaCols     = cols
                                                                                   , args        = [] }
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s) = (ws^.entTbl) ! i
            p               = (ws^.pcTbl)  ! i
            ri              = p^.rmId
            r               = (ws^.rmTbl)  ! ri
            ris             = (ws^.invTbl) ! ri
        in case findExit r dir of
          Nothing -> putTMVar t ws >> (return . Left $ sorry)
          Just (linkTxt, ri', mom, mdm)
            | p'          <- p & rmId .~ ri'
            , r'          <- (ws^.rmTbl)  ! ri'
            , originIs    <- i `delete` ris
            , destIs      <- (ws^.invTbl) ! ri'
            , destIs'     <- sortInv ws $ destIs ++ [i]
            , originPis   <- findPCIds ws originIs
            , destPis     <- findPCIds ws destIs
            , msgAtOrigin <- let d = serialize . mkStdDesig i ws s True $ ris
                             in nlnl $ case mom of
                               Nothing -> T.concat [ d, " ", verb, " ", expandLinkName dir, "." ]
                               Just f  -> f d
            , msgAtDest   <- let d = mkSerializedNonStdDesig i ws s A
                             in nlnl $ case mdm of
                               Nothing -> T.concat [ d, " arrives from ", expandOppLinkName dir, "." ]
                               Just f  -> f d
            , logMsg      <- T.concat [ "moved "
                                      , linkTxt
                                      , " from room "
                                      , showRm ri r
                                      , " to room "
                                      , showRm ri' r'
                                      , "." ]
            -> do
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
    showRm (showText -> ri) (parensQuote . (^.rmName) -> rn) = ri <> " " <> rn


findExit :: Rm -> LinkName -> Maybe (T.Text, Id, Maybe (T.Text -> T.Text), Maybe (T.Text -> T.Text))
findExit ((^.rmLinks) -> rls) ln =
    case [ (showLink rl, getDestId rl, getOriginMsg rl, getDestMsg rl) | rl <- rls, isValid rl ] of
      [] -> Nothing
      xs -> Just . head $ xs
  where
    isValid      StdLink    { .. } = ln == linkDirToCmdName _linkDir
    isValid      NonStdLink { .. } = ln `T.isPrefixOf` _linkName
    showLink     StdLink    { .. } = showText _linkDir
    showLink     NonStdLink { .. } = _linkName
    getDestId    StdLink    { .. } = _stdDestId
    getDestId    NonStdLink { .. } = _nonStdDestId
    getOriginMsg NonStdLink { .. } = Just _originMsg
    getOriginMsg _                 = Nothing
    getDestMsg   NonStdLink { .. } = Just _destMsg
    getDestMsg   _                 = Nothing


mkStdDesig :: Id -> WorldState -> Sing -> Bool -> Inv -> PCDesig
mkStdDesig i ws s ic ris = StdDesig { stdPCEntSing = Just s
                                    , isCap        = ic
                                    , pcEntName    = mkUnknownPCEntName i ws
                                    , pcId         = i
                                    , pcIds        = findPCIds ws ris }


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
look (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let ((^.rmId) -> ri) = (ws^.pcTbl) ! i
        r                = (ws^.rmTbl) ! ri
        primary          = multiWrap cols [ r^.rmName, r^.rmDesc ]
        suppl            = mkExitsSummary cols r <>  mkRmInvCoinsDesc i cols ws ri
    in send mq . nl $ primary <> suppl
look (LowerNub i mq cols as) = helper >>= \case
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
            c                = (ws^.coinsTbl) ! ri
            d                = mkStdDesig i ws s True ris
        in if (not . null $ ris') || (c /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames i ws as ris' c
                   eiss               = [ curry procGecrMisRm gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                = map procReconciledCoinsRm rcs
                   invDesc            = foldl' (helperLookEitherInv ws) "" eiss
                   coinsDesc          = foldl' helperLookEitherCoins    "" ecs
                   ds                 = [ let ((^.sing) -> s') = (ws^.entTbl) ! pi
                                          in mkStdDesig pi ws s' False ris | pi <- extractPCIdsFromEiss ws eiss ]
               in putTMVar t ws >> return (Right $ invDesc <> coinsDesc, Just (d, ds))
          else    putTMVar t ws >> return ( Left . wrapUnlinesNl cols $ "You don't see anything here to look at."
                                          , Nothing )
    helperLookEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperLookEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . nl . multiWrap cols . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
look p = patternMatchFail "look" [ showText p ]


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
mkEntDesc i cols ws (ei@(((ws^.typeTbl) !) -> t), e@(wrapUnlines cols . (^.entDesc) -> ed)) =
    case t of ConType ->                 (ed <>) . mkInvCoinsDesc i cols ws ei $ e
              MobType ->                 (ed <>) . mkEqDesc       i cols ws ei   e $ t
              PCType  -> (pcHeader <>) . (ed <>) . mkEqDesc       i cols ws ei   e $ t
              _       -> ed
  where
    pcHeader = wrapUnlines cols . mkPCDescHeader ei $ ws


mkPCDescHeader :: Id -> WorldState -> T.Text
mkPCDescHeader i ws | (pp -> s, pp -> r) <- getSexRace i ws = T.concat [ "You see a ", s, " ", r, "." ]


mkInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> Ent -> T.Text
mkInvCoinsDesc i cols ws ei ((^.sing) -> s) | is <- (ws^.invTbl)   ! ei
                                            , c  <- (ws^.coinsTbl) ! ei = case (not . null $ is, c /= mempty) of
  (False, False) -> wrapUnlines cols $ if ei == i then dudeYourHandsAreEmpty else "The " <> s <> " is empty."
  (True,  False) -> header <> mkEntsInInvDesc i cols ws is
  (False, True ) -> header <>                                 mkCoinsSummary cols c
  (True,  True ) -> header <> mkEntsInInvDesc i cols ws is <> mkCoinsSummary cols c
  where
    header | ei == i   = nl "You are carrying:"
           | otherwise = wrapUnlines cols $ "The " <> s <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntsInInvDesc i cols ws = T.unlines . concatMap (wordWrapIndent ind cols . helper) . mkNameCountBothList i ws
  where
    helper (bracketPad ind -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (bracketPad ind -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]
    ind = 11


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper [ mkNameAmt cn c' | cn <- coinNames | c' <- mkListFromCoins c ]
  where
    mkNameAmt (bracketQuote -> cn) a = if a == 0 then "" else showText a <> " " <> cn
    helper                           = T.unlines . wordWrapIndent 2 cols . T.intercalate ", " . filter (not . T.null)


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) =
    T.unlines . intercalate [""] . map (wordWrap cols) . filter (not . T.null) $ [ copDesc, silDesc, golDesc ]
  where -- TODO: Come up with good descriptions.
    copDesc = if cop /= 0 then "The copper piece is round and shiny." else ""
    silDesc = if sil /= 0 then "The silver piece is round and shiny." else ""
    golDesc = if gol /= 0 then "The gold piece is round and shiny."   else ""


-----


exits :: Action
exits (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let ((^.rmId) -> ri) = (ws^.pcTbl) ! i
        r                = (ws^.rmTbl) ! ri
    in logPlaExec "exits" i >> (send mq . nl . mkExitsSummary cols $ r)
exits p = withoutArgs exits p


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
inv (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    send mq . nl . mkInvCoinsDesc i cols ws i $ (ws^.entTbl) ! i
inv (LowerNub i mq cols as) = readWSTMVar >>= \ws ->
    let is = (ws^.invTbl)   ! i
        c  = (ws^.coinsTbl) ! i
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames i ws as is c
               eiss               = [ curry procGecrMisPCInv gecr mis | gecr <- gecrs | mis <- miss ]
               ecs                = map procReconciledCoinsPCInv rcs
               invDesc            = foldl' (helperEitherInv ws) "" eiss
               coinsDesc          = foldl' helperEitherCoins    "" ecs
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYourHandsAreEmpty
  where
    helperEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperEitherCoins  acc (Left  msgs) = (acc <>) . nl . multiWrap cols . intersperse "" $ msgs
    helperEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
inv p = patternMatchFail "inv" [ showText p ]


-----


equip :: Action
equip (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let e = (ws^.entTbl) ! i
    in send mq . nl . mkEqDesc i cols ws i e $ PCType
equip (LowerNub i mq cols as) = readWSTMVar >>= \ws ->
    let em@(M.elems -> is) = (ws^.eqTbl) ! i
    in send mq $ if not . M.null $ em
      then let (gecrs, miss, rcs)           = resolveEntCoinNames i ws as is mempty
               eiss                         = [ curry procGecrMisPCEq gecr mis | gecr <- gecrs | mis <- miss ]
               invDesc                      = foldl' (helperEitherInv ws) "" eiss
               coinsDesc | not . null $ rcs = wrapUnlinesNl cols "You don't have any coins among your readied \
                                                                 \equipment."
                         | otherwise        = ""
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYou'reNaked
  where
    helperEitherInv _  acc (Left  msg) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ws acc (Right is ) = nl $ acc <> mkEntDescs i cols ws is
equip p = patternMatchFail "equip" [ showText p ]


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
    none = wrapUnlines cols $ if
      | ei == i      -> dudeYou'reNaked
      | t  == PCType -> parsePCDesig i ws $ d <> " doesn't have anything readied."
      | otherwise    -> "The " <> s <> " doesn't have anything readied."
    header = wrapUnlines cols $ if
      | ei == i      -> "You have readied the following equipment:"
      | t  == PCType -> parsePCDesig i ws $ d <> " has readied the following equipment:"
      | otherwise    -> "The " <> s <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig ei ws s The


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


-----


getAction :: Action
getAction p@AdviseNoArgs     = advise p ["get"] $ "Please specify one or more items to pick up, as \
                                                  \in " <> dblQuote "get sword" <> "."
getAction   (LowerNub' i as) = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "get" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s)  = (ws^.entTbl)   ! i
            ((^.rmId) -> ri) = (ws^.pcTbl)    ! i
            ris              = (ws^.invTbl)   ! ri
            ris'             = i `delete` ris
            rc               = (ws^.coinsTbl) ! ri
            d                = mkStdDesig i ws s True ris
        in if (not . null $ ris') || (rc /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as ris' rc
                   eiss                  = [ curry procGecrMisRm gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map procReconciledCoinsRm rcs
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Get ri i) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Get ri i) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i "You don't see anything here to pick up.", [])
getAction p = patternMatchFail "getAction" [ showText p ]


advise :: ActionParams -> [HelpTopic] -> T.Text -> MudStack ()
advise (Advising mq cols) []  msg = wrapSend mq cols msg
advise (Advising mq cols) [h] msg
  | msgs <- [ msg, "For more information, type " <> (dblQuote . ("help " <>) $ h) <> "." ] = multiWrapSend mq cols msgs
advise (Advising mq cols) hs  msg
  | msgs <- [ msg, "See also the following help topics: " <> helpTopics <> "." ]           = multiWrapSend mq cols msgs
  where
    helpTopics = dblQuote . T.intercalate (dblQuote ", ") $ hs
advise p hs msg = patternMatchFail "advise" [ showText p, showText hs, msg ]


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
helperGetDropEitherInv i d god fi ti a@(ws, _, _) = \case
  Left  (mkBroadcast i -> b) -> over _2 (++ b) a
  Right is | (fis, tis)      <- over both ((ws^.invTbl) !) (fi, ti)
           , ws'             <- ws & invTbl.at fi ?~ deleteFirstOfEach is fis
                                   & invTbl.at ti ?~ sortInv ws (tis ++ is)
           , (bs', logMsgs') <- mkGetDropInvDesc i ws' d god is
           -> set _1 ws' . over _2 (++ bs') . over _3 (++ logMsgs') $ a


mkGetDropInvDesc :: Id -> WorldState -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i ws d god (mkNameCountBothList i ws -> ncbs) | bs <- concatMap helper ncbs = (bs, extractLogMsgs i bs)
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
helperGetDropEitherCoins i d god fi ti a@(ws, _, _) = \case
  Left  msgs -> over _2 (++ [ (msg, [i]) | msg <- msgs ]) a
  Right c | (fc, tc)      <- over both ((ws^.coinsTbl) !) (fi, ti)
          , ws'           <- ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                & coinsTbl.at ti ?~ tc <> c
          , (bs, logMsgs) <- mkGetDropCoinsDesc i d god c
          -> set _1 ws' . over _2 (++ bs) . over _3 (++ logMsgs) $ a


mkGetDropCoinsDesc :: Id -> PCDesig -> GetOrDrop -> Coins -> ([Broadcast], [T.Text])
mkGetDropCoinsDesc i d god (Coins (cop, sil, gol)) | bs <- concat . catMaybes $ [ c, s, g ] = (bs, extractLogMsgs i bs)
  where
    c = if cop /= 0 then Just . helper cop $ "copper piece" else Nothing
    s = if sil /= 0 then Just . helper sil $ "silver piece" else Nothing
    g = if gol /= 0 then Just . helper gol $ "gold piece"   else Nothing
    helper a cn | a == 1 =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " a ", cn, "." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " a ", cn, "." ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", showText a, " ", cn, "s." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", showText a, " ", cn, "s." ], otherPCIds) ]
    otherPCIds = i `delete` pcIds d


-----


dropAction :: Action
dropAction p@AdviseNoArgs     = advise p ["drop"] $ "Please specify one or more things to drop, as \
                                                    \in " <> dblQuote "drop sword" <> "."
dropAction   (LowerNub' i as) = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "drop" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s)  = (ws^.entTbl)   ! i
            ((^.rmId) -> ri) = (ws^.pcTbl)    ! i
            (pis, ris)       = over both ((ws^.invTbl) !) (i, ri)
            pc               = (ws^.coinsTbl) ! i
            d                = mkStdDesig i ws s True ris
        in if (not . null $ pis) || (pc /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as pis pc
                   eiss                  = [ curry procGecrMisPCInv gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map procReconciledCoinsPCInv rcs
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Drop i ri) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Drop i ri) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
dropAction p = patternMatchFail "dropAction" [ showText p ]


-----


putAction :: Action
putAction p@AdviseNoArgs     = advise p ["put"] $ "Please specify one or more things you want to put, followed by \
                                                  \where you want to put them, as in " <> dblQuote "put doll \
                                                  \sack" <> "."
putAction p@(AdviseOneArg a) = advise p ["put"] $ "Please also specify where you want to put it, as \
                                                  \in " <> dblQuote ("put " <> a <> " sack") <> "."
putAction   (Lower' i as)    = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "put" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let ((^.sing) -> s)          = (ws^.entTbl)   ! i
          ((^.rmId) -> ri)         = (ws^.pcTbl)    ! i
          (pis, ris)               = over both ((ws^.invTbl)   !) (i, ri)
          ris'                     = i `delete` ris
          (pc, rc)                 = over both ((ws^.coinsTbl) !) (i, ri)
          cn                       = last as
          (init -> argsWithoutCon) = case as of [_, _] -> as
                                                _      -> (++ [cn]) . nub . init $ as
          d                        = mkStdDesig i ws s True ris
      in if (not . null $ pis) || (pc /= mempty)
        then if T.head cn == rmChar && cn /= T.singleton rmChar
          then if not . null $ ris'
            then shufflePut i (t, ws) d (T.tail cn) True argsWithoutCon ris' rc pis pc procGecrMisRm
            else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
          else shufflePut i (t, ws) d cn False argsWithoutCon pis pc pis pc procGecrMisPCInv
        else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
putAction p = patternMatchFail "putAction" [ showText p ]


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
              Args                                                ->
              InvWithCon                                          ->
              CoinsWithCon                                        ->
              PCInv                                               ->
              PCCoins                                             ->
              ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) ->
              STM ([Broadcast], [T.Text])
shufflePut i (t, ws) d cn icir as is c pis pc f | (gecrs, miss, rcs) <- resolveEntCoinNames i ws [cn] is c =
    if null miss && (not . null $ rcs)
      then putTMVar t ws >> return (mkBroadcast i "You can't put something inside a coin.", [])
      else case f . head . zip gecrs $ miss of
        Left  (mkBroadcast i -> bc)                                   -> putTMVar t ws >> return (bc, [])
        Right [ci] | e <- (ws^.entTbl) ! ci, t' <- (ws^.typeTbl) ! ci -> if t' /= ConType
          then putTMVar t ws >> return (mkBroadcast i $ "The " <> e^.sing <> " isn't a container.", [])
          else let (gecrs', miss', rcs') = resolveEntCoinNames i ws as pis pc
                   eiss                  = [ curry procGecrMisPCInv gecr mis | gecr <- gecrs' | mis <- miss' ]
                   ecs                   = map procReconciledCoinsPCInv rcs'
                   mnom                  = mkMaybeNthOfM icir ws ci e is
                   (ws',  bs,  logMsgs ) = foldl' (helperPutRemEitherInv   i d Put mnom i ci e) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Put mnom i ci e) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
        Right _ -> putTMVar t ws   >> return (mkBroadcast i "You can only put things into one container at a time.", [])


type NthOfM = (Int, Int)


mkMaybeNthOfM :: IsConInRm -> WorldState -> Id -> Ent -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM False _  _ _               _  = Nothing
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
helperPutRemEitherInv i d por mnom fi ti te a@(ws, bs, _) = \case
  Left  (mkBroadcast i -> b) -> over _2 (++ b) a
  Right is | (is', bs')      <- if ti `elem` is
                                  then (filter (/= ti) is, bs ++ [sorry])
                                  else (is, bs)
           , (fis, tis)      <- over both ((ws^.invTbl) !) (fi, ti)
           , ws'             <- ws & invTbl.at fi ?~ deleteFirstOfEach is' fis
                                   & invTbl.at ti ?~ (sortInv ws . (tis ++) $ is')
           , (bs'', logMsgs) <- mkPutRemInvDesc i ws' d por mnom is' te
           -> set _1 ws' . set _2 (bs' ++ bs'') . over _3 (++ logMsgs) $ a
  where
    sorry = ("You can't put the " <> te^.sing <> " inside itself.", [i])


mkPutRemInvDesc :: Id -> WorldState -> PCDesig -> PutOrRem -> Maybe NthOfM -> Inv -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemInvDesc i ws d por mnom is ((^.sing) -> ts) | bs <- concatMap helper . mkNameCountBothList i ws $ is
                                                    = (bs, extractLogMsgs i bs)
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
helperPutRemEitherCoins i d por mnom fi ti te a@(ws, _, _) = \case
  Left  msgs -> over _2 (++ [ (msg, [i]) | msg <- msgs ]) a
  Right c | (fc, tc)      <- over both ((ws^.coinsTbl) !) (fi, ti)
          , ws'           <- ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                & coinsTbl.at ti ?~ tc <> c
          , (bs, logMsgs) <- mkPutRemCoinsDescs i d por mnom c te
          -> set _1 ws' . over _2 (++ bs) . over _3 (++ logMsgs) $ a


mkPutRemCoinsDescs :: Id -> PCDesig -> PutOrRem -> Maybe NthOfM -> Coins -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemCoinsDescs i d por mnom (Coins (cop, sil, gol)) ((^.sing) -> ts) | bs <- concat . catMaybes $ [ c, s, g ]
                                                                         = (bs, extractLogMsgs i bs)
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
remove p@AdviseNoArgs     = advise p ["remove"] $ "Please specify one or more things to remove, followed by the \
                                                  \container you want to remove them from, as in " <> dblQuote "remove \
                                                  \doll sack" <> "."
remove p@(AdviseOneArg a) = advise p ["remove"] $ "Please also specify the container you want to remove it from, as \
                                                  \in " <> dblQuote ("remove " <> a <> " sack") <> "."
remove   (Lower' i as)    = do
    (bs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "remove" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let ((^.sing) -> s)          = (ws^.entTbl) ! i
          ((^.rmId) -> ri)         = (ws^.pcTbl)  ! i
          (pis, ris)               = over both ((ws^.invTbl)   !) (i, ri)
          (pc, rc)                 = over both ((ws^.coinsTbl) !) (i, ri)
          ris'                     = i `delete` ris
          cn                       = last as
          (init -> argsWithoutCon) = case as of [_, _] -> as
                                                _      -> (++ [cn]) . nub . init $ as
          d                        = mkStdDesig i ws s True ris
      in if T.head cn == rmChar && cn /= T.singleton rmChar
        then if not . null $ ris'
          then shuffleRem i (t, ws) d (T.tail cn) True argsWithoutCon ris' rc procGecrMisRm
          else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
        else shuffleRem i (t, ws) d cn False argsWithoutCon pis pc procGecrMisPCInv
remove p = patternMatchFail "remove" [ showText p ]


shuffleRem :: Id                                                  ->
              (TMVar WorldState, WorldState)                      ->
              PCDesig                                             ->
              ConName                                             ->
              IsConInRm                                           ->
              Args                                                ->
              InvWithCon                                          ->
              CoinsWithCon                                        ->
              ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) ->
              STM ([Broadcast], [T.Text])
shuffleRem i (t, ws) d cn icir as is c f
  | (gecrs, miss, rcs) <- resolveEntCoinNames i ws [cn] is c = if null miss && (not . null $ rcs)
    then putTMVar t ws >> return (mkBroadcast i "You can't remove something from a coin.", [])
    else case f . head . zip gecrs $ miss of
      Left  msg -> putTMVar t ws >> return (mkBroadcast i msg, [])
      Right [ci] | e@((^.sing) -> s) <- (ws^.entTbl) ! ci, t' <- (ws^.typeTbl) ! ci ->
        if t' /= ConType
          then putTMVar t ws >> return (mkBroadcast i $ "The " <> s <> " isn't a container.", [])
          else let cis                   = (ws^.invTbl)   ! ci
                   cc                    = (ws^.coinsTbl) ! ci
                   (gecrs', miss', rcs') = resolveEntCoinNames i ws as cis cc
                   eiss                  = [ curry (procGecrMisCon s) gecr mis | gecr <- gecrs' | mis <- miss' ]
                   ecs                   = map (procReconciledCoinsCon s) rcs'
                   mnom                  = mkMaybeNthOfM icir ws ci e is
                   (ws',  bs,  logMsgs)  = foldl' (helperPutRemEitherInv   i d Rem mnom ci i e) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Rem mnom ci i e) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
      Right _ -> do
          putTMVar t ws
          return (mkBroadcast i "You can only remove things from one container at a time.", [])


-----


ready :: Action
ready p@AdviseNoArgs            = advise p ["ready"] $ "Please specify one or more things to ready, as \
                                                       \in " <> dblQuote "ready sword" <> "."
ready   (LowerNub i mq cols as) = do
    (msg, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "ready" i logMsgs
    send mq . nl $ msg
  where
    helper = onWS $ \(t, ws) ->
        let is = (ws^.invTbl)   ! i
            c  = (ws^.coinsTbl) ! i
        in if (not . null $ is) || (c /= mempty)
          then let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols i ws as is mempty
                   eiss                      = [ curry procGecrMisReady gecr mis | gecr <- gecrs | mis <- miss ]
                   msg                       = if null rcs then "" else nl "You can't ready coins."
                   (ws', msg', logMsgs)      = foldl' (helperReady i cols) (ws, msg, []) . zip eiss $ mrols
               in putTMVar t ws' >> return (msg', logMsgs)
          else putTMVar t ws >> return (wrapUnlines cols dudeYourHandsAreEmpty, [])
ready p = patternMatchFail "ready" [ showText p ]


helperReady :: Id                                     ->
               Cols                                   ->
               (WorldState, T.Text, [T.Text])         ->
               (Either T.Text Inv, Maybe RightOrLeft) ->
               (WorldState, T.Text, [T.Text])
helperReady i cols a (eis, mrol) = case eis of
  Left  (wrapUnlines cols -> msg') -> over _2 (<> msg') a
  Right is                         -> foldl' (readyDispatcher i cols mrol) a is


readyDispatcher :: Id                             ->
                   Cols                           ->
                   Maybe RightOrLeft              ->
                   (WorldState, T.Text, [T.Text]) ->
                   Id                             ->
                   (WorldState, T.Text, [T.Text])
readyDispatcher i cols mrol a@(ws, _, _) ei =
    let e = (ws^.entTbl)  ! ei
        t = (ws^.typeTbl) ! ei
    in case t of
      ClothType -> readyCloth i cols mrol a ei e
      WpnType   -> readyWpn   i cols mrol a ei e
      _         -> over _2 (<> (wrapUnlines cols $ "You can't ready a " <> e^.sing <> ".")) a


moveReadiedItem :: Id                             ->
                   Cols                           ->
                   (WorldState, T.Text, [T.Text]) ->
                   EqMap                          ->
                   Slot                           ->
                   Id                             ->
                   T.Text                         ->
                   (WorldState, T.Text, [T.Text])
moveReadiedItem i cols a@(ws, _, _) em s ei msg
  | is  <- (ws^.invTbl) ! i
  , ws' <- ws & invTbl.at i ?~ filter (/= ei) is
              & eqTbl.at  i ?~ (em & at s ?~ ei)
  = set _1 ws' . over _2 (<> wrapUnlines cols msg) . over _3 (++ [msg]) $ a


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
sorryFullClothSlotsOneSide (pp -> s) = "You can't wear any more on your " <> s <> "."


-- Ready clothing:


readyCloth :: Id                             ->
              Cols                           ->
              Maybe RightOrLeft              ->
              (WorldState, T.Text, [T.Text]) ->
              Id                             ->
              Ent                            ->
              (WorldState, T.Text, [T.Text])
readyCloth i cols mrol a@(ws, _, _) ei e@((^.sing) -> s) =
    let em = (ws^.eqTbl)    ! i
        c  = (ws^.clothTbl) ! ei
    in case maybe (getAvailClothSlot cols ws i c em) (getDesigClothSlot cols ws e c em) mrol of
      Left  msg  -> over _2 (<> msg) a
      Right slot -> moveReadiedItem i cols a em slot ei . mkReadyMsg slot $ c
  where
    mkReadyMsg (pp -> slot) = \case NoseC   -> putOnMsg
                                    NeckC   -> putOnMsg
                                    FingerC -> T.concat [ "You slide the ", s, " on your ", slot, "." ]
                                    _       -> wearMsg
      where
        putOnMsg = "You put on the " <> s <> "."
        wearMsg  = T.concat [ "You wear the ", s, " on your ", slot, "." ]


getAvailClothSlot :: Cols -> WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot cols ws i c em | m <- (ws^.mobTbl) ! i, s <- m^.sex, h <- m^.hand = procMaybe $ case c of
  EarC    -> getEarSlotForSex s `mplus` (getEarSlotForSex . otherSex $ s)
  NoseC   -> findAvailSlot em noseSlots
  NeckC   -> findAvailSlot em neckSlots
  WristC  -> getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
  FingerC -> getRingSlot s h
  _       -> undefined -- TODO
  where
    procMaybe             = maybe (Left . wrapUnlines cols . sorryFullClothSlots $ c) Right
    getEarSlotForSex s    = findAvailSlot em $ case s of
      Male   -> lEarSlots
      Female -> rEarSlots
      _      -> patternMatchFail "getAvailClothSlot getEarSlotForSex"    [ showText s ]
    getWristSlotForHand h = findAvailSlot em $ case h of
      RHand  -> lWristSlots
      LHand  -> rWristSlots
      _      -> patternMatchFail "getAvailClothSlot getWristSlotForHand" [ showText h ]
    getRingSlot s h       = findAvailSlot em $ case s of
      Male    -> case h of
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
  | isRingRol rol, c /= FingerC                                           = Left sorryCantWearThere
  | c == FingerC, not . isRingRol $ rol                                   = Left . wrapUnlines cols $ ringHelp
  | otherwise                                                             = case c of
    EarC    -> maybe (Left sorryFullEar)   Right (findSlotFromList rEarSlots   lEarSlots)
    WristC  -> maybe (Left sorryFullWrist) Right (findSlotFromList rWristSlots lWristSlots)
    FingerC -> maybe (Right slotFromRol)
                     (\i -> let e = (ws^.entTbl) ! i in Left . sorry slotFromRol $ e)
                     (em^.at slotFromRol)
    _       -> undefined -- TODO
  where
    sorryCantWearThere     = wrapUnlines cols . T.concat $ [ "You can't wear a ", s, " on your ", pp rol, "." ]
    findSlotFromList rs ls = findAvailSlot em $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot getSlotFromList"  [ showText rol ]
    sorryFullEar   = wrapUnlines cols . sorryFullClothSlotsOneSide . getSlotFromList rEarSlots   $ lEarSlots
    sorryFullWrist = wrapUnlines cols . sorryFullClothSlotsOneSide . getSlotFromList rWristSlots $ lWristSlots
    slotFromRol    = fromRol rol :: Slot
    sorry (pp -> slot) ((^.sing) -> s') = wrapUnlines cols . T.concat $ [ "You're already wearing a "
                                                                        , s'
                                                                        , " on your "
                                                                        , slot
                                                                        , "." ]


-- Ready weapons:


readyWpn :: Id                             ->
            Cols                           ->
            Maybe RightOrLeft              ->
            (WorldState, T.Text, [T.Text]) ->
            Id                             ->
            Ent                            ->
            (WorldState, T.Text, [T.Text])
readyWpn i cols mrol a@(ws, _, _) ei e@((^.sing) -> s) | em  <- (ws^.eqTbl)  ! i
                                                       , w   <- (ws^.wpnTbl) ! ei
                                                       , sub <- w^.wpnSub = if not . isSlotAvail em $ BothHandsS
  then over _2 (<> wrapUnlines cols "You're already wielding a two-handed weapon.") a
  else case maybe (getAvailWpnSlot cols ws i em) (getDesigWpnSlot cols ws e em) mrol of
    Left  msg   -> over _2 (<> msg) a
    Right slot  -> case sub of
      OneHanded -> moveReadiedItem i cols a em slot ei . T.concat $ [ "You wield the "
                                                                    , s
                                                                    , " with your "
                                                                    , pp slot
                                                                    , "." ]
      TwoHanded
        | all (isSlotAvail em) [ RHandS, LHandS ] ->
            moveReadiedItem i cols a em BothHandsS ei $ "You wield the " <> s <> " with both hands."
        | otherwise -> over _2 (<> (wrapUnlines cols $ "Both hands are required to wield the " <> s <> ".")) a


getAvailWpnSlot :: Cols -> WorldState -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot cols ws i em | ((^.hand) -> h) <- (ws^.mobTbl) ! i =
    maybe (Left . wrapUnlines cols $ "You're already wielding two weapons.")
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
    sorryNotRing           = wrapUnlines cols $ "You can't wield a " <> s <> " with your finger!"
    sorry ((^.sing) -> s') = wrapUnlines cols . T.concat $ [ "You're already wielding a "
                                                           , s'
                                                           , " with your "
                                                           , pp desigSlot
                                                           , "." ]
    desigSlot              = case rol of R -> RHandS
                                         L -> LHandS
                                         _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-- Ready armor:


-----


unready :: Action
unready p@AdviseNoArgs            = advise p ["unready"] $ "Please specify one or more things to unready, as \
                                                           \in " <> dblQuote "unready sword" <> "."
unready   (LowerNub i mq cols as) = do
    (msg, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "unready" i logMsgs
    send mq . nl $ msg
  where
    helper = onWS $ \(t, ws) ->
        let em = (ws^.eqTbl) ! i
            is = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as is mempty
                   eiss                  = [ curry procGecrMisPCEq gecr mis | gecr <- gecrs | mis <- miss ]
                   msg                   = if null rcs then "" else nl "You can't unready coins."
                   (ws', msg', logMsgs)  = foldl' (helperUnready i cols em) (ws, msg, []) eiss
               in putTMVar t ws' >> return (msg', logMsgs)
          else putTMVar t ws >> return (wrapUnlines cols dudeYou'reNaked, [])
unready p = patternMatchFail "unready" [ showText p ]


helperUnready :: Id                             ->
                 Cols                           ->
                 EqMap                          ->
                 (WorldState, T.Text, [T.Text]) ->
                 Either T.Text Inv              ->
                 (WorldState, T.Text, [T.Text])
helperUnready i cols em a@(ws, _, _) = \case
  Left  msg -> over _2 (<> wrapUnlines cols msg) a
  Right is | pis  <- (ws^.invTbl) ! i
           , ws'  <- ws & eqTbl.at  i ?~ M.filter (`notElem` is) em
                        & invTbl.at i ?~ (sortInv ws . (pis ++) $ is)
           , msgs <- mkUnreadyDescs i ws' is
           -> set _1 ws' . over _2 (<> (T.concat . map (wrapUnlines cols) $ msgs)) . over _3 (++ msgs) $ a


mkUnreadyDescs :: Id -> WorldState -> Inv -> [T.Text]
mkUnreadyDescs i ws is = [ helper icb | icb <- mkIdCountBothList i ws is ]
  where
    helper (verb -> v, c, b@(s, _)) = T.concat $ if c == 1
      then [ "You ", v, " the ", s, "." ]
      else [ "You ", v, " ", showText c, " ", mkPlurFromBoth b, "." ]
    verb (((ws^.typeTbl) !) -> t) = case t of
      ClothType -> unwearGenericVerb -- TODO
      WpnType   -> "stop wielding"
      _         -> undefined -- TODO
    unwearGenericVerb = "take off"


mkIdCountBothList :: Id -> WorldState -> Inv -> [(Id, Int, BothGramNos)]
mkIdCountBothList i ws is | ebgns <- [ getEffBothGramNos i ws i' | i' <- is ], cs <- mkCountList ebgns =
    nubBy equalCountsAndBoths . zip3 is cs $ ebgns
  where
    equalCountsAndBoths (_, c, b) (_, c', b') = c == c' && b == b'


-----


intro :: Action
intro (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let ((^.introduced) -> intros) = (ws^.pcTbl) ! i
    in if null intros
      then let introsTxt = "No one has introduced themselves to you yet." in do
          wrapSend mq cols introsTxt
          logPlaOut "intro" i [introsTxt]
      else let introsTxt = T.intercalate ", " intros in do
          multiWrapSend mq cols [ "You know the following names:", introsTxt ]
          logPlaOut "intro" i [introsTxt]
intro (LowerNub' i as) = do
    (cbs, logMsgs) <- helper
    unless (null logMsgs) $ logPlaOut "intro" i logMsgs
    bcast . map fromClassifiedBroadcast . sort $ cbs
  where
    helper = onWS $ \(t, ws) ->
        let ((^.sing) -> s)  = (ws^.entTbl)   ! i
            ((^.rmId) -> ri) = (ws^.pcTbl)    ! i
            is               = (ws^.invTbl)   ! ri
            is'              = i `delete` is
            c                = (ws^.coinsTbl) ! ri
        in if (not . null $ is') || (c /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as is' c
                   eiss                  = [ curry procGecrMisRm gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map procReconciledCoinsRm rcs
                   (ws', cbs,  logMsgs ) = foldl' (helperIntroEitherInv s is) (ws, [],  []     ) eiss
                   (     cbs', logMsgs') = foldl' helperIntroEitherCoins      (    cbs, logMsgs) ecs
               in putTMVar t ws' >> return (cbs', logMsgs')
          else do
              putTMVar t ws
              return (mkNTBroadcast i . nlnl $ "You don't see anyone here to introduce yourself to.", [])
    helperIntroEitherInv _ _   a (Left msg) | T.null msg = a
                                            | otherwise  = over _2 (++ (mkNTBroadcast i . nlnl $ msg)) a
    helperIntroEitherInv s ris a (Right is) = foldl' tryIntro a is
      where
        tryIntro a'@(ws, _, _) targetId | targetType               <- (ws^.typeTbl) ! targetId
                                        , ((^.sing) -> targetSing) <- (ws^.entTbl)  ! targetId = case targetType of
          PCType | targetPC@((^.introduced) -> intros)   <- (ws^.pcTbl) ! targetId
                 , pis                                   <- findPCIds ws ris
                 , targetDesig                           <- serialize . mkStdDesig targetId ws targetSing False $ ris
                 , (mkReflexive . (^.sex) -> himHerself) <- (ws^.mobTbl) ! i
                 -> if s `elem` intros
                   then let msg = nlnl $ "You've already introduced yourself to " <> targetDesig <> "."
                        in over _2 (++ mkNTBroadcast i msg) a'
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
                            othersMsg = nlnl . T.concat $ [ serialize srcDesig { stdPCEntSing = Just s }
                                                          , " introduces "
                                                          , himHerself
                                                          , " to "
                                                          , targetDesig
                                                          , "." ]
                            cbs = [ NonTargetBroadcast (srcMsg,    [i])
                                  , TargetBroadcast    (targetMsg, [targetId])
                                  , NonTargetBroadcast (othersMsg, deleteFirstOfEach [ i, targetId ] pis) ]
                        in set _1 ws' . over _2 (++ cbs) . over _3 (++ [srcMsg]) $ a'
          _      | b <- NonTargetBroadcast (nlnl $ "You can't introduce yourself to a " <> targetSing <> ".", [i])
                 -> over _2 (`appendIfUnique` b) a'
    helperIntroEitherCoins a (Left  msgs) =
        over _1 (++ concat [ mkNTBroadcast i . nlnl $ msg | msg <- msgs ]) a
    helperIntroEitherCoins a (Right _   ) =
        over _1 (`appendIfUnique` NonTargetBroadcast (nlnl "You can't introduce yourself to a coin.", [i])) a
    fromClassifiedBroadcast (TargetBroadcast    b) = b
    fromClassifiedBroadcast (NonTargetBroadcast b) = b
intro p = patternMatchFail "intro" [ showText p ]


mkReflexive :: Sex -> T.Text
mkReflexive Male   = "himself"
mkReflexive Female = "herself"
mkReflexive s      = patternMatchFail "mkReflexive" [ showText s ]


-----


-- TODO: Disambiguate player names.
what :: Action
what p@AdviseNoArgs            = advise p ["what"] $ "Please specify one or more abbreviations to disambiguate, as \
                                                     \in " <> dblQuote "what up" <> "."
what   (LowerNub i mq cols as) = readWSTMVar >>= \ws ->
    let ((^.rmId) -> ri) = (ws^.pcTbl) ! i
        r                = (ws^.rmTbl) ! ri
    in logPlaExecArgs "what" as i >> (send mq . T.concat . map (helper ws r) $ as)
  where
    helper ws r n = nl . T.concat $ whatCmd cols r n : [ whatInv i cols ws it n | it <- [ PCInv, PCEq, RmInv ] ]
what p = patternMatchFail "what" [ showText p ]


whatCmd :: Cols -> Rm -> T.Text -> T.Text
whatCmd cols (mkCmdListWithNonStdRmLinks -> cmds) (T.toLower -> n@(dblQuote -> n')) =
    wrapUnlines cols . maybe notFound found . findFullNameForAbbrev n . filter isPlaCmd $ [ cmdName cmd | cmd <- cmds ]
  where
    isPlaCmd               = (`notElem` [ wizCmdChar, debugCmdChar ]) . T.head
    notFound               = n' <> " doesn't refer to any commands."
    found (dblQuote -> cn) = T.concat [ n', " may refer to the ", cn, " command." ]


whatInv :: Id -> Cols -> WorldState -> InvType -> T.Text -> T.Text
whatInv i cols ws it n | (is, gecrs, rcs) <- resolveName = if not . null $ gecrs
  then whatInvEnts i cols ws it n (head gecrs) is
  else T.concat . map (whatInvCoins cols it n) $ rcs
  where
    resolveName | (is, c) <- getLocInvCoins, (gecrs, _, rcs) <- resolveEntCoinNames i ws [n] is c = (is, gecrs, rcs)
    getLocInvCoins   = case it of PCInv -> ((ws^.invTbl)          ! i,  (ws^.coinsTbl) ! i )
                                  PCEq  -> (M.elems $ (ws^.eqTbl) ! i,  mempty             )
                                  RmInv -> ((ws^.invTbl)          ! ri, (ws^.coinsTbl) ! ri)
    ((^.rmId) -> ri) = (ws^.pcTbl) ! i


whatInvEnts :: Id -> Cols -> WorldState -> InvType -> T.Text -> GetEntsCoinsRes -> Inv -> T.Text
whatInvEnts i cols ws it@(getLocTxtForInvType -> locTxt) (dblQuote -> r) gecr is = wrapUnlines cols $ case gecr of
  Mult { entsRes = (Just es), .. }
    | nameSearchedFor == acp -> T.concat [ dblQuote acp
                                         , " may refer to everything "
                                         , locTxt
                                         , supplement
                                         , "." ]
    | e@((^.sing) -> s) <- head es, len <- length es -> if len > 1
      then let ebgns@(head -> h)         = take len [ getEffBothGramNos i ws i' | ((^.entId) -> i') <- es ]
               target | all (== h) ebgns = mkPlurFromBoth h
                      | otherwise        = (<> "s") . bracketQuote . getEffName i ws $ e^.entId
           in T.concat [ r
                       , " may refer to the "
                       , showText len
                       , " "
                       , target
                       , " "
                       , locTxt
                       , "." ]
      else let ens = [ getEffName i ws i' | i' <- is ]
           in T.concat [ r
                       , " may refer to the "
                       , T.pack . checkFirst e $ ens
                       , s
                       , " "
                       , locTxt
                       , "." ]
  Indexed { entRes = (Right e@((^.sing) -> s)), .. } -> T.concat [ r
                                                                 , " may refer to the "
                                                                 , mkOrdinal index
                                                                 , " "
                                                                 , bracketQuote . getEffName i ws $ e^.entId
                                                                 , " "
                                                                 , parensQuote s
                                                                 , " "
                                                                 , locTxt
                                                                 , "." ]
  _                                                  -> T.concat [ r
                                                                 , " doesn't refer to anything "
                                                                 , locTxt
                                                                 , "." ]
  where
    acp                                     = T.singleton allChar
    supplement | it `elem` [ PCInv, RmInv ] = " " <> parensQuote "including any coins"
               | otherwise                  = ""
    checkFirst e ens | en <- getEffName i ws $ e^.entId, matches <- filter (== en) ens =
        guard (length matches > 1) >> "first "


getLocTxtForInvType :: InvType -> T.Text
getLocTxtForInvType = \case PCInv -> "in your inventory"
                            PCEq  -> "in your readied equipment"
                            RmInv -> "in this room"


whatInvCoins :: Cols -> InvType -> T.Text -> ReconciledCoins -> T.Text
whatInvCoins cols it@(getLocTxtForInvType -> locTxt) (dblQuote -> r) rc
  | it == PCEq = ""
  | otherwise  = wrapUnlines cols $ case rc of
    Left  Empty                                 -> T.concat [ r
                                                            , " doesn't refer to any coins "
                                                            , locTxt
                                                            , " "
                                                            , supplementNone "coins"
                                                            , "." ]
    Left  (NoneOf (mkTxtForCoins -> cn))        -> T.concat [ r
                                                            , " doesn't refer to any "
                                                            , cn
                                                            , " "
                                                            , locTxt
                                                            , " "
                                                            , supplementNone cn
                                                            , "." ]
    Left  (SomeOf (mkTxtForCoins -> cn))        -> T.concat [ r
                                                            , " doesn't refer to any "
                                                            , cn
                                                            , " "
                                                            , locTxt
                                                            , " "
                                                            , supplementNotEnough cn
                                                            , "." ]
    Right (SomeOf (mkTxtForCoinsWithAmt -> cn)) -> T.concat [ r
                                                            , " may refer to the "
                                                            , cn
                                                            , " "
                                                            , locTxt
                                                            , "." ]
    _                                           -> patternMatchFail "whatInvCoins" [ showText rc ]
  where
    supplementNone cn      = case it of PCInv -> parensQuote $ "you don't have any "       <> cn
                                        RmInv -> parensQuote $ "there aren't any "         <> cn <> " here"
                                        PCEq  -> oops "supplementNone"
    supplementNotEnough cn = case it of PCInv -> parensQuote $ "you don't have that many " <> cn
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
uptime (NoArgs i mq cols) = do
    logPlaExec "uptime" i
    wrapSend mq cols =<< uptimeHelper =<< getUptime
uptime p = withoutArgs uptime p


uptimeHelper :: Integer -> MudStack T.Text
uptimeHelper ut = helper <$> getRecordUptime
  where
    helper = \case Nothing  -> mkUptimeTxt
                   Just rut -> case ut `compare` rut of GT -> mkNewRecTxt
                                                        _  -> mkRecTxt rut
    mkUptimeTxt                = mkTxtHelper "."
    mkNewRecTxt                = mkTxtHelper " - it's a new record!"
    mkRecTxt (renderIt -> rut) = mkTxtHelper $ " (record uptime: " <> rut <> ")."
    mkTxtHelper                = ("Up " <>) . (renderIt ut <>)
    renderIt                   = T.pack . renderSecs


getUptime :: MudStack Integer
getUptime = round <$> diff
  where
    diff = diffUTCTime <$> liftIO getCurrentTime <*> getNWSRec startTime


-----


quit :: Action
quit (NoArgs' i mq)                        = (liftIO . atomically . writeTQueue mq $ Quit) >> logPlaExec "quit" i
quit ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols msg
  where
    msg = "Type " <> dblQuote "quit" <> " with no arguments to quit the game."


handleEgress :: Id -> MudStack ()
handleEgress i = do
    notifyEgress i
    wsTMVar  <- getWSTMVar
    mqtTMVar <- getNWSRec msgQueueTblTMVar
    ptTMVar  <- getNWSRec plaTblTMVar
    (parensQuote -> n) <- liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        mqt <- takeTMVar mqtTMVar
        pt  <- takeTMVar ptTMVar
        -----
        let ((^.sing)     -> s)   = (ws^.entTbl) ! i
        let ((^.rmId)     -> ri)  = (ws^.pcTbl)  ! i
        let ((i `delete`) -> ris) = (ws^.invTbl) ! ri
        let ws'                   = ws  & typeTbl.at  i  .~ Nothing
                                        & entTbl.at   i  .~ Nothing
                                        & invTbl.at   i  .~ Nothing
                                        & coinsTbl.at i  .~ Nothing
                                        & eqTbl.at    i  .~ Nothing
                                        & mobTbl.at   i  .~ Nothing
                                        & pcTbl.at    i  .~ Nothing
                                        & invTbl.at   ri ?~ ris
        let mqt'                  = mqt & at i .~ Nothing
        let pt'                   = pt  & at i .~ Nothing
        -----
        putTMVar wsTMVar  ws'
        putTMVar mqtTMVar mqt'
        putTMVar ptTMVar  pt'
        return s
    logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", n, " has left the game." ]
    closePlaLog i


notifyEgress :: Id -> MudStack ()
notifyEgress i = readWSTMVar >>= \ws ->
    let ((^.sing)     -> s)   = (ws^.entTbl) ! i
        ((^.rmId)     -> ri)  = (ws^.pcTbl)  ! i
        ris                   = (ws^.invTbl) ! ri
        ((i `delete`) -> pis) = findPCIds ws ris
        d                     = serialize . mkStdDesig i ws s True $ ris
    in bcast [(nlnl $ d <> " has left the game.", pis)]


-- ==================================================
-- Wizard commands:


wizDispCmdList :: Action
wizDispCmdList p@(LowerNub' i as) = do
    logPlaExecArgs (prefixWizCmd "?") as i
    dispCmdList (mkCmdPred . Just $ wizCmdChar) p
wizDispCmdList p = patternMatchFail "wizDispCmdList" [ showText p ]


-----


wizShutdown :: Action
wizShutdown (NoArgs' i mq) = readWSTMVar >>= \ws ->
    let ((^.sing) -> s) = (ws^.entTbl) ! i in do
        massSend "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"
        logPlaExecArgs (prefixWizCmd "shutdown") [] i
        massLogPla "wizShutdown" $ T.concat [ "closing connection due to server shutdown initiated by "
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
wizShutdown (WithArgs i mq _ as) = readWSTMVar >>= \ws ->
    let ((^.sing) -> s) = (ws^.entTbl) ! i
        msg             = T.intercalate " " as
    in do
        massSend msg
        logPlaExecArgs (prefixWizCmd "shutdown") as i
        massLogPla "wizShutdown" . T.concat $ [ "closing connection due to server shutdown initiated by "
                                              , s
                                              , "; reason: "
                                              , msg
                                              , "." ]
        logNotice  "wizShutdown" . T.concat $ [ "server shutdown initiated by ", s, "; reason: ", msg, "." ]
        liftIO . atomically . writeTQueue mq $ Shutdown
wizShutdown _ = patternMatchFail "wizShutdown" []


-----


wizTime :: Action
wizTime (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "time") i
    (ct, zt) <- (,) <$> liftIO (formatThat `fmap` getCurrentTime) <*> liftIO (formatThat `fmap` getZonedTime)
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
  where
    formatThat (T.words . showText -> wordy@((,) <$> head <*> last -> (date, zone)))
      | time <- T.init . T.reverse . T.dropWhile (/= '.') . T.reverse . head . tail $ wordy
      = T.concat [ zone, ": ", date, " ", time ]
wizTime p = withoutArgs wizTime p


-----


wizDate :: Action
wizDate (NoArgs' i mq) = do
    logPlaExec (prefixWizCmd "date") i
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
wizDate p = withoutArgs wizDate p


-----


wizUptime :: Action
wizUptime (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "uptime") i
    (try . send mq . parse =<< runUptime) >>= eitherRet (\e -> logIOEx "wizUptime" e >> sendGenericErrorMsg mq cols)
  where
    runUptime = liftIO . readProcess "uptime" [] $ ""
    parse (span (/= ',') -> (unwords . tail . words -> a, dropWhile isSpace . takeWhile (/= ',') . tail -> b)) =
        nlnl . T.concat $ [ capitalize . T.pack $ a, " ", T.pack b, "." ]
wizUptime p = withoutArgs wizUptime p


-----


wizStart :: Action
wizStart (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "start") i
    wrapSend mq cols . showText =<< getNWSRec startTime
wizStart p = withoutArgs wizStart p


-----


wizName :: Action
wizName (NoArgs i mq cols) = do
    logPlaExec (prefixWizCmd "name") i
    readWSTMVar >>= \ws ->
        let ((^.sing) -> s)     = (ws^.entTbl) ! i
            (pp -> s', pp -> r) = getSexRace i ws
        in wrapSend mq cols . T.concat $ [ "You are ", s, " (a ", s', " ", r, ")." ]
wizName p = withoutArgs wizName p


-- ==================================================
-- Debug commands:


debugDispCmdList :: Action
debugDispCmdList p@(LowerNub' i as) = do
    logPlaExecArgs (prefixDebugCmd "?") as i
    dispCmdList (mkCmdPred . Just $ debugCmdChar) p
debugDispCmdList p = patternMatchFail "debugDispCmdList" [ showText p ]


-----


debugBuffCheck :: Action
debugBuffCheck (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "buffer") i
    try helper >>= eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = do
        (fn@(dblQuote . T.pack -> fn'), h) <- liftIO $ flip openTempFile "temp" =<< getTemporaryDirectory
        (dblQuote . showText -> mode)      <- liftIO . hGetBuffering $ h
        send mq . nl . T.unlines . wordWrapIndent 2 cols . T.concat $ [ parensQuote "Default"
                                                                      , " buffering mode for temp file "
                                                                      , fn'
                                                                      , " is "
                                                                      , mode
                                                                      , "." ]
        liftIO $ hClose h >> removeFile fn
debugBuffCheck p = withoutArgs debugBuffCheck p


-----


debugDispEnv :: Action
debugDispEnv (NoArgs i mq cols) = do
    logPlaExecArgs (prefixDebugCmd "env") [] i
    send mq . nl =<< (mkAssocListTxt cols <$> liftIO getEnvironment)
debugDispEnv (WithArgs i mq cols (nub -> as)) = do
    logPlaExecArgs (prefixDebugCmd "env") as i
    env <- liftIO getEnvironment
    send mq . T.unlines $ [ helper env a | a <- as ]
  where
    helper env a = mkAssocListTxt cols . filter grepPair $ env
      where
        grepPair = uncurry (||) . over both ((a `T.isInfixOf`) . T.pack)
debugDispEnv p = patternMatchFail "debugDispEnv" [ showText p ]


-----


debugLog :: Action
debugLog (NoArgs' i mq) = logPlaExec (prefixDebugCmd "log") i >> helper >> ok mq
  where
    helper       = replicateM 100 . statefulFork $ heavyLogging
    heavyLogging = liftIO myThreadId >>=
        replicateM_ 100 . logNotice "debugLog" . (<> ".") . ("Logging from " <>) . showText
debugLog p = withoutArgs debugLog p


------


debugThrow :: Action
debugThrow (NoArgs'' i) = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow p            = withoutArgs debugThrow p


-----


debugThread :: Action
debugThread (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "thread") i
    (nli, eli) <- over both asyncThreadId <$> getLogAsyncs
    kvs        <- M.assocs <$> readTMVarInNWS threadTblTMVar
    (es, ks)   <- let f = (,) <$> IM.elems <*> IM.keys in f `fmap` readTMVarInNWS plaLogTblTMVar
    ds         <- mapM mkDesc $ head kvs      :
                                (nli, Notice) :
                                (eli, Error)  :
                                tail kvs ++ [ (asyncThreadId . fst $ e, PlaLog k) | e <- es | k <- ks ]
    send mq . frame cols . multiWrap cols $ ds
  where
    mkDesc (ti, bracketPad 15 . mkTypeName -> tn) = (liftIO . threadStatus $ ti) >>= \ts ->
        return . T.concat $ [ showText ti, " ", tn, showText ts ]
    mkTypeName (Server (showText -> i')) = padOrTrunc 8 "Server" <> i'
    mkTypeName (PlaLog (showText -> i')) = padOrTrunc 8 "PlaLog" <> i'
    mkTypeName (showText -> tt)          = tt
debugThread p = withoutArgs debugThread p


-----


debugTalk :: Action
debugTalk (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "talk") i
    send mq . frame cols . multiWrap cols =<< mapM mkDesc . M.elems =<< readTMVarInNWS talkAsyncTblTMVar
  where
    mkDesc a = (liftIO . poll $ a) >>= \status ->
        let statusTxt = case status of Nothing                                    -> "running"
                                       Just (Left  (parensQuote . showText -> e)) -> "exception " <> e
                                       Just (Right ())                            -> "finished"
        in return . T.concat $ [ "Talk async ", showText . asyncThreadId $ a, ": ", statusTxt, "." ]
debugTalk p = withoutArgs debugTalk p


-----


debugPurge :: Action
debugPurge (NoArgs' i mq) = logPlaExec (prefixDebugCmd "purge") i >> purge >> ok mq
debugPurge p              = withoutArgs debugPurge p


-- TODO: This function could be automatically run at certain intervals.
purge :: MudStack ()
purge = logNotice "purge" "purging the thread tables." >> purgePlaLogTbl >> purgeThreadTbl >> purgeTalkAsyncTbl


purgePlaLogTbl :: MudStack ()
purgePlaLogTbl = IM.assocs <$> readTMVarInNWS plaLogTblTMVar >>= \kvs -> do
    let is     = [ fst kv         | kv <- kvs ]
    let asyncs = [ fst . snd $ kv | kv <- kvs ]
    modifyNWS plaLogTblTMVar . flip (foldl' helper) . zip is =<< (liftIO . mapM poll $ asyncs)
  where
    helper m (_, Nothing) = m
    helper m (i, _      ) = IM.delete i m


purgeThreadTbl :: MudStack ()
purgeThreadTbl = do
    tis <- M.keys <$> readTMVarInNWS threadTblTMVar
    ss  <- liftIO . mapM threadStatus $ tis
    modifyNWS threadTblTMVar . flip (foldl' helper) . zip tis $ ss
  where
    helper m (ti, s) | s == ThreadFinished = M.delete ti m
                     | otherwise           = m


purgeTalkAsyncTbl :: MudStack ()
purgeTalkAsyncTbl = do
    asyncs <- M.elems <$> readTMVarInNWS talkAsyncTblTMVar
    ss     <- liftIO . mapM poll $ asyncs
    modifyNWS talkAsyncTblTMVar . flip (foldl' helper) . zip asyncs $ ss
  where
    helper m (_, Nothing) = m
    helper m (a, _      ) = M.delete (asyncThreadId a) m


-----


debugBoot :: Action
debugBoot (NoArgs' i mq) = logPlaExec (prefixDebugCmd "boot") i >> ok mq >> massMsg Boot
debugBoot p              = withoutArgs debugBoot p


-----


debugStop :: Action
debugStop (NoArgs' i mq) = logPlaExec (prefixDebugCmd "stop") i >> ok mq >> massMsg StopThread
debugStop p              = withoutArgs debugStop p


-----


debugCPU :: Action
debugCPU (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "cpu") i
    wrapSend mq cols . ("CPU time: " <>) =<< liftIO cpuTime
  where
    cpuTime = showText . (/ fromIntegral (10 ^ 12)) . fromIntegral <$> getCPUTime
debugCPU p = withoutArgs debugCPU p


-----


debugBroad :: Action
debugBroad (NoArgs'' i) = do
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
debugBroad p = withoutArgs debugBroad p


-----


debugRemPut :: Action
debugRemPut (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "remput") i
    mapM_ (fakeClientInput mq) . take 10 . cycle $ [ remCmd, putCmd ]
  where
    remCmd = "remove" <> rest
    putCmd = "put"    <> rest
    rest   = T.concat [ " ", T.singleton allChar, " ", T.singleton rmChar, "sack" ]
debugRemPut p = withoutArgs debugRemPut p


fakeClientInput :: MsgQueue -> T.Text -> MudStack ()
fakeClientInput mq = liftIO . atomically . writeTQueue mq . FromClient . nl


-----


debugParams :: Action
debugParams p@(WithArgs i mq cols _) = do
    logPlaExec (prefixDebugCmd "params") i
    wrapSend mq cols . showText $ p
debugParams p = patternMatchFail "debugParams" [ showText p ]


-----


debugColor :: Action
debugColor (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "color") i
    send mq . nl . T.concat $ do
        fgi <- intensities
        fgc <- colors
        bgi <- intensities
        bgc <- colors
        let fg   = (fgi, fgc)
        let bg   = (bgi, bgc)
        let ansi = mkColorANSI fg bg
        return . nl . T.concat $ [ mkANSICodeList ansi, mkColorDesc fg bg, ansi, " CurryMUD ", dfltColorANSI ]
  where
    mkANSICodeList = padOrTrunc 28 . T.pack . concatMap ((++ " ") . show . ord) . T.unpack
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName (padOrTrunc 6 . showText -> intensity, padOrTrunc 8 . showText -> color) = intensity <> color
debugColor p = withoutArgs debugColor p
