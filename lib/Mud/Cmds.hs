{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.Cmds (topLvlWrapper) where

import Mud.Ids
import Mud.Logging hiding (logAndDispIOEx, logExMsg, logIOEx, logIOExRethrow, logNotice)
import Mud.MiscDataTypes
import Mud.NameResolution
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.StateInIORefT
import Mud.TheWorld
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Logging as L (logAndDispIOEx, logExMsg, logIOEx, logIOExRethrow, logNotice)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Arrow (first)
import Control.Concurrent (forkFinally, forkIO, killThread, myThreadId, ThreadId)
import Control.Concurrent.Async (asyncThreadId, race_)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar, TMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), AsyncException(..), fromException, IOException, SomeException)
import Control.Exception.Lifted (catch, finally, throwIO, throwTo, try)
import Control.Lens (at, both, folded, over, to)
import Control.Lens.Operators ((&), (?~), (.~), (^.), (^..))
import Control.Monad (forever, guard, mplus, replicateM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Char (isSpace, toUpper)
import Data.Functor ((<$>))
import Data.IntMap.Lazy ((!))
import Data.List (delete, find, foldl', intercalate, nub, nubBy, sort, zip4)
import Data.Maybe (isNothing)
import Data.Monoid ((<>), mempty)
import Data.Text.Strict.Lens (packed)
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import GHC.Conc (threadStatus, ThreadStatus(..))
import Network (accept, listenOn, PortID(..))
import Prelude hiding (pi)
import System.Directory (getDirectoryContents, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetBuffering, hGetLine, hPutStr, hSetBuffering, hSetNewlineMode, openTempFile, universalNewlineMode)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import System.Random (newStdGen, randomR) -- TODO: Use mwc-random or tf-random. QC uses tf-random.
import qualified Data.Map.Lazy as M (assocs, delete, elems, empty, filter, keys, null, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- TODO: Here's the plan:
-- [DONE] 1. Consider using conduit.
-- [DONE] 2. Go into server mode. Accept incoming connections.
-- [DONE] 3. Implement client-based routing of output.
-- 4. Implement the broadcasting of messages.
-- 5. Review your coding guide, and undertake a refactoring of the entire codebase. Consider the following:
-- a. Code reduction.
-- b. Consistency in binding names.
-- c. Stylistic issues:
--    [DONE] Use "++" instead of "<>" where applicable.
--    [DONE] Concat lists of text instead of using "<>".
--    [DONE] ">>=" vs. "=<<".
--    [DONE] "(..)" instead of "(blah)" in import statements.
-- d. [DONE] Check for superfluous exports.


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Cmds"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds"


logNotice :: String -> String -> MudStack ()
logNotice = L.logNotice "Mud.Cmds"


logIOEx :: String -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds"


logAndDispIOEx :: MsgQueue -> Cols -> String -> IOException -> MudStack ()
logAndDispIOEx mq cols = L.logAndDispIOEx mq cols "Mud.Cmds"


logIOExRethrow :: String -> IOException -> MudStack ()
logIOExRethrow = L.logIOExRethrow "Mud.Cmds"


logExMsg :: String -> String -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Cmds"


-- ==================================================


cmdList :: [Cmd]
cmdList = -- ==================================================
          -- Wizard commands:
          [ Cmd { cmdName = prefixWizCmd "?", action = wizDispCmdList, cmdDesc = "Display this command list." }
          , Cmd { cmdName = prefixWizCmd "day", action = wizDay, cmdDesc = "Display the current day of week." }
          , Cmd { cmdName = prefixWizCmd "shutdown", action = wizShutdown, cmdDesc = "Shut down the game server." }
          , Cmd { cmdName = prefixWizCmd "time", action = wizTime, cmdDesc = "Display the current system time." }

          -- ==================================================
          -- Debug commands:
          , Cmd { cmdName = prefixDebugCmd "?", action = debugDispCmdList, cmdDesc = "Display this command list." }
          , Cmd { cmdName = prefixDebugCmd "buffer", action = debugBuffCheck, cmdDesc = "Confirm the default buffering mode." }
          , Cmd { cmdName = prefixDebugCmd "env", action = debugDispEnv, cmdDesc = "Display system environment variables." }
          , Cmd { cmdName = prefixDebugCmd "log", action = debugLog, cmdDesc = "Put the logging service under heavy load." }
          , Cmd { cmdName = prefixDebugCmd "purge", action = debugPurge, cmdDesc = "Purge the thread table." }
          , Cmd { cmdName = prefixDebugCmd "sniff", action = debugSniff, cmdDesc = "Sniff out a dirty thread." }
          , Cmd { cmdName = prefixDebugCmd "throw", action = debugThrow, cmdDesc = "Throw an exception." }

          -- ==================================================
          -- Player commands:
          , Cmd { cmdName = "?", action = plaDispCmdList, cmdDesc = "Display this command list." }
          , Cmd { cmdName = "about", action = about, cmdDesc = "About this game." }
          , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
          , Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop items on the ground." }
          , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
          , Cmd { cmdName = "equip", action = equip, cmdDesc = "Display readied equipment." }
          , Cmd { cmdName = "exits", action = exits, cmdDesc = "Display obvious exits." }
          , Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick items up off the ground." }
          , Cmd { cmdName = "help", action = help, cmdDesc = "Get help on topics or commands." }
          , Cmd { cmdName = "inv", action = inv, cmdDesc = "Inventory." }
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
          , Cmd { cmdName = "uptime", action = uptime, cmdDesc = "Display game server uptime." }
          , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." }
          , Cmd { cmdName = "what", action = what, cmdDesc = "Disambiguate abbreviations." } ]


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd c cn = T.pack [c] <> cn


prefixWizCmd :: CmdName -> T.Text
prefixWizCmd = prefixCmd wizCmdChar


prefixDebugCmd :: CmdName -> T.Text
prefixDebugCmd = prefixCmd debugCmdChar


topLvlWrapper :: MudStack ()
topLvlWrapper = (initAndStart `catch` topLvlExHandler) `finally` closeLogs
  where
    initAndStart = do
        initLogging
        logNotice "topLvlWrapper" "server started"
        initWorld
        listen


topLvlExHandler :: SomeException -> MudStack ()
topLvlExHandler e = let oops msg = logExMsg "topLvlExHandler" msg e
                    in case fromException e of
                      Just UserInterrupt -> logNotice "topLvlExHandler" "exiting on user interrupt"
                      Just ThreadKilled  -> logNotice "topLvlExHandler" "thread killed"
                      Just _             -> (oops . (showIt ++) $ " caught by the top level handler; rethrowing") >> throwIO e
                      Nothing            -> oops "exception caught by the top level handler; exiting gracefully"  >> liftIO exitFailure
  where
    showIt = dblQuoteStr . show $ e


listen :: MudStack ()
listen = do
    registerThread Listen
    logNotice "listen" $ "listening for incoming connections on port " ++ show port
    sock <- liftIO . listenOn . PortNumber . fromIntegral $ port
    forever $ do
        (h, host, port') <- liftIO . accept $ sock
        logNotice "listen" . concat $ [ "connected to ", show host, " on local port ", show port' ]
        s <- get
        liftIO $ forkFinally (runStateInIORefT (talk h) s) (\_ -> hClose h)


registerThread :: ThreadType -> MudStack ()
registerThread threadType = liftIO myThreadId >>= \ti ->
    onNWS threadTblTMVar $ \(t, tt) ->
        let tt' = tt & at ti ?~ threadType
        in putTMVar t tt'


-- TODO: Spawn a log for the new player.
talk :: Handle -> MudStack ()
talk h = do
    liftIO configBuffer
    mq <- liftIO newTQueueIO
    i  <- adHoc mq
    logNotice "talk" $ "new ID for incoming player: " ++ show i
    dumpTitle mq
    prompt mq "> "
    notifyArrival i
    s <- get
    liftIO $ race_ (runStateInIORefT (server  h i mq) s)
                   (runStateInIORefT (receive h i mq) s)
  where
    configBuffer = hSetNewlineMode h universalNewlineMode >> hSetBuffering h LineBuffering


adHoc :: MsgQueue -> MudStack Id
adHoc mq = do
    wsTMVar  <- getWSTMVar
    ptTMVar  <- getNWSTMVar plaTblTMVar
    mqtTMVar <- getNWSTMVar msgQueueTblTMVar
    liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        pt  <- takeTMVar ptTMVar
        mqt <- takeTMVar mqtTMVar
        -----
        let i   = getUnusedId ws
        -----
        let e   = Ent i "player" ("PlayerCharacter" <> showText i) "" "You see an ad-hoc player character." 0
        let is  = []
        let co  = mempty
        let em  = M.empty
        let m   = Mob Male 10 10 10 10 10 10 0 RHand
        let pc  = PC iHill Human
        let ris = i : (ws^.invTbl) ! iHill
        -----
        let pla = Pla 80
        -----
        let ws'  = ws  & typeTbl.at  i     ?~ PCType
                       & entTbl.at   i     ?~ e
                       & invTbl.at   i     ?~ is
                       & coinsTbl.at i     ?~ co
                       & eqTbl.at    i     ?~ em
                       & mobTbl.at   i     ?~ m
                       & pcTbl.at    i     ?~ pc
                       & invTbl.at   iHill ?~ ris
        let pt'  = pt  & at i ?~ pla
        let mqt' = mqt & at i ?~ mq
        -----
        putTMVar wsTMVar  ws'
        putTMVar ptTMVar  pt'
        putTMVar mqtTMVar mqt'
        -----
        return i


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO newStdGen >>= \g ->
    let range = (1, noOfTitles)
        n     = fst . randomR range $ g
        fn    = T.unpack "title" ++ show n
    in (try . takeADump $ fn) >>= eitherRet (readFileExHandler "dumpTitle")
  where
    takeADump fn = send mq =<< (nl <$> (liftIO . T.readFile . (titleDir ++) $ fn))


readFileExHandler :: String -> IOException -> MudStack ()
readFileExHandler fn e
  | isDoesNotExistError e = logIOEx fn e
  | isPermissionError   e = logIOEx fn e
  | otherwise             = logIOExRethrow fn e


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


notifyArrival :: Id -> MudStack ()
notifyArrival i = getWS >>= \ws ->
    let e    = (ws^.entTbl) ! i
        p    = (ws^.pcTbl)  ! i
        ris  = (ws^.invTbl) ! (p^.rmId)
        pcIs = findPCIds ws . delete i $ ris
    in broadcast [ (e^.sing <> " has arrived in the game.", pcIs) ]


server :: Handle -> Id -> MsgQueue -> MudStack ()
server h i mq = (registerThread . Server $ i) >> loop `catch` serverExHandler
  where
    loop = (liftIO . atomically . readTQueue $ mq) >>= \case
      FromServer msg -> (liftIO . hPutStr h . T.unpack . injectCR $ msg) >> loop
      FromClient msg -> let msg' = T.strip . T.pack . stripTelnet . T.unpack $ msg
                        in unless (T.null msg') (handleInp i mq msg')    >> loop
      Prompt     p   -> liftIO ((hPutStr h . T.unpack $ p) >> hFlush h)  >> loop
      Quit       msg -> (liftIO . hPutStr h . T.unpack . injectCR $ msg) >> handleQuit i
      Shutdown       -> commitSuicide


-- TODO: Move? Also write an hunit test for the following:
-- [ 255, 252, 3, 255, 250, 201, 67, 111, 114, 101, 46, 83, 117, 112, 112, 111, 114, 116, 115, 46, 83, 101, 116, 32, 91, 93, 255, 240 ]
stripTelnet :: String -> String
stripTelnet msg@(x:y:_:rest)
  | x == telnetIAC = if y == telnetSB
                       then tail . dropWhile (/= telnetSE) $ rest
                       else stripTelnet rest
  | otherwise      = msg
stripTelnet msg = msg


serverExHandler :: SomeException -> MudStack ()
serverExHandler e = do
    logExMsg "serverExHandler" "exception caught on server thread; rethrowing" e
    ti <- getListenThreadId
    liftIO . throwTo ti $ e


getListenThreadId :: MudStack ThreadId
getListenThreadId = reverseLookup Listen <$> getNWS threadTblTMVar


commitSuicide :: MudStack ()
commitSuicide = liftIO . killThread =<< getListenThreadId


receive :: Handle -> Id -> MsgQueue -> MudStack ()
receive h i mq = do
    registerThread . Receive $ i
    forever . liftIO $ atomically . writeTQueue mq . FromClient . T.pack =<< hGetLine h


handleInp :: Id -> MsgQueue -> T.Text -> MudStack ()
handleInp i mq = maybeVoid (dispatch i mq) . splitInp


type Input = (CmdName, Rest)


splitInp :: T.Text -> Maybe Input
splitInp = splitUp . T.words
  where
    splitUp []     = Nothing
    splitUp [t]    = Just (t, [])
    splitUp (t:ts) = Just (t, ts)


dispatch :: Id -> MsgQueue -> Input -> MudStack ()
dispatch i mq (cn, rest) = do
    cols <- getPlaColumns i
    findAction i cn >>= maybe sorry (\act -> act (i, mq, cols) rest)
    prompt mq "> "
  where
    sorry = send mq . nlnl $ "What?"


findAction :: Id -> CmdName -> MudStack (Maybe Action)
findAction i cn = getWS >>= \ws ->
    let p        = (ws^.pcTbl) ! i
        r        = (ws^.rmTbl) ! (p^.rmId)
        cmdList' = mkCmdListWithNonStdRmLinks r
        cns      = map cmdName cmdList'
    in maybe (return Nothing)
             (\fn -> return . Just . findActionForFullName fn $ cmdList')
             (findFullNameForAbbrev (T.toLower cn) cns)
  where
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks r = cmdList ++ [ mkCmdForRmLink rl | rl <- r^.rmLinks, isNonStdLink rl ]


isNonStdLink :: RmLink -> Bool
isNonStdLink (NonStdLink _ _ _ _) = True
isNonStdLink _                    = False


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink rl = let cn = T.toLower . mkCmdNameForRmLink $ rl
                    in Cmd { cmdName = cn, action = go cn, cmdDesc = "" }
  where
    mkCmdNameForRmLink (StdLink    dir _    ) = linkDirToCmdName dir
    mkCmdNameForRmLink (NonStdLink ln  _ _ _) = ln


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
about (_, mq, cols) [] = try helper >>= eitherRet (\e -> readFileExHandler "about" e >> sendGenericErrorMsg mq cols)
  where
    helper = send mq . nl . T.unlines . concatMap (wordWrap cols) . T.lines =<< (liftIO . T.readFile . (miscDir ++) $ "about")
about mic@(_, mq, cols) rs = ignore mq cols rs >> about mic []


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = send mq . nl . T.unlines . wordWrap cols $ genericErrorMsg


ignore :: MsgQueue -> Cols -> Rest -> MudStack ()
ignore mq cols rs = let ignored = dblQuote . T.unwords $ rs
                    in send mq . T.unlines . wordWrap cols $ "(Ignoring " <> ignored <> "...)"


-----


-- TODO: Automatically execute this cmd after a user authenticates.
motd :: Action
motd     (_, mq, cols) [] = send mq =<< getMotdTxt cols
motd mic@(_, mq, cols) rs = ignore mq cols rs >> motd mic []


getMotdTxt :: Cols -> MudStack T.Text
getMotdTxt cols = (try . liftIO $ helper) >>= eitherRet (\e -> readFileExHandler "getMotdTxt" e >> (return . nl . T.unlines . wordWrap cols $ "Unfortunately, the message of the day could not be retrieved."))
  where
    helper  = return . T.unlines . (++ [ divider, "" ]) . (divider :) . concatMap (wordWrap cols) . T.lines =<< (T.readFile . (miscDir ++) $ "motd")
    divider = mkDividerTxt cols


-----


plaDispCmdList :: Action
plaDispCmdList = dispCmdList (cmdPred Nothing)


dispCmdList :: (Cmd -> Bool) -> Action
dispCmdList p (_, mq, cols) [] = send mq . nl . T.unlines . concatMap (wordWrapIndent 10 cols) . cmdListText $ p
dispCmdList p (_, mq, cols) rs = send mq . nl . T.unlines . concatMap (wordWrapIndent 10 cols) . intercalate [""] $ [ grepTextList r . cmdListText $ p | r <- nub . map T.toLower $ rs ]


cmdListText :: (Cmd -> Bool) -> [T.Text]
cmdListText p = sort . T.lines . T.concat . foldl' mkTxtForCmd [] . filter p $ cmdList
  where
    mkTxtForCmd acc c = T.concat [ padOrTrunc 10 . cmdName $ c, cmdDesc c, "\n" ] : acc


cmdPred :: Maybe Char -> Cmd -> Bool
cmdPred (Just c) cmd = c == (T.head . cmdName $ cmd)
cmdPred Nothing  cmd = (T.head . cmdName $ cmd) `notElem` [wizCmdChar, debugCmdChar]


-----


help :: Action
help (_, mq, cols) [] = try helper >>= eitherRet (\e -> readFileExHandler "help" e >> sendGenericErrorMsg mq cols)
  where
    helper   = send mq . nl . T.unlines . concat . wordWrapLines cols . T.lines =<< readRoot
    readRoot = liftIO . T.readFile . (helpDir ++) $ "root"
help (_, mq, cols) rs = send mq . nl . T.unlines . intercalate [ "", mkDividerTxt cols, "" ] =<< getTopics
  where
    getTopics = mapM (\r -> concat . wordWrapLines cols . T.lines <$> getHelpTopicByName cols r) (nub . map T.toLower $ rs)


type HelpTopic = T.Text


getHelpTopicByName :: Cols -> HelpTopic -> MudStack T.Text
getHelpTopicByName cols r = (liftIO . getDirectoryContents $ helpDir) >>= \fns ->
    let fns' = tail . tail . sort . delete "root" $ fns
        tns  = fns'^..folded.packed
    in maybe sorry
             getHelpTopic
             (findFullNameForAbbrev r tns)
  where
    sorry           = return $ "No help is available on " <> dblQuote r <> "."
    getHelpTopic tn = (try . helper $ tn) >>= eitherRet (\e -> readFileExHandler "getHelpTopicByName" e >> (return . T.unlines . wordWrap cols $ "Unfortunately, the " <> dblQuote tn <> " help file could not be retrieved."))
    helper       tn = liftIO . T.readFile . (helpDir ++) . T.unpack $ tn


-----


go :: T.Text -> Action
go dir imc [] = goDispatcher imc [dir]
go dir imc rs = goDispatcher imc . (dir :) $ rs


goDispatcher :: Action
goDispatcher _   [] = return ()
goDispatcher imc rs = mapM_ (tryMove imc) rs


tryMove :: IdMsgQueueCols -> T.Text -> MudStack ()
tryMove imc@(i, mq, cols) dir = let dir' = T.toLower dir
                                in helper dir' >>= \case
                                  Left  msg -> send mq . nl . T.unlines . wordWrap cols $ msg
                                  Right bs  -> broadcast bs >> look imc []
  where
    helper dir' = onWS $ \(t, ws) ->
        let e   = (ws^.entTbl) ! i
            p   = (ws^.pcTbl)  ! i
            ri  = p^.rmId
            r   = (ws^.rmTbl)  ! ri
            ris = (ws^.invTbl) ! ri
        in case findExit r dir' of
          Nothing  -> putTMVar t ws >> (return . Left . sorry $ dir')
          Just ri' -> let p'        = p & rmId .~ ri'
                          originIs  = delete i ris
                          destIs    = (ws^.invTbl) ! ri'
                          destIs'   = sortInv ws $ i : destIs
                          originPis = findPCIds ws originIs
                          destPis   = findPCIds ws destIs
                          msgAtOrigin
                            | dir' `elem` stdLinkNames = T.concat [ e^.sing, " ", verb dir', " ", expandLinkName dir', "." ]
                            | otherwise = undefined -- TODO: message for non-standard link names.
                          msgAtDest
                            | dir' `elem` stdLinkNames = T.concat [ e^.sing, " arrives from ", expandOppLinkName dir', "." ]
                            | otherwise = undefined -- TODO: message for non-standard link names.
                      in do
                          putTMVar t (ws & pcTbl.at  i   ?~ p'
                                         & invTbl.at ri  ?~ originIs
                                         & invTbl.at ri' ?~ destIs')
                          return . Right $ [ (msgAtOrigin, originPis), (msgAtDest, destPis) ]
    sorry dir' = if dir' `elem` stdLinkNames
                   then "You can't go that way."
                   else dblQuote dir <> " is not a valid exit."
    verb  dir'
      | dir' == "u"              = "goes"
      | dir' == "d"              = "heads"
      | dir' `elem` stdLinkNames = "leaves"
      | otherwise                = "enters"


findExit :: Rm -> LinkName -> Maybe Id
findExit r ln = case [ getDestId rl | rl <- r^.rmLinks, isValid rl ] of
                  [] -> Nothing
                  is -> Just . head $ is
  where
    isValid   (StdLink    dir _    ) = ln == linkDirToCmdName dir
    isValid   (NonStdLink ln' _ _ _) = ln `T.isInfixOf` ln'
    getDestId (StdLink    _   i    ) = i
    getDestId (NonStdLink _   i _ _) = i


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
look (i, mq, cols) [] = getWS >>= \ws ->
    let p       = (ws^.pcTbl) ! i
        ri      = p^.rmId
        r       = (ws^.rmTbl) ! ri
        primary = T.unlines . concatMap (wordWrap cols) $ [ r^.name, r^.desc ]
        suppl   = mkExitsSummary cols r <> ricd
        ricd    = mkRmInvCoinsDesc i cols ws ri
    in send mq . nl $ primary <> suppl
look (i, mq, cols) rs = getWS >>= \ws ->
    let p  = (ws^.pcTbl)    ! i
        ri = p^.rmId
        is = delete i . (! ri) $ ws^.invTbl
        c  = (ws^.coinsTbl) ! ri
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames ws (nub . map T.toLower $ rs) is c
               eiss               = zipWith (curry procGecrMisRm) gecrs miss
               ecs                = map procReconciledCoinsRm rcs
               invDesc            = foldl' (helperLookEitherInv ws) "" eiss
               coinsDesc          = foldl' helperLookEitherCoins    "" ecs
           in invDesc <> coinsDesc
      else nl . T.unlines . wordWrap cols $ "You don't see anything here to look at."
  where
    helperLookEitherInv _  acc (Left  msg) = nl $ acc <> msg
    helperLookEitherInv ws acc (Right is ) = nl $ acc <> mkEntDescs i cols ws is
    helperLookEitherCoins  acc (Left  msg) = nl $ acc <> msg
    helperLookEitherCoins  acc (Right c  ) = nl $ acc <> mkCoinsDesc cols c


-- TODO: Consider implementing a color scheme for lists like these such that the least significant characters of each name are highlighted or bolded somehow.
mkRmInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> T.Text
mkRmInvCoinsDesc i cols ws ri = let is       = delete i . (! ri) $ ws^.invTbl
                                    c        = (ws^.coinsTbl) ! ri
                                    entDescs = T.unlines . concatMap (wordWrapIndent 2 cols . helper) . mkNameCountBothTypeList ws $ is
                                in if c == mempty then entDescs else entDescs <> mkCoinsSummary cols c
  where
    helper (en, c, (s, _), t)
      | c == 1 = f s <> " " <> bracketQuote en
        where
          f = case t of PCType -> id
                        _      -> aOrAn
    helper (en, c, b, _) = T.concat [ showText c, " ", mkPlurFromBoth b, " ", bracketQuote en ]


mkNameCountBothList :: WorldState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList ws is = let es    = [ (ws^.entTbl) ! i    | i <- is ]
                                ens   = [ e^.name             | e <- es ]
                                ebgns = [ getEntBothGramNos e | e <- es ]
                                cs    = mkCountList ebgns
                            in nub . zip3 ens cs $ ebgns


mkNameCountBothTypeList :: WorldState -> Inv -> [(T.Text, Int, BothGramNos, Type)]
mkNameCountBothTypeList ws is = let es    = [ (ws^.entTbl) ! i    | i <- is ]
                                    ens   = [ e^.name             | e <- es ]
                                    ebgns = [ getEntBothGramNos e | e <- es ]
                                    cs    = mkCountList ebgns
                                    ts    = [ (ws^.typeTbl) ! i   | i <- is ]
                                in nub . zip4 ens cs ebgns $ ts


mkEntDescs :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntDescs i cols ws is = let boths = [ (ei, (ws^.entTbl) ! ei) | ei <- is ]
                          in T.intercalate "\n" . map (mkEntDesc i cols ws) $ boths


mkEntDesc :: Id -> Cols -> WorldState -> (Id, Ent) -> T.Text
mkEntDesc i cols ws (ei, e) = let t       = (ws^.typeTbl) ! ei
                                  primary = T.unlines . wordWrap cols $ e^.desc
                                  suppl   = case t of ConType -> mkInvCoinsDesc i cols ws ei e
                                                      MobType -> mkEqDesc       i cols ws ei e t
                                                      PCType  -> mkEqDesc       i cols ws ei e t
                                                      _       -> ""
                              in primary <> suppl


mkInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> Ent -> T.Text
mkInvCoinsDesc i cols ws ei e = let is       = (ws^.invTbl)   ! ei
                                    c        = (ws^.coinsTbl) ! ei
                                    hasInv   = not . null $ is
                                    hasCoins = c /= mempty
                                in case (hasInv, hasCoins) of
                                  (False, False) -> T.unlines . wordWrap cols $ if ei == i
                                                      then dudeYourHandsAreEmpty
                                                      else "The " <> e^.sing <> " is empty."
                                  (True,  False) -> header <> mkEntsInInvDesc cols ws is
                                  (False, True ) -> header <> mkCoinsSummary  cols c
                                  (True,  True ) -> header <> mkEntsInInvDesc cols ws is <> mkCoinsSummary cols c
  where
    header
      | ei == i   = "You are carrying:\n"
      | otherwise = T.unlines . wordWrap cols $ "The " <> e^.sing <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Cols -> WorldState -> Inv -> T.Text
mkEntsInInvDesc cols ws = T.unlines . concatMap (wordWrapIndent ind cols . helper) . mkNameCountBothList ws
  where
    helper (en, c, (s, _)) | c == 1 = nameCol en <> "1 " <> s
    helper (en, c, b     )          = T.concat [ nameCol en, showText c, " ", mkPlurFromBoth b ]
    ind     = 11
    nameCol = bracketPad ind


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper mkCoinsWithNamesList
  where
    helper               = T.unlines . wordWrapIndent 2 cols . T.intercalate ", " . filter (not . T.null) . map mkNameAmt
    mkNameAmt (cn, a)    = if a == 0 then "" else showText a <> " " <> bracketQuote cn
    mkCoinsWithNamesList = zip coinNames . mkListFromCoins $ c


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) = T.unlines . intercalate [""] . map (wordWrap cols) . filter (not . T.null) $ [ copDesc, silDesc, golDesc ]
  where -- TODO: Come up with good descriptions.
    copDesc = if cop /= 0 then "The copper piece is round and shiny." else ""
    silDesc = if sil /= 0 then "The silver piece is round and shiny." else ""
    golDesc = if gol /= 0 then "The gold piece is round and shiny."   else ""


-----


exits :: Action
exits (i, mq, cols) [] = getWS >>= \ws ->
    let p = (ws^.pcTbl) ! i
        r = (ws^.rmTbl) ! (p^.rmId)
    in send mq . nl . mkExitsSummary cols $ r
exits imc@(_, mq, cols) rs = ignore mq cols rs >> exits imc []


mkExitsSummary :: Cols -> Rm -> T.Text
mkExitsSummary cols r = let stdNames    = [ rl^.linkDir.to linkDirToCmdName | rl <- r^.rmLinks, not . isNonStdLink $ rl ]
                            customNames = [ rl^.linkName | rl <- r^.rmLinks, isNonStdLink $ rl ]
                        in T.unlines . wordWrapIndent 2 cols . ("Obvious exits: " <>) . summarize stdNames $ customNames
  where
    summarize []  []  = "None!"
    summarize std cus = T.intercalate ", " . (std ++) $ cus


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (i, mq, cols) [] = getWS >>= \ws ->
    let e = (ws^.entTbl) ! i
    in send mq . nl . mkInvCoinsDesc i cols ws i $ e
inv (i, mq, cols) rs = getWS >>= \ws ->
    let is = (ws^.invTbl)   ! i
        c  = (ws^.coinsTbl) ! i
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames ws (nub . map T.toLower $ rs) is c
               eiss               = zipWith (curry procGecrMisPCInv) gecrs miss
               ecs                = map procReconciledCoinsPCInv rcs
               invDesc            = foldl' (helperEitherInv ws) "" eiss
               coinsDesc          = foldl' helperEitherCoins    "" ecs
           in invDesc <> coinsDesc
      else nl . T.unlines . wordWrap cols $ dudeYourHandsAreEmpty
  where
    helperEitherInv _  acc (Left  msg) = nl $ acc <> msg
    helperEitherInv ws acc (Right is ) = nl $ acc <> mkEntDescs i cols ws is
    helperEitherCoins  acc (Left  msg) = nl $ acc <> msg
    helperEitherCoins  acc (Right c  ) = nl $ acc <> mkCoinsDesc cols c


-----


equip :: Action
equip (i, mq, cols) [] = getWS >>= \ws ->
    let e = (ws^.entTbl) ! i
    in send mq . nl . mkEqDesc i cols ws i e $ PCType
equip (i, mq, cols) rs = getWS >>= \ws ->
    let em = (ws^.eqTbl) ! i
        is = M.elems em
    in send mq $ if not . M.null $ em
      then let (gecrs, miss, rcs) = resolveEntCoinNames ws (nub . map T.toLower $ rs) is mempty
               eiss               = zipWith (curry procGecrMisPCEq) gecrs miss
               invDesc            = foldl' (helperEitherInv ws) "" eiss
               coinsDesc          = if not . null $ rcs
                                      then nl . T.unlines . wordWrap cols $ "You don't have any coins among your readied equipment."
                                      else ""
           in invDesc <> coinsDesc
      else nl . T.unlines . wordWrap cols $ dudeYou'reNaked
  where
    helperEitherInv _  acc (Left  msg) = nl $ acc <> msg
    helperEitherInv ws acc (Right is ) = nl $ acc <> mkEntDescs i cols ws is


mkEqDesc :: Id -> Cols -> WorldState -> Id -> Ent -> Type -> T.Text
mkEqDesc i cols ws ei e t = let em    = (ws^.eqTbl) ! ei
                                descs = map mkDesc . mkSlotNameIdList . M.toList $ em
                            in if null descs
                              then none
                              else (header <>) . T.unlines . concat $ [ wordWrapIndent 15 cols l | l <- descs ]
  where
    mkSlotNameIdList = map (first pp)
    mkDesc (sn, i')  = let sn'      = parensPad 15 noFinger
                           noFinger = fst . T.breakOn " finger" $ sn
                           e'       = (ws^.entTbl) ! i'
                       in T.concat [ sn', e'^.sing, " ", e'^.name.to bracketQuote ]
    none   = T.unlines . wordWrap cols $ if
      | ei == i      -> dudeYou'reNaked
      | t  == PCType -> e^.sing <> " doesn't have anything readied."
      | otherwise    -> "The " <> e^.sing <> " doesn't have anything readied."
    header = T.unlines . wordWrap cols $ if
      | ei == i      -> "You have readied the following equipment:"
      | t  == PCType -> e^.sing <> " has readied the following equipment:"
      | otherwise    -> "The " <> e^.sing <> " has readied the following equipment:"


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


-----


getAction :: Action
getAction (_, mq, cols) [] = advise mq cols ["get"] $ "Please specify one or more items to pick up, as in " <> dblQuote "get sword" <> "."
getAction (i, mq, cols) rs = send mq . nl =<< helper
  where
    helper = onWS $ \(t, ws) ->
        let p   = (ws^.pcTbl) ! i
            ri  = p^.rmId
            ris = delete i . (! ri) $ ws^.invTbl
            rc  = (ws^.coinsTbl) ! ri
        in if (not . null $ ris) || (rc /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames ws (nub . map T.toLower $ rs) ris rc
                   eiss               = zipWith (curry procGecrMisRm) gecrs miss
                   ecs                = map procReconciledCoinsRm rcs
                   (ws',  msgs)       = foldl' (helperGetDropEitherInv   cols Get ri i) (ws, "")    eiss
                   (ws'', msgs')      = foldl' (helperGetDropEitherCoins      Get ri i) (ws', msgs) ecs
               in putTMVar t ws'' >> return msgs'
          else putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You don't see anything here to pick up.")


advise :: MsgQueue -> Cols -> [HelpTopic] -> T.Text -> MudStack ()
advise mq cols []  msg = send mq . nl . T.unlines . wordWrap cols $ msg
advise mq cols [h] msg = send mq . nl . T.unlines . concatMap (wordWrap cols) $ [ msg, "For more information, type " <> (dblQuote . ("help " <>) $ h) <> "." ]
advise mq cols hs  msg = send mq . nl . T.unlines . concatMap (wordWrap cols) $ [ msg, "See also the following help topics: " <> helpTopics <> "." ]
  where
    helpTopics = dblQuote . T.intercalate (dblQuote ", ") $ hs


type FromId = Id
type ToId   = Id


helperGetDropEitherInv :: Cols -> GetOrDrop -> FromId -> ToId -> (WorldState, T.Text) -> Either T.Text Inv -> (WorldState, T.Text)
helperGetDropEitherInv cols god fi ti (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right is  -> let fis = (ws^.invTbl) ! fi
                   tis = (ws^.invTbl) ! ti
                   ws' = ws & invTbl.at fi ?~ deleteFirstOfEach is fis
                            & invTbl.at ti ?~ (sortInv ws . (++) tis $ is)
                   msg = mkGetDropInvDesc cols ws' god is
               in (ws', msgs <> msg)


mkGetDropInvDesc :: Cols -> WorldState -> GetOrDrop -> Inv -> T.Text
mkGetDropInvDesc cols ws god is = T.concat . map (T.unlines . wordWrap cols . helper) . mkNameCountBothList ws $ is
  where
    helper (_, c, (s, _)) | c == 1 = T.concat [ "You ", verb god, " the ", s, "." ]
    helper (_, c, b)               = T.concat [ "You ", verb god, " ", showText c, " ", mkPlurFromBoth b, "." ]
    verb = \case Get  -> "pick up"
                 Drop -> "drop"


helperGetDropEitherCoins :: GetOrDrop -> FromId -> ToId -> (WorldState, T.Text) -> Either T.Text Coins -> (WorldState, T.Text)
helperGetDropEitherCoins god fi ti (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right c   -> let fc  = (ws^.coinsTbl) ! fi
                   tc  = (ws^.coinsTbl) ! ti
                   ws' = ws & coinsTbl.at fi ?~ fc <> negateCoins c
                            & coinsTbl.at ti ?~ tc <> c
                   msg = mkGetDropCoinsDesc god c
               in (ws', msgs <> msg)


mkGetDropCoinsDesc :: GetOrDrop -> Coins -> T.Text
mkGetDropCoinsDesc god (Coins (cop, sil, gol)) = T.concat [ c, s, g ]
  where
    c = if cop /= 0 then helper cop "copper piece" else ""
    s = if sil /= 0 then helper sil "silver piece" else ""
    g = if gol /= 0 then helper gol "gold piece"   else ""
    helper a cn | a == 1 = T.concat [ "You ", verb god, " a ", cn, ".\n" ]
    helper a cn          = T.concat [ "You ", verb god, " ", showText a, " ", cn, "s.\n" ]
    verb = \case Get  -> "pick up"
                 Drop -> "drop"


-----


dropAction :: Action
dropAction (_, mq, cols) [] = advise mq cols ["drop"] $ "Please specify one or more things to drop, as in " <> dblQuote "drop sword" <> "."
dropAction (i, mq, cols) rs = send mq . nl =<< helper
  where
    helper = onWS $ \(t, ws) ->
        let p   = (ws^.pcTbl)    ! i
            pis = (ws^.invTbl)   ! i
            pc  = (ws^.coinsTbl) ! i
            ri  = p^.rmId
        in if (not . null $ pis) || (pc /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames ws (nub . map T.toLower $ rs) pis pc
                   eiss               = zipWith (curry procGecrMisPCInv) gecrs miss
                   ecs                = map procReconciledCoinsPCInv rcs
                   (ws',  msgs)       = foldl' (helperGetDropEitherInv   cols Drop i ri) (ws, "")    eiss
                   (ws'', msgs')      = foldl' (helperGetDropEitherCoins      Drop i ri) (ws', msgs) ecs
               in putTMVar t ws'' >> return msgs'
          else putTMVar t ws >> (return . T.unlines . wordWrap cols $ dudeYourHandsAreEmpty)


-----


putAction :: Action
putAction (_, mq, cols) []  = advise mq cols ["put"] $ "Please specify one or more things you want to put, followed by where you want to put them, as in " <> dblQuote "put doll sack" <> "."
putAction (_, mq, cols) [r] = advise mq cols ["put"] $ "Please also specify where you want to put it, as in " <> dblQuote ("put " <> r <> " sack") <> "."
putAction (i, mq, cols) rs  = send mq . nl =<< helper
  where
    helper = onWS $ \(t, ws) ->
      let p   = (ws^.pcTbl)    ! i
          pis = (ws^.invTbl)   ! i
          pc  = (ws^.coinsTbl) ! i
          ri  = p^.rmId
          ris = delete i . (! ri) $ ws^.invTbl
          rc  = (ws^.coinsTbl) ! ri
          rs' = nub . map T.toLower $ rs
          cn  = last rs'
          restWithoutCon = init rs'
      in if (not . null $ pis) || (pc /= mempty)
        then if T.head cn == rmChar
          then if not . null $ ris
            then shufflePut i cols (t, ws) (T.tail cn) restWithoutCon ris rc pis pc procGecrMisRm
            else putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You don't see any containers here.")
          else shufflePut i cols (t, ws) cn restWithoutCon pis pc pis pc procGecrMisPCInv
      else putTMVar t ws >> (return . T.unlines . wordWrap cols $ dudeYourHandsAreEmpty)


type InvWithCon   = Inv
type CoinsWithCon = Coins
type PCInv         = Inv
type PCCoins       = Coins


shufflePut :: Id -> Cols -> (TMVar WorldState, WorldState) -> ConName -> Rest -> InvWithCon -> CoinsWithCon -> PCInv -> PCCoins -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) -> STM T.Text
shufflePut i cols (t, ws) cn rs is c pis pc f = let (gecrs, miss, rcs) = resolveEntCoinNames ws [cn] is c
                                                in if null miss && (not . null $ rcs)
                                                  then putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You can't put something inside a coin.")
                                                  else case f . head . zip gecrs $ miss of
                                                    Left  msg  -> putTMVar t ws >> return msg
                                                    Right [ci] -> let e  = (ws^.entTbl)  ! ci
                                                                      t' = (ws^.typeTbl) ! ci
                                                                  in if t' /= ConType
                                                                    then putTMVar t ws >> (return . T.unlines . wordWrap cols $ "The " <> e^.sing <> " isn't a container.")
                                                                    else let (gecrs', miss', rcs') = resolveEntCoinNames ws rs pis pc
                                                                             eiss                  = zipWith (curry procGecrMisPCInv) gecrs' miss'
                                                                             ecs                   = map procReconciledCoinsPCInv rcs'
                                                                             (ws',  msgs)          = foldl' (helperPutRemEitherInv   cols Put i ci e) (ws, "")    eiss
                                                                             (ws'', msgs')         = foldl' (helperPutRemEitherCoins cols Put i ci e) (ws', msgs) ecs
                                                                         in putTMVar t ws'' >> return msgs'
                                                    Right _   -> putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You can only put things into one container at a time.")


type ToEnt = Ent


helperPutRemEitherInv :: Cols -> PutOrRem -> FromId -> ToId -> ToEnt -> (WorldState, T.Text) -> Either T.Text Inv -> (WorldState, T.Text)
helperPutRemEitherInv cols por fi ti te (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right is  -> let (is', msgs') = if ti `elem` is
                                    then (filter (/= ti) is, (msgs <>) . T.unlines . wordWrap cols $ "You can't put the " <> te^.sing <> " inside itself.")
                                    else (is, msgs)
                   fis = (ws^.invTbl) ! fi
                   tis = (ws^.invTbl) ! ti
                   ws' = ws & invTbl.at fi ?~ deleteFirstOfEach is' fis
                            & invTbl.at ti ?~ (sortInv ws . (++) tis $ is')
                   msg = mkPutRemInvDesc cols ws' por is' te
               in (ws', msgs' <> msg)


mkPutRemInvDesc :: Cols -> WorldState -> PutOrRem -> Inv -> ToEnt -> T.Text
mkPutRemInvDesc cols ws por is te = T.concat . map (T.unlines . wordWrap cols . helper) . mkNameCountBothList ws $ is
  where
    helper (_, c, (s, _)) | c == 1 = T.concat [ "You ", verb por, " the ", s, " ", prep por, " ", te^.sing, "." ]
    helper (_, c, b)               = T.concat [ "You ", verb por, " ", showText c, " ", mkPlurFromBoth b, " ", prep por, " ", te^.sing, "." ]
    verb = \case Put -> "put"
                 Rem -> "remove"
    prep = \case Put -> "in the"
                 Rem -> "from the"


helperPutRemEitherCoins :: Cols -> PutOrRem -> FromId -> ToId -> ToEnt -> (WorldState, T.Text) -> Either T.Text Coins -> (WorldState, T.Text)
helperPutRemEitherCoins cols por fi ti te (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right c   -> let fc  = (ws^.coinsTbl) ! fi
                   tc  = (ws^.coinsTbl) ! ti
                   ws' = ws & coinsTbl.at fi ?~ fc <> negateCoins c
                            & coinsTbl.at ti ?~ tc <> c
                   msg = mkPutRemCoinsDesc cols por c te
               in (ws', msgs <> msg)


mkPutRemCoinsDesc :: Cols -> PutOrRem -> Coins -> ToEnt -> T.Text
mkPutRemCoinsDesc cols por (Coins (cop, sil, gol)) te = T.unlines . concatMap (wordWrap cols) . filter (not . T.null) $ [ c, s, g ]
  where
    c = if cop /= 0 then helper cop "copper piece" else ""
    s = if sil /= 0 then helper sil "silver piece" else ""
    g = if gol /= 0 then helper gol "gold piece"   else ""
    helper a cn | a == 1 = T.concat [ "You ", verb por, " a ", cn, " ", prep por, " ", te^.sing, "." ]
    helper a cn          = T.concat [ "You ", verb por, " ", showText a, " ", cn, "s ", prep por, " ", te^.sing, "." ]
    verb = \case Put -> "put"
                 Rem -> "remove"
    prep = \case Put -> "in the"
                 Rem -> "from the"


-----


remove :: Action
remove (_, mq, cols) []  = advise mq cols ["remove"] $ "Please specify one or more things to remove, followed by the container you want to remove them from, as in " <> dblQuote "remove doll sack" <> "."
remove (_, mq, cols) [r] = advise mq cols ["remove"] $ "Please also specify the container you want to remove it from, as in " <> dblQuote ("remove " <> r <> " sack") <> "."
remove (i, mq, cols) rs  = send mq . nl =<< helper
  where
    helper = onWS $ \(t, ws) ->
      let p   = (ws^.pcTbl)    ! i
          pis = (ws^.invTbl)   ! i
          pc  = (ws^.coinsTbl) ! i
          ri  = p^.rmId
          ris = delete i . (! ri) $ ws^.invTbl
          rc  = (ws^.coinsTbl) ! ri
          rs' = nub . map T.toLower $ rs
          cn  = last rs'
          restWithoutCon = init rs'
      in if T.head cn == rmChar
          then if not . null $ ris
            then shuffleRem i cols (t, ws) (T.tail cn) restWithoutCon ris rc procGecrMisRm
            else putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You don't see any containers here.")
          else shuffleRem i cols (t, ws) cn restWithoutCon pis pc procGecrMisPCInv


shuffleRem :: Id -> Cols -> (TMVar WorldState, WorldState) -> ConName -> Rest -> InvWithCon -> CoinsWithCon -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) -> STM T.Text
shuffleRem i cols (t, ws) cn rs is c f = let (gecrs, miss, rcs) = resolveEntCoinNames ws [cn] is c
                                         in if null miss && (not . null $ rcs)
                                           then putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You can't remove something from a coin.")
                                           else case f . head . zip gecrs $ miss of
                                             Left  msg  -> putTMVar t ws >> return msg
                                             Right [ci] -> let e  = (ws^.entTbl)  ! ci
                                                               t' = (ws^.typeTbl) ! ci
                                                           in if t' /= ConType
                                                             then putTMVar t ws >> (return . T.unlines . wordWrap cols $ "The " <> e^.sing <> " isn't a container.")
                                                             else let cis                   = (ws^.invTbl)   ! ci
                                                                      cc                    = (ws^.coinsTbl) ! ci
                                                                      (gecrs', miss', rcs') = resolveEntCoinNames ws rs cis cc
                                                                      eiss                  = map (procGecrMisCon (e^.sing)) . zip gecrs' $ miss'
                                                                      ecs                   = map (procReconciledCoinsCon (e^.sing)) rcs'
                                                                      (ws',  msgs)          = foldl' (helperPutRemEitherInv   cols Rem ci i e) (ws, "")    eiss
                                                                      (ws'', msgs')         = foldl' (helperPutRemEitherCoins cols Rem ci i e) (ws', msgs) ecs
                                                                  in putTMVar t ws'' >> return msgs'
                                             Right _   -> putTMVar t ws >> (return . T.unlines . wordWrap cols $ "You can only remove things from one container at a time.")


-----


ready :: Action
ready (_, mq, cols) [] = advise mq cols ["ready"] $ "Please specify one or more things to ready, as in " <> dblQuote "ready sword" <> "."
ready (i, mq, cols) rs = send mq . nl =<< helper
  where
    helper = onWS $ \(t, ws) ->
        let is = (ws^.invTbl)   ! i
            c  = (ws^.coinsTbl) ! i
        in if (not . null $ is) || (c /= mempty)
          then let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols ws (nub . map T.toLower $ rs) is mempty
                   eiss                      = zipWith (curry procGecrMisReady) gecrs miss
                   msgs                      = if null rcs then "" else "You can't ready coins.\n"
                   (ws',  msgs')             = foldl' (helperReady i cols) (ws, msgs) . zip eiss $ mrols
               in putTMVar t ws' >> return msgs'
          else putTMVar t ws >> (return . T.unlines . wordWrap cols $ dudeYourHandsAreEmpty)


helperReady :: Id -> Cols -> (WorldState, T.Text) -> (Either T.Text Inv, Maybe RightOrLeft) -> (WorldState, T.Text)
helperReady i cols (ws, msgs) (eis, mrol) = case eis of
  Left  msg -> (ws, msgs <> msg)
  Right is  -> foldl' (readyDispatcher i cols mrol) (ws, msgs) is


readyDispatcher :: Id -> Cols -> Maybe RightOrLeft -> (WorldState, T.Text) -> Id -> (WorldState, T.Text)
readyDispatcher i cols mrol (ws, msgs) ei = let e = (ws^.entTbl)  ! ei
                                                t = (ws^.typeTbl) ! ei
                                            in case t of
                                              ClothType -> readyCloth i cols mrol (ws, msgs) ei e
                                              WpnType   -> readyWpn   i cols mrol (ws, msgs) ei e
                                              _         -> (ws, (msgs <>) . T.unlines . wordWrap cols $ "You can't ready a " <> e^.sing <> ".")


moveReadiedItem :: Id -> (WorldState, T.Text) -> EqMap -> Slot -> Id -> T.Text -> (WorldState, T.Text)
moveReadiedItem i (ws, msgs) em s ei msg = let is  = (ws^.invTbl) ! i
                                               ws' = ws & invTbl.at i ?~ filter (/= ei) is
                                                        & eqTbl.at  i ?~ (em & at s ?~ ei)
                                           in (ws', msgs <> msg)


-- Helpers for the entity type-specific ready functions:


otherGender :: Gender -> Gender
otherGender Male     = Female
otherGender Female   = Male
otherGender NoGender = NoGender


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


readyCloth :: Id -> Cols -> Maybe RightOrLeft -> (WorldState, T.Text) -> Id -> Ent -> (WorldState, T.Text)
readyCloth i cols mrol (ws, msgs) ei e = let em = (ws^.eqTbl)    ! i
                                             c  = (ws^.clothTbl) ! ei
                                         in case maybe (getAvailClothSlot cols ws i c em) (getDesigClothSlot cols ws e c em) mrol of
                                           Left  msg -> (ws, msgs <> msg)
                                           Right s   -> moveReadiedItem i (ws, msgs) em s ei . mkReadyMsg s $ c
  where
    mkReadyMsg s = \case NoseC   -> putOnMsg
                         NeckC   -> putOnMsg
                         FingerC -> T.unlines . wordWrap cols . T.concat $ [ "You slide the ", e^.sing, " on your ", pp s, "." ]
                         _       -> wearMsg
      where
        putOnMsg = T.unlines . wordWrap cols $ "You put on the " <> e^.sing <> "."
        wearMsg  = T.unlines . wordWrap cols . T.concat $ [ "You wear the ", e^.sing, " on your ", pp s, "." ]


getAvailClothSlot :: Cols -> WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot cols ws i c em = let m = (ws^.mobTbl) ! i
                                       g = m^.gender
                                       h = m^.hand
                                   in procMaybe $ case c of
                                     EarC    -> getEarSlotForGender g `mplus` (getEarSlotForGender . otherGender $ g)
                                     NoseC   -> findAvailSlot em noseSlots
                                     NeckC   -> findAvailSlot em neckSlots
                                     WristC  -> getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
                                     FingerC -> getRingSlot g h
                                     _       -> undefined -- TODO
  where
    procMaybe             = maybe (Left . T.unlines . wordWrap cols . sorryFullClothSlots $ c) Right
    getEarSlotForGender g = findAvailSlot em $ case g of Male   -> lEarSlots
                                                         Female -> rEarSlots
                                                         _      -> patternMatchFail "getAvailClothSlot getEarSlotForGender" [ showText g ]
    getWristSlotForHand h = findAvailSlot em $ case h of RHand  -> lWristSlots
                                                         LHand  -> rWristSlots
                                                         _      -> patternMatchFail "getAvailClothSlot getWristSlotForHand" [ showText h ]
    getRingSlot g h       = findAvailSlot em $ case g of Male   -> case h of
                                                                     RHand -> [ LRingFS, LIndexFS, RRingFS, RIndexFS, LMidFS, RMidFS, LPinkyFS, RPinkyFS ]
                                                                     LHand -> [ RRingFS, RIndexFS, LRingFS, LIndexFS, RMidFS, LMidFS, RPinkyFS, LPinkyFS ]
                                                                     _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
                                                         Female -> case h of
                                                                     RHand -> [ LRingFS, LIndexFS, RRingFS, RIndexFS, LPinkyFS, RPinkyFS, LMidFS, RMidFS ]
                                                                     LHand -> [ RRingFS, RIndexFS, LRingFS, LIndexFS, RPinkyFS, LPinkyFS, RMidFS, LMidFS ]
                                                                     _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
                                                         _      -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText g ]


getDesigClothSlot :: Cols -> WorldState -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot cols ws e c em rol
  | c `elem` [ NoseC, NeckC, UpBodyC, LowBodyC, FullBodyC, BackC, FeetC ] = Left sorryCantWearThere
  | isRingRol rol && c /= FingerC           = Left sorryCantWearThere
  | c == FingerC && (not . isRingRol $ rol) = Left . T.unlines . wordWrap cols $ ringHelp
  | otherwise = case c of EarC    -> maybe (Left sorryFullEar)   Right (findSlotFromList rEarSlots   lEarSlots)
                          WristC  -> maybe (Left sorryFullWrist) Right (findSlotFromList rWristSlots lWristSlots)
                          FingerC -> maybe (Right slotFromRol)
                                           (\i -> let e' = (ws^.entTbl) ! i in Left . sorry slotFromRol $ e')
                                           (em^.at slotFromRol)
                          _       -> undefined -- TODO
  where
    sorryCantWearThere     = T.unlines . wordWrap cols . T.concat $ [ "You can't wear a ", e^.sing, " on your ", pp rol, "." ]
    findSlotFromList rs ls = findAvailSlot em $ case rol of R -> rs
                                                            L -> ls
                                                            _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of R -> rs
                                                L -> ls
                                                _ -> patternMatchFail "getDesigClothSlot getSlotFromList" [ showText rol ]
    sorryFullEar     = T.unlines . wordWrap cols . sorryFullClothSlotsOneSide . getSlotFromList rEarSlots   $ lEarSlots
    sorryFullWrist   = T.unlines . wordWrap cols . sorryFullClothSlotsOneSide . getSlotFromList rWristSlots $ lWristSlots
    slotFromRol      = fromRol rol :: Slot
    sorry s e'       = T.unlines . wordWrap cols . T.concat $ [ "You're already wearing a ", e'^.sing, " on your ", pp s, "." ]


-- Ready weapons:


readyWpn :: Id -> Cols -> Maybe RightOrLeft -> (WorldState, T.Text) -> Id -> Ent -> (WorldState, T.Text)
readyWpn i cols mrol (ws, msgs) ei e = let em  = (ws^.eqTbl)  ! i
                                           w   = (ws^.wpnTbl) ! ei
                                           sub = w^.wpnSub
                                       in if not . isSlotAvail em $ BothHandsS
                                         then (ws, (msgs <>) . T.unlines . wordWrap cols $ "You're already wielding a two-handed weapon.")
                                         else case maybe (getAvailWpnSlot cols ws i em) (getDesigWpnSlot cols ws e em) mrol of
                                           Left  msg -> (ws, msgs <> msg)
                                           Right s   -> case sub of
                                                          OneHanded -> moveReadiedItem i (ws, msgs) em s ei . T.unlines . wordWrap cols . T.concat $ [ "You wield the ", e^.sing, " with your ", pp s, "." ]
                                                          TwoHanded -> if all (isSlotAvail em) [ RHandS, LHandS ]
                                                                         then moveReadiedItem i (ws, msgs) em BothHandsS ei . T.unlines . wordWrap cols $ "You wield the " <> e^.sing <> " with both hands."
                                                                         else (ws, (msgs <>) . T.unlines . wordWrap cols $ "Both hands are required to wield the " <> e^.sing <> ".")


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
getDesigWpnSlot cols ws e em rol
  | isRingRol rol = Left sorryNotRing
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e' = (ws^.entTbl) ! i in Left . sorry $ e')
                          (em^.at desigSlot)
  where
    sorryNotRing = T.unlines . wordWrap cols $ "You can't wield a " <> e^.sing <> " with your finger!"
    sorry e'     = T.unlines . wordWrap cols . T.concat $ [ "You're already wielding a ", e'^.sing, " with your ", pp desigSlot, "." ]
    desigSlot    = case rol of R -> RHandS
                               L -> LHandS
                               _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-- Ready armor:


-----


unready :: Action
unready (_, mq, cols) [] = advise mq cols ["unready"] $ "Please specify one or more things to unready, as in " <> dblQuote "unready sword" <> "."
unready (i, mq, cols) rs = send mq . nl =<< helper
  where
    helper = onWS $ \(t, ws) ->
        let em = (ws^.eqTbl) ! i
            is = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs) = resolveEntCoinNames ws (nub . map T.toLower $ rs) is mempty
                   eiss               = zipWith (curry procGecrMisPCEq) gecrs miss
                   msgs               = if null rcs then "" else "You can't unready coins.\n"
                   (ws',  msgs')      = foldl' (helperUnready i cols) (ws, msgs) eiss
               in putTMVar t ws' >> return msgs'
          else putTMVar t ws >> (return . T.unlines . wordWrap cols $ dudeYou'reNaked)


helperUnready :: Id -> Cols -> (WorldState, T.Text) -> Either T.Text Inv -> (WorldState, T.Text)
helperUnready i cols (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right is  -> let em  = (ws^.eqTbl)  ! i
                   pis = (ws^.invTbl) ! i
                   ws' = ws & eqTbl.at  i ?~ M.filter (`notElem` is) em
                            & invTbl.at i ?~ (sortInv ws . (pis ++) $ is)
                   msg = mkUnreadyDesc cols ws' is
               in (ws', msgs <> msg)


mkUnreadyDesc :: Cols -> WorldState -> Inv -> T.Text
mkUnreadyDesc cols ws is = T.concat [ helper icb | icb <- mkIdCountBothList ws is ]
  where
    helper (i, c, b@(s, _)) = let v = verb i in T.unlines . wordWrap cols . T.concat $ if c == 1
      then [ "You ", v, " the ", s, "." ]
      else [ "You ", v, " ", showText c, " ", mkPlurFromBoth b, "." ]
    verb i = let t = (ws^.typeTbl) ! i
             in case t of
               ClothType -> unwearGenericVerb -- TODO
               WpnType   -> "stop wielding"
               _         -> undefined -- TODO
    unwearGenericVerb = "take off"


mkIdCountBothList :: WorldState -> Inv -> [(Id, Int, BothGramNos)]
mkIdCountBothList ws is = let es    = [ (ws^.entTbl) ! i    | i <- is ]
                              ebgns = [ getEntBothGramNos e | e <- es ]
                              cs    = mkCountList ebgns
                          in nubBy equalCountsAndBoths . zip3 is cs $ ebgns
  where
    equalCountsAndBoths (_, c, b) (_, c', b') = c == c' && b == b'


-----


what :: Action
what (_, mq, cols) [] = advise mq cols ["what"] $ "Please specify one or more abbreviations to disambiguate, as in " <> dblQuote "what up" <> "."
what (i, mq, cols) rs = getWS >>= \ws ->
  let p  = (ws^.pcTbl) ! i
      r  = (ws^.rmTbl) ! (p^.rmId)
  in send mq . T.concat . map (helper ws r) . nub . map T.toLower $ rs
  where
    helper ws r n = T.concat [ whatCmd   cols r        n
                             , whatInv i cols ws PCInv n
                             , whatInv i cols ws PCEq  n
                             , whatInv i cols ws RmInv n
                             , "\n" ]


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
                           then whatInvEnts cols ws it n (head gecrs) is
                           else T.concat . map (whatInvCoins cols it n) $ rcs
  where
    resolveName = let (is, c)         = getLocInvCoins
                      (gecrs, _, rcs) = resolveEntCoinNames ws [n] is c
                  in (is, gecrs, rcs)
    getLocInvCoins = case it of PCInv -> ((ws^.invTbl) ! i,          (ws^.coinsTbl) ! i)
                                PCEq  -> (M.elems $ (ws^.eqTbl) ! i, mempty)
                                RmInv -> ((ws^.invTbl) ! ri,         (ws^.coinsTbl) ! ri)
    p  = (ws^.pcTbl) ! i
    ri = p^.rmId


whatInvEnts :: Cols -> WorldState -> InvType -> T.Text -> GetEntsCoinsRes -> Inv -> T.Text
whatInvEnts cols ws it r gecr is = case gecr of
  Mult _ n (Just es) _ | n == acp  -> T.unlines . wordWrap cols . T.concat $ [ dblQuote acp, " may refer to everything ", getLocTxtForInvType it, supplement, "." ]
                       | otherwise -> let e   = head es
                                          len = length es
                                      in if len > 1
                                        then let ebgns  = take len [ getEntBothGramNos e' | e' <- es ]
                                                 h      = head ebgns
                                                 target = if all (== h) ebgns then mkPlurFromBoth h else e^.name.to bracketQuote <> "s"
                                             in T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " may refer to the ", showText len, " ", target, " ", getLocTxtForInvType it, "." ]
                                        else let ens = [ let e' = (ws^.entTbl) ! i in e'^.name | i <- is ]
                                             in T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " may refer to the ", T.pack . checkFirst e $ ens, e^.sing, " ", getLocTxtForInvType it, "." ]
  Indexed x _ (Right e) -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " may refer to the ", mkOrdinal x, " ", e^.name.to bracketQuote, " ", e^.sing.to parensQuote, " ", getLocTxtForInvType it, "." ]
  _                     -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " doesn't refer to anything ", getLocTxtForInvType it, "." ]
  where
    acp                                     = T.pack [allChar]
    supplement | it `elem` [ PCInv, RmInv ] = " (including any coins)"
               | otherwise                  = ""
    checkFirst e ens                        = let matches = filter (== e^.name) ens
                                              in guard (length matches > 1) >> T.unpack "first "


getLocTxtForInvType :: InvType -> T.Text
getLocTxtForInvType = \case PCInv -> "in your inventory"
                            PCEq  -> "in your readied equipment"
                            RmInv -> "in this room"


whatInvCoins :: Cols -> InvType -> T.Text -> ReconciledCoins -> T.Text
whatInvCoins cols it r rc
  | it == PCEq = ""
  | otherwise = case rc of
    Left  Empty      -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " doesn't refer to any coins ", getLocTxtForInvType it, " ", supplementNone "coins" it, "." ]
    Left  (NoneOf c) -> let cn = mkTxtForCoins c in T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " doesn't refer to any ", cn, " ", getLocTxtForInvType it, " ", supplementNone cn it, "." ]
    Left  (SomeOf c) -> let cn = mkTxtForCoins c in T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " doesn't refer to any ", cn, " ", getLocTxtForInvType it, " ", supplementNotEnough cn it, "." ]
    Right (SomeOf c) -> T.unlines . wordWrap cols . T.concat $ [ dblQuote r, " may refer to the ", mkTxtForCoinsWithAmt c, " ", getLocTxtForInvType it, "." ]
    _                -> patternMatchFail "whatInvCoins" [ showText rc ]
  where
    supplementNone cn      = \case PCInv -> "(you don't have any "       <> cn <> ")"
                                   RmInv -> "(there aren't any "         <> cn <> " here)"
                                   PCEq  -> oops "supplementNone"
    supplementNotEnough cn = \case PCInv -> "(you don't have that many " <> cn <> ")"
                                   RmInv -> "(there aren't that many "   <> cn <> " here)"
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
uptime (_, mq, cols) [] = (try . send mq . parse =<< runUptime) >>= eitherRet (\e -> logIOEx "uptime" e >> sendGenericErrorMsg mq cols)
  where
    runUptime = liftIO . readProcess "uptime" [] $ ""
    parse ut  = let (a, b) = span (/= ',') ut
                    a'     = unwords . tail . words $ a
                    b'     = dropWhile isSpace . takeWhile (/= ',') . tail $ b
                    c      = (toUpper . head $ a') : tail a'
                in T.concat [ T.pack c, " ", T.pack b', ".\n\n" ]
uptime imc@(_, mq, cols) rs = ignore mq cols rs >> uptime imc []


-----


quit :: Action
quit (_, mq, cols) [] = liftIO . atomically . writeTQueue mq . Quit . nl . T.unlines . wordWrap cols $ "Thanks for playing! See you next time."
quit (_, mq, cols) _  = send mq . nl . T.unlines . wordWrap cols $ "Type " <> dblQuote "quit" <> " with no arguments to quit the game."


handleQuit :: Id -> MudStack ()
handleQuit i = do
    logNotice "handleQuit" $ "player " <> show i <> " has quit"
    wsTMVar  <- getWSTMVar
    ptTMVar  <- getNWSTMVar plaTblTMVar
    mqtTMVar <- getNWSTMVar msgQueueTblTMVar
    liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        pt  <- takeTMVar ptTMVar
        mqt <- takeTMVar mqtTMVar
        -----
        let p    = (ws^.pcTbl)  ! i
        let ri   = p^.rmId
        let ris  = (ws^.invTbl) ! ri
        let ws'  = ws  & typeTbl.at  i  .~ Nothing
                       & entTbl.at   i  .~ Nothing
                       & invTbl.at   i  .~ Nothing
                       & coinsTbl.at i  .~ Nothing
                       & eqTbl.at    i  .~ Nothing
                       & mobTbl.at   i  .~ Nothing
                       & pcTbl.at    i  .~ Nothing
                       & invTbl.at   ri ?~ delete i ris
        let pt'  = pt  & at i .~ Nothing
        let mqt' = mqt & at i .~ Nothing
        -----
        putTMVar wsTMVar  ws'
        putTMVar ptTMVar  pt'
        putTMVar mqtTMVar mqt'


-- ==================================================
-- Wizard commands:


wizDispCmdList :: Action
wizDispCmdList = dispCmdList (cmdPred . Just $ wizCmdChar)


-----


wizShutdown :: Action
wizShutdown (_, mq, _   ) [] = logNotice "wizShutdown" "shutting down" >> (liftIO . atomically . writeTQueue mq $ Shutdown)
wizShutdown (_, mq, cols) _  = send mq . nl . T.unlines . wordWrap cols $  "Type " <> quoted <> " with no arguments to shut down the game server."
  where
    quoted = dblQuote . prefixWizCmd $ "shutdown"


-----


wizTime :: Action
wizTime (_, mq, cols) [] = do
    ct <- liftIO getCurrentTime
    zt <- liftIO getZonedTime
    send mq . T.unlines . concatMap (wordWrap cols) $ [ "At the tone, the time will be...", formatThat ct, formatThat zt, "" ]
  where
    formatThat t = let wordy = T.words . showText $ t
                       zone  = last wordy
                       date  = head wordy
                       time  = T.init . T.reverse . T.dropWhile (/= '.') . T.reverse . head . tail $ wordy
                   in T.concat [ zone, ": ", date, " ", time ]
wizTime imc@(_, mq, cols) rs = ignore mq cols rs >> wizTime imc []


-----


wizDay :: Action
wizDay     (_, mq, _)    [] =  send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
wizDay imc@(_, mq, cols) rs = ignore mq cols rs >> wizDay imc []


-- ==================================================
-- Debug commands:


debugDispCmdList :: Action
debugDispCmdList = dispCmdList (cmdPred . Just $ debugCmdChar)


-----


debugBuffCheck :: Action
debugBuffCheck (_, mq, cols) [] = try helper >>= eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = do
        td      <- liftIO getTemporaryDirectory
        (fn, h) <- liftIO . openTempFile td $ "temp"
        bm      <- liftIO . hGetBuffering $ h
        liftIO $ hClose h >> removeFile fn
        send mq . nl . T.unlines . wordWrapIndent 2 cols . T.concat $ [ "(Default) buffering mode for temp file ", dblQuote . T.pack $ fn, " is ", dblQuote . showText $ bm, "." ]
debugBuffCheck imc@(_, mq, cols) rs = ignore mq cols rs >> debugBuffCheck imc []


-----


debugDispEnv :: Action
debugDispEnv (_, mq, cols) [] = send mq . nl =<< (mkAssocListTxt cols <$> liftIO getEnvironment)
debugDispEnv (_, mq, cols) rs = liftIO getEnvironment >>= \env ->
    send mq . T.unlines . map (helper env) . nub $ rs
  where
    helper env r = mkAssocListTxt cols . filter grepPair $ env
      where
        grepPair = uncurry (||) . over both (grep . T.pack)
        grep     = (r `T.isInfixOf`)


-----


debugLog :: Action
debugLog (_, mq, _) [] = helper >> ok mq
  where
    helper       = replicateM_ 100 . liftIO . forkIO . void . runStateInIORefT heavyLogging =<< get
    heavyLogging = replicateM_ 100 . logNotice "debugLog" . ("Logging from " ++) . show =<< liftIO myThreadId
debugLog imc@(_, mq, cols) rs = ignore mq cols rs >> debugLog imc []


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"

------


debugThrow :: Action
debugThrow _                 [] = throwIO DivideByZero
debugThrow imc@(_, mq, cols) rs = ignore mq cols rs >> debugThrow imc []


-----


debugSniff :: Action
debugSniff (_, mq, cols) [] = do
    (nli, eli) <- over both asyncThreadId <$> getLogAsyncs
    kvs <- M.assocs <$> getNWS threadTblTMVar
    ds  <- mapM mkDesc $ head kvs : (nli, Notice) : (eli, Error) : tail kvs
    let msg = T.unlines . concatMap (wordWrap cols) $ ds
    send mq . nl $ divider <> msg <> divider
  where
    mkDesc (k, v) = (liftIO . threadStatus $ k) >>= \s ->
        return . T.concat $ [ showText k, " ", bracketPad 15 . mkTypeName $ v, showText s ]
    mkTypeName (Server i) = "Server  " <> showText i
    mkTypeName t          = showText t
    divider               = nl . mkDividerTxt $ cols
debugSniff imc@(_, mq, cols) rs = ignore mq cols rs >> debugSniff imc []


-----


-- TODO: This command could be automatically run at certain intervals.
debugPurge :: Action
debugPurge     (_, mq, _)    [] = purge >> ok mq
debugPurge imc@(_, mq, cols) rs = ignore mq cols rs >> debugPurge imc []


purge :: MudStack ()
purge = do
    ks <- M.keys <$> getNWS threadTblTMVar
    ss <- liftIO . mapM threadStatus $ ks
    let kss = zip ks ss
    modifyNWS threadTblTMVar $ \tt -> foldl' helper tt kss
  where
    helper m (k, s)
      | s == ThreadFinished = M.delete k m
      | otherwise           = m
