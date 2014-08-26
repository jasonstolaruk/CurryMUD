{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}
-- TODO: -Werror
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.Cmds (serverWrapper) where

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
import Control.Concurrent (forkFinally, forkIO, myThreadId)
import Control.Concurrent.Async (asyncThreadId, race_)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar, TMVar)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (ArithException(..), fromException, IOException, SomeException)
import Control.Exception.Lifted (catch, finally, throwIO, try)
import Control.Lens (_1, at, both, folded, over, to)
import Control.Lens.Operators ((&), (?~), (.~), (^.), (^..))
import Control.Monad (forever, forM_, guard, mplus, replicateM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, gets)
import Data.Char (isSpace, toUpper)
import Data.Functor ((<$>))
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete, find, foldl', intercalate, nub, nubBy, sort)
import Data.Maybe (isNothing)
import Data.Monoid ((<>), mempty)
import Data.Text.Strict.Lens (packed, unpacked)
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import GHC.Conc (threadStatus)
import Network (accept, listenOn, PortID(..))
import Prelude hiding (pi)
import System.Directory (getDirectoryContents, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(ExitSuccess), exitFailure, exitSuccess)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetBuffering, hGetLine, hPutStr, hSetBuffering, hSetNewlineMode, openTempFile, universalNewlineMode)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import System.Random (newStdGen, randomR) -- TODO: Use mwc-random or tf-random. QC uses tf-random.
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Map.Lazy as M (elems, empty, filter, null, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- TODO: Here's the plan:
-- [DONE] 1. Consider using conduit.
-- [DONE] 2. Go into server mode. Accept incoming connections.
-- 3. Implement client-based routing of output.
-- 4. Implement the broadcasting of messages.
-- 5. Review your coding guide, and undertake a refactoring of the entire codebase. Consider the following:
-- a. Code reduction.
-- b. Consistency in binding names.
-- c. Sylistic issues. Use ++ instead of <> where applicable. Use <> or concat lists of text(?), etc.


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
          , Cmd { cmdName = prefixDebugCmd "throw", action = debugThrow, cmdDesc = "Throw an exception." }
          , Cmd { cmdName = prefixDebugCmd "sniff", action = debugSniff, cmdDesc = "Sniff out a dirty thread." }

          -- ==================================================
          -- Player commands:
          , Cmd { cmdName = "?", action = plaDispCmdList, cmdDesc = "Display this command list." }
          , Cmd { cmdName = "about", action = about, cmdDesc = "About this game." }
          , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
          --, Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop items on the ground." }
          , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
          --, Cmd { cmdName = "equip", action = equip, cmdDesc = "Display readied equipment." }
          , Cmd { cmdName = "exits", action = exits, cmdDesc = "Display obvious exits." }
          --, Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick items up off the ground." }
          , Cmd { cmdName = "help", action = help, cmdDesc = "Get help on topics or commands." }
          , Cmd { cmdName = "inv", action = inv, cmdDesc = "Inventory." }
          , Cmd { cmdName = "look", action = look, cmdDesc = "Look." }
          , Cmd { cmdName = "motd", action = motd, cmdDesc = "Display the message of the day." }
          , Cmd { cmdName = "n", action = go "n", cmdDesc = "Go north." }
          , Cmd { cmdName = "ne", action = go "ne", cmdDesc = "Go northeast." }
          , Cmd { cmdName = "nw", action = go "nw", cmdDesc = "Go northwest." }
          --, Cmd { cmdName = "put", action = putAction, cmdDesc = "Put items in a container." }
          --, Cmd { cmdName = "quit", action = quit, cmdDesc = "Quit." }
          --, Cmd { cmdName = "ready", action = ready, cmdDesc = "Ready items." }
          --, Cmd { cmdName = "remove", action = remove, cmdDesc = "Remove items from a container." }
          , Cmd { cmdName = "s", action = go "s", cmdDesc = "Go south." }
          , Cmd { cmdName = "se", action = go "se", cmdDesc = "Go southeast." }
          , Cmd { cmdName = "sw", action = go "sw", cmdDesc = "Go southwest." }
          , Cmd { cmdName = "u", action = go "u", cmdDesc = "Go up." }
          --, Cmd { cmdName = "unready", action = unready, cmdDesc = "Unready items." }
          , Cmd { cmdName = "uptime", action = uptime, cmdDesc = "Display game server uptime." }
          , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." } ]
          --, Cmd { cmdName = "what", action = what, cmdDesc = "Disambiguate abbreviations." } ]


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd c cn = [c]^.packed <> cn


prefixWizCmd :: CmdName -> T.Text
prefixWizCmd = prefixCmd wizCmdChar


prefixDebugCmd :: CmdName -> T.Text
prefixDebugCmd = prefixCmd debugCmdChar


serverWrapper :: MudStack ()
serverWrapper = (initAndStart `catch` topLvlExHandler) `finally` closeLogs
  where
    initAndStart = do
        initLogging
        logNotice "serverWrapper" "server started"
        initWorld
        listen


topLvlExHandler :: SomeException -> MudStack ()
topLvlExHandler e = let oops msg = logExMsg "topLvlExHandler" msg e >> liftIO exitFailure
                    in case fromException e of
                      Just ExitSuccess -> logNotice "topLvlExHandler" "exiting normally"
                      Just _           -> oops $ dblQuoteStr "ExitFailure" <> " caught by the top level handler; rethrowing"
                      Nothing          -> oops "exception caught by the top level handler; exiting gracefully"


listen :: MudStack ()
listen = do
    logNotice "listen" $ "listening for incoming connections on port " <> show port
    sock <- liftIO . listenOn . PortNumber . fromIntegral $ port
    forever $ do
        (h, host, port') <- liftIO . accept $ sock
        logNotice "listen" $ "connected to " <> show host <> " on local port " <> show port'
        s <- get
        liftIO $ forkFinally (runStateInIORefT (talk h) s) (\_ -> hClose h)


talk :: Handle -> MudStack ()
talk h = do
    liftIO configBuffer
    mq <- liftIO newTQueueIO
    i  <- adHoc mq
    logNotice "talk" $ "new ID for incoming player: " <> show i
    dumpTitle mq
    prompt mq "> "
    s <- get
    liftIO $ race_ (runStateInIORefT (server  h mq i) s)
                   (runStateInIORefT (receive h mq)   s)
  where
    configBuffer = hSetNewlineMode h universalNewlineMode >> hSetBuffering h LineBuffering


adHoc :: MsgQueue -> MudStack Id
adHoc mq = do
    wsTMVar  <- gets (^.worldStateTMVar)
    ptTMVar  <- gets (^.nonWorldState.plaTblTMVar)
    mqtTMVar <- gets (^.nonWorldState.msgQueueTblTMVar)
    let helper = liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        pt  <- takeTMVar ptTMVar
        mqt <- takeTMVar mqtTMVar
        -----
        let ks  = ws^.typeTbl.to IM.keys
        let i   = head . (\\) [0..] $ ks
        -----
        let e   = Ent i "player" "Player Character" "" "You see an ad-hoc player character." 0
        let is  = [iKewpie1, iBag1, iClub]
        let co  = mempty
        let em  = M.empty
        let m   = Mob Male 10 10 10 10 10 10 0 RHand
        let pc  = PC 1 Human
        -----
        let pla = Pla 30
        -----
        let ws'  = ws  & typeTbl.at i ?~ PCType & entTbl.at i ?~ e & invTbl.at i ?~ is & coinsTbl.at i ?~ co & eqTbl.at i ?~ em & mobTbl.at i ?~ m & pcTbl.at i ?~ pc
        let pt'  = pt  & at i ?~ pla
        let mqt' = mqt & at i ?~ mq
        -----
        putTMVar wsTMVar  ws'
        putTMVar ptTMVar  pt'
        putTMVar mqtTMVar mqt'
        -----
        return i
    helper


dumpTitle :: MsgQueue -> MudStack ()
dumpTitle mq = liftIO newStdGen >>= \g ->
    let range = (1, noOfTitles)
        n     = randomR range g^._1
        fn    = "title"^.unpacked ++ show n
    in (try . takeADump $ fn) >>= eitherRet dumpTitleTxtExHandler
  where
    takeADump fn = send mq =<< ((<> nlt) <$> (liftIO . T.readFile . (titleDir ++) $ fn))


dumpTitleTxtExHandler :: IOException -> MudStack ()
dumpTitleTxtExHandler e = f "dumpTitleTxt" e
  where
    f = if | isDoesNotExistError e -> logIOEx
           | isPermissionError   e -> logIOEx
           | otherwise             -> logIOExRethrow


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


server :: Handle -> MsgQueue -> Id -> MudStack ()
server h mq i = forever $ (liftIO . atomically . readTQueue $ mq) >>= \case
  FromServer msg -> liftIO . hPutStr h $ msg^.unpacked
  FromClient msg -> let msg' = T.strip msg
                    in unless (T.null msg') (handleInp mq i msg' `catch` topLvlExHandler) -- TODO: Figure out how to handle exceptions.
  Prompt     p   -> liftIO $ hPutStr h (p^.unpacked) >> hFlush h


receive :: Handle -> MsgQueue -> MudStack ()
receive h mq = forever $ (liftIO . hGetLine $ h) >>= \msg ->
    liftIO . atomically . writeTQueue mq . FromClient $ msg^.packed


handleInp :: MsgQueue -> Id -> T.Text -> MudStack ()
handleInp mq i = maybeVoid (dispatch mq i) . splitInp


type Input = (CmdName, Rest)


splitInp :: T.Text -> Maybe Input
splitInp = splitUp . T.words
  where
    splitUp []     = Nothing
    splitUp [t]    = Just (t, [])
    splitUp (t:ts) = Just (t, ts)


dispatch :: MsgQueue -> Id -> Input -> MudStack ()
dispatch mq i (cn, rest) = do
    cols <- getPlaColumns i
    findAction i cn >>= maybe sorry (\act -> act (mq, i, cols) rest)
    prompt mq "> "
  where
    sorry = send mq $ "What?" <> nlt <> nlt


findAction :: Id -> CmdName -> MudStack (Maybe Action)
findAction i cn = getWS >>= \ws ->
    let p        = (ws^.pcTbl) ! i
        r        = (ws^.rmTbl) ! (p^.rmId)
        cmdList' = mkCmdListWithRmLinks r
        cns      = map cmdName cmdList'
    in maybe (return Nothing)
             (\fn -> return . Just . findActionForFullName fn $ cmdList')
             (findFullNameForAbbrev (T.toLower cn) cns)
  where
    findActionForFullName fn = action . head . filter ((== fn) . cmdName)


mkCmdListWithRmLinks :: Rm -> [Cmd]
mkCmdListWithRmLinks r = cmdList <> [ mkCmdForRmLink rl | rl <- r^.rmLinks, rl^.linkName `notElem` stdLinkNames ]
  where
    mkCmdForRmLink rl = let ln = rl^.linkName.to T.toLower
                        in Cmd { cmdName = ln, action = go ln, cmdDesc = "" }


-- ==================================================
-- Player commands:


about :: Action
about (mq, _, cols) [] = try helper >>= eitherRet (readFileExHandler mq cols "about")
  where
    helper = (liftIO . T.readFile . (miscDir ++) $ "about") >>= \contents ->
        send mq . T.unlines . (++ [""]) . concat . map (wordWrap cols) . T.lines $ contents
about mic@(mq, _, cols) rs = ignore mq cols rs >> about mic []


readFileExHandler :: MsgQueue -> Cols -> String -> IOException -> MudStack ()
readFileExHandler mq cols fn e = handleThat >> (send mq . T.unlines . wordWrap cols $ genericErrorMsg)
  where
    handleThat
      | isDoesNotExistError e = logIOEx fn e
      | isPermissionError   e = logIOEx fn e
      | otherwise             = logIOExRethrow fn e


ignore :: MsgQueue -> Cols -> Rest -> MudStack ()
ignore mq cols rs = let ignored = dblQuote . T.unwords $ rs
                    in send mq . T.unlines . wordWrap cols $ "(Ignoring " <> ignored <> "...)"


-----


-- TODO: Automatically execute this cmd after a user authenticates.
motd :: Action
motd     (mq, _, cols) [] = send mq =<< getMotdTxt mq cols
motd mic@(mq, _, cols) rs = ignore mq cols rs >> motd mic []


getMotdTxt :: MsgQueue -> Cols -> MudStack T.Text
getMotdTxt mq cols = (try . liftIO $ helper) >>= eitherRet (\e -> readFileExHandler mq cols "getMotdTxt" e >> return genericErrorMsg)
  where
    helper  = (T.readFile . (miscDir ++) $ "motd") >>= \contents ->
        return (T.unlines . (++ [divider, ""]) . (divider :) . concatMap (wordWrap cols) . T.lines $ contents)
    divider = mkDividerTxt cols


-----


plaDispCmdList :: Action
plaDispCmdList = dispCmdList (cmdPred Nothing)


dispCmdList :: (Cmd -> Bool) -> Action
dispCmdList p (mq, _, cols) [] = send mq . T.unlines . (++ [""]) . concatMap (wordWrapIndent 10 cols) . cmdListText $ p
dispCmdList p (mq, _, cols) rs = send mq . T.unlines . (++ [""]) . concatMap (wordWrapIndent 10 cols) . intercalate [""] $ [ grepTextList r . cmdListText $ p | r <- rs ]


cmdListText :: (Cmd -> Bool) -> [T.Text]
cmdListText p = sort . T.lines . T.concat . foldl' mkTxtForCmd [] . filter p $ cmdList
  where
    mkTxtForCmd acc c = T.concat [ padOrTrunc 10 . cmdName $ c, cmdDesc c, nlt ] : acc


cmdPred :: Maybe Char -> Cmd -> Bool
cmdPred (Just c) cmd = c == (T.head . cmdName $ cmd)
cmdPred Nothing  cmd = (T.head . cmdName $ cmd) `notElem` [wizCmdChar, debugCmdChar]


-----


help :: Action
help (mq, _, cols) [] = try helper >>= eitherRet (readFileExHandler mq cols "help")
  where
    helper = (liftIO . T.readFile . (helpDir ++) $ "root") >>= \contents ->
        send mq . T.unlines . (++ [""]) . concat . wordWrapLines cols . T.lines $ contents
help (mq, _, cols) rs = do
    topics  <- mapM (\r -> concat . wordWrapLines cols . T.lines <$> getHelpTopicByName mq cols r) rs
    send mq . (<> nlt) . T.unlines . intercalate ["", mkDividerTxt cols, ""] $ topics


type HelpTopic = T.Text


getHelpTopicByName :: MsgQueue -> Cols -> HelpTopic -> MudStack T.Text
getHelpTopicByName mq cols r = (liftIO . getDirectoryContents $ helpDir) >>= \fns ->
    let fns' = tail . tail . sort . delete "root" $ fns
        tns  = fns'^..folded.packed
    in maybe sorry
             getHelpTopic
             (findFullNameForAbbrev r tns)
  where
    sorry           = return ("No help is available on " <> dblQuote r <> ".")
    getHelpTopic tn = (try . helper $ tn) >>= eitherRet (\e -> readFileExHandler mq cols "getHelpTopicByName" e >> return genericErrorMsg) -- TODO: Return an error message elsewhere, too?
    helper       tn = liftIO . T.readFile . (helpDir ++) $ tn^.unpacked


-----


go :: T.Text -> Action
go dir mic [] = goDispatcher mic [dir]
go dir mic rs = goDispatcher mic . (dir :) $ rs


goDispatcher :: Action
goDispatcher _   [] = return ()
goDispatcher mic rs = mapM_ (tryMove mic) rs


tryMove :: MsgQueueIdCols -> T.Text -> MudStack ()
tryMove mic@(mq, i, cols) dir = let dir' = T.toLower dir
                                in helper dir' >>= \case
                                  Left  msg -> send mq . (<> nlt) . T.unlines . wordWrap cols $ msg
                                  Right _   -> look mic []
  where
    helper dir' = onWS $ \(t, ws) ->
                      let p = (ws^.pcTbl) ! i
                          r = (ws^.rmTbl) ! (p^.rmId)
                      in case findExit r dir' of
                        Nothing -> putTMVar t ws >> return (Left . sorry $ dir')
                        Just ri -> let p' = p & rmId .~ ri
                                   in putTMVar t (ws & pcTbl.at i ?~ p') >> return (Right ())
    sorry dir'  = if dir' `elem` stdLinkNames
                    then "You can't go that way."
                    else dblQuote dir <> " is not a valid direction."


findExit :: Rm -> LinkName -> Maybe Id
findExit r ln = case [ rl^.destId | rl <- r^.rmLinks, isValid rl ] of
                  [] -> Nothing
                  is -> Just . head $ is
  where
    isValid rl = ln `elem` stdLinkNames && ln == (rl^.linkName) || ln `notElem` stdLinkNames && ln `T.isInfixOf` (rl^.linkName)


-----


look :: Action
look (mq, i, cols) [] = getWS >>= \ws ->
    let p       = (ws^.pcTbl) ! i
        ri      = p^.rmId
        r       = (ws^.rmTbl) ! ri
        primary = T.unlines . concatMap (wordWrap cols) $ [ r^.name, r^.desc ]
        suppl   = mkExitsSummary cols r <> ricd
        ricd    = mkRmInvCoinsDesc i cols ws ri
    in send mq $ primary <> suppl <> nlt
look (mq, i, cols) rs = let rs' = nub . map T.toLower $ rs in getWS >>= \ws -> -- TODO: Are you nubbing everywhere you should nub?
    let p  = (ws^.pcTbl)    ! i
        ri = p^.rmId
        is = (ws^.invTbl)   ! ri
        c  = (ws^.coinsTbl) ! ri
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames ws rs' is c
               eiss               = zipWith (curry procGecrMisRm) gecrs miss
               ecs                = map procReconciledCoinsRm rcs
               invDesc            = foldl' (helperLookEitherInv ws) "" eiss
               coinsDesc          = foldl' (helperLookEitherCoins ) "" ecs
           in invDesc <> coinsDesc
      else (<> nlt) . T.unlines . wordWrap cols $ "You don't see anything here to look at."
  where
    helperLookEitherInv _  acc (Left  msg) = acc <> msg                     <> nlt
    helperLookEitherInv ws acc (Right is ) = acc <> mkEntDescs i cols ws is <> nlt
    helperLookEitherCoins  acc (Left  msg) = acc <> msg                     <> nlt
    helperLookEitherCoins  acc (Right c  ) = acc <> mkCoinsDesc cols c      <> nlt


-- TODO: Consider implementing a color scheme for lists like these such that the least significant characters of each name are highlighted or bolded somehow.
mkRmInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> T.Text
mkRmInvCoinsDesc i cols ws ri = let is       = (ws^.invTbl)   ! ri
                                    c        = (ws^.coinsTbl) ! ri
                                    entDescs = T.unlines . concatMap (wordWrapIndent 2 cols . mkEntInRmDesc cols) . mkNameCountBothList ws $ is
                                in if c == mempty then entDescs else entDescs <> mkCoinsSummary cols c
  where
    mkEntInRmDesc cols (en, count, (s, _)) | count == 1 = aOrAn s <> " " <> bracketQuote en
    mkEntInRmDesc cols (en, count, b)                   = showText count <> " " <> mkPlurFromBoth b <> " " <> bracketQuote en


mkNameCountBothList :: WorldState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList ws is = let es    = [ (ws^.entTbl) ! i    | i <- is ]
                                ens   = [ e^.name             | e <- es ]
                                ebgns = [ getEntBothGramNos e | e <- es ]
                                cs    = mkCountList ebgns
                            in nub . zip3 ens cs $ ebgns


mkEntDescs :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntDescs i cols ws is = let boths = [ (ei, (ws^.entTbl) ! ei) | ei <- is ]
                          in T.intercalate nlt . map (mkEntDesc i cols ws) $ boths


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
                                                      then dudeYourHandsAreEmpty <> nlt
                                                      else "The " <> e^.sing <> " is empty." <> nlt
                                  (True,  False) -> header <> mkEntsInInvDesc cols ws is
                                  (False, True ) -> header <> mkCoinsSummary  cols c
                                  (True,  True ) -> header <> mkEntsInInvDesc cols ws is <> mkCoinsSummary cols c
  where
    header
      | ei == i   = "You are carrying:" <> nlt
      | otherwise = T.unlines . wordWrap cols $ "The " <> e^.sing <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Cols -> WorldState -> Inv -> T.Text
mkEntsInInvDesc cols ws = T.unlines . concatMap (wordWrapIndent ind cols . helper) . mkNameCountBothList ws
  where
    helper (en, c, (s, _)) | c == 1 = nameCol en <> "1 " <> s
    helper (en, c, b     )          = nameCol en <> showText c <> " "  <> mkPlurFromBoth b
    ind     = 11
    nameCol = bracketPad ind


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper mkCoinsWithNamesList
  where
    helper               = T.unlines . wordWrapIndent 2 cols . T.intercalate ", " . filter (not . T.null) . map mkNameAmt
    mkNameAmt (cn, a)    = if a == 0 then "" else showText a <> " " <> bracketQuote cn
    mkCoinsWithNamesList = zip coinNames . mkListFromCoins $ c


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) = T.unlines . intercalate [""] . map (wordWrap cols) . filter (not . T.null) $ [copDesc, silDesc, golDesc]
  where -- TODO: Come up with good descriptions.
    copDesc = if cop /= 0 then "The copper piece is round and shiny." else ""
    silDesc = if sil /= 0 then "The silver piece is round and shiny." else ""
    golDesc = if gol /= 0 then "The gold piece is round and shiny."   else ""


-----


exits :: Action
exits (mq, i, cols) [] = getWS >>= \ws ->
    let p = (ws^.pcTbl) ! i
        r = (ws^.rmTbl) ! (p^.rmId)
    in send mq . (<> nlt) . mkExitsSummary cols $ r
exits mic@(mq, _, cols) rs = ignore mq cols rs >> exits mic []


mkExitsSummary :: Cols -> Rm -> T.Text
mkExitsSummary cols r = let rlns        = [ rl^.linkName | rl <- r^.rmLinks ]
                            stdNames    = [ sln | sln <- stdLinkNames, sln `elem` rlns ]
                            customNames = filter (`notElem` stdLinkNames) rlns
                        in T.unlines . wordWrapIndent 2 cols . ("Obvious exits: " <>)  . T.intercalate ", " . (stdNames ++) $ customNames


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (mq, i, cols) [] = getWS >>= \ws ->
    let e = (ws^.entTbl) ! i
    in send mq . (<> nlt) . mkInvCoinsDesc i cols ws i $ e
inv (mq, i, cols) rs = let rs' = nub . map T.toLower $ rs in getWS >>= \ws ->
    let is = (ws^.invTbl)   ! i
        c  = (ws^.coinsTbl) ! i
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames ws rs' is c
               eiss               = zipWith (curry procGecrMisPCInv) gecrs miss
               ecs                = map procReconciledCoinsPCInv rcs
               invDesc            = foldl' (helperInvEitherInv ws) "" eiss
               coinsDesc          = foldl' (helperInvEitherCoins ) "" ecs
           in invDesc <> coinsDesc
      else (<> nlt) . T.unlines . wordWrap cols $ dudeYourHandsAreEmpty
  where
    helperInvEitherInv _  acc (Left  msg) = acc <> msg                     <> nlt
    helperInvEitherInv ws acc (Right is ) = acc <> mkEntDescs i cols ws is <> nlt
    helperInvEitherCoins  acc (Left  msg) = acc <> msg                     <> nlt
    helperInvEitherCoins  acc (Right c  ) = acc <> mkCoinsDesc cols c      <> nlt


-----


{-
equip :: Action
equip [] = getWS >>= \ws ->
    let e = (ws^.entTbl) ! 0
    in mkEqDesc ws 0 e
equip rs = let rs' = nub . map T.toLower $ rs in getWS >>= \ws ->
    let em = (ws^.eqTbl) ! 0
        is = M.elems em
    in if not . M.null $ em
      then let (gecrs, miss, rcs) = resolveEntCoinNames ws rs' is mempty
           in undefined --do
               --mapM_ (procGecrMisPCEq_ (descEnts ws)) . zip gecrs $ miss
               --unless (null rcs) $ output ("You don't have any coins among your readied equipment." <> nlt <> nlt)
      else output $ dudeYou'reNaked <> nlt
-}


mkEqDesc :: Id -> Cols -> WorldState -> Id -> Ent -> Type -> T.Text
mkEqDesc i cols ws ei e t = let em    = (ws^.eqTbl) ! ei
                                descs = map mkDesc . mkSlotNameIdList . M.toList $ em
                            in if null descs
                              then none
                              else (header <>) . T.unlines . concat $ [ wordWrapIndent 15 cols l | l <- descs ]
  where
    mkSlotNameIdList = map (first pp)
    mkDesc (sn, i')  = let sn'      = parensPad 15 noFinger
                           noFinger = T.breakOn " finger" sn ^._1
                           e'       = (ws^.entTbl) ! i'
                       in sn' <> e'^.sing <> " " <> e'^.name.to bracketQuote
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


{-
getAction :: Action
getAction [] = advise ["get"] $ "Please specify one or more items to pick up, as in " <> dblQuote "get sword" <> "."
getAction rs = helper >>= output . (<> nlt)
  where
    helper = onWS $ \(t, ws) ->
        let p   = (ws^.pcTbl)    ! 0
            i   = p^.rmId
            ris = (ws^.invTbl)   ! i
            rc  = (ws^.coinsTbl) ! i
        in if (not . null $ ris) || (rc /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames ws rs ris rc
                   eiss               = zipWith (curry procGecrMisRm) gecrs miss
                   ecs                = map procReconciledCoinsRm rcs
                   (ws',  msgs)       = foldl' (helperGetDropEitherInv   Get i 0) (ws, "")    eiss
                   (ws'', msgs')      = foldl' (helperGetDropEitherCoins Get i 0) (ws', msgs) ecs
               in putTMVar t ws'' >> return msgs'
          else putTMVar t ws >> return ("You don't see anything here to pick up." <> nlt)


advise :: [HelpTopic] -> T.Text -> MudStack ()
advise []  msg = output $ msg <> nlt
advise [h] msg = output msg >> outputCon [ "For more information, type ", dblQuote . (<>) "help " $ h, ".", nlt ]
advise hs  msg = output msg >> outputCon [ "See also the following help topics: ", helpTopics,         ".", nlt ]
  where
    helpTopics = dblQuote . T.intercalate (dblQuote ", ") $ hs


type FromId = Id
type ToId   = Id


helperGetDropEitherInv :: GetOrDrop -> FromId -> ToId -> (WorldState, T.Text) -> Either T.Text Inv -> (WorldState, T.Text)
helperGetDropEitherInv god fi ti (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right is  -> let fis = (ws^.invTbl) ! fi
                   tis = (ws^.invTbl) ! ti
                   ws' = ws & invTbl.at fi ?~ deleteFirstOfEach is fis
                            & invTbl.at ti ?~ (sortInv ws . (++) tis $ is)
                   msg = mkGetDropInvDesc ws' god is
               in (ws', msgs <> msg)


mkGetDropInvDesc :: WorldState -> GetOrDrop -> Inv -> T.Text
mkGetDropInvDesc ws god is = T.concat . map helper . mkNameCountBothList ws $ is
  where
    helper (_, c, (s, _)) | c == 1 = "You " <> verb god <> " the " <> s <> "." <> nlt
    helper (_, c, b)               = "You " <> verb god <> " " <> showText c <> " " <> mkPlurFromBoth b <> "." <> nlt
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
mkGetDropCoinsDesc god (Coins (cop, sil, gol)) = T.concat [c, s, g]
  where
    c = if cop /= 0 then helper cop "copper piece" else ""
    s = if sil /= 0 then helper sil "silver piece" else ""
    g = if gol /= 0 then helper gol "gold piece"   else ""
    helper a cn | a == 1 = "You " <> verb god <> " a " <> cn <> "." <> nlt
    helper a cn          = "You " <> verb god <> " " <> showText a <> " " <> cn <> "s." <> nlt
    verb = \case Get  -> "pick up"
                 Drop -> "drop"


-----


dropAction :: Action
dropAction [] = advise ["drop"] $ "Please specify one or more things to drop, as in " <> dblQuote "drop sword" <> "."
dropAction rs = helper >>= output . (<> nlt)
  where
    helper = onWS $ \(t, ws) ->
        let p   = (ws^.pcTbl)    ! 0
            pis = (ws^.invTbl)   ! 0
            pc  = (ws^.coinsTbl) ! 0
            i   = p^.rmId
        in if (not . null $ pis) || (pc /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames ws rs pis pc
                   eiss               = zipWith (curry procGecrMisPCInv) gecrs miss
                   ecs                = map procReconciledCoinsPCInv rcs
                   (ws',  msgs)       = foldl' (helperGetDropEitherInv   Drop 0 i) (ws, "")    eiss
                   (ws'', msgs')      = foldl' (helperGetDropEitherCoins Drop 0 i) (ws', msgs) ecs
               in putTMVar t ws'' >> return msgs'
          else putTMVar t ws >> return dudeYourHandsAreEmpty


-----


putAction :: Action
putAction []  = advise ["put"] $ "Please specify one or more things you want to put, followed by where you want to put them, as in " <> dblQuote "put doll sack" <> "."
putAction [r] = advise ["put"] $ "Please also specify where you want to put it, as in " <> dblQuote ("put " <> r <> " sack") <> "."
putAction rs  = helper >>= output . (<> nlt)
  where
    helper = onWS $ \(t, ws) ->
      let p   = (ws^.pcTbl)    ! 0
          pis = (ws^.invTbl)   ! 0
          pc  = (ws^.coinsTbl) ! 0
          ri  = p^.rmId
          ris = (ws^.invTbl)   ! ri
          rc  = (ws^.coinsTbl) ! ri
          cn  = last rs
          restWithoutCon = init rs
      in if (not . null $ pis) || (pc /= mempty)
        then if T.head cn == rmChar
          then if not . null $ ris
            then shufflePut (t, ws) (T.tail cn) restWithoutCon ris rc pis pc procGecrMisRm
            else putTMVar t ws >> return ("You don't see any containers here." <> nlt)
          else shufflePut (t, ws) cn restWithoutCon pis pc pis pc procGecrMisPCInv
      else putTMVar t ws >> return dudeYourHandsAreEmpty


type InvWithCont   = Inv
type CoinsWithCont = Coins
type InvToPut      = Inv
type CoinsToPut    = Coins


shufflePut :: (TMVar WorldState, WorldState) -> ConName -> Rest -> InvWithCont -> CoinsWithCont -> InvToPut -> CoinsToPut -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) -> STM T.Text
shufflePut (t, ws) cn rs is c pis pc f = let (gecrs, miss, rcs) = resolveEntCoinNames ws [cn] is c
                                           in if null miss && (not . null $ rcs)
                                             then putTMVar t ws >> return ("You can't put something inside a coin." <> nlt)
                                             else case f . head . zip gecrs $ miss of
                                               Left  msg -> putTMVar t ws >> return msg
                                               Right [i] -> let e  = (ws^.entTbl)  ! i
                                                                t' = (ws^.typeTbl) ! i
                                                            in if t' /= ConType
                                                              then putTMVar t ws >> return ("The " <> e^.sing <> " isn't a container." <> nlt)
                                                              else let (gecrs', miss', rcs') = resolveEntCoinNames ws rs pis pc
                                                                       eiss                  = zipWith (curry procGecrMisPCInv) gecrs' miss'
                                                                       ecs                   = map procReconciledCoinsPCInv rcs'
                                                                       (ws',  msgs)          = foldl' (helperPutRemEitherInv   Put 0 i e) (ws, "")    eiss
                                                                       (ws'', msgs')         = foldl' (helperPutRemEitherCoins Put 0 i e) (ws', msgs) ecs
                                                                   in putTMVar t ws'' >> return msgs'
                                               Right _   -> putTMVar t ws >> return ("You can only put things into one container at a time." <> nlt)


type ToEnt = Ent


helperPutRemEitherInv :: PutOrRem -> FromId -> ToId -> ToEnt -> (WorldState, T.Text) -> Either T.Text Inv -> (WorldState, T.Text)
helperPutRemEitherInv por fi ti te (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right is  -> let (is', msgs') = if ti `elem` is
                                    then (filter (/= ti) is, msgs <> "You can't put the " <> te^.sing <> " inside itself." <> nlt)
                                    else (is, msgs)
                   fis = (ws^.invTbl) ! fi
                   tis = (ws^.invTbl) ! ti
                   ws' = ws & invTbl.at fi ?~ deleteFirstOfEach is' fis
                            & invTbl.at ti ?~ (sortInv ws . (++) tis $ is')
                   msg = mkPutRemInvDesc ws' por is' te
               in (ws', msgs' <> msg)


mkPutRemInvDesc :: WorldState -> PutOrRem -> Inv -> ToEnt -> T.Text
mkPutRemInvDesc ws por is te = T.concat . map helper . mkNameCountBothList ws $ is
  where
    helper (_, c, (s, _)) | c == 1 = "You " <> verb por <> " the " <> s <> " " <> prep por <> " " <> te^.sing <> "." <> nlt
    helper (_, c, b)               = "You " <> verb por <> " " <> showText c <> " " <> mkPlurFromBoth b <> " " <> prep por <> " " <> te^.sing <> "." <> nlt
    verb = \case Put -> "put"
                 Rem -> "remove"
    prep = \case Put -> "in the"
                 Rem -> "from the"


helperPutRemEitherCoins :: PutOrRem -> FromId -> ToId -> ToEnt -> (WorldState, T.Text) -> Either T.Text Coins -> (WorldState, T.Text)
helperPutRemEitherCoins por fi ti te (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right c   -> let fc  = (ws^.coinsTbl) ! fi
                   tc  = (ws^.coinsTbl) ! ti
                   ws' = ws & coinsTbl.at fi ?~ fc <> negateCoins c
                            & coinsTbl.at ti ?~ tc <> c
                   msg = mkPutRemCoinsDesc por c te
               in (ws', msgs <> msg)


mkPutRemCoinsDesc :: PutOrRem -> Coins -> ToEnt -> T.Text
mkPutRemCoinsDesc por (Coins (cop, sil, gol)) te = T.concat [c, s, g]
  where
    c = if cop /= 0 then helper cop "copper piece" else ""
    s = if sil /= 0 then helper sil "silver piece" else ""
    g = if gol /= 0 then helper gol "gold piece"   else ""
    helper a cn | a == 1 = "You " <> verb por <> " a " <> cn <> " " <> prep por <> " " <> te^.sing <> "." <> nlt
    helper a cn          = "You " <> verb por <> " " <> showText a <> " " <> cn <> "s " <> prep por <> " " <> te^.sing <> "." <> nlt
    verb = \case Put -> "put"
                 Rem -> "remove"
    prep = \case Put -> "in the"
                 Rem -> "from the"


-----


remove :: Action
remove []  = advise ["remove"] $ "Please specify one or more things to remove, followed by the container you want to remove them from, as in " <> dblQuote "remove doll sack" <> "."
remove [r] = advise ["remove"] $ "Please also specify the container you want to remove it from, as in " <> dblQuote ("remove " <> r <> " sack") <> "."
remove rs  = helper >>= output . (<> nlt)
  where
    helper = onWS $ \(t, ws) ->
      let p   = (ws^.pcTbl)    ! 0
          pis = (ws^.invTbl)   ! 0
          pc  = (ws^.coinsTbl) ! 0
          ri  = p^.rmId
          ris = (ws^.invTbl)   ! ri
          rc  = (ws^.coinsTbl) ! ri
          cn  = last rs
          restWithoutCon = init rs
      in if T.head cn == rmChar
          then if not . null $ ris
            then shuffleRem (t, ws) (T.tail cn) restWithoutCon ris rc procGecrMisRm
            else putTMVar t ws >> return ("You don't see any containers here." <> nlt)
          else shuffleRem (t, ws) cn restWithoutCon pis pc procGecrMisPCInv


shuffleRem :: (TMVar WorldState, WorldState) -> ConName -> Rest -> InvWithCont -> CoinsWithCont -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) -> STM T.Text
shuffleRem (t, ws) cn rs is c f = let (gecrs, miss, rcs) = resolveEntCoinNames ws [cn] is c
                                  in if null miss && (not . null $ rcs)
                                    then putTMVar t ws >> return ("You can't remove something from a coin." <> nlt)
                                    else case f . head . zip gecrs $ miss of
                                      Left  msg -> putTMVar t ws >> return msg
                                      Right [i] -> let e  = (ws^.entTbl)  ! i
                                                       t' = (ws^.typeTbl) ! i
                                                   in if t' /= ConType
                                                     then putTMVar t ws >> return ("The " <> e^.sing <> " isn't a container." <> nlt)
                                                     else let cis                   = (ws^.invTbl)   ! i
                                                              cc                    = (ws^.coinsTbl) ! i
                                                              (gecrs', miss', rcs') = resolveEntCoinNames ws rs cis cc
                                                              eiss                  = map (procGecrMisCon (e^.sing)) . zip gecrs' $ miss'
                                                              ecs                   = map (procReconciledCoinsCon (e^.sing)) rcs'
                                                              (ws',  msgs)          = foldl' (helperPutRemEitherInv   Rem i 0 e) (ws, "")    eiss
                                                              (ws'', msgs')         = foldl' (helperPutRemEitherCoins Rem i 0 e) (ws', msgs) ecs
                                                          in putTMVar t ws'' >> return msgs'
                                      Right _   -> putTMVar t ws >> return ("You can only remove things from one container at a time." <> nlt)


-----


ready :: Action
ready [] = advise ["ready"] $ "Please specify one or more things to ready, as in " <> dblQuote "ready sword" <> "."
ready rs = helper >>= output . (<> nlt)
  where
    helper = onWS $ \(t, ws) ->
        let is = (ws^.invTbl)   ! 0
            c  = (ws^.coinsTbl) ! 0
        in if (not . null $ is) || (c /= mempty)
          then let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols ws rs is mempty
                   eiss                      = zipWith (curry procGecrMisReady) gecrs miss
                   msgs                      = if null rcs then "" else "You can't ready coins." <> nlt
                   (ws',  msgs')             = foldl' (helperReady 0) (ws, msgs) . zip eiss $ mrols
               in putTMVar t ws' >> return msgs'
          else putTMVar t ws >> return dudeYourHandsAreEmpty


helperReady :: Id -> (WorldState, T.Text) -> (Either T.Text Inv, Maybe RightOrLeft) -> (WorldState, T.Text)
helperReady pi (ws, msgs) (eis, mrol) = case eis of
  Left  msg -> (ws, msgs <> msg)
  Right is  -> foldl' (readyDispatcher pi mrol) (ws, msgs) is


readyDispatcher :: Id -> Maybe RightOrLeft -> (WorldState, T.Text) -> Id -> (WorldState, T.Text)
readyDispatcher pi mrol (ws, msgs) i = let e = (ws^.entTbl)  ! i
                                           t = (ws^.typeTbl) ! i
                                       in case t of
                                         ClothType -> readyCloth pi mrol (ws, msgs) i e
                                         WpnType   -> readyWpn   pi mrol (ws, msgs) i e
                                         _         -> (ws, msgs <> "You can't ready a " <> e^.sing <> "." <> nlt)


moveReadiedItem :: (WorldState, T.Text) -> Id -> EqMap -> Slot -> Id -> T.Text -> (WorldState, T.Text)
moveReadiedItem (ws, msgs) pi em s i msg = let pis = (ws^.invTbl) ! pi
                                               ws' = ws & invTbl.at pi ?~ filter (/= i) pis
                                                        & eqTbl.at  pi ?~ (em & at s ?~ i)
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
rEarSlots   = [REar1S, REar2S]
lEarSlots   = [LEar1S, LEar2S]
noseSlots   = [Nose1S, Nose2S]
neckSlots   = [Neck1S   .. Neck3S]
rWristSlots = [RWrist1S .. RWrist3S]
lWristSlots = [LWrist1S .. LWrist3S]


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = em^.at s.to isNothing


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


sorryFullClothSlots :: Cloth -> T.Text
sorryFullClothSlots c = "You can't wear any more " <> whatWhere c <> nlt
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
sorryFullClothSlotsOneSide s = "You can't wear any more on your " <> pp s <> "." <> nlt


-- Ready clothing:


readyCloth :: Id -> Maybe RightOrLeft -> (WorldState, T.Text) -> Id -> Ent -> (WorldState, T.Text)
readyCloth pi mrol (ws, msgs) i e = let em = (ws^.eqTbl)    ! pi
                                        c  = (ws^.clothTbl) ! i
                                    in case maybe (getAvailClothSlot ws pi c em) (getDesigClothSlot ws e c em) mrol of
                                      Left  msg -> (ws, msgs <> msg)
                                      Right s   -> moveReadiedItem (ws, msgs) pi em s i . mkReadyMsg s $ c
  where
    mkReadyMsg s = \case NoseC   -> putOnMsg
                         NeckC   -> putOnMsg
                         FingerC -> "You slide the " <> e^.sing <> " on your " <> pp s <> "." <> nlt
                         _       -> wearMsg
      where
        putOnMsg = "You put on the " <> e^.sing <> "." <> nlt
        wearMsg  = "You wear the "   <> e^.sing <> " on your " <> pp s <> "." <> nlt


getAvailClothSlot :: WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot ws pi c em = let m = (ws^.mobTbl) ! pi
                                   g = m^.gender
                                   h = m^.hand
                               -- TODO: Move "procMaybe" to the left of the "case".
                               in case c of EarC    -> procMaybe $ getEarSlotForGender g `mplus` (getEarSlotForGender . otherGender $ g)
                                            NoseC   -> procMaybe $ findAvailSlot em noseSlots
                                            NeckC   -> procMaybe $ findAvailSlot em neckSlots
                                            WristC  -> procMaybe $ getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
                                            FingerC -> procMaybe $ getRingSlot g h
                                            _       -> undefined -- TODO
  where
    procMaybe             = maybe (Left . sorryFullClothSlots $ c) Right
    getEarSlotForGender g = findAvailSlot em $ case g of Male   -> lEarSlots
                                                         Female -> rEarSlots
                                                         _      -> patternMatchFail "getAvailClothSlot getEarSlotForGender" [ showText g ]
    getWristSlotForHand h = findAvailSlot em $ case h of RHand  -> lWristSlots
                                                         LHand  -> rWristSlots
                                                         _      -> patternMatchFail "getAvailClothSlot getWristSlotForHand" [ showText h ]
    getRingSlot g h       = findAvailSlot em $ case g of Male   -> case h of
                                                                     RHand -> [LRingFS, LIndexFS, RRingFS, RIndexFS, LMidFS, RMidFS, LPinkyFS, RPinkyFS]
                                                                     LHand -> [RRingFS, RIndexFS, LRingFS, LIndexFS, RMidFS, LMidFS, RPinkyFS, LPinkyFS]
                                                                     _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
                                                         Female -> case h of
                                                                     RHand -> [LRingFS, LIndexFS, RRingFS, RIndexFS, LPinkyFS, RPinkyFS, LMidFS, RMidFS]
                                                                     LHand -> [RRingFS, RIndexFS, LRingFS, LIndexFS, RPinkyFS, LPinkyFS, RMidFS, LMidFS]
                                                                     _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
                                                         _      -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText g ]


getDesigClothSlot :: WorldState -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot ws e c em rol
  | c `elem` [NoseC, NeckC, UpBodyC, LowBodyC, FullBodyC, BackC, FeetC] = Left sorryCantWearThere
  | isRingRol rol && c /= FingerC           = Left sorryCantWearThere
  | c == FingerC && (not . isRingRol $ rol) = Left ringHelp
  | otherwise = case c of EarC    -> maybe (Left sorryFullEar)   Right (findSlotFromList rEarSlots   lEarSlots)
                          WristC  -> maybe (Left sorryFullWrist) Right (findSlotFromList rWristSlots lWristSlots)
                          FingerC -> maybe (Right slotFromRol)
                                           (\i -> let e' = (ws^.entTbl) ! i in Left . sorry slotFromRol $ e')
                                           (em^.at slotFromRol)
                          _       -> undefined -- TODO
  where
    sorryCantWearThere     = "You can't wear a " <> e^.sing <> " on your " <> pp rol <> "." <> nlt
    findSlotFromList rs ls = findAvailSlot em $ case rol of R -> rs
                                                            L -> ls
                                                            _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of R -> rs
                                                L -> ls
                                                _ -> patternMatchFail "getDesigClothSlot getSlotFromList" [ showText rol ]
    sorryFullEar     = sorryFullClothSlotsOneSide (getSlotFromList rEarSlots   lEarSlots)
    sorryFullWrist   = sorryFullClothSlotsOneSide (getSlotFromList rWristSlots lWristSlots)
    slotFromRol      = fromRol rol :: Slot
    sorry s e'       = "You're already wearing a " <> e'^.sing <> " on your " <> pp s <> "." <> nlt


-- Ready weapons:


readyWpn :: Id -> Maybe RightOrLeft -> (WorldState, T.Text) -> Id -> Ent -> (WorldState, T.Text)
readyWpn pi mrol (ws, msgs) i e = let em  = (ws^.eqTbl)  ! pi
                                      w   = (ws^.wpnTbl) ! i
                                      sub = w^.wpnSub
                                  in if not . isSlotAvail em $ BothHandsS
                                    then (ws, msgs <> "You're already wielding a two-handed weapon." <> nlt)
                                    else case maybe (getAvailWpnSlot ws pi em) (getDesigWpnSlot ws e em) mrol of
                                      Left  msg -> (ws, msgs <> msg)
                                      Right s   -> case sub of
                                                     OneHanded -> moveReadiedItem (ws, msgs) pi em s i $ "You wield the " <> e^.sing <> " with your " <> pp s <> "." <> nlt
                                                     TwoHanded -> if all (isSlotAvail em) [RHandS, LHandS]
                                                                    then moveReadiedItem (ws, msgs) pi em BothHandsS i $ "You wield the " <> e^.sing <> " with both hands." <> nlt
                                                                    else (ws, msgs <> "Both hands are required to wield the " <> e^.sing <> "." <> nlt)


getAvailWpnSlot :: WorldState -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot ws pi em = let m = (ws^.mobTbl) ! pi
                               h = m^.hand
                           in maybe (Left $ "You're already wielding two weapons." <> nlt)
                                    Right
                                    (findAvailSlot em . map getSlotForHand $ [ h, otherHand h ])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> patternMatchFail "getAvailWpnSlot getSlotForHand" [ showText h ]


getDesigWpnSlot :: WorldState -> Ent -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigWpnSlot ws e em rol
  | isRingRol rol = Left sorryNotRing
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e' = (ws^.entTbl) ! i in Left . sorry $ e')
                          (em^.at desigSlot)
  where
    sorryNotRing = "You can't wield a " <> e^.sing <> " with your finger!" <> nlt
    sorry e'     = "You're already wielding a " <> e'^.sing <> " with your " <> pp desigSlot <> "." <> nlt
    desigSlot    = case rol of R -> RHandS
                               L -> LHandS
                               _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-- Ready armor:


-----


unready :: Action
unready [] = advise ["unready"] $ "Please specify one or more things to unready, as in " <> dblQuote "unready sword" <> "."
unready rs = helper >>= output . (<> nlt)
  where
    helper = onWS $ \(t, ws) ->
        let em = (ws^.eqTbl) ! 0
            is = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs) = resolveEntCoinNames ws rs is mempty
                   eiss               = zipWith (curry procGecrMisPCEq) gecrs miss
                   msgs               = if null rcs then "" else "You can't unready coins." <> nlt
                   (ws',  msgs')      = foldl' (helperUnready 0) (ws, msgs) eiss
               in putTMVar t ws' >> return msgs'
          else putTMVar t ws >> return dudeYou'reNaked


helperUnready :: Id -> (WorldState, T.Text) -> Either T.Text Inv -> (WorldState, T.Text)
helperUnready pi (ws, msgs) = \case
  Left  msg -> (ws, msgs <> msg)
  Right is  -> let em  = (ws^.eqTbl)  ! pi
                   pis = (ws^.invTbl) ! pi
                   ws' = ws & eqTbl.at  pi ?~ M.filter (`notElem` is) em
                            & invTbl.at pi ?~ (sortInv ws . (++) pis $ is)
                   msg = mkUnreadyDesc ws' is
               in (ws', msgs <> msg)


mkUnreadyDesc :: WorldState -> Inv -> T.Text
mkUnreadyDesc ws is = T.concat [ helper icb | icb <- mkIdCountBothList ws is ]
  where
    helper (i, c, b@(s, _)) = let v = verb i in if c == 1
      then "You " <> v <> " the " <> s <> "." <> nlt
      else "You " <> v <> " " <> showText c <> " " <> mkPlurFromBoth b <> "." <> nlt
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
what [] = advise ["what"] $ "Please specify one or more abbreviations to disambiguate, as in " <> dblQuote "what up" <> "."
what rs = getWS >>= \ws ->
  let pi  = 0
      p   = (ws^.pcTbl)    ! pi
      ri  = p^.rmId
      r   = (ws^.rmTbl)    ! ri
      helper n = sequence_ [ whatCmd r n
                           , whatInv ws pi PCInv n
                           , whatInv ws pi PCEq  n
                           , whatInv ws pi RmInv n
                           , liftIO newLine ]
  in mapM_ helper rs


whatCmd :: Rm -> T.Text -> MudStack ()
whatCmd r n = maybe notFound found . findFullNameForAbbrev (T.toLower n) $ cs
  where
    cs       = filter isPlaCmd . map cmdName . mkCmdListWithRmLinks $ r
    isPlaCmd = (`notElem` [wizCmdChar, debugCmdChar]) . T.head
    notFound = output $ dblQuote n <> " doesn't refer to any commands." <> nlt
    found cn = outputCon [ dblQuote n, " may refer to the ", dblQuote cn, " command." ]


whatInv :: WorldState -> Id -> InvType -> T.Text -> MudStack ()
whatInv ws pi it n = let (is, gecrs, rcs) = resolveName
                     in if not . null $ gecrs
                       then whatInvEnts ws it n (head gecrs) is
                       else mapM_ (whatInvCoins it n) rcs
  where
    resolveName = let (is, c)         = getLocInvCoins
                      (gecrs, _, rcs) = resolveEntCoinNames ws [n] is c
                  in (is, gecrs, rcs)
    getLocInvCoins = case it of PCInv -> ((ws^.invTbl) ! pi,           (ws^.coinsTbl) ! pi)
                                PCEq  -> (M.elems $ (ws^.eqTbl)  ! pi, mempty)
                                RmInv -> ((ws^.invTbl) ! ri,           (ws^.coinsTbl) ! ri)
    p  = (ws^.pcTbl) ! pi
    ri = p^.rmId


whatInvEnts :: WorldState -> InvType -> T.Text -> GetEntsCoinsRes -> Inv -> MudStack ()
whatInvEnts ws it r gecr is = case gecr of
  Mult _ n (Just es) _ | n == acp  -> outputCon [ dblQuote acp, " may refer to everything ", getLocTxtForInvType it, supplement, "." ]
                       | otherwise -> let e   = head es
                                          len = length es
                                      in if len > 1
                                        then let ebgns  = take len [ getEntBothGramNos e' | e' <- es ]
                                                 h      = head ebgns
                                                 target = if all (== h) ebgns then mkPlurFromBoth h else e^.name.to bracketQuote <> "s"
                                             in outputCon [ dblQuote r, " may refer to the ", showText len, " ", target, " ", getLocTxtForInvType it, "." ]
                                        else let ens = [ let e' = (ws^.entTbl) ! i in e'^.name | i <- is ]
                                             in outputCon [ dblQuote r, " may refer to the ", checkFirst e ens ^.packed, e^.sing, " ", getLocTxtForInvType it, "." ]
  Indexed x _ (Right e) -> outputCon [ dblQuote r, " may refer to the ", mkOrdinal x, " ", e^.name.to bracketQuote, " ", e^.sing.to parensQuote, " ", getLocTxtForInvType it, "." ]
  _                     -> outputCon [ dblQuote r, " doesn't refer to anything ", getLocTxtForInvType it, "." ]
  where
    acp                                   = [allChar]^.packed
    supplement | it `elem` [PCInv, RmInv] = " (including any coins)"
               | otherwise                = ""
    checkFirst e ens                      = let matches = filter (== e^.name) ens
                                            in guard (length matches > 1) >> ("first "^.unpacked)


getLocTxtForInvType :: InvType -> T.Text
getLocTxtForInvType = \case PCInv -> "in your inventory"
                            PCEq  -> "in your readied equipment"
                            RmInv -> "in this room"


whatInvCoins :: InvType -> T.Text -> ReconciledCoins -> MudStack ()
whatInvCoins it r rc
  | it == PCEq = return ()
  | otherwise = case rc of
    Left  Empty      -> outputCon [ dblQuote r, " doesn't refer to any coins ", getLocTxtForInvType it, " ", supplementNone "coins" it, "." ]
    Left  (NoneOf c) -> let cn = mkTxtForCoins c in outputCon [ dblQuote r, " doesn't refer to any ", cn, " ", getLocTxtForInvType it, " ", supplementNone cn it, "." ]
    Left  (SomeOf c) -> let cn = mkTxtForCoins c in outputCon [ dblQuote r, " doesn't refer to any ", cn, " ", getLocTxtForInvType it, " ", supplementNotEnough cn it, "." ]
    Right (SomeOf c) -> outputCon [ dblQuote r, " refers to ", mkTxtForCoinsWithAmt c, " ", getLocTxtForInvType it, "." ]
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
-}


-----


uptime :: Action
uptime (mq, _, cols) [] = (try . send mq . parse =<< runUptime) >>= eitherRet (uptimeExHandler mq cols)
  where
    runUptime = liftIO . readProcess "uptime" [] $ ""
    parse ut  = let (a, b) = span (/= ',') ut
                    a'     = unwords . tail . words $ a
                    b'     = dropWhile isSpace . takeWhile (/= ',') . tail $ b
                    c      = (toUpper . head $ a') : tail a'
                in T.concat [ c^.packed, " ", b'^.packed, ".", nlt, nlt ]
uptime mic@(mq, _, cols) rs = ignore mq cols rs >> uptime mic []


uptimeExHandler :: MsgQueue -> Cols -> IOException -> MudStack ()
uptimeExHandler mq cols e = logIOEx "uptime" e >> (send mq . T.unlines . wordWrap cols $ genericErrorMsg)


-----


{-
quit :: Action
quit [] = output ("Thanks for playing! See you next time." <> nlt <> nlt) >> liftIO exitSuccess
quit _  = outputCon [ "Type ", dblQuote "quit", " with no arguments to quit the game.", nlt ]
-}


-- ==================================================
-- Wizard commands:


wizDispCmdList :: Action
wizDispCmdList = dispCmdList (cmdPred . Just $ wizCmdChar)


-----


wizShutdown :: Action
wizShutdown _             [] = logNotice "wizShutdown" "shutting down" >> liftIO exitSuccess
wizShutdown (mq, _, cols) _  = send mq . T.unlines . (++ [""]) . wordWrap cols $  "Type " <> quoted <> " with no arguments to shut down the game server."
  where
    quoted = dblQuote . prefixWizCmd $ "shutdown"


-----


wizTime :: Action
wizTime (mq, _, cols) [] = do
    ct <- liftIO getCurrentTime
    zt <- liftIO getZonedTime
    send mq . T.unlines . concatMap (wordWrap cols) $ [ "At the tone, the time will be...", formatThat ct, formatThat zt, "" ]
  where
    formatThat t = let wordy = T.words . showText $ t
                       zone  = last wordy
                       date  = head wordy
                       time  = T.init . T.reverse . T.dropWhile (/= '.') . T.reverse . head . tail $ wordy
                   in T.concat [ zone, ": ", date, " ", time ]
wizTime mic@(mq, _, cols) rs = ignore mq cols rs >> wizTime mic []


-----


wizDay :: Action
wizDay (mq, _, _) [] = liftIO getZonedTime >>= \zt ->
    send mq $ formatTime defaultTimeLocale "%A %B %d" zt ^.packed <> nlt <> nlt
wizDay mic@(mq, _, cols) rs = ignore mq cols rs >> wizDay mic []


-- ==================================================
-- Debug commands:


debugDispCmdList :: Action
debugDispCmdList = dispCmdList (cmdPred . Just $ debugCmdChar)


-----


debugBuffCheck :: Action
debugBuffCheck (mq, _, cols) [] = try helper >>= eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = do
        td      <- liftIO getTemporaryDirectory
        (fn, h) <- liftIO . openTempFile td $ "temp"
        bm      <- liftIO . hGetBuffering $ h
        liftIO $ hClose h >> removeFile fn
        send mq . T.unlines . (++ [""]) . wordWrapIndent 2 cols . T.concat $ [ "(Default) buffering mode for temp file ", fn^.packed.to dblQuote, " is ", dblQuote . showText $ bm, "." ]
debugBuffCheck mic@(mq, _, cols) rs = ignore mq cols rs >> debugBuffCheck mic []


-----


debugDispEnv :: Action
debugDispEnv (mq, _, cols) [] = send mq . (<> nlt) =<< (mkAssocListTxt cols <$> liftIO getEnvironment)
debugDispEnv (mq, _, cols) rs = liftIO getEnvironment >>= \env ->
    send mq . T.unlines . map (helper env) $ rs
  where
    helper env r = mkAssocListTxt cols . filter grepPair $ env
      where
        grepPair = uncurry (||) . over both (^.packed.to grep)
        grep     = (r `T.isInfixOf`)


-----


debugLog :: Action
debugLog (mq, _, _) [] = helper >> (send mq $ "OK!" <> nlt <> nlt)
  where
    helper       = replicateM_ 100 . liftIO . forkIO . void . runStateInIORefT heavyLogging =<< get
    heavyLogging = liftIO myThreadId >>= \i ->
        replicateM_ 100 . logNotice "debugLog" $ "Logging from " ++ show i
debugLog mic@(mq, _, cols) rs = ignore mq cols rs >> debugLog mic []


------


-- TODO: Also write a command that throws an exception from a child thread. Perhaps use "error".
debugThrow :: Action
debugThrow _                 [] = liftIO . throwIO $ DivideByZero
debugThrow mic@(mq, _, cols) rs = ignore mq cols rs >> debugThrow mic []


-----


-- TODO: Show the status of other threads, too.
debugSniff :: Action
debugSniff (mq, _, cols) [] = gets (^.nonWorldState.logServices) >>= \ls ->
    let Just (nla, _) = ls^.noticeLog
        Just (ela, _) = ls^.errorLog
        nli           = asyncThreadId nla
        eli           = asyncThreadId ela
    in do
        nls <- liftIO . threadStatus $ nli
        els <- liftIO . threadStatus $ eli
        let msg = T.unlines . concatMap (wordWrap cols) $ [ "Notice log thread status: " <> showText nls, "Error  log thread status: " <> showText els ]
        send mq $ divider <> msg <> divider <> nlt
  where
    divider = mkDividerTxt cols <> nlt
debugSniff mic@(mq, _, cols) rs = ignore mq cols rs >> debugSniff mic []
