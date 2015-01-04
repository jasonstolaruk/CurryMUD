{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, ParallelListComp, PatternSynonyms, TupleSections, ViewPatterns #-}

module Mud.Cmds.Debug ( debugCmds
                      , purgeThreadTbls ) where

import Mud.ANSI
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.STM
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Msgs
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logAndDispIOEx, logNotice, logPlaExec, logPlaExecArgs)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((***))
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (asyncThreadId, poll)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (ArithException(..), IOException)
import Control.Exception.Lifted (throwIO, try)
import Control.Lens (both, over)
import Control.Lens.Getter (view)
import Control.Monad (replicateM, replicateM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.IntMap.Lazy ((!))
import Data.List (delete, foldl', nub, sort)
import Data.Maybe (fromJust, isNothing)
import Data.Monoid ((<>))
import GHC.Conc (ThreadStatus(..), threadStatus)
import System.CPUTime (getCPUTime)
import System.Console.ANSI (Color(..), ColorIntensity(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (hClose, hGetBuffering, openTempFile)
import qualified Data.IntMap.Lazy as IM (assocs, delete, elems, keys, lookup)
import qualified Data.Map.Lazy as M (assocs, delete, elems, keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Debug"


-----


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols = L.logAndDispIOEx mq cols "Mud.Cmds.Debug"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Debug"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Debug"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Debug"


-- ==================================================


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
    , Cmd { cmdName = prefixDebugCmd "params", action = debugParams, cmdDesc = "Show \"ActionParams\"." }
    , Cmd { cmdName = prefixDebugCmd "purge", action = debugPurge, cmdDesc = "Purge the thread tables." }
    , Cmd { cmdName = prefixDebugCmd "remput", action = debugRemPut, cmdDesc = "In quick succession, remove from and \
                                                                               \put into a sack on the ground." }
    , Cmd { cmdName = prefixDebugCmd "rotate", action = debugRotate, cmdDesc = "Send the signal to rotate your player \
                                                                               \log." }
    , Cmd { cmdName = prefixDebugCmd "talk", action = debugTalk, cmdDesc = "Dump the talk async table." }
    , Cmd { cmdName = prefixDebugCmd "thread", action = debugThread, cmdDesc = "Dump the thread table." }
    , Cmd { cmdName = prefixDebugCmd "throw", action = debugThrow, cmdDesc = "Throw an exception." }
    , Cmd { cmdName = prefixDebugCmd "throwlog", action = debugThrowLog, cmdDesc = "Throw an exception on your player \
                                                                                   \log thread." }
    , Cmd { cmdName = prefixDebugCmd "underline", action = debugUnderline, cmdDesc = "Perform an underline test." }
    , Cmd { cmdName = prefixDebugCmd "wrap", action = debugWrap, cmdDesc = "Test the wrapping of a line containing \
                                                                           \ANSI escape sequences." }
    , Cmd { cmdName = prefixDebugCmd "wrapindent", action = debugWrapIndent, cmdDesc = "Test the indented wrapping of \
                                                                                       \a line containing ANSI escape \
                                                                                       \sequences." } ]


prefixDebugCmd :: CmdName -> T.Text
prefixDebugCmd = prefixCmd debugCmdChar


-----


debugDispCmdList :: Action
debugDispCmdList p@(LowerNub' i as) = logPlaExecArgs (prefixDebugCmd "?") as i >> dispCmdList debugCmds p
debugDispCmdList p = patternMatchFail "debugDispCmdList" [ showText p ]


-----


debugBoot :: Action
debugBoot (NoArgs' i mq) = logPlaExec (prefixDebugCmd "boot") i >> ok mq >> (massMsg . MsgBoot $ dfltBootMsg)
debugBoot p              = withoutArgs debugBoot p


-----


debugBroad :: Action
debugBroad (NoArgs'' i) = do
    logPlaExec (prefixDebugCmd "broad") i
    bcastNl . mkBroadcast i $ msg
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


debugBuffCheck :: Action
debugBuffCheck (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "buffer") i
    try helper >>= eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = do
        (fn@(dblQuote . T.pack -> fn'), h) <- liftIO $ flip openTempFile "temp" =<< getTemporaryDirectory
        (dblQuote . showText -> mode)      <- liftIO . hGetBuffering $ h
        send mq . nl . T.unlines . wrapIndent 2 cols . T.concat $ [ parensQuote "Default"
                                                                  , " buffering mode for temp file "
                                                                  , fn'
                                                                  , " is "
                                                                  , mode
                                                                  , "." ]
        liftIO $ hClose h >> removeFile fn
debugBuffCheck p = withoutArgs debugBuffCheck p


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
        return . nl . T.concat $ [ padOrTrunc 15 . showText $ ansi
                                 , mkColorDesc fg bg
                                 , ansi
                                 , " CurryMUD "
                                 , dfltColorANSI ]
  where
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName = uncurry (<>) . (padOrTrunc 6 . showText *** padOrTrunc 8 . showText)
debugColor p = withoutArgs debugColor p


-----


debugCPU :: Action
debugCPU (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "cpu") i
    wrapSend mq cols . ("CPU time: " <>) =<< liftIO cpuTime
  where
    cpuTime = showText . (/ fromIntegral (10 ^ 12)) . fromIntegral <$> getCPUTime
debugCPU p = withoutArgs debugCPU p


-----


debugDispEnv :: Action
debugDispEnv (NoArgs i mq cols) = do
    logPlaExecArgs (prefixDebugCmd "env") [] i
    send mq . nl =<< (mkAssocListTxt cols <$> liftIO getEnvironment)
debugDispEnv (WithArgs i mq cols (nub -> as)) = do
    logPlaExecArgs (prefixDebugCmd "env") as i
    env <- liftIO getEnvironment
    send mq . T.unlines $ [ helper a env | a <- as ]
  where
    helper a = mkAssocListTxt cols . filter grepPair
      where
        grepPair = uncurry (||) . over both ((a `T.isInfixOf`) . T.pack)
debugDispEnv p = patternMatchFail "debugDispEnv" [ showText p ]


mkAssocListTxt :: (Show a, Show b) => Cols -> [(a, b)] -> T.Text
mkAssocListTxt cols = T.concat . map helper
  where
    helper            = T.unlines . wrapIndent 2 cols . mkAssocTxt . (unquote . showText *** showText)
    mkAssocTxt (a, b) = T.concat [ cyan, a, ": ", dfltColorANSI, b ]


-----


debugLog :: Action
debugLog (NoArgs' i mq) = logPlaExec (prefixDebugCmd "log") i >> helper >> ok mq
  where
    helper       = replicateM 100 . statefulFork_ $ heavyLogging
    heavyLogging = liftIO myThreadId >>=
        replicateM_ 100 . logNotice "debugLog" . (<> ".") . ("Logging from " <>) . showText
debugLog p = withoutArgs debugLog p


-----


debugParams :: Action
debugParams p@(WithArgs i mq cols _) = do
    logPlaExec (prefixDebugCmd "params") i
    wrapSend mq cols . showText $ p
debugParams p = patternMatchFail "debugParams" [ showText p ]


-----


debugPurge :: Action
debugPurge (NoArgs' i mq) = logPlaExec (prefixDebugCmd "purge") i >> purgeThreadTbls >> ok mq
debugPurge p              = withoutArgs debugPurge p


purgeThreadTbls :: MudStack ()
purgeThreadTbls =
    logNotice "purgeThreadTbls" "purging the thread tables." >> purgePlaLogTbl >> purgeTalkAsyncTbl >> purgeThreadTbl


purgePlaLogTbl :: MudStack ()
purgePlaLogTbl = IM.assocs <$> readTMVarInNWS plaLogTblTMVar >>= \kvs -> do
    let is     = [ fst kv         | kv <- kvs ]
    let asyncs = [ fst . snd $ kv | kv <- kvs ]
    modifyNWS plaLogTblTMVar . flip (foldl' helper) . zip is =<< (liftIO . mapM poll $ asyncs)
  where
    helper m (_, Nothing) = m
    helper m (i, _      ) = IM.delete i m


purgeTalkAsyncTbl :: MudStack ()
purgeTalkAsyncTbl = do
    asyncs <- M.elems <$> readTMVarInNWS talkAsyncTblTMVar
    ss     <- liftIO . mapM poll $ asyncs
    modifyNWS talkAsyncTblTMVar . flip (foldl' helper) . zip asyncs $ ss
  where
    helper m (_, Nothing) = m
    helper m (a, _      ) = M.delete (asyncThreadId a) m


purgeThreadTbl :: MudStack ()
purgeThreadTbl = do
    tis <- M.keys <$> readTMVarInNWS threadTblTMVar
    ss  <- liftIO . mapM threadStatus $ tis
    modifyNWS threadTblTMVar . flip (foldl' helper) . zip tis $ ss
  where
    helper m (ti, s) | s == ThreadFinished = M.delete ti m
                     | otherwise           = m


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


debugRotate :: Action
debugRotate (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "rotate") i
    q <- snd . fromJust . IM.lookup i <$> readTMVarInNWS plaLogTblTMVar
    liftIO . atomically . writeTQueue q $ RotateLog
    ok mq
debugRotate p = withoutArgs debugRotate p


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


debugThread :: Action
debugThread (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "thread") i
    (uncurry (:) . ((, Notice) *** pure . (, Error)) -> logAsyncKvs) <- over both asyncThreadId <$> getLogAsyncs
    threadTblKvs <- M.assocs <$> readTMVarInNWS threadTblTMVar
    (es, ks)     <- let f = (,) <$> IM.elems <*> IM.keys in f `fmap` readTMVarInNWS plaLogTblTMVar
    let plaLogTblKvs = [ (asyncThreadId . fst $ e, PlaLog k) | e <- es | k <- ks ]
    ds <- mapM mkDesc . sort $ logAsyncKvs ++ threadTblKvs ++ plaLogTblKvs
    send mq . frame cols . multiWrap cols $ ds
  where
    mkDesc (ti, bracketPad 18 . mkTypeName -> tn) = (liftIO . threadStatus $ ti) >>= \(showText -> ts) ->
        return . T.concat $ [ padOrTrunc 16 . showText $ ti, tn, ts ]
    mkTypeName (PlaLog  (showText -> i')) = padOrTrunc 10 "PlaLog"  <> i'
    mkTypeName (Receive (showText -> i')) = padOrTrunc 10 "Receive" <> i'
    mkTypeName (Server  (showText -> i')) = padOrTrunc 10 "Server"  <> i'
    mkTypeName (Talk    (showText -> i')) = padOrTrunc 10 "Talk"    <> i'
    mkTypeName (showText -> tt)           = tt
debugThread p = withoutArgs debugThread p


getLogAsyncs :: MudStack (LogAsync, LogAsync)
getLogAsyncs = helper <$> gets (view nonWorldState)
  where
    helper     = (getAsync noticeLog *** getAsync errorLog) . dup
    getAsync l = fst . fromJust . view l


-----


debugThrow :: Action
debugThrow (NoArgs'' i) = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow p            = withoutArgs debugThrow p


-----


debugThrowLog :: Action
debugThrowLog (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "throwlog") i
    (snd . (! i) -> q) <- readTMVarInNWS plaLogTblTMVar
    liftIO . atomically . writeTQueue q $ Throw
    ok mq
debugThrowLog p = withoutArgs debugThrowLog p


-----


debugUnderline :: Action
debugUnderline (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "underline") i
    wrapSend mq cols underlined
  where
    underlined = T.concat [ showText underlineANSI
                          , underlineANSI
                          , " This text is underlined. "
                          , noUnderlineANSI
                          , showText noUnderlineANSI ]
debugUnderline p = withoutArgs debugUnderline p


-----


debugWrap :: Action
debugWrap p@AdviseNoArgs             = advise p [ prefixDebugCmd "wrap" ] advice
  where
    advice = "Please specify line length, as in " <> dblQuote (prefixDebugCmd "wrap" <> " 40") <> "."
debugWrap   (WithArgs i mq cols [a]) = case (reads . T.unpack $ a :: [(Int, String)]) of
  []            -> sorryParse
  [(cols', "")] -> helper cols'
  _             -> sorryParse
  where
    sorryParse = wrapSend mq cols $ dblQuote a <> " is not a valid line length."
    helper cols'
      | cols' < 0                          = sorryWtf
      | cols' < minCols || cols' > maxCols = sorryLineLen
      | otherwise                          = do
          logPlaExecArgs (prefixDebugCmd "wrap") [a] i
          send mq . frame cols' . wrapUnlines cols' $ msg
    sorryWtf     = wrapSend mq cols $ magenta                                                             <>
                                      "What the fuck is wrong with you? Are you trying to make me crash?" <>
                                      dfltColorANSI
    sorryLineLen = wrapSend mq cols . T.concat $ [ "The line length must be between "
                                                 , showText minCols
                                                 , " and "
                                                 , showText maxCols
                                                 , " characters." ]
    msg        =
        let ls = [ T.concat [ u
                            , mkFgColorANSI (Dull, c)
                            , "This is "
                            , showText c
                            , " text." ] | c <- Black `delete` colors, u <- [ underlineANSI, noUnderlineANSI ] ]
        in (<> dfltColorANSI) . T.intercalate " " $ ls
debugWrap p = advise p [ prefixDebugCmd "wrap" ] advice
  where
    advice = "Please provide one argument: line length, as in " <> dblQuote (prefixDebugCmd "wrap" <> " 40") <> "."


-----


debugWrapIndent :: Action
debugWrapIndent p@AdviseNoArgs                = advise p [ prefixDebugCmd "wrapindent" ] advice
  where
    advice = "Please specify line length followed by indent amount, as in " <>
             dblQuote (prefixDebugCmd "wrapindent" <> " 40 4") <> "."
debugWrapIndent p@(AdviseOneArg _)            = advise p [ prefixDebugCmd "wrapindent" ] advice
  where
    advice = "Please also specify indent amount, as in " <>
             dblQuote (prefixDebugCmd "wrapindent" <> " 40 4") <> "."
debugWrapIndent   (WithArgs i mq cols [a, b]) = do
    parsed <- (,) <$> parse a sorryParseLineLen <*> parse b sorryParseIndent
    unless (uncurry (||) . (over both isNothing) $ parsed) . uncurry helper . (over both fromJust) $ parsed
  where
    parse txt sorry   = case (reads . T.unpack $ txt :: [(Int, String)]) of
      []        -> sorry >> return Nothing
      [(x, "")] -> return . Just $ x
      _         -> sorry >> return Nothing
    sorryParseLineLen = wrapSend mq cols $ dblQuote a <> " is not a valid line length."
    sorryParseIndent  = wrapSend mq cols $ dblQuote b <> " is not a valid width amount."
    helper cols' indent
      | cols' < 0 || indent < 0            = sorryWtf
      | cols' < minCols || cols' > maxCols = sorryLineLen
      | indent >= cols'                    = sorryIndent
      | otherwise                          = do
          logPlaExecArgs (prefixDebugCmd "wrapindent") [a, b] i
          send mq . frame cols' . T.unlines . wrapIndent indent cols' $ msg
    sorryWtf     = wrapSend mq cols $ magenta                                                             <>
                                      "What the fuck is wrong with you? Are you trying to make me crash?" <>
                                      dfltColorANSI
    sorryLineLen = wrapSend mq cols . T.concat $ [ "The line length must be between "
                                                 , showText minCols
                                                 , " and "
                                                 , showText maxCols
                                                 , " characters." ]
    sorryIndent  = wrapSend mq cols "The indent amount must be less than the line length."
    msg          =
        let ls = [ T.concat [ u
                            , mkFgColorANSI (Dull, c)
                            , "This is "
                            , showText c
                            , " text." ] | c <- Black `delete` colors, u <- [ underlineANSI, noUnderlineANSI ] ]
        in (<> dfltColorANSI) . T.intercalate " " $ ls

debugWrapIndent p = advise p [ prefixDebugCmd "wrapindent" ] advice
  where
    advice = "Please provide two arguments: line length and indent amount, as in " <>
             dblQuote (prefixDebugCmd "wrapindent" <> " 40 4") <> "."
