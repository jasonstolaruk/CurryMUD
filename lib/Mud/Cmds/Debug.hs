{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, ParallelListComp, PatternSynonyms, TupleSections, ViewPatterns #-}

module Mud.Cmds.Debug ( debugCmds
                      , purgeThreadTbls ) where

import Mud.Cmds.Util
import Mud.Color
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logAndDispIOEx, logNotice, logPlaExec, logPlaExecArgs)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((***))
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (asyncThreadId, poll)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (ArithException(..), IOException)
import Control.Exception.Lifted (throwIO, try)
import Control.Lens (both, over)
import Control.Monad (replicateM, replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Char (ord)
import Data.List (foldl', nub, sort)
import Data.Monoid ((<>))
import Formatting ((%), sformat)
import Formatting.Formatters (stext)
import GHC.Conc (ThreadStatus(..), threadStatus)
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (hClose, hGetBuffering, openTempFile)
import qualified Data.IntMap.Lazy as IM (assocs, delete, elems, keys)
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
    , Cmd { cmdName = prefixDebugCmd "throw", action = debugThrow, cmdDesc = "Throw an exception." } ]


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
    mkANSICodeList = padOrTrunc 28 . T.concatMap ((<> " ") . showText . ord)
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
    helper  = T.unlines . wordWrapIndent 2 cols . uncurry builder . (unquote . showText *** showText)
    builder = sformat $ stext % ": " % stext


-----


debugLog :: Action
debugLog (NoArgs' i mq) = logPlaExec (prefixDebugCmd "log") i >> helper >> ok mq
  where
    helper       = replicateM 100 . statefulFork $ heavyLogging
    heavyLogging = liftIO myThreadId >>=
        replicateM_ 100 . logNotice "debugLog" . (<> ".") . ("Logging from " <>) . showText
debugLog p = withoutArgs debugLog p


------


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
debugRotate (NoArgs' i mq) = logPlaExec (prefixDebugCmd "rotate") i >> ok mq >> return () -- TODO: ...rotatePlaLog i
{-
doIfLogging :: Id -> (LogQueue -> MudStack ()) -> MudStack ()
doIfLogging i f = (IM.lookup i <$> readTMVarInNWS plaLogTblTMVar) >>= \case
  Nothing     -> return ()
  Just (_, q) -> f q

liftIO . atomically . flip writeTQueue RotateLog
-}
debugRotate p              = withoutArgs debugRotate p


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


-----


debugThrow :: Action
debugThrow (NoArgs'' i) = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow p            = withoutArgs debugThrow p
