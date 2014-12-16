{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, ParallelListComp, ViewPatterns #-}

module Mud.Cmds.DebugCmds (debugCmds) where

import Mud.Cmds.CmdUtil
import Mud.Color
import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logAndDispIOEx, logNotice, logPlaExec, logPlaExecArgs)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
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
import Data.List (foldl', nub)
import Data.Monoid ((<>))
import GHC.Conc (ThreadStatus(..), threadStatus)
import Prelude hiding (pi)
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (hClose, hGetBuffering, openTempFile)
import qualified Data.IntMap.Lazy as IM (assocs, delete, elems, keys)
import qualified Data.Map.Lazy as M (assocs, delete, elems, keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.DebugCmds"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.DebugCmds"


logAndDispIOEx :: MsgQueue -> Cols -> T.Text -> IOException -> MudStack ()
logAndDispIOEx mq cols = L.logAndDispIOEx mq cols "Mud.Cmds.DebugCmds"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.DebugCmds"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.DebugCmds"


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
    , Cmd { cmdName = prefixDebugCmd "purge", action = debugPurge, cmdDesc = "Purge the thread tables." }
    , Cmd { cmdName = prefixDebugCmd "remput", action = debugRemPut, cmdDesc = "In quick succession, remove from and \
                                                                               \put into a sack on the ground." }
    , Cmd { cmdName = prefixDebugCmd "params", action = debugParams, cmdDesc = "Show \"ActionParams\"." }
    , Cmd { cmdName = prefixDebugCmd "stop", action = debugStop, cmdDesc = "Stop all server threads." }
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
    send mq . T.unlines $ [ helper a env | a <- as ]
  where
    helper a = mkAssocListTxt cols . filter grepPair
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
debugBoot (NoArgs' i mq) = logPlaExec (prefixDebugCmd "boot") i >> ok mq >> (massMsg . MsgBoot $ dfltBootMsg)
debugBoot p              = withoutArgs debugBoot p


-- TODO: When you've made a wiz cmd to boot a player, move this next to the function for that cmd.
dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


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
    mkANSICodeList = padOrTrunc 28 . T.concatMap ((<> " ") . showText . ord)
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName (padOrTrunc 6 . showText -> intensity, padOrTrunc 8 . showText -> color) = intensity <> color
debugColor p = withoutArgs debugColor p
