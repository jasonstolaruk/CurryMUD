{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, TupleSections, ViewPatterns #-}

module Mud.Cmds.Debug ( debugCmds
                      , purgeThreadTbls ) where

import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Msgs
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Token
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logAndDispIOEx, logNotice, logPlaExec, logPlaExecArgs)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((***))
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Async (asyncThreadId, poll)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (ArithException(..), IOException)
import Control.Exception.Lifted (throwIO, try)
import Control.Lens (both, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad ((>=>), replicateM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.List (delete, sort)
import Data.Maybe (fromJust, isNothing)
import Data.Monoid ((<>))
import GHC.Conc (ThreadStatus(..), threadStatus)
import Prelude hiding (pi)
import System.CPUTime (getCPUTime)
import System.Console.ANSI (Color(..), ColorIntensity(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (hClose, hGetBuffering, openTempFile)
import qualified Data.IntMap.Lazy as IM (assocs, delete)
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
    [ mkDebugCmd "?"          debugDispCmdList "Display or search this command list."
    , mkDebugCmd "boot"       debugBoot        "Boot all players (including yourself)."
    , mkDebugCmd "broad"      debugBroad       "Broadcast (to yourself) a multi-line message."
    , mkDebugCmd "buffer"     debugBuffCheck   "Confirm the default buffering mode for file handles."
    , mkDebugCmd "color"      debugColor       "Perform a color test."
    , mkDebugCmd "cpu"        debugCPU         "Display the CPU time."
    , mkDebugCmd "env"        debugDispEnv     "Display or search system environment variables."
    , mkDebugCmd "log"        debugLog         "Put the logging service under heavy load."
    , mkDebugCmd "params"     debugParams      "Show \"ActionParams\"."
    , mkDebugCmd "purge"      debugPurge       "Purge the thread tables."
    , mkDebugCmd "remput"     debugRemPut      "In quick succession, remove from and put into a sack on the ground."
    , mkDebugCmd "rotate"     debugRotate      "Send the signal to rotate your player log."
    , mkDebugCmd "talk"       debugTalk        "Dump the talk async table."
    , mkDebugCmd "thread"     debugThread      "Dump the thread table."
    , mkDebugCmd "throw"      debugThrow       "Throw an exception."
    , mkDebugCmd "throwlog"   debugThrowLog    "Throw an exception on your player log thread."
    , mkDebugCmd "token"      debugToken       "Test token parsing."
    , mkDebugCmd "underline"  debugUnderline   "Test underlining."
    , mkDebugCmd "wrap"       debugWrap        "Test the wrapping of a line containing ANSI escape sequences."
    , mkDebugCmd "wrapindent" debugWrapIndent  "Test the indented wrapping of a line containing ANSI escape \
                                               \sequences." ]


mkDebugCmd :: T.Text -> Action -> CmdDesc -> Cmd
mkDebugCmd (prefixDebugCmd -> cn) act cd = Cmd { cmdName           = cn
                                               , cmdPriorityAbbrev = Nothing
                                               , cmdFullName       = cn
                                               , action            = act
                                               , cmdDesc           = cd }


prefixDebugCmd :: T.Text -> CmdName
prefixDebugCmd = prefixCmd debugCmdChar


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
    helper |$| try >=> eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = liftIO (flip openTempFile "temp" =<< getTemporaryDirectory) >>= \(fn, h) -> do
        send mq . nl =<< [ T.unlines . wrapIndent 2 cols $ msg | (mkMsg fn -> msg) <- liftIO . hGetBuffering $ h ]
        liftIO $ hClose h >> removeFile fn
    mkMsg (dblQuote . T.pack -> fn) (dblQuote . showText -> mode) =
        T.concat [ parensQuote "Default", " buffering mode for temp file ", fn, " is ", mode, "." ]
debugBuffCheck p = withoutArgs debugBuffCheck p


-----


debugColor :: Action
debugColor (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "color") i
    send mq . nl . T.concat $ msg
  where
    msg = [ nl . T.concat $ [ padOrTrunc 15 . showText $ ansi, mkColorDesc fg bg, ansi, " CurryMUD ", dfltColor ]
          | fgi <- intensities, fgc <- colors, bgi <- intensities, bgc <- colors
          , let fg = (fgi, fgc), let bg = (bgi, bgc), let ansi = mkColorANSI fg bg ]
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName = uncurry (<>) . (padOrTrunc 6 . showText *** padOrTrunc 8 . showText)
debugColor p = withoutArgs debugColor p


-----


debugCPU :: Action
debugCPU (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "cpu") i
    wrapSend mq cols =<< [ "CPU time: " <> time | time <- liftIO cpuTime ]
  where
    cpuTime = showText . (/ fromIntegral (10 ^ 12)) . fromIntegral <$> getCPUTime
debugCPU p = withoutArgs debugCPU p


-----


debugDispCmdList :: Action
debugDispCmdList p@(LowerNub' i as) = logPlaExecArgs (prefixDebugCmd "?") as i >> dispCmdList debugCmds p
debugDispCmdList p                  = patternMatchFail "debugDispCmdList" [ showText p ]


-----


debugDispEnv :: Action
debugDispEnv (NoArgs i mq cols)  = do
    logPlaExecArgs (prefixDebugCmd "env") [] i
    pager i mq =<< [ concatMap (wrapIndent 2 cols) . mkEnvListTxt $ env | env <- liftIO getEnvironment ]
debugDispEnv p@(ActionParams { plaId, args }) = do
    logPlaExecArgs (prefixDebugCmd "env") args plaId
    dispMatches p 2 =<< [ mkEnvListTxt env | env <- liftIO getEnvironment ]


mkEnvListTxt :: [(String, String)] -> [T.Text]
mkEnvListTxt = map (mkAssocTxt . over both T.pack)
  where
    mkAssocTxt (a, b) = T.concat [ envVarColor, a, ": ", dfltColor, b ]


-----


debugLog :: Action
debugLog (NoArgs' i mq) = logPlaExec (prefixDebugCmd "log") i >> helper >> ok mq
  where
    helper       = replicateM_ 100 $ liftIO . void . forkIO . runReaderT heavyLogging =<< ask
    heavyLogging = replicateM_ 100 . logNotice "debugLog heavyLogging" =<< mkMsg
    mkMsg        = [ "Logging from " <> ti <> "." | (showText -> ti) <- liftIO myThreadId ]
debugLog p = withoutArgs debugLog p


-----


debugParams :: Action
debugParams p@(WithArgs i mq cols _) = logPlaExec (prefixDebugCmd "params") i >> (wrapSend mq cols . showText $ p)
debugParams p = patternMatchFail "debugParams" [ showText p ]


-----


debugPurge :: Action
debugPurge (NoArgs' i mq) = logPlaExec (prefixDebugCmd "purge") i >> purgeThreadTbls >> ok mq
debugPurge p              = withoutArgs debugPurge p


purgeThreadTbls :: MudStack ()
purgeThreadTbls = do
    logNotice "purgeThreadTbls" "purging the thread tables."
    sequence_ [ purgePlaLogTbl, purgeTalkAsyncTbl, purgeThreadTbl ]


purgePlaLogTbl :: MudStack ()
purgePlaLogTbl = getState >>= \(views plaLogTbl (unzip . IM.assocs) -> (is, map fst -> asyncs)) -> do
    zipped <- [ zip is statuses | statuses <- liftIO . mapM poll $ asyncs ]
    modifyState $ \ms -> let plt = foldr purger (ms^.plaLogTbl) zipped in (ms & plaLogTbl .~ plt, ())
  where
    purger (_, Nothing) tbl = tbl
    purger (i, _      ) tbl = IM.delete i tbl


purgeTalkAsyncTbl :: MudStack ()
purgeTalkAsyncTbl = getState >>= \(views talkAsyncTbl M.elems -> asyncs) -> do
    zipped <- [ zip asyncs statuses | statuses <- liftIO . mapM poll $ asyncs ]
    modifyState $ \ms -> let tat = foldr purger (ms^.talkAsyncTbl) zipped in (ms & talkAsyncTbl .~ tat, ())
  where
    purger (_,                   Nothing) tbl = tbl
    purger (asyncThreadId -> ti, _      ) tbl = M.delete ti tbl


purgeThreadTbl :: MudStack ()
purgeThreadTbl = getState >>= \(views threadTbl M.keys -> threadIds) -> do
    zipped <- [ zip threadIds statuses | statuses <- liftIO . mapM threadStatus $ threadIds ]
    modifyState $ \ms -> let tt = foldr purger (ms^.threadTbl) zipped in (ms & threadTbl .~ tt, ())
  where
    purger (ti, status) tbl = status == ThreadFinished ? M.delete ti tbl :? tbl


-----


debugRemPut :: Action
debugRemPut (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "remput") i
    mapM_ (fakeClientInput mq) . take 10 . cycle . map (<> rest) $ [ "remove", "put" ]
  where
    rest = T.concat [ " ", T.singleton allChar, " ", T.singleton rmChar, "sack" ]
debugRemPut p = withoutArgs debugRemPut p


fakeClientInput :: MsgQueue -> T.Text -> MudStack ()
fakeClientInput mq = liftIO . atomically . writeTQueue mq . FromClient . nl


-----


debugRotate :: Action
debugRotate (NoArgs' i mq) = getState >>= \ms -> let lq = getLogQueue i ms in do
    logPlaExec (prefixDebugCmd "rotate") i
    liftIO . atomically . writeTQueue lq $ RotateLog
    ok mq
debugRotate p = withoutArgs debugRotate p


-----


debugTalk :: Action
debugTalk (NoArgs i mq cols) = getState >>= \(views talkAsyncTbl M.elems -> asyncs) -> do
    logPlaExec (prefixDebugCmd "talk") i
    send mq =<< [ frame cols . multiWrap cols $ descs | descs <- mapM mkDesc asyncs ]
  where
    mkDesc a    = [ T.concat [ "Talk async ", showText . asyncThreadId $ a, ": ", statusTxt, "." ]
                  | statusTxt <- mkStatusTxt <$> (liftIO . poll $ a) ]
    mkStatusTxt = \case Nothing                                    -> "running"
                        Just (Left  (parensQuote . showText -> e)) -> "exception " <> e
                        Just (Right ()                           ) -> "finished"
debugTalk p = withoutArgs debugTalk p


-----


debugThread :: Action
debugThread (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "thread") i
    (uncurry (:) . ((, Notice) *** pure . (, Error)) -> logAsyncKvs) <- over both asyncThreadId . getLogAsyncs <$> ask -- TODO: Does reader have a more idiomatic way rather than just fmapping onto ask?
    (plt, M.assocs -> threadTblKvs) <- (view plaLogTbl *** view threadTbl) . dup <$> getState
    let plaLogTblKvs = [ (asyncThreadId . fst $ v, PlaLog k) | (k, v) <- IM.assocs plt ]
    send mq . frame cols . multiWrap cols =<< (mapM mkDesc . sort $ logAsyncKvs ++ threadTblKvs ++ plaLogTblKvs)
  where
    mkDesc (ti, bracketPad 18 . mkTypeName -> tn) = [ T.concat [ padOrTrunc 16 . showText $ ti, tn, ts ]
                                                    | (showText -> ts) <- liftIO . threadStatus $ ti ]
    mkTypeName (PlaLog  (showText -> pi)) = padOrTrunc 10 "PlaLog"  <> pi
    mkTypeName (Receive (showText -> pi)) = padOrTrunc 10 "Receive" <> pi
    mkTypeName (Server  (showText -> pi)) = padOrTrunc 10 "Server"  <> pi
    mkTypeName (Talk    (showText -> pi)) = padOrTrunc 10 "Talk"    <> pi
    mkTypeName (showText -> tt)             = tt
debugThread p = withoutArgs debugThread p


getLogAsyncs :: MudData -> (LogAsync, LogAsync)
getLogAsyncs = (getAsync noticeLog *** getAsync errorLog) . dup
  where
    getAsync l = views l fst


-----


debugThrow :: Action
debugThrow (NoArgs'' i) = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow p            = withoutArgs debugThrow p


-----


debugThrowLog :: Action
debugThrowLog (NoArgs' i mq) = getState >>= \ms -> let lq = getLogQueue i ms in
    logPlaExec (prefixDebugCmd "throwlog") i >> (liftIO . atomically . writeTQueue lq $ Throw) >> ok mq
debugThrowLog p = withoutArgs debugThrowLog p


-----


debugToken :: Action
debugToken (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "token") i
    multiWrapSend mq cols . T.lines . parseTokens . T.unlines $ tokenTxts
  where
    tokenTxts = [ charTokenDelimiter  `T.cons` "a allChar"
                , charTokenDelimiter  `T.cons` "c adverbCloseChar"
                , charTokenDelimiter  `T.cons` "d adminCmdChar"
                , charTokenDelimiter  `T.cons` "e emoteNameChar"
                , charTokenDelimiter  `T.cons` "i indexChar"
                , charTokenDelimiter  `T.cons` "m amountChar"
                , charTokenDelimiter  `T.cons` "o adverbOpenChar"
                , charTokenDelimiter  `T.cons` "r rmChar"
                , charTokenDelimiter  `T.cons` "s slotChar"
                , charTokenDelimiter  `T.cons` "t sayToChar"
                , styleTokenDelimiter `T.cons` ("aabbrevColor"       <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("ddfltColor"         <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("hheaderColor"       <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("nnoUnderlineANSI"   <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("qquoteColor"        <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("rarrowColor"        <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("ssyntaxSymbolColor" <> dfltColorStyleToken  )
                , styleTokenDelimiter `T.cons` ("uunderlineANSI"     <> noUnderlineStyleToken)
                , styleTokenDelimiter `T.cons` ("zzingColor"         <> dfltColorStyleToken  )
                , "dfltBootMsg: "     <> (msgTokenDelimiter `T.cons` "b")
                , "dfltShutdownMsg: " <> (msgTokenDelimiter `T.cons` "s") ]
    dfltColorStyleToken   = styleTokenDelimiter `T.cons` "d"
    noUnderlineStyleToken = styleTokenDelimiter `T.cons` "n"
debugToken p = withoutArgs debugToken p


-----


debugUnderline :: Action
debugUnderline (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "underline") i
    wrapSend mq cols . T.concat $ [ showText underlineANSI
                                  , underlineANSI
                                  , " This text is underlined. "
                                  , noUnderlineANSI
                                  , showText noUnderlineANSI ]
debugUnderline p = withoutArgs debugUnderline p


-----


debugWrap :: Action
debugWrap p@AdviseNoArgs = advise p [] advice
  where
    advice = T.concat [ "Please specify line length, as in "
                      , quoteColor
                      , dblQuote $ prefixDebugCmd "wrap" <> " 40"
                      , dfltColor
                      , "." ]
debugWrap (WithArgs i mq cols [a]) = case reads . T.unpack $ a :: [(Int, String)] of
  []              -> sorryParse
  [(lineLen, "")] -> helper lineLen
  _               -> sorryParse
  where
    sorryParse = wrapSend mq cols $ dblQuote a <> " is not a valid line length."
    helper lineLen | lineLen < 0                            = wrapSorryWtf     mq cols
                   | lineLen < minCols || lineLen > maxCols = wrapSorryLineLen mq cols
                   | otherwise                              = do
                       logPlaExecArgs (prefixDebugCmd "wrap") [a] i
                       send mq . frame lineLen . wrapUnlines lineLen $ wrapMsg
debugWrap p = advise p [] advice
  where
    advice = T.concat [ "Please provide one argument: line length, as in "
                      , quoteColor
                      , dblQuote $ prefixDebugCmd "wrap" <> " 40"
                      , dfltColor
                      , "." ]


wrapSorryWtf :: MsgQueue -> Cols -> MudStack ()
wrapSorryWtf mq cols = wrapSend mq cols $ wtfColor <> "He don't." <> dfltColor


wrapSorryLineLen :: MsgQueue -> Cols -> MudStack ()
wrapSorryLineLen mq cols = wrapSend mq cols . T.concat $ [ "The line length must be between "
                                                         , showText minCols
                                                         , " and "
                                                         , showText maxCols
                                                         , " characters." ]


wrapMsg :: T.Text
wrapMsg = let ls = [ T.concat [ u
                              , mkFgColorANSI (Dull, c)
                              , "This is "
                              , showText c
                              , " text." ] | c <- Black `delete` colors, u <- [ underlineANSI, noUnderlineANSI ] ]
          in (<> dfltColor) . T.unwords $ ls


-----


debugWrapIndent :: Action
debugWrapIndent p@AdviseNoArgs = advise p [] advice
  where
    advice = T.concat [ "Please specify line length followed by indent amount, as in "
                      , quoteColor
                      , dblQuote $ prefixDebugCmd "wrapindent" <> " 40 4"
                      , dfltColor
                      , "." ]
debugWrapIndent p@(AdviseOneArg _) = advise p [] advice
  where
    advice = T.concat [ "Please also specify indent amount, as in "
                      , quoteColor
                      , dblQuote $ prefixDebugCmd "wrapindent" <> " 40 4"
                      , dfltColor
                      , "." ]
debugWrapIndent (WithArgs i mq cols [a, b]) = do
    parsed <- (,) <$> parse a sorryParseLineLen <*> parse b sorryParseIndent
    unless (uncurry (||) . over both isNothing $ parsed) . uncurry helper . over both fromJust $ parsed
  where
    parse txt sorry = case reads . T.unpack $ txt :: [(Int, String)] of
      []        -> sorry >> return Nothing
      [(x, "")] -> return . Just $ x
      _         -> sorry >> return Nothing
    sorryParseLineLen = wrapSend mq cols $ dblQuote a <> " is not a valid line length."
    sorryParseIndent  = wrapSend mq cols $ dblQuote b <> " is not a valid width amount."
    helper lineLen indent | lineLen < 0 || indent < 0              = wrapSorryWtf     mq cols
                          | lineLen < minCols || lineLen > maxCols = wrapSorryLineLen mq cols
                          | indent >= lineLen                      = sorryIndent
                          | otherwise                              = do
                              logPlaExecArgs (prefixDebugCmd "wrapindent") [a, b] i
                              send mq . frame lineLen . T.unlines . wrapIndent indent lineLen $ wrapMsg
    sorryIndent = wrapSend mq cols "The indent amount must be less than the line length."
debugWrapIndent p = advise p [] advice
  where
    advice = T.concat [ "Please provide two arguments: line length and indent amount, as in "
                      , quoteColor
                      , dblQuote $ prefixDebugCmd "wrapindent" <> " 40 4"
                      , dfltColor
                      , "." ]
