{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, TupleSections, ViewPatterns #-}

module Mud.Cmds.Debug ( debugCmds
                      , purgeThreadTbls
                      , {- Not a typo. -} ) where

import Mud.Cmds.Msgs.Advice
import Mud.Cmds.Msgs.CmdDesc
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Util.CmdPrefixes
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Interp.Misc
import Mud.Interp.MultiLine
import Mud.Misc.ANSI
import Mud.Misc.EffectFuns
import Mud.Misc.Logging (writeLog)
import Mud.Misc.Persist
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iPidge)
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Threads.NpcServer
import Mud.Threads.ThreadTblPurger
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Telnet
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Token
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logAndDispIOEx, logNotice, logPlaExec, logPlaExecArgs)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative (Const)
import Control.Arrow ((***))
import Control.Concurrent (getNumCapabilities, myThreadId)
import Control.Concurrent.Async (asyncThreadId, poll)
import Control.Exception (ArithException(..), IOException)
import Control.Exception.Lifted (throwIO, try)
import Control.Lens (Optical, both, to, view, views)
import Control.Lens.Operators ((%~), (&), (^.))
import Control.Monad ((>=>), replicateM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Char (ord, digitToInt, isDigit, toLower)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (delete, intercalate, sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Sum(..))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Conc (threadStatus)
import Numeric (readInt)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (IntMap, assocs, keys, toList)
import qualified Data.Map.Lazy as M (assocs, elems, keysSet, toList)
import qualified Data.Set as S (toAscList)
import qualified Data.Text as T
import System.Console.ANSI (Color(..), ColorIntensity(..))
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.IO (hClose, hGetBuffering, openTempFile)


{-# ANN module ("HLint: ignore Redundant where" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase"   :: String) #-}


-- ==================================================


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Debug"


-----


logAndDispIOEx :: MsgQueue -> Cols -> Text -> IOException -> MudStack ()
logAndDispIOEx mq cols = L.logAndDispIOEx mq cols "Mud.Cmds.Debug"


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Debug"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Debug"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Debug"


-- ==================================================


debugCmds :: [Cmd]
debugCmds =
    [ mkDebugCmd "?"          debugDispCmdList ("\&" <> cmdDescDispCmdList)
    , mkDebugCmd "ap"         debugAp          "Show \"ActionParams\", including any arguments you provide."
    , mkDebugCmd "boot"       debugBoot        "Boot all players (including yourself)."
    , mkDebugCmd "broadcast"  debugBcast       "Broadcast a multi-line message to yourself."
    , mkDebugCmd "buffer"     debugBuffCheck   "Confirm the default buffering mode for file handles."
    , mkDebugCmd "cins"       debugCins        "Dump all channel ID/names for a given player ID."
    , mkDebugCmd "color"      debugColor       "Perform a color test."
    , mkDebugCmd "cores"      debugCores       "Display the number of processor cores."
    , mkDebugCmd "cpu"        debugCPU         "Display the CPU time."
    , mkDebugCmd "echowill"   debugEchoWill    "Send IAC WILL ECHO (hide user input)."
    , mkDebugCmd "echowon't"  debugEchoWon't   "Send IAC WON'T ECHO (show user input)."
    , mkDebugCmd "effect"     debugEffect      "Add 10-20 to your ST for 30 seconds."
    , mkDebugCmd "env"        debugEnv         "Display or search system environment variables."
    , mkDebugCmd "exp"        debugExp         "Award yourself 5,000 exp."
    , mkDebugCmd "fun"        debugFun         "Dump the keys of the \"FunTbl\", \"HookFunTbl\", \"RmActionFunTbl\", \
                                               \and \"EffectFunTbl\"."
    , mkDebugCmd "id"         debugId          "Search the \"MudState\" tables for a given ID."
    , mkDebugCmd "keys"       debugKeys        "Dump a list of \"MudState\" table keys."
    , mkDebugCmd "liquid"     debugLiq         "Consume a given amount (in mouthfuls) of a given liquid (by distinct \
                                               \liquid ID)."
    , mkDebugCmd "log"        debugLog         "Put the logging service under heavy load."
    , mkDebugCmd "multiline"  debugMultiLine   "Test multi line input."
    , mkDebugCmd "npcserver"  debugNpcServer   "Stop all NPC server threads."
    , mkDebugCmd "number"     debugNumber      "Display the decimal equivalent of a given number in a given base."
    , mkDebugCmd "out"        debugOut         "Dump the inventory of the logged out room."
    , mkDebugCmd "persist"    debugPersist     "Attempt to persist the world multiple times in quick succession."
    , mkDebugCmd "pidge"      debugPidge       "Send a message to Pidge."
    , mkDebugCmd "pmf"        debugPmf         "Trigger a pattern match failure."
    , mkDebugCmd "purge"      debugPurge       "Purge the thread tables."
    , mkDebugCmd "random"     debugRandom      "Dump a series of random numbers."
    , mkDebugCmd "regen"      debugRegen       "Display regen amounts and delays for a given mob ID."
    , mkDebugCmd "remput"     debugRemPut      "In quick succession, remove from and put into a sack on the ground."
    , mkDebugCmd "rnt"        debugRnt         "Dump your random names table, or generate a random name for a given PC."
    , mkDebugCmd "rotate"     debugRotate      "Send the signal to rotate your player log."
    , mkDebugCmd "talk"       debugTalk        "Dump the talk async table."
    , mkDebugCmd "threads"    debugThreads     "Display or search the thread table."
    , mkDebugCmd "throw"      debugThrow       "Throw an exception."
    , mkDebugCmd "throwlog"   debugThrowLog    "Throw an exception on your player log thread."
    , mkDebugCmd "tinnitus"   debugTinnitus    "Ringing in the ears."
    , mkDebugCmd "token"      debugToken       "Test token parsing."
    , mkDebugCmd "underline"  debugUnderline   "Test underlining."
    , mkDebugCmd "weight"     debugWeight      "Calculate weight for a given ID."
    , mkDebugCmd "wrap"       debugWrap        "Test the wrapping of a line containing ANSI escape sequences."
    , mkDebugCmd "wrapindent" debugWrapIndent  "Test the indented wrapping of a line containing ANSI escape \
                                               \sequences." ]
  where {- -}


mkDebugCmd :: Text -> ActionFun -> CmdDesc -> Cmd
mkDebugCmd (prefixDebugCmd -> cn) f cd = Cmd { cmdName           = cn
                                             , cmdPriorityAbbrev = Nothing
                                             , cmdFullName       = cn
                                             , cmdAction         = Action f True
                                             , cmdDesc           = cd }


-----


debugAp :: ActionFun
debugAp p@(WithArgs i mq cols _) = (wrapSend mq cols . showText $ p) >> logPlaExec (prefixDebugCmd "ap") i
debugAp p                        = patternMatchFail "debugAp" . showText $ p


-----


debugBoot :: ActionFun
debugBoot (NoArgs' i mq) = do { ok mq; massMsg . MsgBoot $ dfltBootMsg; logPlaExec (prefixDebugCmd "boot") i }
debugBoot p              = withoutArgs debugBoot p


-----


debugBcast :: ActionFun
debugBcast (NoArgs'' i) = bcastNl (mkBcast i msg) >> logPlaExec (prefixDebugCmd "broadcast") i
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
debugBcast p = withoutArgs debugBcast p


-----


debugBuffCheck :: ActionFun
debugBuffCheck (NoArgs i mq cols) = do
    helper |&| try >=> eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
    logPlaExec (prefixDebugCmd "buffer") i
  where
    helper = liftIO (flip openTempFile "temp" =<< getTemporaryDirectory) >>= \(fn, h) -> do
        send mq . nl =<< [ T.unlines . wrapIndent 2 cols $ msg | (mkMsg fn -> msg) <- liftIO . hGetBuffering $ h ]
        liftIO $ hClose h >> removeFile fn
    mkMsg (dblQuote . T.pack -> fn) (dblQuote . showText -> mode) =
        T.concat [ "Default buffering mode for temp file ", fn, " is ", mode, "." ]
debugBuffCheck p = withoutArgs debugBuffCheck p


-----


debugCins :: ActionFun
debugCins p@AdviseNoArgs       = advise p [] adviceDCinsNoArgs
debugCins (OneArg i mq cols a) = getState >>= \ms -> case reads . T.unpack $ a :: [(Int, String)] of
  [(targetId, "")] -> helper ms targetId
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper ms targetId@(showText -> targetIdTxt)
      | targetId < 0                              = wrapSend mq cols sorryWtf
      | targetId `notElem` views pcTbl IM.keys ms = wrapSend mq cols . sorryNonexistentId targetId . pure $ "PC"
      | otherwise                                 = do
          multiWrapSend mq cols . (header :) . pure . showText =<< getAllChanIdNames i ms
          logPlaExecArgs (prefixDebugCmd "cins") (pure a) i
      where
        header = T.concat [ "All channel ID/names "
                          , parensQuote "IM.IntMap [(Id, Text)]"
                          , " for ID "
                          , targetIdTxt
                          , ":" ]
debugCins p = advise p [] adviceDCinsExcessArgs


-----


debugColor :: ActionFun
debugColor (NoArgs' i mq) = (send mq . nl . T.concat $ msg) >> logPlaExec (prefixDebugCmd "color") i
  where
    msg :: [] Text
    msg = [ nl $ (pad 15 . showText $ ansi) <> mkColorDesc fg bg <> (colorWith ansi . spaced $ "CurryMUD")
          | fgi <- intensities, fgc <- colors, bgi <- intensities, bgc <- colors
          , let fg = (fgi, fgc), let bg = (bgi, bgc), let ansi = mkColorANSI fg bg ]
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName = uncurry (<>) . (pad 6 . showText *** padColorName . showText)
debugColor p = withoutArgs debugColor p

-----


debugCores :: ActionFun
debugCores (NoArgs i mq cols) = do
    wrapSend mq cols =<< [ T.concat [ showText cores, " processor core", theLetterS (cores > 1), "." ]
                         | cores <- liftIO getNumCapabilities ]
    logPlaExec (prefixDebugCmd "cores") i
debugCores p = withoutArgs debugCores p


-----


debugCPU :: ActionFun
debugCPU (NoArgs i mq cols) = do
    wrapSend mq cols =<< [ "CPU time: " <> time | time <- liftIO cpuTime ]
    logPlaExec (prefixDebugCmd "cpu") i
  where
    cpuTime = showText . (`divide` 10 ^ 12) <$> getCPUTime
debugCPU p = withoutArgs debugCPU p


-----


debugDispCmdList :: ActionFun
debugDispCmdList p@(LowerNub' i as) = dispCmdList debugCmds p >> logPlaExecArgs (prefixDebugCmd "?") as i
debugDispCmdList p                  = patternMatchFail "debugDispCmdList" . showText $ p


-----


debugEchoWill :: ActionFun
debugEchoWill (NoArgs' i mq) = do
    send mq . T.pack $ [ telnetIAC, telnetWILL, telnetECHO ]
    ok mq
    logPlaExec (prefixDebugCmd "echowill") i
debugEchoWill p = withoutArgs debugEchoWill p


-----


debugEchoWon't :: ActionFun
debugEchoWon't (NoArgs' i mq) = do
    send mq . T.pack $ [ telnetIAC, telnetWON'T, telnetECHO ]
    ok mq
    logPlaExec (prefixDebugCmd "echowon't") i
debugEchoWon't p = withoutArgs debugEchoWon't p


-----


debugEffect :: ActionFun
debugEffect (NoArgs' i mq) = do
    ok mq
    startEffect i . Effect (MobEffectAttrib St) (Just . RangeVal $ (10, 20)) 30 $ Nothing
    logPlaExec (prefixDebugCmd "effect") i
debugEffect p = withoutArgs debugEffect p


-----


debugEnv :: ActionFun
debugEnv (NoArgs i mq cols) = do
    pager i mq =<< [ concatMap (wrapIndent 2 cols) . mkEnvListTxt $ env | env <- liftIO getEnvironment ]
    logPlaExecArgs (prefixDebugCmd "env") [] i
debugEnv p@ActionParams { myId, args } = do
    dispMatches p 2 =<< [ mkEnvListTxt env | env <- liftIO getEnvironment ]
    logPlaExecArgs (prefixDebugCmd "env") args myId


mkEnvListTxt :: [(String, String)] -> [Text]
mkEnvListTxt = map (mkAssocTxt . (both %~ T.pack))
  where
    mkAssocTxt (a, b) = colorWith envVarColor (a <> ": ") <> b


-----


debugExp :: ActionFun
debugExp (NoArgs' i mq) = let cn = prefixDebugCmd "exp" in do
    ok mq
    logPlaExec cn i
    awardExp 5000 ("executed " <> dblQuote cn) i
debugExp p = withoutArgs debugExp p


-----


debugFun :: ActionFun
debugFun (NoArgs i mq cols) = getState >>= \ms -> do
    let helper t lens = t <> ":" : views lens (S.toAscList . M.keysSet) ms
    pager i mq . concatMap (wrapIndent 2 cols) . intercalateDivider cols $ [ helper "FunTbl"            funTbl
                                                                           , helper "HookFunTbl"        hookFunTbl
                                                                           , helper "RmActionFunTbl"    rmActionFunTbl
                                                                           , helper "EffectFunTbl"      effectFunTbl
                                                                           , helper "InstaEffectFunTbl" instaEffectFunTbl ]
    logPlaExec (prefixDebugCmd "fun") i
debugFun p = withoutArgs debugFun p


-----


debugId :: ActionFun
debugId p@AdviseNoArgs       = advise p [] adviceDIdNoArgs
debugId (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(searchId, "")] -> helper searchId
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper searchId@(showText -> searchIdTxt)
      | searchId < 0 = wrapSend mq cols sorryWtf
      | otherwise    = getState >>= \ms -> do
          let f     = commas . map (showText . fst)
              mkTxt =
                  [ [ "Tables containing key " <> searchIdTxt <> ":"
                    , commas . map fst . filter ((searchId `elem`) . snd) . mkTblNameKeysList $ ms ]
                  , [ T.concat [ "Channels with a ", dblQuote "chanId", " of ", searchIdTxt, ":" ]
                    , f . filter ((== searchId) . view chanId . snd) . tblToList chanTbl $ ms ]
                  , [ T.concat [ "Entities with an ", dblQuote "entId", " of ", searchIdTxt, ":" ]
                    , f . filter ((== searchId) . view entId . snd) . tblToList entTbl $ ms ]
                  , [ T.concat [ "Foods with a ", dblQuote "foodId", " of ", searchIdTxt, ":" ]
                    , f . filter ((== DistinctFoodId searchId) . view foodId . snd) . tblToList foodTbl $ ms ]
                  , [ T.concat [ "Equipment maps containing ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . M.elems . snd) . tblToList eqTbl $ ms ]
                  , [ T.concat [ "Inventories containing ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . snd) . tblToList invTbl $ ms ]
                  , [ T.concat [ "Mobiles with a ", dblQuote "rmId", " of ", searchIdTxt, ":" ]
                    , f . filter ((== searchId) . view rmId . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "Mobiles following ID ", searchIdTxt, ":" ]
                    , f . filter ((== Just searchId) . view (party.following) . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "Mobiles followed by ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view (party.followers) . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "Mobiles whose group includes ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view (party.myGroup) . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "Mobiles who are a member of ID ", searchIdTxt, "'s group:" ]
                    , f . filter ((== Just searchId) . view (party.memberOf) . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "Mobiles whose stomach contains ", dblQuote "DistinctFoodId", " ", searchIdTxt, ":" ]
                    , let g = filter $ views distinctId (== (Right . DistinctFoodId $ searchId))
                      in f . filter ((()!#) . g . view stomach . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "Mobiles whose stomach contains ", dblQuote "DistinctLiqId", " ", searchIdTxt, ":" ]
                    , let g = filter $ views distinctId (== (Left . DistinctLiqId $ searchId))
                      in f . filter ((()!#) . g . view stomach . snd) . tblToList mobTbl $ ms ]
                  , [ T.concat [ "NPCs possessed by ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view npcPossessor . snd) . tblToList npcTbl $ ms ]
                  , [ T.concat [ "Players peeped by ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view peepers . snd) . tblToList plaTbl $ ms ]
                  , [ T.concat [ "Players peeping ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view peeping . snd) . tblToList plaTbl $ ms ]
                  , [ T.concat [ "Players possessing ID ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view possessing . snd) . tblToList plaTbl $ ms ]
                  , [ T.concat [ "Players with a ", dblQuote "lastRmId", " of ", searchIdTxt, ":" ]
                    , f . filter ((searchId `elem`) . view lastRmId . snd) . tblToList plaTbl $ ms ]
                  , [ T.concat [ "Rooms with a ", dblQuote "StdLink", " to ID ", searchIdTxt, ":" ]
                    , let g (StdLink _ di _) = di == searchId
                          g NonStdLink {}    = False
                      in f . filter ((()!#) . filter g . view rmLinks . snd) . tblToList rmTbl $ ms ]
                  , [ T.concat [ "Rooms with a ", dblQuote "NonStdLink", " to ID ", searchIdTxt, ":" ]
                    , let g StdLink {}              = False
                          g (NonStdLink _ di _ _ _) = di == searchId
                      in f . filter ((()!#) . filter g . view rmLinks . snd) . tblToList rmTbl $ ms ]
                  , [ T.concat [ "Vessels containing ", dblQuote "liqId", " ", searchIdTxt, ":" ]
                    , let g = views vesselCont (maybe False (views liqId (== DistinctLiqId searchId) . fst)) . snd
                      in f . filter g . tblToList vesselTbl $ ms ] ]
          pager i mq . concat . wrapLines cols . intercalate [""] $ mkTxt
          logPlaExecArgs (prefixDebugCmd "id") (pure a) i
debugId p = advise p [] adviceDIdExcessArgs


tblToList :: Optical (->) (->) (Const [(Id, a)]) MudState MudState (IM.IntMap a) (IM.IntMap a) -> MudState -> [(Id, a)]
tblToList lens = views lens IM.toList


mkTblNameKeysList :: MudState -> [(Text, Inv)]
mkTblNameKeysList ms = [ ("ActiveEffects", tblKeys activeEffectsTbl ms)
                       , ("Arm",           tblKeys armTbl           ms)
                       , ("Chan",          tblKeys chanTbl          ms)
                       , ("Cloth",         tblKeys clothTbl         ms)
                       , ("Coins",         tblKeys coinsTbl         ms)
                       , ("Con",           tblKeys conTbl           ms)
                       , ("DistinctFood",  tblKeys distinctFoodTbl  ms)
                       , ("DistinctLiq",   tblKeys distinctLiqTbl   ms)
                       , ("Ent",           tblKeys entTbl           ms)
                       , ("EqMap",         tblKeys eqTbl            ms)
                       , ("Food",          tblKeys foodTbl          ms)
                       , ("Inv",           tblKeys invTbl           ms)
                       , ("Mob",           tblKeys mobTbl           ms)
                       , ("MsgQueue",      tblKeys msgQueueTbl      ms)
                       , ("Npc",           tblKeys npcTbl           ms)
                       , ("Obj",           tblKeys objTbl           ms)
                       , ("PausedEffects", tblKeys pausedEffectsTbl ms)
                       , ("PC",            tblKeys pcTbl            ms)
                       , ("PickPts",       tblKeys pickPtsTbl       ms)
                       , ("PlaLog",        tblKeys plaLogTbl        ms)
                       , ("Pla",           tblKeys plaTbl           ms)
                       , ("Rm",            tblKeys rmTbl            ms)
                       , ("RmTeleName",    tblKeys rmTeleNameTbl    ms)
                       , ("RndmNames",     tblKeys rndmNamesMstrTbl ms)
                       , ("TeleLink",      tblKeys teleLinkMstrTbl  ms)
                       , ("Type",          tblKeys typeTbl          ms)
                       , ("Vessel",        tblKeys vesselTbl        ms)
                       , ("Wpn",           tblKeys wpnTbl           ms)
                       , ("Writable",      tblKeys writableTbl      ms) ]


tblKeys :: Optical (->) (->) (Const Inv) MudState MudState (IM.IntMap a) (IM.IntMap a) -> MudState -> Inv
tblKeys lens = views lens IM.keys


-----


debugKeys :: ActionFun
debugKeys (NoArgs i mq cols) = getState >>= \ms -> do
    pager i mq . concat . wrapLines cols . intercalate [""] . map mkKeysTxt . mkTblNameKeysList $ ms
    logPlaExec (prefixDebugCmd "keys") i
  where
    mkKeysTxt (tblName, ks) = [ tblName <> ": ", showText ks ]
debugKeys p = withoutArgs debugKeys p


-----


debugLiq :: ActionFun
debugLiq p@AdviseNoArgs            = advise p [] adviceDLiqNoArgs
debugLiq p@(AdviseOneArg _       ) = advise p [] adviceDLiqNoId
debugLiq   (WithArgs i mq cols as) = getState >>= \ms ->
    parseTwoIntArgs mq cols as sorryParseAmt sorryParseId (helper ms)
  where
    helper ms amt di
      | amt < 1 || di < 0                            = wrapSend mq cols sorryWtf
      | di `notElem` views distinctLiqTbl IM.keys ms = wrapSend mq cols . sorryNonexistentId di . pure $ "distinct liquid"
      | otherwise = do
          ok mq
          logPlaExecArgs (prefixDebugCmd "liquid") as i
          consume i =<< mkStomachConts <$> liftIO getCurrentTime
      where
        mkStomachConts now = replicate amt . StomachCont (Left . DistinctLiqId $ di) now $ False
debugLiq p = advise p [] adviceDLiqExcessArgs


parseTwoIntArgs :: MsgQueue
                -> Cols
                -> [Text]
                -> (Text -> Text)
                -> (Text -> Text)
                -> (Int -> Int -> MudStack ())
                -> MudStack ()
parseTwoIntArgs mq cols [a, b] sorryParseA sorryParseB helper = do
    parsed <- (,) <$> parse a sorryParseA
                  <*> parse b sorryParseB
    unless (uncurry (||) $ parsed & both %~ (()#)) . uncurry helper $ parsed & both %~ (getSum . fromJust)
  where
    parse txt sorry = case reads . T.unpack $ txt :: [(Int, String)] of
      [(x, "")] -> unadulterated . Sum $ x
      _         -> emptied . wrapSend mq cols . sorry $ txt
parseTwoIntArgs _ _ as _ _ _ = patternMatchFail "parseTwoIntArgs" . showText $ as


-----


debugLog :: ActionFun
debugLog (NoArgs' i mq) = helper >> ok mq >> logPlaExec (prefixDebugCmd "log") i
  where
    helper       = replicateM_ 100 . onNewThread $ heavyLogging
    heavyLogging = replicateM_ 100 . logNotice "debugLog heavyLogging" =<< mkMsg
    mkMsg        = [ prd $ "Logging from " <> ti | (showText -> ti) <- liftIO myThreadId ]
debugLog p = withoutArgs debugLog p


-----


debugMultiLine :: ActionFun
debugMultiLine (NoArgs i mq cols) = promptMultiLine mq >> (setInterp i . Just . interpMutliLine f $ [])
  where
    f ts = do
        multiWrapSend mq cols $ nlPrefix "You entered:" : ts
        sendDfltPrompt mq i
        resetInterp i
debugMultiLine p = withoutArgs debugMultiLine p


-----


debugNpcServer :: ActionFun
debugNpcServer (NoArgs' i mq) = ok mq >> stopNpcServers >> logPlaExec (prefixDebugCmd "npcserver") i
debugNpcServer p              = withoutArgs debugNpcServer p


-----


type Base = Int


debugNumber :: ActionFun
debugNumber p@AdviseNoArgs     = advise p [] adviceDNumberNoArgs
debugNumber p@(AdviseOneArg _) = advise p [] adviceDNumberNoBase
debugNumber (WithArgs i mq cols [ numTxt, baseTxt ]) =
    case reads . T.unpack $ baseTxt :: [(Base, String)] of
      [(base, "")] | not . inRange (2, 36) $ base -> wrapSend mq cols . sorryParseBase $ baseTxt
                   | otherwise -> case numTxt `inBase` base of
                     [(res, "")] -> do
                         send mq . nlnl . showText $ res
                         logPlaExecArgs (prefixDebugCmd "number") [ numTxt, baseTxt ] i
                     _ -> wrapSend mq cols . sorryParseNum numTxt . showText $ base
      _ -> wrapSend mq cols . sorryParseBase $ baseTxt
debugNumber p = advise p [] adviceDNumberExcessArgs


inBase :: Text -> Base -> [(Int, String)]
numTxt `inBase` base = readInt base (isValidDigit base) letterToNum . T.unpack $ numTxt


isValidDigit :: Base -> Char -> Bool
isValidDigit base (toLower -> c) | isDigit c                                         = digitToInt c < base
                                 | not . inRange ('a', 'z') $ c                      = False
                                 | val <- fromJust . lookup c . zip ['a'..] $ [11..] = val <= base


letterToNum :: Char -> Int
letterToNum c | isDigit c = digitToInt c
              | otherwise = let (🍪) = (-) `on` ord in (c 🍪 'a') + 10


-----


debugOut :: ActionFun
debugOut (NoArgs i mq cols) = getState >>= \ms -> do
    wrapSend mq cols . showText . getInv iLoggedOut $ ms
    logPlaExec (prefixDebugCmd "out") i
debugOut p = withoutArgs debugOut p


-----


debugPersist :: ActionFun
debugPersist (NoArgs' i mq) = replicateM_ 10 (persist >> ok mq) >> logPlaExec (prefixDebugCmd "persist") i
debugPersist p              = withoutArgs debugPersist p


-----


debugPidge :: ActionFun
debugPidge (NoArgs' i mq) = serialize . flip (mkStdDesig i) Don'tCap <$> getState >>= \d -> do
    bcastNl . mkBcast iPidge $ "Geetings from " <> d <> "!"
    ok mq
    logPlaExec (prefixDebugCmd "pidge") i
debugPidge p = withoutArgs debugPidge p


-----


debugPmf :: ActionFun
debugPmf (NoArgs'' i) = do
    logPlaExec (prefixDebugCmd "pmf") i
    patternMatchFail "debugPmf" . showText $ (0, 'a', "abcdef")
debugPmf p = withoutArgs debugPmf p


-----


debugPurge :: ActionFun
debugPurge (NoArgs' i mq) = purgeThreadTbls >> ok mq >> logPlaExec (prefixDebugCmd "purge") i
debugPurge p              = withoutArgs debugPurge p


-----


debugRandom :: ActionFun
debugRandom (NoArgs i mq cols) = do
    wrapSend mq cols . showText =<< rndmRs 10 (0, 99)
    logPlaExec (prefixDebugCmd "random") i
debugRandom p = withoutArgs debugRandom p


-----


debugRegen :: ActionFun
debugRegen p@AdviseNoArgs       = advise p [] adviceDRegenNoArgs
debugRegen (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(targetId, "")] -> helper targetId =<< getState
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper targetId ms
      | targetId < 0 = wrapSend mq cols sorryWtf
      | targetId `notElem` uncurry (++) (ms^.npcTbl.to IM.keys, ms^.pcTbl.to IM.keys) =
          wrapSend mq cols . sorryNonexistentId targetId $ [ "NPC", "PC" ]
      | otherwise = do
          multiWrapSend mq cols descRegens
          logPlaExecArgs (prefixDebugCmd "regen") (pure a) i
      where
        descRegens = map (uncurry3 descRegen) [ ("hp", calcRegenHpAmt, calcRegenHpDelay)
                                              , ("mp", calcRegenMpAmt, calcRegenMpDelay)
                                              , ("pp", calcRegenPpAmt, calcRegenPpDelay)
                                              , ("fp", calcRegenFpAmt, calcRegenFpDelay) ]
        descRegen t calcAmt calcDelay = T.concat [ t
                                                 , ": "
                                                 , showText . calcAmt   targetId $ ms
                                                 , " / "
                                                 , showText . calcDelay targetId $ ms
                                                 , " sec" ]
debugRegen p = advise p [] adviceDRegenExcessArgs


-----


debugRemPut :: ActionFun
debugRemPut (NoArgs' i mq) = do
    mapM_ (fakeClientInput mq) . take 10 . cycle . map (<> rest) $ [ "remove", "put" ]
    logPlaExec (prefixDebugCmd "remput") i
  where
    rest = (spaced . T.singleton $ allChar) <> ('r' `T.cons` selectorChar `T.cons` "sack")
debugRemPut p = withoutArgs debugRemPut p


-----


debugRnt :: ActionFun
debugRnt (NoArgs i mq cols) = do
    wrapSend mq cols . showText . M.toList . getRndmNamesTbl i =<< getState
    logPlaExec (prefixDebugCmd "rnt") i
debugRnt (OneArgNubbed i mq cols (capitalize -> a)) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryPCName $ a
        found match = do
            rndmName <- updateRndmName i . getIdForMobSing match $ ms
            wrapSend mq cols . T.concat $ [ dblQuote rndmName, " has been randomly generated for ", match, "." ]
            logPlaExec (prefixDebugCmd "rnt") i
        pcSings = [ ms^.entTbl.ind pcId.sing | pcId <- views pcTbl IM.keys ms ]
    in findFullNameForAbbrev a pcSings |&| maybe notFound found
debugRnt p = advise p [] adviceDRntExcessArgs


-----


debugRotate :: ActionFun
debugRotate (NoArgs' i mq) = getState >>= \ms -> let lq = getLogQueue i ms in do
    writeLog lq RotateLog
    ok mq
    logPlaExec (prefixDebugCmd "rotate") i
debugRotate p = withoutArgs debugRotate p


-----


debugTalk :: ActionFun
debugTalk (NoArgs i mq cols) = getState >>= \(views talkAsyncTbl M.elems -> asyncs) -> do
    pager i mq =<< [ concatMap (wrapIndent 2 cols) descs | descs <- mapM mkDesc asyncs ]
    logPlaExec (prefixDebugCmd "talk") i
  where
    mkDesc a    = [ T.concat [ "Talk async ", showText . asyncThreadId $ a, ": ", statusTxt, "." ]
                  | statusTxt <- mkStatusTxt <$> liftIO (poll a) ]
    mkStatusTxt = \case Nothing         -> "running"
                        Just (Left  e ) -> "exception " <> (parensQuote . showText $ e)
                        Just (Right ()) -> "finished"
debugTalk p = withoutArgs debugTalk p


-----


debugThreads :: ActionFun
debugThreads (NoArgs i mq cols) = do
    pager i mq . concatMap (wrapIndent 2 cols) =<< descThreads
    logPlaExec (prefixDebugCmd "threads") i
debugThreads p@ActionParams { myId, args } = do
    dispMatches p 2 =<< descThreads
    logPlaExecArgs (prefixDebugCmd "threads") args myId


descThreads :: MudStack [Text]
descThreads = do
    (uncurry (:) . ((, Notice) *** pure . (, Error)) -> logAsyncKvs) <- asks $ (both %~ asyncThreadId) . getLogAsyncs
    (plt, M.assocs -> threadTblKvs) <- (view plaLogTbl *** view threadTbl) . dup <$> getState
    let plaLogTblKvs = [ (asyncThreadId . fst $ v, PlaLog k) | (k, v) <- IM.assocs plt ]
    mapM mkDesc . sort $ logAsyncKvs ++ threadTblKvs ++ plaLogTblKvs
  where
    mkDesc (ti, bracketPad 20 . mkTypeName -> tn) = [ T.concat [ padOrTrunc 16 . showText $ ti, tn, ts ]
                                                    | (showText -> ts) <- liftIO . threadStatus $ ti ]
    mkTypeName (Biodegrader    (showText -> pi)) = padOrTrunc padAmt "Biodegrader"  <> pi
    mkTypeName (Digester       (showText -> pi)) = padOrTrunc padAmt "Digester"     <> pi
    mkTypeName (DrinkingThread (showText -> pi)) = padOrTrunc padAmt "Drinking"     <> pi
    mkTypeName (EatingThread   (showText -> pi)) = padOrTrunc padAmt "Eating"       <> pi
    mkTypeName (MovingThread   (showText -> pi)) = padOrTrunc padAmt "Moving"       <> pi
    mkTypeName (EffectListener (showText -> pi)) = padOrTrunc padAmt "Eff Listen"   <> pi
    mkTypeName (EffectThread   (showText -> pi)) = padOrTrunc padAmt "Eff Thread"   <> pi
    mkTypeName (EffectTimer    (showText -> pi)) = padOrTrunc padAmt "Eff Timer"    <> pi
    mkTypeName (FeelingTimer   (showText -> pi)) = padOrTrunc padAmt "FeelingTimer" <> pi
    mkTypeName (InacTimer      (showText -> pi)) = padOrTrunc padAmt "InacTimer"    <> pi
    mkTypeName (NpcServer      (showText -> pi)) = padOrTrunc padAmt "NpcServer"    <> pi
    mkTypeName (PlaLog         (showText -> pi)) = padOrTrunc padAmt "PlaLog"       <> pi
    mkTypeName (Receive        (showText -> pi)) = padOrTrunc padAmt "Receive"      <> pi
    mkTypeName (RegenChild     (showText -> pi)) = padOrTrunc padAmt "RegenChild"   <> pi
    mkTypeName (RegenParent    (showText -> pi)) = padOrTrunc padAmt "RegenParent"  <> pi
    mkTypeName (RmFun          (showText -> pi)) = padOrTrunc padAmt "RmFun"        <> pi
    mkTypeName (Server         (showText -> pi)) = padOrTrunc padAmt "Server"       <> pi
    mkTypeName (Talk           (showText -> pi)) = padOrTrunc padAmt "Talk"         <> pi
    mkTypeName (showText -> tt)                  = tt
    padAmt                                       = 13


-----


debugThrow :: ActionFun
debugThrow (NoArgs'' i) = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow p            = withoutArgs debugThrow p


-----


debugThrowLog :: ActionFun
debugThrowLog (NoArgs' i mq) = getState >>= \ms -> let lq = getLogQueue i ms in
    writeLog lq Throw >> ok mq >> logPlaExec (prefixDebugCmd "throwlog") i
debugThrowLog p = withoutArgs debugThrowLog p


-----


debugTinnitus :: ActionFun
debugTinnitus (NoArgs' i mq) = do
    ok mq
    startEffect i . Effect (EffectOther tinnitusEffectFunName) Nothing (2 * 60) . Just . EffectFeeling "potTinnitus" $ 2 * 60
    logPlaExec (prefixDebugCmd "tinnitus") i
debugTinnitus p = withoutArgs debugTinnitus p


-----


debugToken :: ActionFun
debugToken (NoArgs i mq cols) = do
    multiWrapSend mq cols . T.lines . parseTokens . T.unlines $ tokenTxts
    logPlaExec (prefixDebugCmd "token") i
  where
    tokenTxts = [ charTokenDelimiter  `T.cons` charTokenDelimiter `T.cons` " literal charTokenDelimiter"
                , charTokenDelimiter  `T.cons` "a allChar"
                , charTokenDelimiter  `T.cons` "c adverbCloseChar"
                , charTokenDelimiter  `T.cons` "d adminCmdChar"
                , charTokenDelimiter  `T.cons` "e emoteNameChar"
                , charTokenDelimiter  `T.cons` "h chanTargetChar"
                , charTokenDelimiter  `T.cons` "i indexChar"
                , charTokenDelimiter  `T.cons` "l selectorChar"
                , charTokenDelimiter  `T.cons` "m amountChar"
                , charTokenDelimiter  `T.cons` "o adverbOpenChar"
                , charTokenDelimiter  `T.cons` "p expCmdChar"
                , charTokenDelimiter  `T.cons` "q quoteChar"
                , charTokenDelimiter  `T.cons` "r emoteTargetChar"
                , charTokenDelimiter  `T.cons` "s slotChar"
                , charTokenDelimiter  `T.cons` "t sayToChar"
                , charTokenDelimiter  `T.cons` "x emoteChar"
                , styleTokenDelimiter `T.cons` styleTokenDelimiter `T.cons` " literal styleTokenDelimiter"
                , styleTokenDelimiter `T.cons` ("aabbrevColor"       <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ccharDescColor"     <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ddfltColor"         <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("hheaderColor"       <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("iasteriskColor"     <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("kknownNameColor"    <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("lselectorColor"     <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("nnoUnderlineANSI"   <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ounknownNameColor"  <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("pprefixColor"       <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("qquoteColor"        <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("rarrowColor"        <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ssyntaxSymbolColor" <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("t " <> dfltColorStyleToken <> " <- toNpcColor")
                , styleTokenDelimiter `T.cons` ("uunderlineANSI"     <> noUnderlineStyleToken  )
                , styleTokenDelimiter `T.cons` ("zzingColor"         <> dfltColorStyleToken    )
                , "literal miscTokenDelimiter: " <> (T.pack . replicate 2 $ miscTokenDelimiter)
                , "dfltBootMsg: "     <> (miscTokenDelimiter `T.cons` "b")
                , "isDebug: "         <> (miscTokenDelimiter `T.cons` "d")
                , "pwWarningMsg: "    <> (miscTokenDelimiter `T.cons` "p")
                , "dfltShutdownMsg: " <> (miscTokenDelimiter `T.cons` "s")
                , "isZBackDoor: "     <> (miscTokenDelimiter `T.cons` "z") ]
    dfltColorStyleToken   = styleTokenDelimiter `T.cons` "d"
    noUnderlineStyleToken = styleTokenDelimiter `T.cons` "n"
debugToken p = withoutArgs debugToken p


-----


debugUnderline :: ActionFun
debugUnderline (NoArgs i mq cols) = do
    wrapSend mq cols $ showText underlineANSI <> underline " This text is underlined. " <> showText noUnderlineANSI
    logPlaExec (prefixDebugCmd "underline") i
debugUnderline p = withoutArgs debugUnderline p


-----


debugWeight :: ActionFun
debugWeight p@AdviseNoArgs       = advise p [] adviceDWeightNoArgs
debugWeight (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(searchId, "")] -> helper searchId =<< getState
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper searchId ms
      | searchId < 0                               = wrapSend mq cols sorryWtf
      | searchId `notElem` views objTbl IM.keys ms = wrapSend mq cols . sorryNonexistentId searchId . pure $ "object"
      | otherwise                                  = do
          send mq . nlnl . showText . calcWeight searchId $ ms
          logPlaExecArgs (prefixDebugCmd "weight") (pure a) i
debugWeight p = advise p [] adviceDWeightExcessArgs


-----


debugWrap :: ActionFun
debugWrap p@AdviseNoArgs       = advise p [] adviceDWrapNoArgs
debugWrap (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(lineLen, "")] -> helper lineLen
  _               -> wrapSend mq cols . sorryParseLineLen $ a
  where
    helper lineLen | lineLen < 0                                = wrapSend mq cols sorryWtf
                   | not . inRange (minCols, maxCols) $ lineLen = wrapSend mq cols sorryWrapLineLen
                   | otherwise                                  = do
                       send mq . frame lineLen . wrapUnlines lineLen $ wrapMsg
                       logPlaExecArgs (prefixDebugCmd "wrap") (pure a) i
debugWrap p = advise p [] adviceDWrapExcessArgs


wrapMsg :: Text
wrapMsg = T.unwords wordy <> dfltColor
  where
    wordy :: [] Text
    wordy = [ T.concat [ u
                       , mkFgColorANSI (Dull, c)
                       , "This is "
                       , showText c
                       , " text." ] | c <- Black `delete` colors, u <- [ underlineANSI, noUnderlineANSI ] ]


-----


debugWrapIndent :: ActionFun
debugWrapIndent p@AdviseNoArgs            = advise p [] adviceDWrapIndentNoArgs
debugWrapIndent p@(AdviseOneArg _       ) = advise p [] adviceDWrapIndentNoAmt
debugWrapIndent   (WithArgs i mq cols as) = parseTwoIntArgs mq cols as sorryParseLineLen sorryParseIndent helper
  where
    helper lineLen indent | any (< 0) [ lineLen, indent ]              = wrapSend mq cols sorryWtf
                          | not . inRange (minCols, maxCols) $ lineLen = wrapSend mq cols sorryWrapLineLen
                          | indent >= lineLen                          = wrapSend mq cols sorryIndent
                          | otherwise                                  = do
                              send mq . frame lineLen . T.unlines . wrapIndent indent lineLen $ wrapMsg
                              logPlaExecArgs (prefixDebugCmd "wrapindent") as i
debugWrapIndent p = advise p [] adviceDWrapIndentExcessArgs
