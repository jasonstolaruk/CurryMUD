{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExistentialQuantification, LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, PatternSynonyms, ScopedTypeVariables, ViewPatterns #-}

module Mud.Cmds.Debug ( debugCmds
                      , purgeThreadTbls
                      , {- Not a typo. -} ) where

import           Mud.Cmds.ExpCmds
import           Mud.Cmds.Msgs.Advice
import           Mud.Cmds.Msgs.CmdDesc
import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Msgs.Sorry
import           Mud.Cmds.Util.CmdPrefixes
import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.GMCP
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Data.State.Util.Random
import           Mud.Interp.Misc
import           Mud.Interp.MultiLine
import           Mud.Interp.Pause
import           Mud.Misc.ANSI
import           Mud.Misc.CurryTime
import           Mud.Misc.Logging (writeLog)
import qualified Mud.Misc.Logging as L (logAndDispIOEx, logNotice, logPlaExec, logPlaExecArgs)
import           Mud.Misc.Misc
import           Mud.Misc.Persist
import           Mud.TheWorld.Liqs
import           Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iPidge)
import           Mud.Threads.Effect
import           Mud.Threads.Misc
import           Mud.Threads.NpcServer
import           Mud.Threads.ThreadTblPurger
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Misc
import           Mud.TopLvlDefs.Seconds
import           Mud.TopLvlDefs.Telnet.Chars
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Weights
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Padding
import           Mud.Util.Quoting
import           Mud.Util.Text
import           Mud.Util.Token
import           Mud.Util.Wrapping

import           Control.Applicative (Const)
import           Control.Arrow ((***), first, second)
import           Control.Concurrent (ThreadId, getNumCapabilities, myThreadId)
import           Control.Concurrent.Async (asyncThreadId, poll)
import           Control.Exception (ArithException(..), IOException)
import           Control.Exception.Lifted (throwIO, try)
import           Control.Lens (Optical, both, views)
import           Control.Lens.Operators ((&), (%~))
import           Control.Monad ((>=>), replicateM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (encode, toJSON)
import           Data.Bits (zeroBits)
import           Data.Char (ord, digitToInt, isDigit, toLower)
import           Data.Function (on)
import qualified Data.IntMap.Strict as IM (IntMap, assocs, filter, keys, notMember, toList)
import           Data.Ix (inRange)
import           Data.List (delete, intercalate, sort)
import qualified Data.Map.Strict as M (assocs, elems, filter, keys, keysSet, toList)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>), Sum(..))
import qualified Data.Set as S (toAscList)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE (decodeUtf8)
import           Data.Time (getCurrentTime)
import           GHC.Conc (threadStatus)
import           GHC.Stack (HasCallStack)
import           Numeric (readInt)
import           Prelude hiding (pi)
import           Servant.Auth.Server (generateKey)
import           System.CPUTime (getCPUTime)
import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Environment (getEnvironment)
import           System.IO (hClose, hGetBuffering, openTempFile)
import           Unsafe.Coerce (unsafeCoerce)


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Cmds.Debug"


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


debugCmds :: HasCallStack => [Cmd]
debugCmds =
    [ mkDebugCmd "?"           debugDispCmdList ("\&" <> cmdDescDispCmdList)
    , mkDebugCmd "ap"          debugAp          "Show \"ActionParams\", including any arguments you provide."
    , mkDebugCmd "ayt"         debugAYT         "Send IAC AYT."
    , mkDebugCmd "bonus"       debugBonus       "Calculate experience bonus for a given PC."
    , mkDebugCmd "boot"        debugBoot        "Boot all players (including yourself)."
    , mkDebugCmd "broadcast"   debugBcast       "Broadcast a multi-line message to yourself."
    , mkDebugCmd "buffer"      debugBuffCheck   "Confirm the default buffering mode for file handles."
    , mkDebugCmd "cins"        debugCins        "Dump all channel ID/names for a given player ID."
    , mkDebugCmd "coercepenny" debugCoercePenny "Coerce Penny."
    , mkDebugCmd "color"       debugColor       "Perform a color test."
    , mkDebugCmd "cores"       debugCores       "Display the number of processor cores."
    , mkDebugCmd "cpu"         debugCPU         "Display the CPU time."
    , mkDebugCmd "currytime"   debugCurryTime   "Display a given number of seconds in Curry Time."
    , mkDebugCmd "echowill"    debugEchoWill    "Send IAC WILL ECHO (hide user input)."
    , mkDebugCmd "echowon't"   debugEchoWon't   "Send IAC WON'T ECHO (show user input)."
    , mkDebugCmd "effect"      debugEffect      "Add 10-20 to your ST for 30 seconds."
    , mkDebugCmd "env"         debugEnv         "Display or regex search system environment variables."
    , mkDebugCmd "exp"         debugExp         "Award yourself 5,000 exp."
    , mkDebugCmd "fun"         debugFun         "Dump the keys of the function tables."
    , mkDebugCmd "gmcpdo"      debugGmcpDo      "Send IAC DO GMCP."
    , mkDebugCmd "gmcproom"    debugGmcpRm      "Send GMCP Room Info."
    , mkDebugCmd "gmcpvitals"  debugGmcpVitals  "Send GMCP Vitals."
    , mkDebugCmd "gmcpwill"    debugGmcpWill    "Send IAC WILL GMCP."
    , mkDebugCmd "handle"      debugHandle      "Display information about the handle for your network connection."
    , mkDebugCmd "id"          debugId          "Search the \"MudState\" tables for a given ID."
    , mkDebugCmd "jwt"         debugJWT         "Generate a JWT key and serialize it to JSON."
    , mkDebugCmd "keys"        debugKeys        "Dump a list of \"MudState\" \"IntMap\" keys."
    , mkDebugCmd "liquid"      debugLiq         "Consume a given amount (in mouthfuls) of a given liquid (by distinct \
                                                \liquid ID)."
    , mkDebugCmd "log"         debugLog         "Put the logging service under heavy load."
    , mkDebugCmd "missing"     debugMissing     "Attempt to look up the ent description of a nonexistent ID."
    , mkDebugCmd "mkkewpie"    debugMkKewpie    "Create a kewpie doll."
    , mkDebugCmd "multiline"   debugMultiLine   "Test multi-line input."
    , mkDebugCmd "nop"         debugNOP         "Send IAC NOP."
    , mkDebugCmd "npcserver"   debugNpcServer   "Stop all NPC server threads."
    , mkDebugCmd "number"      debugNumber      "Display the decimal equivalent of a given number in a given base."
    , mkDebugCmd "out"         debugOut         "Dump the inventory of the logged out room."
    , mkDebugCmd "pause"       debugPause       "Test the pause interp."
    , mkDebugCmd "persist"     debugPersist     "Attempt to persist the world multiple times in quick succession."
    , mkDebugCmd "pidge"       debugPidge       "Send a message to Pidge."
    , mkDebugCmd "pmf"         debugPmf         "Trigger a pattern match failure."
    , mkDebugCmd "propnames"   debugPropNames   "Initialize the proper names table."
    , mkDebugCmd "purge"       debugPurge       "Purge the thread tables."
    , mkDebugCmd "random"      debugRandom      "Display random numbers generated with \"rndmRs\" and \"rndmInts\"."
    , mkDebugCmd "regen"       debugRegen       "Display regen amounts and delays for a given mob ID."
    , mkDebugCmd "remput"      debugRemPut      "In quick succession, remove from and put into a sack on the ground."
    , mkDebugCmd "rndm"        debugRndm        "Display or regex search the random names master table."
    , mkDebugCmd "rnt"         debugRnt         "Dump your random names table, or generate a random name for a given PC."
    , mkDebugCmd "rotate"      debugRotate      "Send the signal to rotate your player log."
    , mkDebugCmd "rules"       debugRules       "Display the rules message."
    , mkDebugCmd "shiver"      debugShiver      "Test the spiritize shiver random do."
    , mkDebugCmd "stopeffects" debugStopEffects "Stop all effects for your PC."
    , mkDebugCmd "talk"        debugTalk        "Dump the talk async table."
    , mkDebugCmd "tele"        debugTele        "Display or regex search the telepathic links master table."
    , mkDebugCmd "threads"     debugThreads     "Display or regex search the thread table."
    , mkDebugCmd "throw"       debugThrow       "Throw an exception."
    , mkDebugCmd "throwlog"    debugThrowLog    "Throw an exception on your player log thread."
    , mkDebugCmd "tinnitus"    debugTinnitus    "Ringing in the ears."
    , mkDebugCmd "token"       debugToken       "Test token parsing."
    , mkDebugCmd "underline"   debugUnderline   "Test underlining."
    , mkDebugCmd "volume"      debugVolume      "Calculate carried volume for a given mob ID."
    , mkDebugCmd "weight"      debugWeight      "Calculate weight for a given ID."
    , mkDebugCmd "words"       debugWords       "Initialize the words table."
    , mkDebugCmd "wrap"        debugWrap        "Test the wrapping of a line containing ANSI escape sequences."
    , mkDebugCmd "wrapindent"  debugWrapIndent  "Test the indented wrapping of a line containing ANSI escape \
                                                \sequences." ]
  where {- -}


mkDebugCmd :: HasCallStack => Text -> ActionFun -> CmdDesc -> Cmd
mkDebugCmd (prefixDebugCmd -> cn) f cd = Cmd { cmdName           = cn
                                             , cmdPriorityAbbrev = Nothing
                                             , cmdFullName       = cn
                                             , cmdAction         = Action f True
                                             , cmdDesc           = cd }


-----


debugAp :: HasCallStack => ActionFun
debugAp p@(WithArgs i mq cols _) = logPlaExec (prefixDebugCmd "ap") i >> wrapSend mq cols (showTxt p)
debugAp p                        = pmf "debugAp" p


-----


debugAYT :: HasCallStack => ActionFun
debugAYT (NoArgs' i mq) = logPlaExec (prefixDebugCmd "ayt") i >> send mq (T.pack [ telnetIAC, telnetAYT ]) >> ok mq
debugAYT p              = withoutArgs debugAYT p


-----


debugBcast :: HasCallStack => ActionFun
debugBcast (NoArgs'' i) = logPlaExec (prefixDebugCmd "broadcast") i >> bcastNl (mkBcast i msg)
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


debugBonus :: HasCallStack => ActionFun
debugBonus p@AdviseNoArgs                               = advise p [] adviceDBonusNoArgs
debugBonus   (OneArgNubbed i mq cols (capitalize -> a)) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryPCName $ a
        found match = let bonus = calcBonus (getIdForPCSing match ms) ms
                      in sequence_ [ logPlaExec (prefixDebugCmd "bonus") i, send mq . nlnl . commaShow $ bonus ]
        pcSings     = [ getSing pcId ms | pcId <- views pcTbl IM.keys ms ]
    in findFullNameForAbbrev a pcSings |&| maybe notFound found
debugBonus p = advise p [] adviceDBonusExcessArgs


-----


debugBoot :: HasCallStack => ActionFun
debugBoot (NoArgs' i mq) = logPlaExec (prefixDebugCmd "boot") i >> ok mq >> massMsg (MsgBoot dfltBootMsg)
debugBoot p              = withoutArgs debugBoot p


-----


debugBuffCheck :: HasCallStack => ActionFun
debugBuffCheck (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "buffer") i
                                       helper |&| try >=> eitherRet (logAndDispIOEx mq cols "debugBuffCheck")
  where
    helper = liftIO (flip openTempFile "temp" =<< getTemporaryDirectory) >>= \(fn, h) -> do
        send mq . nl =<< [ T.unlines . wrapIndent 2 cols $ msg | msg <- mkMsg fn <$> liftIO (hGetBuffering h) ]
        liftIO $ hClose h >> removeFile fn
    mkMsg (dblQuote . T.pack -> fn) (dblQuote . showTxt -> mode) =
        T.concat [ "Default buffering mode for temp file ", fn, " is ", mode, "." ]
debugBuffCheck p = withoutArgs debugBuffCheck p


-----


debugCins :: HasCallStack => ActionFun
debugCins p@AdviseNoArgs         = advise p [] adviceDCinsNoArgs
debugCins   (OneArg i mq cols a) = getState >>= \ms -> case reads . T.unpack $ a :: [(Int, String)] of
  [(targetId, "")] -> helper ms targetId
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper ms targetId@(showTxt -> targetIdTxt)
      | targetId < 0                             = wrapSend mq cols sorryWtf
      | views pcTbl (targetId `IM.notMember`) ms = wrapSend mq cols . sorryNonexistentId targetId . pure $ "PC"
      | otherwise = do logPlaExecArgs (prefixDebugCmd "cins") (pure a) i
                       multiWrapSend mq cols . (header :) . pure . showTxt =<< getAllChanIdNames i ms
      where
        header = T.concat [ "All channel ID/names "
                          , parensQuote "IM.IntMap [(Id, Text)]"
                          , " for ID "
                          , targetIdTxt
                          , ":" ]
debugCins p = advise p [] adviceDCinsExcessArgs


-----


class Myモルモット a where {}; instance Myモルモット Bool where {}
data Penny = forall a. (Myモルモット a) => Pennyちゃん a


debugCoercePenny :: HasCallStack => ActionFun
debugCoercePenny (NoArgs' i mq) = let penny        = Pennyちゃん True
                                      coercedPenny = coercePenny penny
                                  in do logPlaExec (prefixDebugCmd "coercepenny") i
                                        send mq . nlnl $ "Coerced Penny: " <> showTxt coercedPenny
  where
    coercePenny :: HasCallStack => Penny -> Bool
    coercePenny (Pennyちゃん a) = unsafeCoerce a
debugCoercePenny p = withoutArgs debugCoercePenny p


-----


debugColor :: HasCallStack => ActionFun
debugColor (NoArgs' i mq) = sequence_ [ logPlaExec (prefixDebugCmd "color") i, send mq . nl . T.concat $ msg ]
  where
    msg :: HasCallStack => [] Text
    msg = [ nl $ pad 15 (showTxt ansi) <> mkColorDesc fg bg <> colorWith ansi (spaced "CurryMUD")
          | fgi <- intensities, fgc <- colors
          , bgi <- intensities, bgc <- colors
          , let fg = (fgi, fgc), let bg = (bgi, bgc), let ansi = mkColorANSI fg bg ]
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName = (<>) <$> pad 6 . showTxt <*> padColorName . showTxt
debugColor p = withoutArgs debugColor p


-----


debugCores :: HasCallStack => ActionFun
debugCores (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "cores") i
    wrapSend mq cols =<< [ T.concat [ showTxt cores, " processor core", sOnNon1 cores, "." ]
                         | cores <- liftIO . safePerformIO $ getNumCapabilities ]
debugCores p = withoutArgs debugCores p


-----


debugCPU :: HasCallStack => ActionFun
debugCPU (NoArgs i mq cols) = let cpuTime = showTxt . (`divide` 10 ^ 12) <$> getCPUTime in do
    logPlaExec (prefixDebugCmd "cpu") i
    wrapSend mq cols =<< [ "CPU time: " <> time | time <- liftIO . safePerformIO $ cpuTime ]
debugCPU p = withoutArgs debugCPU p


-----


debugCurryTime :: HasCallStack => ActionFun
debugCurryTime (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(secs, "")] -> helper secs
  _            -> wrapSend mq cols . sorryParseSeconds $ a
  where
    helper x | x < 0     = wrapSend mq cols sorryWtf
             | otherwise = do logPlaExecArgs (prefixDebugCmd "currytime") (pure a) i
                              multiWrapSend mq cols . showCurryTime . secsToCurryTime $ x
debugCurryTime p = advise p [] adviceDCurryTimeExcessArgs


-----


debugDispCmdList :: HasCallStack => ActionFun
debugDispCmdList p@(LowerNub' i as) = logPlaExecArgs (prefixDebugCmd "?") as i >> dispCmdList debugCmds p
debugDispCmdList p                  = pmf "debugDispCmdList" p


-----


debugEchoWill :: HasCallStack => ActionFun
debugEchoWill (NoArgs' i mq) = do logPlaExec (prefixDebugCmd "echowill") i
                                  send mq . T.pack $ [ telnetIAC, telnetWILL, telnetECHO ]
                                  ok mq
debugEchoWill p              = withoutArgs debugEchoWill p


-----


debugEchoWon't :: HasCallStack => ActionFun
debugEchoWon't (NoArgs' i mq) = do logPlaExec (prefixDebugCmd "echowon't") i
                                   send mq . T.pack $ [ telnetIAC, telnetWON'T, telnetECHO ]
                                   ok mq
debugEchoWon't p              = withoutArgs debugEchoWon't p


-----


debugEffect :: HasCallStack => ActionFun
debugEffect (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "effect") i
    ok mq
    let tag     = Just "debugEffect"
        effSub  = MobEffectAttrib St
        effVal  = Just . EffectRangedVal $ (10, 20)
        effDur  = 30
        effFeel = Nothing
    startEffect i . Effect tag effSub effVal effDur $ effFeel
debugEffect p = withoutArgs debugEffect p


-----


debugEnv :: HasCallStack => ActionFun
debugEnv (NoArgs   i mq cols   ) = do logPlaExecArgs (prefixDebugCmd "env") [] i
                                      pager i mq Nothing =<< [ concatMap (wrapIndent 2 cols) . mkEnvListTxt $ env
                                                             | env <- liftIO . safePerformIO $ getEnvironment ]
debugEnv (WithArgs i mq cols as) = do
    logPlaExecArgs (prefixDebugCmd "env") as i
    dispMatches i mq cols 2 IsRegex as =<< [ mkEnvListTxt env | env <- liftIO . safePerformIO $ getEnvironment ]
debugEnv p = pmf "debugEnv" p


mkEnvListTxt :: HasCallStack => [(String, String)] -> [Text]
mkEnvListTxt = map (uncurry (<>) . first (<> ": ") . (both %~ T.pack))


-----


debugExp :: HasCallStack => ActionFun
debugExp (NoArgs' i mq) = let cn = prefixDebugCmd "exp" in do logPlaExec cn i
                                                              awardExp 5000 ("executed " <> dblQuote cn) i
                                                              ok mq
debugExp p              = withoutArgs debugExp p


-----


debugFun :: HasCallStack => ActionFun
debugFun (NoArgs i mq cols) = getState >>= \ms ->
    let helper t lens = t <> ":" : views lens (S.toAscList . M.keysSet) ms
        tss           = [ helper "EffectFunTbl"      effectFunTbl
                        , helper "FeelingFunTbl"     feelingFunTbl
                        , helper "FunTbl"            funTbl
                        , helper "HookFunTbl"        hookFunTbl
                        , helper "InstaEffectFunTbl" instaEffectFunTbl
                        , helper "RmActionFunTbl"    rmActionFunTbl ]
    in do logPlaExec (prefixDebugCmd "fun") i
          pager i mq Nothing . concatMap (wrapIndent 2 cols) . intercalateDivider cols $ tss
debugFun p = withoutArgs debugFun p


-----


debugGmcpDo :: HasCallStack => ActionFun
debugGmcpDo (NoArgs' i mq) = do logPlaExec (prefixDebugCmd "gmcpdo") i
                                send mq . T.pack $ [ telnetIAC, telnetDO, telnetGMCP ]
                                ok mq
debugGmcpDo p              = withoutArgs debugGmcpDo p


-----


debugGmcpRm :: HasCallStack => ActionFun
debugGmcpRm (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "gmcproom") i
    ((>>) <$> wrapSend mq cols . gmcpRmInfo Nothing i <*> sendGmcpRmInfo Nothing i) =<< getState
debugGmcpRm p = withoutArgs debugGmcpRm p


-----


debugGmcpVitals :: HasCallStack => ActionFun
debugGmcpVitals (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "gmcpvitals") i
                                        ((>>) <$> wrapSend mq cols . gmcpVitals i <*> sendGmcpVitals i) =<< getState
debugGmcpVitals p                  = withoutArgs debugGmcpVitals p


-----


debugGmcpWill :: HasCallStack => ActionFun
debugGmcpWill (NoArgs' i mq) = do logPlaExec (prefixDebugCmd "gmcpwill") i
                                  send mq . T.pack $ [ telnetIAC, telnetWILL, telnetGMCP ]
                                  ok mq
debugGmcpWill p              = withoutArgs debugGmcpWill p


-----


debugHandle :: HasCallStack => ActionFun
debugHandle (NoArgs' i mq) = logPlaExec (prefixDebugCmd "handle") i >> writeMsg mq ShowHandle
debugHandle p              = withoutArgs debugHandle p


-----


debugId :: HasCallStack => ActionFun
debugId p@AdviseNoArgs         = advise p [] adviceDIdNoArgs
debugId   (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(searchId, "")] -> helper searchId
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper searchId@(showTxt -> searchIdTxt)
      | searchId < 0 = wrapSend mq cols sorryWtf
      | otherwise    = getState >>= \ms -> do
          logPlaExecArgs (prefixDebugCmd "id") (pure a) i
          let f     = commas . map showTxt
              mkTxt =
                  [ [ "Tables containing key " <> searchIdTxt <> ":"
                    , commas . map fst . filter ((searchId `elem`) . snd) . mkTblNameKeysList $ ms ]
                  , [ T.concat [ "Channels with a ", dblQuote "chanId", " of ", searchIdTxt, ":" ]
                    , f . views chanTbl (IM.keys . IM.filter (views chanId (== searchId))) $ ms ]
                  , [ T.concat [ "Entities with an ", dblQuote "entId", " of ", searchIdTxt, ":" ]
                    , f . views entTbl (IM.keys . IM.filter (views entId (== searchId))) $ ms ]
                  , [ T.concat [ "Foods with a ", dblQuote "foodId", " of ", searchIdTxt, ":" ]
                    , f . views foodTbl (IM.keys . IM.filter (views foodId (== DistinctFoodId searchId))) $ ms ]
                  , [ T.concat [ "Equipment maps containing ID ", searchIdTxt, ":" ]
                    , f . views eqTbl (IM.keys . IM.filter ((searchId `elem`) . M.elems)) $ ms ]
                  , [ T.concat [ "Inventories containing ID ", searchIdTxt, ":" ]
                    , f . views invTbl (IM.keys . IM.filter (searchId `elem`)) $ ms ]
                  , [ T.concat [ "Mobiles with a ", dblQuote "rmId", " of ", searchIdTxt, ":" ]
                    , f . views mobTbl (IM.keys . IM.filter (views rmId (== searchId))) $ ms ]
                  , [ T.concat [ "Mobiles with a ", dblQuote "lastRmId", " of ", searchIdTxt, ":" ]
                    , f . views mobTbl (IM.keys . IM.filter (views lastRmId (== searchId))) $ ms ]
                  , [ T.concat [ "Mobiles following ID ", searchIdTxt, ":" ]
                    , f . views mobTbl (IM.keys . IM.filter (views (party.following) (== Just searchId))) $ ms ]
                  , [ T.concat [ "Mobiles followed by ID ", searchIdTxt, ":" ]
                    , f . views mobTbl (IM.keys . IM.filter (views (party.followers) (searchId `elem`))) $ ms ]
                  , [ T.concat [ "Mobiles whose group includes ID ", searchIdTxt, ":" ]
                    , f . views mobTbl (IM.keys . IM.filter (views (party.myGroup) (searchId `elem`))) $ ms ]
                  , [ T.concat [ "Mobiles who are a member of ID ", searchIdTxt, "'s group:" ]
                    , f . views mobTbl (IM.keys . IM.filter (views (party.memberOf) (== Just searchId))) $ ms ]
                  , [ T.concat [ "Mobiles whose stomach contains ", dblQuote "DistinctFoodId", " ", searchIdTxt, ":" ]
                    , let g = filter $ views distinctId (== (Right . DistinctFoodId $ searchId))
                      in f . views mobTbl (IM.keys . IM.filter (views stomach ((()!#) . g))) $ ms ]
                  , [ T.concat [ "Mobiles whose stomach contains ", dblQuote "DistinctLiqId", " ", searchIdTxt, ":" ]
                    , let g = filter $ views distinctId (== (Left . DistinctLiqId $ searchId))
                      in f . views mobTbl (IM.keys . IM.filter (views stomach ((()!#) . g))) $ ms ]
                  , [ T.concat [ "NPCs possessed by ID ", searchIdTxt, ":" ]
                    , f . views npcTbl (IM.keys . IM.filter (views npcPossessor (searchId `elem`))) $ ms ]
                  , [ T.concat [ "PC ", dblQuote "sing", "s with an ID of ", searchIdTxt, " in the ", dblQuote "PCSingTbl", ":" ]
                    , views pcSingTbl (commas . M.keys . M.filter (== searchId)) ms ]
                  , [ T.concat [ "Players peeped by ID ", searchIdTxt, ":" ]
                    , f . views plaTbl (IM.keys . IM.filter (views peepers (searchId `elem`))) $ ms ]
                  , [ T.concat [ "Players peeping ID ", searchIdTxt, ":" ]
                    , f . views plaTbl (IM.keys . IM.filter (views peeping (searchId `elem`))) $ ms ]
                  , [ T.concat [ "Players possessing ID ", searchIdTxt, ":" ]
                    , f . views plaTbl (IM.keys . IM.filter (views possessing (searchId `elem`))) $ ms ]
                  , [ T.concat [ "Players with a ", dblQuote "logoutRmId", " of ", searchIdTxt, ":" ]
                    , f . views plaTbl (IM.keys . IM.filter (views logoutRmId (searchId `elem`))) $ ms ]
                  , [ T.concat [ "Rooms with a ", dblQuote "StdLink", " to ID ", searchIdTxt, ":" ]
                    , let g (StdLink _ di _) = di == searchId
                          g _                = False
                      in f . views rmTbl (IM.keys . IM.filter (views rmLinks ((()!#) . filter g))) $ ms ]
                  , [ T.concat [ "Rooms with a ", dblQuote "NonStdLink", " to ID ", searchIdTxt, ":" ]
                    , let g (NonStdLink _ di _ _ _) = di == searchId
                          g _                       = False
                      in f . views rmTbl (IM.keys . IM.filter (views rmLinks ((()!#) . filter g))) $ ms ]
                  , [ T.concat [ "Vessels containing ", dblQuote "liqId", " ", searchIdTxt, ":" ]
                    , let g = views vesselCont (maybe False (views liqId (== DistinctLiqId searchId) . fst))
                      in f . views vesselTbl (IM.keys . IM.filter g) $ ms ] ]
          pager i mq Nothing . concat . wrapLines cols . intercalate mMempty $ mkTxt
debugId p = advise p [] adviceDIdExcessArgs


mkTblNameKeysList :: HasCallStack => MudState -> [(Text, Inv)]
mkTblNameKeysList ms = [ ("Arm",                 tblKeys armTbl                 ms)
                       , ("Chan",                tblKeys chanTbl                ms)
                       , ("Cloth",               tblKeys clothTbl               ms)
                       , ("Coins",               tblKeys coinsTbl               ms)
                       , ("Con",                 tblKeys conTbl                 ms)
                       , ("Corpse",              tblKeys corpseTbl              ms)
                       , ("CorpseDecompAsync",   tblKeys corpseDecompAsyncTbl   ms)
                       , ("DistinctFood",        tblKeys distinctFoodTbl        ms)
                       , ("DistinctLiq",         tblKeys distinctLiqTbl         ms)
                       , ("DurationalEffect",    tblKeys durationalEffectTbl    ms)
                       , ("Ent",                 tblKeys entTbl                 ms)
                       , ("Eq",                  tblKeys eqTbl                  ms)
                       , ("Food",                tblKeys foodTbl                ms)
                       , ("HolySymbol",          tblKeys holySymbolTbl          ms)
                       , ("Inv",                 tblKeys invTbl                 ms)
                       , ("Mob",                 tblKeys mobTbl                 ms)
                       , ("MsgQueue",            tblKeys msgQueueTbl            ms)
                       , ("Npc",                 tblKeys npcTbl                 ms)
                       , ("Obj",                 tblKeys objTbl                 ms)
                       , ("PausedCorpseDecomps", tblKeys pausedCorpseDecompsTbl ms)
                       , ("PausedEffect",        tblKeys pausedEffectTbl        ms)
                       , ("PC",                  tblKeys pcTbl                  ms)
                       , ("PickPts",             tblKeys pickPtsTbl             ms)
                       , ("PlaLog",              tblKeys plaLogTbl              ms)
                       , ("Pla",                 tblKeys plaTbl                 ms)
                       , ("Rm",                  tblKeys rmTbl                  ms)
                       , ("RmTeleName",          tblKeys rmTeleNameTbl          ms)
                       , ("RndmNamesMstr",       tblKeys rndmNamesMstrTbl       ms)
                       , ("TeleLinkMstr",        tblKeys teleLinkMstrTbl        ms)
                       , ("Type",                tblKeys typeTbl                ms)
                       , ("Vessel",              tblKeys vesselTbl              ms)
                       , ("Wpn",                 tblKeys wpnTbl                 ms)
                       , ("Writable",            tblKeys writableTbl            ms) ]


tblKeys :: HasCallStack => Optical (->) (->) (Const Inv) MudState MudState (IM.IntMap a) (IM.IntMap a) -> MudState -> Inv
tblKeys lens = views lens IM.keys


-----


debugJWT :: HasCallStack => ActionFun
debugJWT (NoArgs i mq cols) = do
    logPlaExec (prefixDebugCmd "jwt") i
    wrapSend mq cols . LT.toStrict . LTE.decodeUtf8 . encode . toJSON =<< liftIO generateKey
debugJWT p = withoutArgs debugJWT p


-----


debugKeys :: HasCallStack => ActionFun
debugKeys (NoArgs i mq cols) = getState >>= \ms -> let mkKeysTxt (tblName, ks) = [ tblName <> ": ", showTxt ks ] in do
    logPlaExec (prefixDebugCmd "keys") i
    pager i mq Nothing . concat . wrapLines cols . intercalate mMempty . map mkKeysTxt . mkTblNameKeysList $ ms
debugKeys p = withoutArgs debugKeys p


-----


debugLiq :: HasCallStack => ActionFun
debugLiq p@AdviseNoArgs            = advise p [] adviceDLiqNoArgs
debugLiq p@AdviseOneArg            = advise p [] adviceDLiqNoId
debugLiq   (WithArgs i mq cols as) = getState >>= \ms ->
    parseTwoIntArgs mq cols as sorryParseAmt sorryParseId (helper ms)
  where
    helper ms amt di
      | amt < 1 || di < 0                           = wrapSend mq cols sorryWtf
      | views distinctLiqTbl (di `IM.notMember`) ms = wrapSend mq cols . sorryNonexistentId di . pure $ "distinct liquid"
      | otherwise = do logPlaExecArgs (prefixDebugCmd "liquid") as i
                       ok mq
                       onNewThread (consume i =<< mkStomachConts <$> liftIO getCurrentTime)
      where
        mkStomachConts now = replicate amt . StomachCont (Left . DistinctLiqId $ di) now $ False
debugLiq p = advise p [] adviceDLiqExcessArgs


parseTwoIntArgs :: HasCallStack => MsgQueue
                                -> Cols
                                -> [Text]
                                -> (Text -> Text)
                                -> (Text -> Text)
                                -> (Int -> Int -> MudStack ())
                                -> MudStack ()
parseTwoIntArgs mq cols [a, b] sorryParseA sorryParseB helper =
    map getSum . catMaybes <$> mapM parse [ (a, sorryParseA), (b, sorryParseB) ] >>= \case [x, y] -> helper x y
                                                                                           _      -> unit
  where
    parse :: HasCallStack => (Text, Text -> Text) -> MudStack (Maybe (Sum Int))
    parse (txt, sorry) = case reads . T.unpack $ txt :: [(Int, String)] of
      [(x, "")] -> unadulterated . Sum $ x
      _         -> emptied . wrapSend mq cols . sorry $ txt
parseTwoIntArgs _ _ as _ _ _ = pmf "parseTwoIntArgs" as


-----


debugLog :: HasCallStack => ActionFun
debugLog (NoArgs' i mq) = logPlaExec (prefixDebugCmd "log") i >> helper >> ok mq
  where
    helper       = replicateM_ 100 . onNewThread $ heavyLogging
    heavyLogging = replicateM_ 100 . logNotice "debugLog heavyLogging" =<< mkMsg
    mkMsg        = [ prd $ "Logging from " <> ti | ti <- showTxt <$> liftIO myThreadId ]
debugLog p = withoutArgs debugLog p


-----


debugMissing :: HasCallStack => ActionFun
debugMissing (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "missing") i
                                     wrapSend mq cols =<< getEntDesc (-1) <$> getState
debugMissing p                  = withoutArgs debugMissing p


-----


debugMkKewpie :: HasCallStack => ActionFun
debugMkKewpie (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "mkkewpie") i
    modifyStateSeq $ \ms -> let et = EntTemplate (Just "doll")
                                                 "kewpie doll" ""
                                                 "The kewpie doll is disgustingly cute."
                                                 Nothing
                                                 zeroBits
                                ot = ObjTemplate dollWeight
                                                 dollVol
                                                 Nothing
                                                 zeroBits
                            in second (++ pure (ok mq)) . dropFst . newObj ms et ot . getRmId i $ ms
debugMkKewpie p = withoutArgs debugMkKewpie p


-----


debugMultiLine :: HasCallStack => ActionFun
debugMultiLine (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "multiline") i
                                       wrapSend1Nl mq cols . thrice prd $ "Beginning multi-line input"
                                       setInterp i . Just . interpMutliLine f $ []

  where
    f ts = do multiWrapSend mq cols $ nlPrefix "You entered:" : ts
              sendDfltPrompt mq i
              resetInterp i
debugMultiLine p = withoutArgs debugMultiLine p


-----


debugNOP :: HasCallStack => ActionFun
debugNOP (NoArgs' i mq) = do logPlaExec (prefixDebugCmd "nop") i
                             send mq . T.pack $ [ telnetIAC, telnetNOP ]
                             ok mq
debugNOP p              = withoutArgs debugNOP p


-----


debugNpcServer :: HasCallStack => ActionFun
debugNpcServer (NoArgs' i mq) = logPlaExec (prefixDebugCmd "npcserver") i >> stopNpcServers >> ok mq
debugNpcServer p              = withoutArgs debugNpcServer p


-----


type Base = Int


debugNumber :: HasCallStack => ActionFun
debugNumber p@AdviseNoArgs                             = advise p [] adviceDNumberNoArgs
debugNumber p@AdviseOneArg                             = advise p [] adviceDNumberNoBase
debugNumber   (WithArgs i mq cols [ numTxt, baseTxt ]) = case reads . T.unpack $ baseTxt :: [(Base, String)] of
      [(base, "")] | not . inRange (2, 36) $ base -> wrapSend mq cols . sorryParseBase $ baseTxt
                   | otherwise -> case numTxt `inBase` base of
                     [(res, "")] -> do logPlaExecArgs (prefixDebugCmd "number") [ numTxt, baseTxt ] i
                                       send mq $ fmap nlnl showTxt res
                     _ -> wrapSend mq cols . sorryParseNum numTxt . showTxt $ base
      _ -> wrapSend mq cols . sorryParseBase $ baseTxt
debugNumber p = advise p [] adviceDNumberExcessArgs


inBase :: HasCallStack => Text -> Base -> [(Int, String)]
numTxt `inBase` base = readInt base (isValidDigit base) letterToNum . T.unpack $ numTxt


isValidDigit :: HasCallStack => Base -> Char -> Bool
isValidDigit base (toLower -> c) | isDigit c                    = digitToInt c < base
                                 | not . inRange ('a', 'z') $ c = False
                                 | otherwise                    = maybe False (<= base) . lookup c . zip a $ b
  where { a = 'a' `enumFromTo` 'z'; b = enumFrom 11 }


letterToNum :: HasCallStack => Char -> Int
letterToNum c | isDigit c = digitToInt c
              | otherwise = let (🍪) = (-) `on` ord in (c 🍪 'a') + 10


-----


debugOut :: HasCallStack => ActionFun
debugOut (NoArgs i mq cols) = getState >>= \ms -> do logPlaExec (prefixDebugCmd "out") i
                                                     wrapSend mq cols . showTxt . getInv iLoggedOut $ ms
debugOut p                  = withoutArgs debugOut p


-----


debugPause :: HasCallStack => ActionFun
debugPause (NoArgs' i mq) = logPlaExec (prefixDebugCmd "pause") i >> pause i mq Nothing
debugPause p              = withoutArgs debugPause p


-----


debugPersist :: HasCallStack => ActionFun
debugPersist (NoArgs' i mq) = logPlaExec (prefixDebugCmd "persist") i >> replicateM_ 10 (persist >> ok mq)
debugPersist p              = withoutArgs debugPersist p


-----


debugPidge :: HasCallStack => ActionFun
debugPidge (NoArgs' i mq) = serialize . flip (mkStdDesig i) Don'tCap <$> getState >>= \d -> do
    logPlaExec (prefixDebugCmd "pidge") i
    bcastNl . mkBcast iPidge $ "Greetings from " <> d <> "!"
    ok mq
debugPidge p = withoutArgs debugPidge p


-----


debugPmf :: HasCallStack => ActionFun
debugPmf (NoArgs'' i) = logPlaExec (prefixDebugCmd "pmf") i >> pmf "debugPmf" (0, 'a', "abcdef")
debugPmf p            = withoutArgs debugPmf p


-----


debugPropNames :: HasCallStack => ActionFun
debugPropNames (NoArgs' i mq) = logPlaExec (prefixDebugCmd "propnames") i >> initPropNamesTbl >> ok mq
debugPropNames p              = withoutArgs debugPropNames p


-----


debugPurge :: HasCallStack => ActionFun
debugPurge (NoArgs' i mq) = logPlaExec (prefixDebugCmd "purge") i >> purgeThreadTbls >> ok mq
debugPurge p              = withoutArgs debugPurge p


-----


debugRandom :: HasCallStack => ActionFun
debugRandom (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "random") i
                                    wrapSend mq cols . showTxt =<< rndmRs   10 (0, 99)
                                    wrapSend mq cols . showTxt =<< rndmInts 10
debugRandom p                  = withoutArgs debugRandom p


-----


debugRegen :: HasCallStack => ActionFun
debugRegen p@AdviseNoArgs         = advise p [] adviceDRegenNoArgs
debugRegen   (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(targetId, "")] -> helper targetId =<< getState
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper targetId ms
      | targetId < 0 = wrapSend mq cols sorryWtf
      | targetId `notElem` ((++) <$> views npcTbl IM.keys <*> views pcTbl IM.keys) ms =
          wrapSend mq cols . sorryNonexistentId targetId $ [ "NPC", "PC" ]
      | otherwise = logPlaExecArgs (prefixDebugCmd "regen") (pure a) i >> multiWrapSend mq cols descRegens
      where
        descRegens = map (uncurry3 descRegen) [ ("hp", calcRegenHpAmt, calcRegenHpDelay)
                                              , ("mp", calcRegenMpAmt, calcRegenMpDelay)
                                              , ("pp", calcRegenPpAmt, calcRegenPpDelay)
                                              , ("fp", calcRegenFpAmt, calcRegenFpDelay) ]
        descRegen t calcAmt calcDelay = T.concat [ t
                                                 , ": "
                                                 , showTxt . calcAmt   targetId $ ms
                                                 , " / "
                                                 , showTxt . calcDelay targetId $ ms
                                                 , " sec" ]
debugRegen p = advise p [] adviceDRegenExcessArgs


-----


debugRemPut :: HasCallStack => ActionFun
debugRemPut (NoArgs' i mq) = do logPlaExec (prefixDebugCmd "remput") i
                                mapM_ (fakeClientInput mq) . take 10 . cycle . map (<> rest) $ [ "remove", "put" ]
  where
    rest = spaced (T.singleton allChar) <> ('r' `T.cons` selectorChar `T.cons` "sack")
debugRemPut p = withoutArgs debugRemPut p


-----


debugRndm :: HasCallStack => ActionFun
debugRndm (NoArgs i mq cols) = do
    logPlaExecArgs (prefixDebugCmd "rndm") [] i
    pager i mq Nothing . concatMap (wrapIndent 2 cols) . mkRndmNamesMstrTblTxt =<< getState
debugRndm (WithArgs i mq cols as) = do logPlaExecArgs (prefixDebugCmd "rndm") as i
                                       dispMatches i mq cols 2 IsRegex as . mkRndmNamesMstrTblTxt =<< getState
debugRndm p                       = pmf "debugRndm" p


mkRndmNamesMstrTblTxt :: HasCallStack => MudState -> [Text]
mkRndmNamesMstrTblTxt ms = sort . views rndmNamesMstrTbl helper $ ms
  where
    helper :: HasCallStack => RndmNamesMstrTbl -> [Text]
    helper = map f . IM.toList
      where
        f :: HasCallStack => (Id, RndmNamesTbl) -> Text
        f = uncurry (|<>|) . ((<> ":") . (`descSingId` ms) *** noneOnNull . commas . map (uncurry (|<>|)) . M.toList)


-----


debugRnt :: HasCallStack => ActionFun
debugRnt (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "rnt") i
                                 wrapSend mq cols . showTxt . M.toList . getRndmNamesTbl i =<< getState
debugRnt (OneArgNubbed i mq cols (capitalize -> a)) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryPCName $ a
        found match = do
            logPlaExec (prefixDebugCmd "rnt") i
            rndmName <- updateRndmName i . getIdForPCSing match $ ms
            wrapSend mq cols . T.concat $ [ dblQuote rndmName, " has been randomly generated for ", match, "." ]
        pcSings = [ getSing pcId ms | pcId <- views pcTbl IM.keys ms ]
    in findFullNameForAbbrev a pcSings |&| maybe notFound found
debugRnt p = advise p [] adviceDRntExcessArgs


-----


debugRotate :: HasCallStack => ActionFun
debugRotate (NoArgs' i mq) = getState >>= \ms -> let lq = getLogQueue i ms in do writeLog lq RotateLog
                                                                                 ok mq
                                                                                 logPlaExec (prefixDebugCmd "rotate") i
debugRotate p              = withoutArgs debugRotate p


-----


debugRules :: HasCallStack => ActionFun
debugRules (NoArgs i mq cols) = getServerSettings >>= \s -> do logPlaExec (prefixDebugCmd "rules") i'
                                                               pager i' mq Nothing . parseWrapXform s cols $ rulesMsg
  where
    i' = safeCoerce (i :: Id) :: Int
debugRules p = withoutArgs debugRules p


-----


debugShiver :: HasCallStack => ActionFun
debugShiver (NoArgs' i mq) = getState >>= \ms -> do
    logPlaExec (prefixDebugCmd "shiver") i
    replicateM_ 50 . rndmDo_ (calcProbSpiritizeShiver i ms) . mkExpAction "shiver" . mkActionParams i ms $ []
    ok mq
debugShiver p = withoutArgs debugShiver p


-----


debugStopEffects :: HasCallStack => ActionFun
debugStopEffects (NoArgs' i mq) = logPlaExec (prefixDebugCmd "stopeffects") i >> stopEffects i >> ok mq
debugStopEffects p              = withoutArgs debugStopEffects p


-----


debugTalk :: HasCallStack => ActionFun
debugTalk (NoArgs i mq cols) = views talkAsyncTbl M.toList <$> getState >>= \(pairs :: [(ThreadId, TalkAsync)]) -> do
    logPlaExec (prefixDebugCmd "talk") i
    pager i mq Nothing =<< [ concatMap (wrapIndent 2 cols) descs | descs <- mapM mkDesc pairs ]
  where
    mkDesc (tid, async) = [ T.concat [ showTxt tid, ": ", statusTxt, "." ]
                          | statusTxt <- mkStatusTxt <$> liftIO (poll async) ]
    mkStatusTxt = \case Nothing         -> "running"
                        Just (Left  e ) -> "exception " <> parensQuote (showTxt e)
                        Just (Right ()) -> "finished"
debugTalk p = withoutArgs debugTalk p


-----


debugTele :: HasCallStack => ActionFun
debugTele (NoArgs i mq cols) = do
    logPlaExecArgs (prefixDebugCmd "tele") [] i
    pager i mq Nothing . concatMap (wrapIndent 2 cols) . mkTeleLinkMstrTblTxt =<< getState
debugTele (WithArgs i mq cols as) = do logPlaExecArgs (prefixDebugCmd "tele") as i
                                       dispMatches i mq cols 2 IsRegex as . mkTeleLinkMstrTblTxt =<< getState
debugTele p                       = pmf "debugTele" p


mkTeleLinkMstrTblTxt :: HasCallStack => MudState -> [Text]
mkTeleLinkMstrTblTxt ms = sort . views teleLinkMstrTbl helper $ ms
  where
    helper :: HasCallStack => TeleLinkMstrTbl -> [Text]
    helper = map f . IM.toList
      where
        f :: HasCallStack => (Id, TeleLinkTbl) -> Text
        f = uncurry (|<>|) . ((<> ":") . (`descSingId` ms) *** noneOnNull . commas . map g . M.toList)
          where
            g :: HasCallStack => (Sing, IsTuned) -> Text
            g = uncurry (|<>|) . second showTxt


-----


debugThreads :: HasCallStack => ActionFun
debugThreads (NoArgs   i mq cols   ) = do logPlaExec (prefixDebugCmd "threads") i
                                          pager i mq Nothing . concatMap (wrapIndent 2 cols) =<< descThreads
debugThreads (WithArgs i mq cols as) = do logPlaExecArgs (prefixDebugCmd "threads") as i
                                          dispMatches i mq cols 2 IsRegex as =<< descThreads
debugThreads p                       = pmf "debugThreads" p


descThreads :: HasCallStack => MudStack [Text]
descThreads = do logAsyncKvs <- getLogThreadIds >>= \case [ a, b ] -> return [ (a, Notice), (b, Error) ]
                                                          _        -> mMempty
                 (plt, M.assocs -> threadTblKvs) <- plaLogTbl `fanView` threadTbl <$> getState
                 let plaLogTblKvs = [ (asyncThreadId . fst $ v, PlaLog k) | (k, v) <- IM.assocs plt ]
                 mapM mkDesc . sort . concat $ [ logAsyncKvs, threadTblKvs, plaLogTblKvs ]
  where
    mkDesc (ti, bracketPad 20 . mkTypeName -> tn) = [ T.concat [ padOrTrunc 16 . showTxt $ ti, tn, ts ]
                                                    | (showTxt -> ts) <- liftIO . threadStatus $ ti ]
    f pi t                            = padOrTrunc padAmt t <> showTxt pi
    mkTypeName (AttackingThread   pi) = f pi "Attacking"
    mkTypeName (Biodegrader       pi) = f pi "Biodegrader"
    mkTypeName (CorpseDecomposer  pi) = f pi "CorpseDecomp"
    mkTypeName (Digester          pi) = f pi "Digester"
    mkTypeName (DrinkingThread    pi) = f pi "Drinking"
    mkTypeName (EatingThread      pi) = f pi "Eating"
    mkTypeName (EffectListener    pi) = f pi "EffListen"
    mkTypeName (EffectThread      pi) = f pi "EffThread"
    mkTypeName (EffectTimer       pi) = f pi "EffTimer"
    mkTypeName (FeelingTimer      pi) = f pi "FeelingTimer"
    mkTypeName (InacTimer         pi) = f pi "InacTimer"
    mkTypeName (NpcServer         pi) = f pi "NpcServer"
    mkTypeName (PlaLog            pi) = f pi "PlaLog"
    mkTypeName (Receive           pi) = f pi "Receive"
    mkTypeName (RegenChild        pi) = f pi "RegenChild"
    mkTypeName (RegenParent       pi) = f pi "RegenParent"
    mkTypeName (RmFun             pi) = f pi "RmFun"
    mkTypeName (SacrificingThread pi) = f pi "Sacrificing"
    mkTypeName (Server            pi) = f pi "Server"
    mkTypeName (SpiritTimer       pi) = f pi "SpiritTimer"
    mkTypeName (Talk              pi) = f pi "Talk"
    mkTypeName (showTxt -> t        ) = t -- For thread types without an ID.
    padAmt                            = 13


-----


debugThrow :: HasCallStack => ActionFun
debugThrow (NoArgs'' i) = logPlaExec (prefixDebugCmd "throw") i >> throwIO DivideByZero
debugThrow p            = withoutArgs debugThrow p


-----


debugThrowLog :: HasCallStack => ActionFun
debugThrowLog (NoArgs' i mq) = getState >>= \ms -> let lq = getLogQueue i ms in
    logPlaExec (prefixDebugCmd "throwlog") i >> writeLog lq Throw >> ok mq
debugThrowLog p = withoutArgs debugThrowLog p


-----


debugTinnitus :: HasCallStack => ActionFun
debugTinnitus (NoArgs' i mq) = do
    logPlaExec (prefixDebugCmd "tinnitus") i
    ok mq
    let tag     = potTinnitusTag
        effSub  = EffectOther tag
        effVal  = Nothing
        effDur  = twoMinsInSecs
        effFeel = Just . EffectFeeling potTinnitusTag $ twoMinsInSecs
    startEffect i . Effect (Just tag) effSub effVal effDur $ effFeel
debugTinnitus p = withoutArgs debugTinnitus p


-----


debugToken :: HasCallStack => ActionFun
debugToken (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "token") i
                                   s <- getServerSettings
                                   multiWrapSend mq cols . T.lines . parseTokens s . T.unlines $ tokenTxts
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
                , charTokenDelimiter  `T.cons` "u multiLineEndChar"
                , charTokenDelimiter  `T.cons` "x emoteChar"
                -----
                , styleTokenDelimiter `T.cons` styleTokenDelimiter `T.cons` " literal styleTokenDelimiter"
                , styleTokenDelimiter `T.cons` ("aabbrevColor"       <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ddfltColor"         <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("hheaderColor"       <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("iasteriskColor"     <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("kknownNameColor"    <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("lselectorColor"     <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("mtempDescColor"     <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("nnoUnderlineANSI"   <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ounknownNameColor"  <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("pprefixColor"       <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("qquoteColor"        <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("rarrowColor"        <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("ssyntaxSymbolColor" <> dfltColorStyleToken    )
                , styleTokenDelimiter `T.cons` ("t " <> dfltColorStyleToken <> " <- toNpcColor")
                , styleTokenDelimiter `T.cons` ("uunderlineANSI"     <> noUnderlineStyleToken  )
                , styleTokenDelimiter `T.cons` ("zzingColor"         <> dfltColorStyleToken    )
                -----
                , miscTokenDelimiter `T.cons` miscTokenDelimiter `T.cons` " literal miscTokenDelimiter"
                , "dfltBootMsg: "      <> (miscTokenDelimiter `T.cons` "b")
                , "descRule5: "        <> "elided"
                , "isDebug: "          <> (miscTokenDelimiter `T.cons` "d")
                , "descRulesMsg: "     <> "elided"
                , "leadingSpaceChar: " <> (miscTokenDelimiter `T.cons` "l")
                , "dfltZoom: "         <> (miscTokenDelimiter `T.cons` "o")
                , "pwWarningMsg: "     <> (miscTokenDelimiter `T.cons` "p")
                , "rulesIntroMsg: "    <> (miscTokenDelimiter `T.cons` "r")
                , "dfltShutdownMsg: "  <> (miscTokenDelimiter `T.cons` "s")
                , "rulesMsg: "         <> "elided"
                , "violationMsg: "     <> (miscTokenDelimiter `T.cons` "v")
                , "isZBackDoor: "      <> (miscTokenDelimiter `T.cons` "z") ]
    dfltColorStyleToken   = styleTokenDelimiter `T.cons` "d"
    noUnderlineStyleToken = styleTokenDelimiter `T.cons` "n"
debugToken p = withoutArgs debugToken p


-----


debugUnderline :: HasCallStack => ActionFun
debugUnderline (NoArgs i mq cols) = do logPlaExec (prefixDebugCmd "underline") i
                                       let pair = (underlineANSI, noUnderlineANSI) & both %~ showTxt
                                       wrapSend mq cols . quoteWith' pair . underline . spaced $  " This text is underlined. "
debugUnderline p = withoutArgs debugUnderline p


-----


debugVolume :: HasCallStack => ActionFun
debugVolume p@AdviseNoArgs         = advise p [] adviceDVolumeNoArgs
debugVolume   (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(searchId, "")] -> helper searchId =<< getState
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper searchId ms
      | searchId < 0                              = wrapSend mq cols sorryWtf
      | views mobTbl (searchId `IM.notMember`) ms = wrapSend mq cols . sorryNonexistentId searchId . pure $ "mobile"
      | otherwise                                 = do logPlaExecArgs (prefixDebugCmd "volume") (pure a) i
                                                       send mq . nlnl . showTxt . calcCarriedVol searchId $ ms
debugVolume p = advise p [] adviceDVolumeExcessArgs


-----


debugWeight :: HasCallStack => ActionFun
debugWeight p@AdviseNoArgs         = advise p [] adviceDWeightNoArgs
debugWeight   (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(searchId, "")] -> helper searchId =<< getState
  _                -> wrapSend mq cols . sorryParseId $ a
  where
    helper searchId ms
      | searchId < 0                              = wrapSend mq cols sorryWtf
      | views objTbl (searchId `IM.notMember`) ms = wrapSend mq cols . sorryNonexistentId searchId . pure $ "object"
      | otherwise                                 = do logPlaExecArgs (prefixDebugCmd "weight") (pure a) i
                                                       send mq . nlnl . showTxt . calcWeight searchId $ ms
debugWeight p = advise p [] adviceDWeightExcessArgs


-----


debugWords :: HasCallStack => ActionFun
debugWords (NoArgs' i mq) = logPlaExec (prefixDebugCmd "words") i >> initWordsTbl >> ok mq
debugWords p              = withoutArgs debugWords p


-----


debugWrap :: HasCallStack => ActionFun
debugWrap p@AdviseNoArgs         = advise p [] adviceDWrapNoArgs
debugWrap   (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(lineLen, "")] -> helper lineLen
  _               -> wrapSend mq cols . sorryParseLineLen $ a
  where
    helper lineLen | lineLen < 0                                = wrapSend mq cols sorryWtf
                   | not . inRange (minCols, maxCols) $ lineLen = wrapSend mq cols sorryWrapLineLen
                   | otherwise                                  = do logPlaExecArgs (prefixDebugCmd "wrap") (pure a) i
                                                                     send mq . frame lineLen . wrapUnlines lineLen $ wrapMsg
debugWrap p = advise p [] adviceDWrapExcessArgs


wrapMsg :: HasCallStack => Text
wrapMsg = T.unwords wordy <> dfltColor
  where
    wordy :: HasCallStack => [] Text
    wordy = [ T.concat [ u
                       , mkFgColorANSI (Dull, c)
                       , "This is "
                       , showTxt c
                       , " text." ] | c <- Black `delete` colors, u <- [ underlineANSI, noUnderlineANSI ] ]


-----


debugWrapIndent :: HasCallStack => ActionFun
debugWrapIndent p@AdviseNoArgs            = advise p [] adviceDWrapIndentNoArgs
debugWrapIndent p@AdviseOneArg            = advise p [] adviceDWrapIndentNoAmt
debugWrapIndent   (WithArgs i mq cols as) = parseTwoIntArgs mq cols as sorryParseLineLen sorryParseIndent helper
  where
    helper lineLen indent | any (< 0) [ lineLen, indent ]              = wrapSend mq cols sorryWtf
                          | not . inRange (minCols, maxCols) $ lineLen = wrapSend mq cols sorryWrapLineLen
                          | indent >= lineLen                          = wrapSend mq cols sorryIndent
                          | otherwise                                  = do
                              logPlaExecArgs (prefixDebugCmd "wrapindent") as i
                              send mq . frame lineLen . T.unlines . wrapIndent indent lineLen $ wrapMsg
debugWrapIndent p = advise p [] adviceDWrapIndentExcessArgs
