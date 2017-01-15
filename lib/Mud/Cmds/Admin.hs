{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, ScopedTypeVariables, TransformListComp, TupleSections, ViewPatterns #-}

module Mud.Cmds.Admin (adminCmds) where

import Mud.Cmds.Debug
import Mud.Cmds.ExpCmds
import Mud.Cmds.Msgs.Advice
import Mud.Cmds.Msgs.CmdDesc
import Mud.Cmds.Msgs.Hint
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Pla
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.CmdPrefixes
import Mud.Cmds.Util.EmoteExp.EmoteExp
import Mud.Cmds.Util.EmoteExp.TwoWayEmoteExp
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Death
import Mud.Data.State.Util.Egress
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Misc.ANSI
import Mud.Misc.CurryTime
import Mud.Misc.Database
import Mud.Misc.Gods
import Mud.Misc.Misc
import Mud.Misc.Persist
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iRoot, iWelcome)
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights
import Mud.Util.List
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut, massLogPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***), (&&&), first, second)
import Control.Concurrent.Async (asyncThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, _4, _5, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (?~), (^.))
import Control.Monad ((<=<), (>=>), forM, forM_, join, unless, when)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (validatePassword)
import Data.Aeson (eitherDecode)
import Data.Bits (setBit, zeroBits)
import Data.Char (isDigit, isLower, isUpper)
import Data.Either (rights)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List ((\\), delete, foldl', groupBy, intercalate, intersperse, nub, partition, sort, sortBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>), Any(..), Sum(..), getSum)
import Data.Text (Text)
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, getZonedTime, utcToLocalTime, utcToZonedTime)
import Database.SQLite.Simple (FromRow, fromOnly)
import GHC.Conc (ThreadStatus(..), threadStatus)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import Prelude hiding (exp, pi)
import qualified Data.IntMap.Lazy as IM (elems, filter, filterWithKey, keys, lookup, size, toList)
import qualified Data.Map.Lazy as M (elems, foldl, foldrWithKey, keys, null, size, toList)
import qualified Data.Set as S (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T (putStrLn)
import System.Directory (getDirectoryContents)
import System.Process (readProcess)
import System.Time.Utils (renderSecs)
import Text.Regex.Posix ((=~))


{-# ANN module ("HLint: ignore Use ||" :: String) #-}


-----


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Admin"


-----


logIOEx :: Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Admin"


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Admin"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Admin"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Admin"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Admin"


logPlaOut :: CmdName -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.Admin"


massLogPla :: Text -> Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds.Admin"


-- ==================================================


adminCmds :: HasCallStack => [Cmd]
adminCmds =
    [ mkAdminCmd "?"          adminDispCmdList True  cmdDescDispCmdList
    , mkAdminCmd "admin"      adminAdmin       True  ("Send a message on the admin channel " <> plusRelatedMsg)
    , mkAdminCmd "alertexec"  adminAlertExec   True  "Dump the alert exec database."
    , mkAdminCmd "alertmsg"   adminAlertMsg    True  "Dump the alert msg database."
    , mkAdminCmd "announce"   adminAnnounce    True  "Send a message to all players."
    , mkAdminCmd "as"         adminAs          False "Execute a command as someone else."
    , mkAdminCmd "banhost"    adminBanHost     True  "Dump the banned hostname database, or ban/unban a host."
    , mkAdminCmd "banpc"      adminBanPC       True  "Dump the banned PC database, or ban/unban a PC."
    , mkAdminCmd "boot"       adminBoot        True  "Boot a player, optionally with a custom message."
    , mkAdminCmd "bug"        adminBug         True  "Dump the bug database."
    , mkAdminCmd "channel"    adminChan        True  "Display information about one or more telepathic channels."
    , mkAdminCmd "count"      adminCount       True  "Display or search a list of miscellaneous running totals."
    , mkAdminCmd "currytime"  adminCurryTime   True  "Display the current Curry Time."
    , mkAdminCmd "date"       adminDate        True  "Display the current system date."
    , mkAdminCmd "discover"   adminDiscover    True  "Dump the discover database."
    , mkAdminCmd "examine"    adminExamine     True  "Display the properties of one or more IDs."
    , mkAdminCmd "experience" adminExp         True  "Dump the experience table."
    , mkAdminCmd "exself"     adminExamineSelf True  "Self-examination."
    , mkAdminCmd "farewell"   adminFarewell    True  "Display the farewell stats for one or more PCs."
    , mkAdminCmd "gods"       adminGods        True  "Display a list of the gods."
    , mkAdminCmd "hash"       adminHash        True  "Compare a plain-text password with a hashed password."
    , mkAdminCmd "holysymbol" adminHolySymbol  True  "Create a given number of holy symbols of a given god by god name."
    , mkAdminCmd "host"       adminHost        True  "Display a report of connection statistics for one or more \
                                                     \players."
    , mkAdminCmd "incognito"  adminIncognito   True  "Toggle your incognito status."
    , mkAdminCmd "ip"         adminIp          True  "Display the server's IP addresses and listening port."
    , mkAdminCmd "kill"       adminKill        True  "Instantly kill one or more mobiles by ID."
    , mkAdminCmd "link"       adminLink        True  "Dump two-way links for one or more PCs, sorted by volume of \
                                                     \messages in descending order."
    , mkAdminCmd "locate"     adminLocate      True  "Locate one or more IDs."
    , mkAdminCmd "message"    adminMsg         True  "Send a message to a regular player."
    , mkAdminCmd "moon"       adminMoon        True  "Display the current phase of the moon."
    , mkAdminCmd "mychannels" adminMyChans     True  "Display information about telepathic channels for one or more \
                                                     \players."
    , mkAdminCmd "password"   adminPassword    True  "Change a player's password."
    , mkAdminCmd "peep"       adminPeep        True  "Start or stop peeping one or more players."
    , mkAdminCmd "persist"    adminPersist     True  "Persist the world (save the current world state to disk)."
    , mkAdminCmd "possess"    adminPossess     False "Temporarily take control of an NPC."
    , mkAdminCmd "print"      adminPrint       True  "Print a message to the server console."
    , mkAdminCmd "profanity"  adminProfanity   True  "Dump the profanity database."
    , mkAdminCmd "search"     adminSearch      True  "Search for names and IDs using a regular expression."
    , mkAdminCmd "security"   adminSecurity    True  "Display security Q&A for one or more players."
    , mkAdminCmd "set"        adminSet         True  "Set one or more values for a given ID."
    , mkAdminCmd "shutdown"   adminShutdown    False "Shut down CurryMUD, optionally with a custom message."
    , mkAdminCmd "sudoer"     adminSudoer      True  "Toggle a player's admin status."
    , mkAdminCmd "summon"     adminSummon      True  "Teleport a PC to your current room."
    , mkAdminCmd "teleid"     adminTeleId      True  "Teleport to an entity or room by ID."
    , mkAdminCmd "telepc"     adminTelePC      True  "Teleport to a PC by name."
    , mkAdminCmd "telerm"     adminTeleRm      True  "Display a list of named rooms to which you may teleport, or \
                                                     \teleport to a room by name."
    , mkAdminCmd "time"       adminTime        True  "Display the current system time."
    , mkAdminCmd "ttype"      adminTType       True  "Display a report of hosts by terminal type."
    , mkAdminCmd "typo"       adminTypo        True  "Dump the typo database."
    , mkAdminCmd "uptime"     adminUptime      True  "Display the system uptime."
    , mkAdminCmd "whoin"      adminWhoIn       True  "Display or search a list of all the PCs that are currently \
                                                     \logged in."
    , mkAdminCmd "whoout"     adminWhoOut      True  "Display or search a list of all the PCs that are currently \
                                                     \logged out."
    , mkAdminCmd "wiretap"    adminWire        True  "Start or stop tapping one or more telepathic channels." ]


mkAdminCmd :: HasCallStack => Text -> ActionFun -> Bool -> CmdDesc -> Cmd
mkAdminCmd (prefixAdminCmd -> cn) f b cd = Cmd { cmdName           = cn
                                               , cmdPriorityAbbrev = Nothing
                                               , cmdFullName       = cn
                                               , cmdAction         = Action f b
                                               , cmdDesc           = cd }


-----


adminAdmin :: HasCallStack => ActionFun
adminAdmin (NoArgs i mq cols) = getState >>= \ms ->
    let triples = sortBy (compare `on` view _2) [ (ai, as, isTuned) | ai <- getLoggedInAdminIds ms
                                                                    , let as      = getSing ai ms
                                                                    , let ap      = getPla  ai ms
                                                                    , let isTuned = isTunedAdmin ap ]
        ([self],   others   )  = partition (views _1 (== i)) triples
        (tunedIns, tunedOuts)  = partition (view _3)         others
        styleds                = styleAbbrevs Don'tQuote . select _2 $ tunedIns
        others'                = zipWith (\triple styled -> triple & _2 .~ styled) tunedIns styleds ++ tunedOuts
        mkDesc (_, n, isTuned) = padName n <> tunedInOut isTuned
        descs                  = mkDesc self : map mkDesc others'
    in logPlaExecArgs (prefixAdminCmd "admin") [] i >> multiWrapSend mq cols descs
adminAdmin (Msg i mq cols msg) = getState >>= \ms ->
    if isTunedAdminId i ms
      then case getTunedAdminIds ms of
        [_]      -> wrapSend mq cols . sorryChanNoOneListening $ "admin"
        tunedIds ->
          let tunedSings         = map (`getSing` ms) tunedIds
              getStyled targetId = let styleds = styleAbbrevs Don'tQuote $ getSing targetId ms `delete` tunedSings
                                   in head . filter ((== s) . dropANSI) $ styleds
              s                  = getSing i ms
              format (txt, is)   = if i `elem` is
                then (formatChanMsg "Admin" s txt, pure i) : mkBsWithStyled (i `delete` is)
                else mkBsWithStyled is
                where
                  mkBsWithStyled is' = [ (flip (formatChanMsg "Admin") txt . getStyled &&& pure) i' | i' <- is' ]
              f bs = ioHelper s (concatMap format bs)
              ws   = wrapSend      mq cols
              mws  = multiWrapSend mq cols
          in case adminChanTargetify tunedIds tunedSings msg of
            Left errorMsg    -> ws errorMsg
            Right (Right bs) -> f bs . mkLogMsg $ bs
            Right (Left  ()) -> case adminChanEmotify i ms tunedIds tunedSings msg of
              Left  errorMsgs  -> mws errorMsgs
              Right (Right bs) -> f bs . mkLogMsg $ bs
              Right (Left  ()) -> case adminChanExpCmdify i ms tunedIds tunedSings msg of
                Left  errorMsg     -> ws errorMsg
                Right (bs, logMsg) -> f bs logMsg
      else wrapSend mq cols . sorryTunedOutOOCChan $ "admin"
  where
    getTunedAdminIds ms  = [ ai | ai <- getLoggedInAdminIds ms, isTunedAdminId ai ms ]
    mkLogMsg []          = ""
    mkLogMsg ((t, _):_)  = dropANSI t
    ioHelper s bs logMsg = logHelper >> bcastNl bs
      where
        logHelper = liftIO mkTimestamp >>= \ts ->
            logMsg |#| ((>>) <$> logPlaOut (prefixAdminCmd "admin") i . pure
                             <*> withDbExHandler_ "adminAdmin" . insertDbTblAdminChan . AdminChanRec ts s)
adminAdmin p = patternMatchFail "adminAdmin" . showText $ p


-----


adminAlertExec :: HasCallStack => ActionFun
adminAlertExec p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "alert_exec" f "alertexec" p
  where
    f :: HasCallStack => [AlertExecRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


dumpCmdHelper :: (HasCallStack, FromRow a) => Text -> ([a] -> MudStack ()) -> CmdName -> ActionFun
dumpCmdHelper tblName f cn (NoArgs i mq cols) = withDbExHandler "dumpCmdHelper" (getDbTblRecs tblName) >>= \case
  Just xs -> logPlaExec (prefixAdminCmd cn) i >> f xs
  Nothing -> dbError mq cols
dumpCmdHelper tblName f cn p = withoutArgs (dumpCmdHelper tblName f cn) p


dumpDbTblHelper :: (HasCallStack, Pretty a) => MsgQueue -> Cols -> [a] -> MudStack ()
dumpDbTblHelper mq cols [] = wrapSend mq cols dbEmptyMsg
dumpDbTblHelper mq cols xs = multiWrapSend mq cols . map pp $ xs


-----


adminAlertMsg :: HasCallStack => ActionFun
adminAlertMsg p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "alert_msg" f "alertmsg" p
  where
    f :: HasCallStack => [AlertMsgRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminAnnounce :: HasCallStack => ActionFun
adminAnnounce p@AdviseNoArgs    = advise p [ prefixAdminCmd "announce" ] adviceAAnnounceNoArgs
adminAnnounce   (Msg' i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    logPla    "adminAnnounce" i $          "announcing "  <> dblQuote msg
    logNotice "adminAnnounce"   $ s <> " is announcing, " <> dblQuote msg
    ok mq
    massSend . colorWith announceColor $ msg
adminAnnounce p = patternMatchFail "adminAnnounce" . showText $ p


-----


adminAs :: HasCallStack => ActionFun
adminAs p@(NoArgs' i mq    ) = advise p [ prefixAdminCmd "as" ] adviceAAsNoArgs >> sendDfltPrompt mq i
adminAs p@(OneArg  i mq _ _) = advise p [ prefixAdminCmd "as" ] adviceAAsNoCmd  >> sendDfltPrompt mq i
adminAs   (WithTarget i mq cols target rest) = getState >>= \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The target ID"
        as targetId         = let s = getSing targetId ms in case getType targetId ms of
          NpcType -> let npcMq        = getNpcMsgQueue targetId ms
                         notPossessed = do ioHelper targetId s
                                           liftIO . atomically . writeTQueue npcMq . ExternCmd mq cols $ rest
                         isPossessed pi = sorry $ if pi == i
                           then sorryAlreadyPossessing s
                           else sorryAlreadyPossessed  s . getSing pi $ ms
                     in maybe notPossessed isPossessed . getPossessor targetId $ ms
          PCType  | targetId == i                           -> sorry sorryAsSelf
                  | isAdminId targetId ms                   -> sorry sorryAsAdmin
                  | not . isLoggedIn . getPla targetId $ ms -> sorry . sorryLoggedOut $ s
                  | isAdHoc targetId ms                     -> sorry sorryAsAdHoc
                  | (targetMq, targetCols) <- getMsgQueueColumns targetId ms -> do ioHelper targetId s
                                                                                   wrapSend targetMq targetCols asMsg
                                                                                   fakeClientInput targetMq rest
          _ -> sorry . sorryAsType $ s
        ioHelper targetId s = do
            let ts = [ "Executing ", dblQuote rest, " as ", aOrAnOnLower . descSingId targetId $ ms, "." ]
            logPla "adminAs" i . T.concat $ ts
            sendFun . parensQuote . thrice prd $ "Executing as " <> aOrAnOnLower s
        sorry txt  = sendFun txt >> sendDfltPrompt mq i
        sorryParse = sorry . sorryParseId $ strippedTarget'
    in case reads . T.unpack $ strippedTarget :: [(Int, String)] of
      [(targetId, "")] | targetId < 0                -> sorry sorryWtf
                       | not . hasType targetId $ ms -> sorryParse
                       | otherwise                   -> as targetId
      _ -> sorryParse
adminAs p = patternMatchFail "adminAs" . showText $ p


-----


adminBanHost :: HasCallStack => ActionFun
adminBanHost (NoArgs i mq cols) = (withDbExHandler "adminBanHost" . getDbTblRecs $ "ban_host") >>= \case
  Just xs -> logPlaExecArgs (prefixAdminCmd "banhost") [] i >> dumpDbTblHelper mq cols (xs :: [BanHostRec])
  Nothing -> dbError mq cols
adminBanHost p@AdviseOneArg = advise p [ prefixAdminCmd "banhost" ] adviceABanHostNoReason
adminBanHost   (MsgWithTarget i mq cols (uncapitalize -> target) msg) = getState >>= \ms ->
    withDbExHandler "adminBanHost" (isHostBanned target) >>= \case
      Nothing      -> dbError mq cols
      Just (Any b) -> let newStatus = not b in liftIO mkTimestamp >>= \ts -> do
          let banHost = BanHostRec ts target newStatus msg
          withDbExHandler_ "adminBanHost" . insertDbTblBanHost $ banHost
          notifyBan i mq cols (getSing i ms) target newStatus banHost
adminBanHost p = patternMatchFail "adminBanHost" . showText $ p


notifyBan :: (HasCallStack, Pretty a) => Id -> MsgQueue -> Cols -> Sing -> Text -> Bool -> a -> MudStack ()
notifyBan i mq cols selfSing target newStatus x =
    let fn          = "notifyBan"
        (v, suffix) = newStatus ? ("banned", [ ": " <> pp x ]) :? ("unbanned", [ ": " <> pp x ])
    in do logNotice fn       . T.concat $ [ selfSing, spaced v,      target ] ++ suffix
          logPla    fn i     . T.concat $ [                  v, " ", target ] ++ suffix
          wrapSend mq cols   . T.concat $ [ "You have ",     v, " ", target ] ++ suffix
          bcastOtherAdmins i . T.concat $ [ selfSing, spaced v,      target ] ++ suffix


-----


adminBanPC :: HasCallStack => ActionFun
adminBanPC (NoArgs i mq cols) = withDbExHandler "adminBanPC" (getDbTblRecs "ban_pc") >>= \case
  Just xs -> logPlaExecArgs (prefixAdminCmd "banpc") [] i >> dumpDbTblHelper mq cols (xs :: [BanPCRec])
  Nothing -> dbError mq cols
adminBanPC p@AdviseOneArg                         = advise p [ prefixAdminCmd "banpc" ] adviceABanPCNoReason
adminBanPC p@(MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let fn                  = "adminBanPC"
        SingleTarget { .. } = mkSingleTarget mq cols target "The name of the PC you wish to ban"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      []      -> sendFun $ sorryPCName strippedTarget |<>| hintABan
      [banId] -> let selfSing = getSing i     ms
                     pla      = getPla  banId ms
                 in if
                   | banId == i       -> sendFun sorryBanSelf
                   | isAdmin pla      -> sendFun sorryBanAdmin
                   | isAdHoc banId ms -> sendFun sorryBanAdHoc
                   | otherwise        -> withDbExHandler "adminBanPC" (isPCBanned strippedTarget) >>= \case
                     Nothing      -> dbError mq cols
                     Just (Any b) -> let newStatus = not b in liftIO mkTimestamp >>= \ts -> do
                         let rec = BanPCRec ts strippedTarget newStatus msg
                         withDbExHandler_ "adminBanPC" . insertDbTblBanPC $ rec
                         notifyBan i mq cols selfSing strippedTarget newStatus rec
                         when (newStatus && isLoggedIn pla) . adminBoot $ p { args = strippedTarget : T.words bannedMsg }
      xs      -> patternMatchFail fn . showText $ xs
adminBanPC p = patternMatchFail "adminBanPC" . showText $ p


-----


adminBoot :: HasCallStack => ActionFun
adminBoot p@AdviseNoArgs                         = advise p [ prefixAdminCmd "boot" ] adviceABootNoArgs
adminBoot   (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to boot"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      []       -> sendFun $ sorryPCName strippedTarget |<>| hintABoot
      [bootId] -> let selfSing = getSing i ms
                      bootPla  = getPla bootId ms
                  in if | not . isLoggedIn $ bootPla -> sendFun . sorryLoggedOut $ strippedTarget
                        | isAdmin bootPla            -> sendFun . sorryBootAdmin $ strippedTarget
                        | bootId == i                -> sendFun sorryBootSelf
                        | bootMq <- getMsgQueue bootId ms, f <- ()# msg ? dfltMsg :? customMsg -> do
                            sendFun . prd $ "You have booted " <> strippedTarget
                            sendMsgBoot bootMq =<< f bootId strippedTarget selfSing
                            bcastAdminsExcept [ i, bootId ] . T.concat $ [ selfSing, " booted ", strippedTarget, "." ]
      xs       -> patternMatchFail "adminBoot" . showText $ xs
  where
    dfltMsg bootId target' s = emptied $ do
        logPla "adminBoot dfltMsg"   i      $ T.concat [ "booted ", target', " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   bootId $ T.concat [ "booted by ", s,    " ", parensQuote "no message given", "." ]
    customMsg bootId target' s = do
        logPla "adminBoot customMsg" i      $ T.concat [ "booted ", target', "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" bootId $ T.concat [ "booted by ", s,    "; message: ", dblQuote msg ]
        unadulterated msg
adminBoot p = patternMatchFail "adminBoot" . showText $ p


-----


adminBug :: HasCallStack => ActionFun
adminBug p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "bug" f "bug" p
  where
    f :: HasCallStack => [BugRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminChan :: HasCallStack => ActionFun
adminChan (NoArgs i mq cols) = getState >>= \ms -> case views chanTbl (map (mkChanReport i ms) . IM.elems) ms of
  []      -> informNoChans mq cols
  reports -> adminChanIOHelper i mq reports
adminChan (LowerNub i mq cols as) = getState >>= \ms ->
    let helper a = case reads . T.unpack $ a :: [(Int, String)] of
          [(ci, "")] | ci < 0                                      -> pure sorryWtf
                     | views chanTbl ((ci `notElem`) . IM.keys) ms -> sorry
                     | otherwise                                   -> mkChanReport i ms . getChan ci $ ms
          _                                                        -> sorry
          where
            sorry = pure . sorryParseChanId $ a
        reports = map helper as
    in case views chanTbl IM.size ms of 0 -> informNoChans mq cols
                                        _ -> adminChanIOHelper i mq reports
adminChan p = patternMatchFail "adminChan" . showText $ p


informNoChans :: HasCallStack => MsgQueue -> Cols -> MudStack ()
informNoChans mq cols = wrapSend mq cols "No channels exist!"


adminChanIOHelper :: HasCallStack => Id -> MsgQueue -> [[Text]] -> MudStack ()
adminChanIOHelper i mq reports = sequence_ [ logPlaExec (prefixAdminCmd "channel") i
                                           , pager i mq Nothing . intercalate [""] $ reports ]


-----


adminCount :: HasCallStack => ActionFun
adminCount   (NoArgs i mq cols)          = do logPlaExecArgs (prefixAdminCmd "count") [] i
                                              pager i mq Nothing . concatMap (wrapIndent 2 cols) =<< mkCountTxt
adminCount p@ActionParams { myId, args } = do logPlaExecArgs (prefixAdminCmd "count") args myId
                                              dispMatches p 2 =<< mkCountTxt


mkCountTxt :: HasCallStack => MudStack [Text]
mkCountTxt = (uncurry mappend . second commaShow) `fmap2` helper
  where
    helper = getState >>= \ms -> do
        let countType t = views typeTbl  (IM.size . IM.filter (== t)) ms
            countWealth = views coinsTbl (f . mconcat . IM.elems    ) ms
              where
                f (Coins (c, s, g)) = let x = c `divideRound` 100
                                          y = s `divideRound` 10
                                      in sum [ x, y, g ]
            countLoggedOutPlas   = views plaTbl  (length . (\\ getLoggedInPlaIds ms) . IM.keys . IM.filter (not . isAdmin)) ms
            countMaleFemale sexy = views plaTbl  (IM.size . IM.filterWithKey f) ms
              where
                f i p = getSex i ms == sexy && not (isAdmin p)
            countRace r = views plaTbl (length . filter ((== r) . (`getRace` ms)) . IM.keys . IM.filter (not . isAdmin)) ms
        [ noOfPlaHelpCmds, noOfPlaHelpTopics, noOfAdminHelpCmds, noOfAdminHelpTopics ] <- countHelps
        noticeErrorThrIds <- getLogThreadIds
        let plaLogThrIds = views plaLogTbl (map (asyncThreadId . fst) . IM.elems) ms
            otherThrIds  = views threadTbl M.keys ms
            threadIds    = concat [ noticeErrorThrIds, plaLogThrIds, otherThrIds ]
        noOfThreads <- length . filterThreads <$> mapM (liftIO . threadStatus) threadIds
        return [ ("Armor: ",        countType ArmType     )
               , ("Clothing: ",     countType ClothType   )
               , ("Containers: ",   countType ConType     )
               , ("Corpses: ",      countType CorpseType  )
               , ("Foods: ",        countType FoodType    )
               , ("NPCs: ",         countType NpcType     )
               , ("Objects: ",      countType ObjType     )
               , ("PCs: ",          countType PCType      )
               , ("Rooms: ",        countType RmType      )
               , ("Vessels: ",      countType VesselType  )
               , ("Weapons: ",      countType WpnType     )
               , ("Writables: ",    countType WritableType)
               , ("Typed things: ",     ms^.typeTbl        .to IM.size)
               , ("Distinct foods: ",   ms^.distinctFoodTbl.to IM.size)
               , ("Distinct liquids: ", ms^.distinctLiqTbl .to IM.size)
               , ("Wealth (gp): ",         countWealth)
               , ("Players logged in: ",   length . getLoggedInPlaIds $ ms                  )
               , ("Players logged out: ",  countLoggedOutPlas                               )
               , ("Admins logged in: ",    length . getLoggedInAdminIds $ ms                )
               , ("Admins logged out: ",   length $ getAdminIds ms \\ getLoggedInAdminIds ms)
               , ("Unique hosts: ",        views hostTbl (length . nub . M.elems) ms        )
               , ("Pick pts table size: ", views pickPtsTbl IM.size ms                      )
               , ("Male PCs: ",        countMaleFemale Male  )
               , ("Female PCs: ",      countMaleFemale Female)
               , ("Dwarves: ",         countRace Dwarf       )
               , ("Elves: ",           countRace Elf         )
               , ("Felinoids: ",       countRace Felinoid    )
               , ("Hobbits: ",         countRace Hobbit      )
               , ("Humans: ",          countRace Human       )
               , ("Lagomorphs: ",      countRace Lagomorph   )
               , ("Nymphs: ",          countRace Nymph       )
               , ("Vulpenoids: ",      countRace Vulpenoid   )
               , ("Channels: ",        ms^.chanTbl.to IM.size)
               , ("Player commands: ", noOfPlaCmds           )
               , ("Spirit commands: ", noOfSpiritCmds        )
               , ("NPC commands: ",    noOfNpcCmds           )
               , ("Exp commands: ",    length expCmds        )
               , ("Admin commands: ",  length adminCmds      )
               , ("Debug commands: ",  length debugCmds      )
               , ("Player help commands: ", noOfPlaHelpCmds             )
               , ("Player help topics: ",   noOfPlaHelpTopics           )
               , ("Admin help commands: ",  noOfAdminHelpCmds           )
               , ("Admin help topics: ",    noOfAdminHelpTopics         )
               , ("Room teleport names: ",  ms^.rmTeleNameTbl.to IM.size)
               , ("Functions in the function table: ", ms^.funTbl           .to M.size)
               , ("Effect functions: ",                ms^.effectFunTbl     .to M.size)
               , ("Instantaneous effect functions: ",  ms^.instaEffectFunTbl.to M.size)
               , ("Feeling functions: ",               ms^.feelingFunTbl    .to M.size)
               , ("Hook functions: ",                  ms^.hookFunTbl       .to M.size)
               , ("Room action functions: ",           ms^.rmActionFunTbl   .to M.size)
               , ("Active threads: ",                  noOfThreads                    ) ]
    countHelps     = liftIO . mapM (countFiles <=< mkMudFilePath) $ [ plaHelpCmdsDirFun
                                                                    , plaHelpTopicsDirFun
                                                                    , adminHelpCmdsDirFun
                                                                    , adminHelpTopicsDirFun ]
    countFiles dir = (length . dropIrrelevantFiles <$> getDirectoryContents dir) `catch` handler
      where
        handler :: HasCallStack => IOException -> IO Int
        handler = const . return $ 0
    filterThreads = filter $ \case ThreadRunning   -> True
                                   ThreadBlocked _ -> True
                                   _               -> False


-----


adminCurryTime :: HasCallStack => ActionFun
adminCurryTime (NoArgs i mq cols) = liftIO ((,) <$> getSecsFromCurryEpoch <*> getCurrentTimeZone) >>= \(secs, z) -> do
    logPlaExec (prefixAdminCmd "currytime") i
    let ct     = secsToCurryTime secs
        (a, b) = ((,) <$> formatTimeHelper <*> formatTimeHelper . utcToZonedTime z) curryEpoch
    multiWrapSend mq cols $ showCurryTime ct ++ [ commaShow secs <> " seconds have passed since the Curry Epoch:", a, b ]
adminCurryTime p = withoutArgs adminCurryTime p


-----


adminDate :: HasCallStack => ActionFun
adminDate (NoArgs' i mq) = do logPlaExec (prefixAdminCmd "date") i
                              send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
adminDate p              = withoutArgs adminDate p


-----


adminDiscover :: HasCallStack => ActionFun
adminDiscover p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "discover" f "discover" p
  where
    f :: HasCallStack => [DiscoverRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminDispCmdList :: HasCallStack => ActionFun
adminDispCmdList p@(LowerNub' i as) = logPlaExecArgs (prefixAdminCmd "?") as i >> dispCmdList adminCmds p
adminDispCmdList p                  = patternMatchFail "adminDispCmdList" . showText $ p


-----


adminExamine :: HasCallStack => ActionFun
adminExamine p@AdviseNoArgs            = advise p [ prefixAdminCmd "examine" ] adviceAExamineNoArgs
adminExamine   (LowerNub i mq cols as) = getState >>= \ms ->
    let helper a = case reads . T.unpack $ a :: [(Int, String)] of
          [(targetId, "")] | targetId < 0                -> pure sorryWtf
                           | not . hasType targetId $ ms -> sorry
                           | otherwise                   -> examineHelper ms targetId
          _                                              -> sorry
          where
            sorry = pure . sorryParseId $ a
    in do logPlaExecArgs (prefixAdminCmd "examine") as i
          pager i mq Nothing . concatMap (wrapIndent 2 cols) . intercalateDivider cols . map helper $ as
adminExamine p = patternMatchFail "adminExamine" . showText $ p


examineHelper :: HasCallStack => MudState -> Id -> [Text]
examineHelper ms targetId = let t = getType targetId ms in helper t $ case t of
  ArmType        -> [ examineEnt, examineObj,   examineArm        ]
  ClothType      -> [ examineEnt, examineObj,   examineCloth      ]
  ConType        -> [ examineEnt, examineObj,   examineInv,   examineCoins, examineCon ]
  CorpseType     -> [ examineEnt, examineObj,   examineInv,   examineCoins, examineCon, examineCorpse ]
  FoodType       -> [ examineEnt, examineObj,   examineFood       ]
  HolySymbolType -> [ examineEnt, examineObj,   examineHolySymbol ]
  NpcType        -> [ examineEnt, examineInv,   examineCoins, examineEqMap, examineMob, examineNpc    ]
  ObjType        -> [ examineEnt, examineObj ]
  PCType         -> [ examineEnt, examineInv,   examineCoins, examineEqMap, examineMob, examinePC, examinePla, examinePickPts ]
  RmType         -> [ examineInv, examineCoins, examineRm         ]
  VesselType     -> [ examineEnt, examineObj,   examineVessel     ]
  WpnType        -> [ examineEnt, examineObj,   examineWpn        ]
  WritableType   -> [ examineEnt, examineObj,   examineWritable   ]
  where
    helper t fs = let header = T.concat [ showText targetId, " ", parensQuote . pp $ t ]
                  in header : "" : concatMap (((targetId, ms) |&|) . uncurry) fs


type ExamineHelper = Id -> MudState -> [Text]


examineArm :: HasCallStack => ExamineHelper
examineArm i ms = let a = getArm i ms in [ "Type: " <> a^.armSub  .to pp
                                         , "AC: "   <> a^.armClass.to showText ]


examineCloth :: HasCallStack => ExamineHelper
examineCloth i ms = let c = getCloth i ms in [ "Type: " <> pp c ]


examineCoins :: HasCallStack => ExamineHelper
examineCoins i ms = let (map commaShow . coinsToList -> cs) = getCoins i ms in [ "Coins: " <> commas cs ]


examineCon :: HasCallStack => ExamineHelper
examineCon i ms = let c = getCon i ms in [ "Is clothing: " <> c^.conIsCloth.to showText
                                         , T.concat [ "Volume/capacity: "
                                                    , showText . calcInvCoinsVol i $ ms
                                                    , " / "
                                                    , c^.conCapacity.to commaShow
                                                    , " "
                                                    , parensQuote $ (<> "%") . commaShow . calcConPerFull i $ ms ]
                                         , "Container flags: " <> (commas . dropBlanks . descFlags $ c) ]
  where
    descFlags c | c^.conFlags == zeroBits = none
                | otherwise               = none -- TODO: Con flags.


examineCorpse :: HasCallStack => ExamineHelper
examineCorpse i ms = case getCorpse i ms of
  PCCorpse cSing cDesc cSex cRace -> [ "Corpse sing: "             <> cSing
                                     , "Corpse description: "      <> cDesc
                                     , "Corpse sex: "              <> pp cSex
                                     , "Corpse race: "             <> pp cRace ]
  NpcCorpse cDesc                 -> pure $ "Corpse description: " <> cDesc


examineEnt :: HasCallStack => ExamineHelper
examineEnt i ms = let e = getEnt i ms in [ "Name: "           <> e^.entName .to (fromMaybe none)
                                         , "Sing: "           <> e^.sing    .to noneOnNull
                                         , "Plur: "           <> e^.plur    .to noneOnNull
                                         , "Description: "    <> e^.entDesc .to noneOnNull
                                         , "Smell: "          <> e^.entSmell.to (fromMaybe none)
                                         , "Entity flags: "   <> (commas . dropBlanks . descFlags $ e)
                                         , "Active effects: " <> descActiveEffects
                                         , "Paused effects: " <> descPausedEffects ]
  where
    descFlags e       | e^.entFlags == zeroBits = none
                      | otherwise               = none -- TODO: Ent flags.
    descActiveEffects = descEffect getActiveEffects
    descPausedEffects = descEffect getPausedEffects
    descEffect f      = ppList . f i $ ms


ppList :: (HasCallStack, Pretty a) => [a] -> Text
ppList = noneOnNull . commas . map pp


examineEqMap :: HasCallStack => ExamineHelper
examineEqMap i ms = map helper . M.toList . getEqMap i $ ms
  where
    helper (slot, i') = bracketQuote (pp slot) |<>| descSingId i' ms


examineFood :: HasCallStack => ExamineHelper
examineFood i ms =
    let f  = getFood i ms
        df = getDistinctFoodForFood f ms
    in [ "Distinct food ID: "    <> f^.foodId.to showText
       , "Eat description: "     <> f^.foodEatDesc
       , "Remaining mouthfuls: " <> f^.foodRemMouthfuls.to showText
       , "Distinct mouthfuls: "  <> df^.foodMouthfuls  .to showText ] ++ df^.foodEdibleEffects.to descEdibleEffects


descEdibleEffects :: HasCallStack => EdibleEffects -> [Text]
descEdibleEffects (EdibleEffects d c) =
    [ "Digest effect list: "           <> maybe none descEffectList                           d
    , "Consumption effects amount: "   <> maybe none (views consumpAmt        showText      ) c
    , "Consumption effects interval: " <> maybe none (views consumpInterval   showText      ) c
    , "Consumption effect list: "      <> maybe none (views consumpEffectList descEffectList) c ]


descEffectList :: HasCallStack => EffectList -> Text
descEffectList (EffectList xs) = commas . map helper $ xs
  where
    helper (Left  instaEff) = pp instaEff
    helper (Right eff     ) = pp eff


examineHolySymbol :: HasCallStack => ExamineHelper
examineHolySymbol i = helper . getHolySymbol i
  where
    helper (HolySymbol gn) = maybeEmp f . getGodForGodName $ gn
    f god                  = [ "God: " <> pp god ]


examineInv :: HasCallStack => ExamineHelper
examineInv i ms = let is  = getInv i ms
                      txt = commas . map (`descSingId` ms) $ is
                  in [ "Contents: " <> noneOnNull txt ]


examineMob :: HasCallStack => ExamineHelper
examineMob i ms =
    let m                  = getMob i ms
        showAttrib a       = showText (getBaseAttrib a i ms) |<>| (parensQuote . showText . calcEffAttrib a i $ ms)
        showPts x y        = m^.x.to showText <> " / " <> m^.y.to showText
        descSingIdHelper f = noneOnNull . commas . map (`descSingId` ms) . f i $ ms
    in [ "Sex: "                <> m^.sex.to pp
       , "ST: "                 <> showAttrib St
       , "DX: "                 <> showAttrib Dx
       , "HT: "                 <> showAttrib Ht
       , "MA: "                 <> showAttrib Ma
       , "PS: "                 <> showAttrib Ps
       , "HP: "                 <> showPts curHp maxHp
       , "MP: "                 <> showPts curMp maxMp
       , "PP: "                 <> showPts curPp maxPp
       , "FP: "                 <> showPts curFp maxFp
       , "Exp: "                <> m^.exp .to commaShow
       , "Level: "              <> m^.lvl .to showText
       , "Handedness: "         <> m^.hand.to pp
       , "Know languages: "     <> m^.knownLangs.to ppList
       , "Room: "               <> m^.rmId     .to rmHelper
       , "Last room: "          <> m^.lastRmId .to rmHelper
       , "Room description: "   <> m^.mobRmDesc.to (fromMaybe none)
       , "Temp description: "   <> m^.tempDesc .to (fromMaybe none)
       , "Size: "               <> m^.mobSize         .to ppMaybe
       , "Corpse weight: "      <> m^.corpseWeight    .to commaShow
       , "Corpse volume: "      <> m^.corpseVol       .to commaShow
       , "Corpse capacity: "    <> m^.corpseCapacity  .to commaShow
       , "Corpse decomp secs: " <> m^.corpseDecompSecs.to commaShow
       , "Following: "          <> descMaybeId ms (getFollowing i ms)
       , "Followers: "          <> descSingIdHelper getFollowers
       , "My group: "           <> descSingIdHelper getMyGroup
       , "Member of: "          <> descMaybeId ms (getMemberOf i ms)
       , "Stomach: "            <> m^.stomach.to ppList
       , "Stomach ratio: "      <> let (mouths, size, perFull) = each %~ showText $ ( length . getStomach i $ ms
                                                                                    , calcStomachSize     i   ms
                                                                                    , calcStomachPerFull  i   ms )
                                   in T.concat [ mouths, " / ", size, " ", parensQuote $ perFull <> "%" ]
       , "Feeling map: "        <> let f tag feel = (tag |<>| pp feel :)
                                   in noneOnNull . commas . views feelingMap (M.foldrWithKey f []) $ m
       , "Now eating: "         <> m^.nowEating  .to (fromMaybe none)
       , "Now drinking: "       <> m^.nowDrinking.to (maybe none drinkHelper)
       , encHelper i ms ]
  where
    rmHelper ri                        = getRmName ri ms |<>| parensQuote (showText ri)
    drinkHelper (view liqNoun -> n, s) = f n |<>| parensQuote s
      where
        f (DoArticle    t) = t
        f (Don'tArticle t) = t


encHelper :: HasCallStack => Id -> MudState -> Text
encHelper i ms = let (w, maxEnc, encPer) = (calcWeight i ms, calcMaxEnc i ms, calcEncPer i ms) & each %~ showText
                 in T.concat [ "Weight/max enc: ", w, " / ", maxEnc, " ", parensQuote $ encPer <> "%" ]


examineNpc :: HasCallStack => ExamineHelper
examineNpc i ms = [ "Possessor: " <> (descMaybeId ms . getPossessor i $ ms) ]


examineObj :: HasCallStack => ExamineHelper
examineObj i ms = let o = getObj i ms in [ "Weight: "       <> o^.objWeight.to commaShow
                                         , "Volume: "       <> o^.objVol   .to commaShow
                                         , "Taste: "        <> o^.objTaste .to (fromMaybe none)
                                         , "Object flags: " <> (commas . dropBlanks . descFlags $ o) ]
  where
    descFlags o | o^.objFlags == zeroBits = none
                | otherwise               = let pairs = [ (isBiodegradable, "biodegradable")
                                                        , (isHumming,       "humming"      ) ]
                                            in [ f o |?| t | (f, t) <- pairs ]


examinePC :: HasCallStack => ExamineHelper
examinePC i ms = let p = getPC i ms in [ "Entry in the PCSingTbl: " <> ms^.pcSingTbl .to f
                                       , "Race: "                   <> p ^.race      .to pp
                                       , "Known names: "            <> p ^.introduced.to (noneOnNull . commas)
                                       , "Links: "                  <> p ^.linked    .to (noneOnNull . commas)
                                       , "Skill points: "           <> p ^.skillPts  .to commaShow ]
  where
    f = noneOnNull . commas . map h . filter g . M.toList
    g = (||) <$> (== i) . snd  <*> (== getSing i ms) . fst
    h = parensQuote . uncurry (<>) . ((<> ", ") *** showText)


examinePickPts :: HasCallStack => ExamineHelper
examinePickPts i = pure . ("Pick pts: " <>) . views pickPtsTbl (maybe none commaShow . (i `IM.lookup`))


examinePla :: HasCallStack => ExamineHelper
examinePla i ms = let p = getPla i ms
                  in [ "Host: "              <> p^.currHostName   .to (noneOnNull . T.pack)
                     , "Connect time: "      <> p^.connectTime    .to (maybe none showText)
                     , "Login time: "        <> p^.loginTime      .to (maybe none showText)
                     , "Disconnect time: "   <> p^.disconnectTime .to (maybe none showText)
                     , "Player flags: "      <> (commas . dropBlanks . descFlags $ p)
                     , "Columns: "           <> p^.columns     .to showText
                     , "Lines: "             <> p^.pageLines   .to showText
                     , "Peepers: "           <> p^.peepers     .to helper
                     , "Peeping: "           <> p^.peeping     .to helper
                     , "Possessing: "        <> p^.possessing  .to (descMaybeId ms)
                     , "Retained messages: " <> p^.retainedMsgs.to (noneOnNull . slashes)
                     , "Logout room: "       <> let f ri = getRmName ri ms |<>| parensQuote (showText ri)
                                                in p^.logoutRmId.to (maybe none f)
                     , "Bonus time: "        <> p^.bonusTime.to (maybe none showText) ]
  where
    descFlags p | p^.plaFlags == zeroBits = none
                | otherwise               = let pairs = [ (hasRazzled,                  "has razzled"                   )
                                                        , (isAdmin,                     "admin"                         )
                                                        , (isGmcp,                      "gmcp"                          )
                                                        , (isIncognito,                 "incognito"                     )
                                                        , (isNotFirstAdminMsg,          "not first admin msg"           )
                                                        , (isNotFirstMobSay,            "not first mob say"             )
                                                        , (isNotFirstSpiritCmdNotFound, "not first spirit cmd not found")
                                                        , (isShowingFp,                 "showing FP"                    )
                                                        , (isShowingHp,                 "showing HP"                    )
                                                        , (isShowingMp,                 "showing MP"                    )
                                                        , (isShowingPp,                 "showing PP"                    )
                                                        , (isSpirit,                    "spirit"                        )
                                                        , (isTunedAdmin,                "tuned admin"                   )
                                                        , (isTunedQuestion,             "tuned question"                ) ]
                                            in [ f p |?| t | (f, t) <- pairs ]
    helper = noneOnNull . commas . map (`descSingId` ms)


examineRm :: HasCallStack => ExamineHelper
examineRm i ms = let r = getRm i ms in [ "Name: "           <> r^.rmName
                                       , "Description: "    <> r^.rmDesc  .to xformNls
                                       , "Listen: "         <> r^.rmListen.to (fromMaybe none)
                                       , "Smell: "          <> r^.rmSmell .to (fromMaybe none)
                                       , "Room flags: "     <> (commas . dropBlanks . descFlags $ r)
                                       , "Links: "          <> r^.rmLinks   .to (noneOnNull . commas . map linkHelper)
                                       , "Coordinates: "    <> r^.rmCoords  .to showText
                                       , "Environment: "    <> r^.rmEnv     .to pp
                                       , "Label: "          <> r^.rmLabel   .to (fromMaybe none)
                                       , "Hooks: "          <> r^.rmHookMap .to hookHelper
                                       , "Room functions: " <> r^.rmFunNames.to (noneOnNull . commas) ]
  where
    descFlags r | r^.rmFlags == zeroBits = none
                | otherwise              = none -- TODO: Rm flags.
    linkHelper  = \case (StdLink    dir destId moveCost    ) -> f (pp dir) destId moveCost
                        (NonStdLink dir destId moveCost _ _) -> f dir      destId moveCost
      where
        f dir destId moveCost = spaces [ dir
                                       , "to"
                                       , getRmName destId ms
                                       , parensQuote . showText $ destId
                                       , showText moveCost ]
    hookHelper hookMap | M.null hookMap = none
                       | otherwise      = commas . map f . M.toList $ hookMap
      where
        f (cmdName, hooks)     = T.concat [ cmdName, ": ", bracketQuote . slashes . map g $ hooks ]
        g (Hook name triggers) = parensQuote name |<>| commas triggers


xformNls :: HasCallStack => Text -> Text
xformNls = T.replace theNl (colorWith nlColor "\\n")


examineVessel :: HasCallStack => ExamineHelper
examineVessel i ms = let v = getVessel i ms in
    [ "Max mouthfuls: "   <> v^.vesselMaxMouthfuls.to showText
    , "Vessel contents: " <> v^.vesselCont        .to (descCont v) ] ++ views vesselCont (maybeEmp (descLiq . fst)) v
  where
    descCont _ Nothing       = none
    descCont v (Just (l, m)) = T.concat [ showText m
                                        , " mouthfuls of "
                                        , renderLiqNoun l aOrAn
                                        , " "
                                        , parensQuote $ showText (calcVesselPerFull v m) <> "%" ]
    descLiq l                = let dl = getDistinctLiqForLiq l ms
                               in [ "Distinct liquid ID: " <> l^.liqId.to showText
                                  , "Liquid smell: "       <> l^.liqSmellDesc.to noneOnNull
                                  , "Liquid taste: "       <> l^.liqTasteDesc.to noneOnNull
                                  , "Drink description: "  <> l^.liqDrinkDesc ] ++ dl^.liqEdibleEffects.to descEdibleEffects


examineWpn :: HasCallStack => ExamineHelper
examineWpn i ms = let w = getWpn i ms in [ "Type: "   <> w^.wpnSub.to pp
                                         , "Damage: " <> w^.wpnMinDmg.to showText <> " / " <> w^.wpnMaxDmg.to showText ]


examineWritable :: HasCallStack => ExamineHelper
examineWritable i ms = let w = getWritable i ms in [ "Message: "   <> w^.writMessage.to (maybe none (xformNls . fst))
                                                   , "Language: "  <> w^.writMessage.to (maybe none (pp . snd))
                                                   , "Recipient: " <> w^.writRecip  .to (fromMaybe none) ]


-----


adminExamineSelf :: HasCallStack => ActionFun
adminExamineSelf p@(NoArgs'' i) = adminExamine p { args = pure . showText $ i }
adminExamineSelf p              = withoutArgs adminExamineSelf p


-----


adminExp :: HasCallStack => ActionFun
adminExp (NoArgs' i mq) = logPlaExec (prefixAdminCmd "experience") i >> pager i mq Nothing mkReport
  where
    mkReport = header ++ pure zero ++ take 25 (map helper calcLvlExps)
    header   = [ "Level  Experience", T.replicate 17 "=" ]
    zero     = uncurry (<>) . dupFirst (pad 7) $ "0"
    helper   = (<>) <$> pad 7 . showText . fst <*> commaShow . snd
adminExp p = withoutArgs adminExp p


-----


adminFarewell :: HasCallStack => ActionFun
adminFarewell p@AdviseNoArgs            = advise p [ prefixAdminCmd "farewell" ] adviceAFarewellNoArgs
adminFarewell   (LowerNub i mq cols as) = getState >>= \ms ->
    let helper target | notFound <- pure . sorryPCName $ target
                      , found    <- \(targetId, _) -> mkFarewellStats targetId ms
                      = findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
    in do logPlaExecArgs (prefixAdminCmd "farewell") as i
          pager i mq Nothing . concat . wrapLines cols . intercalateDivider cols . map (helper . capitalize) $ as
adminFarewell p = patternMatchFail "adminFarewell" . showText $ p


-----


adminGods :: HasCallStack => ActionFun
adminGods (NoArgs i mq cols) = logPlaExec (prefixAdminCmd "gods") i >> (multiWrapSend mq cols . map pp . S.toList $ godSet)
adminGods p                  = withoutArgs adminGods p


-----


adminHash :: HasCallStack => ActionFun
adminHash p@AdviseNoArgs                      = advise p [ prefixAdminCmd "hash" ] adviceAHashNoArgs
adminHash p@AdviseOneArg                      = advise p [ prefixAdminCmd "hash" ] adviceAHashNoHash
adminHash   (WithArgs i mq cols [ pw, hash ]) = do
    logPlaExec (prefixAdminCmd "hash") i
    wrapSend mq cols $ if uncurry validatePassword ((hash, pw) & both %~ T.encodeUtf8)
      then "It's a match!"
      else "The plain-text password does not match the hashed password."
adminHash p = advise p [ prefixAdminCmd "hash" ] adviceAHashExcessArgs


-----


adminHolySymbol :: HasCallStack => ActionFun
adminHolySymbol p@AdviseNoArgs                                = advise p [ prefixAdminCmd "holysymbol" ] adviceAHolySymbolNoArgs
adminHolySymbol   (WithArgs i mq cols [ numTxt, godNameTxt ]) = case reads . T.unpack $ numTxt :: [(Int, String)] of
  [(n, "")] | not . inRange (1, 100) $ n -> sorryAmt
            | otherwise                  -> modifyStateSeq $ \ms ->
                let sorry               = (ms, ) . pure . wrapSend mq cols
                    found godName       = case filter ((== godName) . snd) [ (x, pp x) | x <- allGodNames ] of
                      []          -> notFound
                      ((gn, _):_) -> let et  = EntTemplate (Just "holy")
                                                           ("holy symbol of " <> pp gn) ("holy symbols of " <> pp gn)
                                                           (mkHolySymbolDesc gn)
                                                           Nothing
                                                           zeroBits
                                         ot  = ObjTemplate holySymbolWeight
                                                           holySymbolVol
                                                           Nothing
                                                           (setBit zeroBits . fromEnum $ IsBiodegradable)
                                         msg = T.concat [ "created ", showText n, " holy symbol", sOnNon1 n, " of ", god, "." ]
                                         god = maybe "unknown" pp . getGodForGodName $ gn
                                         helper 0 pair      = pair
                                         helper x (ms', fs) = let pair = dropFst . newHolySymbol ms' et ot (HolySymbol gn) $ i
                                                              in helper (pred x) . second (fs ++) $ pair
                                     in second (++ [ logPla "adminHolySymbol found" i msg
                                                   , wrapSend mq cols . capitalize $ msg ]) . helper n $ (ms, [])
                    notFound    = sorry . sorryHolySymbolGodName $ godNameTxt
                    allGodNames = allValues :: [GodName]
                in findFullNameForAbbrev (capitalize . T.toLower $ godNameTxt) (map pp allGodNames) |&| maybe notFound found
  _ -> sorryAmt
  where
    sorryAmt = wrapSend mq cols . sorryHolySymbolAmt $ numTxt
adminHolySymbol p = advise p [ prefixAdminCmd "holysymbol" ] adviceAHolySymbolExcessArgs


-----


adminHost :: HasCallStack => ActionFun
adminHost p@AdviseNoArgs            = advise p [ prefixAdminCmd "host" ] adviceAHostNoArgs
adminHost   (LowerNub i mq cols as) = getState >>= \ms -> do
    logPlaExecArgs (prefixAdminCmd "host") as i
    (now, zone) <- (,) <$> liftIO getCurrentTime <*> liftIO getCurrentTimeZone
    let helper target | notFound <- pure . sorryPCName $ target
                      , found    <- uncurry . mkHostReport ms now $ zone
                      = findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
    multiWrapSend mq cols . intercalate [""] . map (helper . capitalize) $ as
adminHost p = patternMatchFail "adminHost" . showText $ p


mkHostReport :: HasCallStack => MudState -> UTCTime -> TimeZone -> Id -> Sing -> [Text]
mkHostReport ms now zone i s = case getHostMap s ms of Nothing      -> pure . prd $ "There are no host records for " <> s
                                                       Just hostMap -> maybe oops (helper hostMap) . getConnectTime i $ ms
  where
    oops              = pure . prd $ "There is no connect time for " <> s
    helper hostMap ct | duration' <- ili |?| duration
                      , total     <- M.foldl (\acc -> views secsConnected (+ acc)) 0 hostMap + getSum duration'
                      , totalDesc <- "Grand total time connected: " <> renderIt total
                      = concat [ header, M.foldrWithKey f [] hostMap, pure totalDesc ]
      where
        ili      = isLoggedIn . getPla i $ ms
        duration = Sum . round $ now `diffUTCTime` ct
        renderIt = T.pack . renderSecs
        header   =
            [ s <> ": "
            , let ts = [ "in from ", T.pack . getCurrHostName i $ ms, " ", parensQuote . renderIt . getSum $ duration, "." ]
              in "Currently logged " <> (ili ? T.concat ts :? "out.") ]
        f host r = (T.concat [ T.pack host
                             , ": "
                             , let { n = r^.noOfLogouts; suffix = n > 1 |?| "s" } in showText n <> " time" <> suffix
                             , ", "
                             , views secsConnected renderIt r
                             , ", "
                             , views lastLogout (showText . utcToLocalTime zone) r ] :)


-----


adminIncognito :: HasCallStack => ActionFun
adminIncognito (NoArgs i mq cols) = modifyStateSeq $ \ms ->
    let s              = getSing i ms
        isIncog        = isIncognitoId i ms
        fs | isIncog   = [ logPla "adminIncognito helper fs" i "going visible."
                         , wrapSend mq cols "You are no longer incognito."
                         , bcastOtherAdmins i $ s <> " is no longer incognito." ]
           | otherwise = [ logPla "adminIncognito helper fs" i "going incognito."
                         , wrapSend mq cols "You have gone incognito."
                         , bcastOtherAdmins i $ s <> " has gone incognito." ]
    in (ms & plaTbl.ind i %~ setPlaFlag IsIncognito (not isIncog), fs)
adminIncognito p = withoutArgs adminIncognito p


-----


adminIp :: HasCallStack => ActionFun
adminIp (NoArgs i mq cols) = do
    logPlaExec (prefixAdminCmd "ip") i
    ifList <- liftIO mkInterfaceList
    multiWrapSend mq cols [ "Interfaces: " <> ifList, prd $ "Listening on port " <> showText port ]
adminIp p = withoutArgs adminIp p


-----


adminKill :: HasCallStack => ActionFun
adminKill p@AdviseNoArgs            = advise p [ prefixAdminCmd "kill" ] adviceAKillNoArgs
adminKill   (LowerNub i mq cols as) = getState >>= \ms -> do
    let (is, toSelfs) = helper ms
        f             = logPla (prefixAdminCmd "kill")
    unless (()# is) $ do f i . prd . ("killing " <>) . commas . map (`descSingId` ms) $ is
                         forM_ is . flip f $ prd ("killed by " <> getSing i ms)
    multiWrapSend mq cols toSelfs
    bcast . mkBs ms $ is
    mapM_ handleDeath is
  where
    helper ms = foldl' f mempties as
      where
        f pair a = case reads . T.unpack $ a :: [(Int, String)] of
          [(targetId, "")] | targetId < 0                -> sorryHelper sorryWtf
                           | not . hasType targetId $ ms -> sorry
                           | otherwise                   -> go targetId
          _                                              -> sorry
          where
            sorryHelper msg = pair & _2 <>~ pure msg
            sorry           = sorryHelper . sorryParseId $ a
            go targetId     | targetId == i     = sorryHelper sorryKillSelf
                            | isNpc targetId ms = kill
                            | isPC  targetId ms =
                                if | isAdminId     targetId   ms -> sorryHelper . sorryKillAdmin  $ singId
                                   | isDead        targetId   ms -> sorryHelper . sorryKillDead   $ singId
                                   | not . isAwake targetId $ ms -> sorryHelper . sorryKillAsleep $ singId
                                   | isSpiritId    targetId   ms -> sorryHelper . sorryKillSpirit $ singId
                                   | isAdHoc       targetId   ms -> sorryHelper . sorryKillAdHoc  $ singId
                                   | otherwise                   -> kill
                            | otherwise = sorryHelper . sorryKillType $ targetId
              where
                kill   = pair & _1 <>~ pure targetId
                              & _2 <>~ pure (prd $ "You kill " <> aOrAnOnLower singId)
                singId = descSingId targetId ms
    mkBs ms = concatMap f
      where
        f targetId = let d        = mkStdDesig targetId ms Don'tCap
                         toTarget = g (nl $ adminKillMsg "you are",              pure targetId               )
                         toOthers = g (nl . adminKillMsg $ serialize d <> " is", targetId `delete` desigIds d)
                     in [ toTarget, toOthers ]
        g          = first (colorWith adminKillColor)
adminKill p = patternMatchFail "adminKill" . showText $ p


-----


adminLink :: HasCallStack => ActionFun
adminLink p@AdviseNoArgs            = advise p [ prefixAdminCmd "link" ] adviceALinkNoArgs
adminLink   (LowerNub i mq cols as) = getState >>= \ms -> do
    logPlaExecArgs (prefixAdminCmd "link") as i
    let helper target =
            let notFound              = unadulterated . sorryPCName $ target
                found (_, targetSing) = (\case
                  [] -> header none
                  ss -> header . mkReport $ ss) <$> liftIO (lookupTeleNames targetSing)
                  where
                    header          = (targetSing <> "'s two-way links:" :)
                    mkReport     ss | pairs <- sortBy (flip compare `on` fst) . mkCountSings $ ss
                                    = map (uncurry (flip (|<>|)) . first (parensQuote . showText)) pairs
                    mkCountSings ss = [ (length &&& head) g | g <- sortGroup . map fromOnly $ ss ]
            in findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
    pager i mq Nothing . noneOnNull . intercalateDivider cols =<< forM as (helper . capitalize . T.toLower)
adminLink p = patternMatchFail "adminLink" . showText $ p


-----


adminLocate :: HasCallStack => ActionFun
adminLocate p@AdviseNoArgs            = advise p [ prefixAdminCmd "locate" ] adviceALocateNoArgs
adminLocate   (LowerNub i mq cols as) = getState >>= \ms ->
    let helper a = case reads . T.unpack $ a :: [(Int, String)] of
          [(targetId, "")] | targetId < 0                -> sorryWtf
                           | not . hasType targetId $ ms -> sorryParseId a
                           | otherwise                   -> locate targetId
          _                                              -> sorryParseId a
          where
            locate targetId = let (_, desc) = locateHelper ms [] targetId
                              in mkNameTypeIdDesc targetId ms <> (()!# desc |?| (", " <> desc))
    in do logPlaExecArgs (prefixAdminCmd "locate") as i
          multiWrapSend mq cols . intersperse "" . map helper $ as
adminLocate p = patternMatchFail "adminLocate" . showText $ p


-----


adminMoon :: HasCallStack => ActionFun
adminMoon (NoArgs i mq cols) = getMoonPhaseForDayOfMonth . curryDayOfMonth <$> liftIO getCurryTime >>= f . \case
  Nothing    -> "The phase of the moon cannot be determined."
  Just phase -> "The moon is in its " <> pp phase <> " phase."
  where
    f = (>>) <$> logPlaOut (prefixAdminCmd "moon") i . pure <*> wrapSend mq cols
adminMoon p = withoutArgs adminMoon p


-----


adminMsg :: HasCallStack => ActionFun
adminMsg p@AdviseNoArgs                         = advise p [ prefixAdminCmd "message" ] adviceAMsgNoArgs
adminMsg p@AdviseOneArg                         = advise p [ prefixAdminCmd "message" ] adviceAMsgNoMsg
adminMsg   (MsgWithTarget i mq cols target msg) = getState >>= helper >>= (|#| mapM_ (uncurry . logPla $ "adminMsg"))
  where
    helper ms =
        let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to message"
            s                   = getSing i ms
            notFound            = emptied . sendFun . sorryRegPlaName $ strippedTarget
            found pair@(targetId, targetSing) = case emotifyTwoWay (prefixAdminCmd "message") i ms targetId msg of
              Left  errorMsgs  -> emptied . multiSendFun $ errorMsgs
              Right (Right bs) -> ioHelper pair bs
              Right (Left  ()) -> case expCmdifyTwoWay i ms targetId targetSing msg of
                Left  errorMsg -> emptied . sendFun $ errorMsg
                Right bs       -> ioHelper pair bs
            ioHelper (targetId, targetSing) [ fst -> toSelf, fst -> toTarget ] = if
              | isLoggedIn targetPla, isIncognitoId i ms -> emptied . sendFun $ sorryMsgIncog
              | isLoggedIn targetPla                     ->
                  let (targetMq, targetCols) = getMsgQueueColumns targetId ms
                      adminSings             = map snd . filter f . mkAdminIdSingList $ ms
                      f (_, "Root")          = isAwake iRoot ms
                      f _                    = True
                      me                     = head . filter g . styleAbbrevs Don'tQuote $ adminSings
                      g                      = (== s) . dropANSI
                      toTarget'              = quoteWith "__" me |<>| toTarget
                  in do
                      sendFun formatted
                      (multiWrapSend targetMq targetCols =<<) $ if not (isAdHoc targetId ms) && isNotFirstAdminMsg targetPla
                        then unadulterated toTarget'
                        else [ toTarget' : hints | hints <- firstAdminMsg targetId s ]
                      dbHelper
              | otherwise -> do multiSendFun [ formatted, parensQuote "Message retained." ]
                                retainedMsg targetId ms . mkRetainedMsgFromPerson s $ toTarget
                                dbHelper
              where
                targetPla = getPla targetId ms
                formatted = parensQuote ("to " <> targetSing) <> spaced (quoteWith "__" s) <> toSelf
                dbHelper  = do ts <- liftIO mkTimestamp
                               withDbExHandler_ "admin_msg" . insertDbTblAdminMsg . AdminMsgRec ts s targetSing $ toSelf
                               return [ sentLogMsg, receivedLogMsg ]
                sentLogMsg     = (i,        T.concat [ "sent message to ", targetSing, ": ", toSelf   ])
                receivedLogMsg = (targetId, T.concat [ "received message from ", s,    ": ", toTarget ])
            ioHelper _ xs = patternMatchFail "adminMsg helper ioHelper" . showText $ xs
        in findFullNameForAbbrev strippedTarget (mkPlaIdSingList ms) |&| maybe notFound found
adminMsg p = patternMatchFail "adminMsg" . showText $ p


firstAdminMsg :: HasCallStack => Id -> Sing -> MudStack [Text]
firstAdminMsg i adminSing =
    modifyState $ (, [ "", hintAMsg adminSing ]) . (plaTbl.ind i %~ setPlaFlag IsNotFirstAdminMsg True)


-----


adminMyChans :: HasCallStack => ActionFun
adminMyChans p@AdviseNoArgs            = advise p [ prefixAdminCmd "mychannels" ] adviceAMyChansNoArgs
adminMyChans   (LowerNub i mq cols as) = getState >>= \ms ->
    let helper target = let notFound                     = pure . sorryPCName $ target
                            found (targetId, targetSing) = case getPCChans targetId ms of
                              [] -> header none
                              cs -> header . intercalate [""] . map (mkChanReport i ms) $ cs
                              where
                                header = (targetSing <> "'s channels:" :) . ("" :)
                        in findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
        allReports = intercalateDivider cols . map (helper . capitalize) $ as
    in case views chanTbl IM.size ms of
      0 -> informNoChans mq cols
      _ -> logPlaExec (prefixAdminCmd "mychannels") i >> pager i mq Nothing allReports
adminMyChans p = patternMatchFail "adminMyChans" . showText $ p


-----


adminPassword :: HasCallStack => ActionFun
adminPassword p@AdviseNoArgs = advise p [ prefixAdminCmd "password" ] adviceAPasswordNoArgs
adminPassword p@AdviseOneArg = advise p [ prefixAdminCmd "password" ] adviceAPasswordNoPw
adminPassword p@(WithTarget i mq cols target pw)
  | length (T.words pw) > 1 = advise p [ prefixAdminCmd "password" ] adviceAPasswordExcessArgs
  | otherwise               = getState >>= \ms ->
      let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player whose password you wish to change"
          changePW            = join <$> withDbExHandler fn (lookupPW strippedTarget) >>= \case
            Nothing    -> dbError mq cols
            Just oldPW -> let msg      = T.concat [ getSing i ms, " is changing ", strippedTarget, "'s password" ]
                              oldPwMsg = prd . spcL $ parensQuote ("was " <> dblQuote oldPW)
                          in do logPla fn i . T.concat $ [ "changing ", strippedTarget, "'s password", oldPwMsg ]
                                logNotice fn $ msg <> oldPwMsg
                                bcastOtherAdmins i . prd $ msg
                                withDbExHandler_ fn . insertDbTblUnPw . UnPwRec strippedTarget $ pw
                                sendFun $ strippedTarget <> "'s password has been changed."
      in if
        | not . inRange (minPwLen, maxPwLen) . T.length $ pw -> sendFun sorryInterpNewPwLen
        | helper isUpper                                     -> sendFun sorryInterpNewPwUpper
        | helper isLower                                     -> sendFun sorryInterpNewPwLower
        | helper isDigit                                     -> sendFun sorryInterpNewPwDigit
        | otherwise -> case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
          []         -> sendFun $ sorryPCName strippedTarget |<>| hintAPassword
          [targetId] -> let targetPla = getPla targetId ms in if | targetId == i     -> sendFun sorryAdminPasswordSelf
                                                                 | isAdmin targetPla -> sendFun sorryAdminPasswordAdmin
                                                                 | otherwise         -> changePW
          xs         -> patternMatchFail "adminPassword" . showText $ xs
  where
    fn       = "adminPassword"
    helper f = ()# T.filter f pw
adminPassword p = patternMatchFail "adminPassword" . showText $ p


-----


adminPeep :: HasCallStack => ActionFun
adminPeep p@AdviseNoArgs            = advise p [ prefixAdminCmd "peep" ] adviceAPeepNoArgs
adminPeep   (LowerNub i mq cols as) = do (msgs, unzip -> (logMsgsSelf, logMsgsOthers)) <- modifyState helper
                                         logPla "adminPeep" i . prd . slashes $ logMsgsSelf
                                         forM_ logMsgsOthers . uncurry . logPla $ "adminPeep"
                                         multiWrapSend mq cols msgs
  where
    helper ms =
        let s     = getSing i ms
            apiss = [ apis | apis@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
            peep target a@(pt, _, _) =
                let notFound = a & _2 %~ (sorryPCNameLoggedIn target :)
                    found (peepId@(flip getPla ms -> peepPla), peepSing) = if peepId `notElem` pt^.ind i.peeping
                      then if | peepId == i     -> a & _2 %~ (sorryPeepSelf  :)
                              | isAdmin peepPla -> a & _2 %~ (sorryPeepAdmin :)
                              | otherwise       ->
                                let pt'     = pt & ind i     .peeping %~ (peepId :)
                                                 & ind peepId.peepers %~ (i      :)
                                    msg     = prd $ "You are now peeping " <> peepSing
                                    logMsgs = [("started peeping " <> peepSing, (peepId, s <> " started peeping."))]
                                in a & _1 .~ pt' & _2 %~ (msg :) & _3 <>~ logMsgs
                      else let pt'     = pt & ind i     .peeping %~ (peepId `delete`)
                                            & ind peepId.peepers %~ (i      `delete`)
                               msg     = prd $ "You are no longer peeping " <> peepSing
                               logMsgs = [("stopped peeping " <> peepSing, (peepId, s <> " stopped peeping."))]
                           in a & _1 .~ pt' & _2 %~ (msg :) & _3 <>~ logMsgs
                in findFullNameForAbbrev target apiss |&| maybe notFound found
            res = foldr (peep . capitalize) (ms^.plaTbl, [], []) as
        in (ms & plaTbl .~ res^._1, (_2 `fanView` _3) res)
adminPeep p = patternMatchFail "adminPeep" . showText $ p


-----


adminPersist :: HasCallStack => ActionFun
adminPersist (NoArgs' i mq) = logPlaExec (prefixAdminCmd "persist") i >> persist >> ok mq
adminPersist p              = withoutArgs adminPersist p


-----


adminPossess :: HasCallStack => ActionFun
adminPossess p@(NoArgs' i mq) = advise p [ prefixAdminCmd "possess" ] adviceAPossessNoArgs >> sendDfltPrompt mq i
adminPossess   (OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The ID of the NPC you wish to possess"
        possess targetId    = if isNpc targetId ms
          then maybe canPossess can'tPossess . getPossessor targetId $ ms
          else sorry . sorryPossessType $ targetSing
          where
            targetSing      = getSing targetId ms
            can'tPossess pi = sorry . sorryAlreadyPossessed targetSing . getSing pi $ ms
            canPossess      = ( upd ms [ plaTbl.ind i       .possessing   ?~ targetId
                                       , npcTbl.ind targetId.npcPossessor ?~ i ]
                              , [ logPla "adminPossess" i logMsg
                                , sendFun . prd $ "You are now possessing " <> aOrAnOnLower targetSing
                                , sendDfltPrompt mq targetId ] )
            logMsg          = prd $ "started possessing " <> aOrAnOnLower (descSingId targetId ms)
        sorry txt = (ms, [ sendFun txt, sendDfltPrompt mq i ])
    in case reads . T.unpack $ strippedTarget :: [(Int, String)] of
      [(targetId, "")]
        | targetId < 0                -> sorry sorryWtf
        | not . hasType targetId $ ms -> sorry . sorryParseId $ strippedTarget'
        | otherwise                   -> case getPossessing i ms of
          Nothing -> possess targetId
          Just pi -> sorry . sorryAlreadyPossessing . getSing pi $ ms
      _ -> sorry . sorryParseId $ strippedTarget'
adminPossess ActionParams { myId, plaMsgQueue, plaCols } = do wrapSend plaMsgQueue plaCols adviceAPossessExcessArgs
                                                              sendDfltPrompt plaMsgQueue myId


-----


adminPrint :: HasCallStack => ActionFun
adminPrint p@AdviseNoArgs    = advise p [ prefixAdminCmd "print" ] adviceAPrintNoArgs
adminPrint   (Msg' i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
    liftIO . T.putStrLn $ bracketQuote s |<>| colorWith printConsoleColor msg
    ok mq
adminPrint p = patternMatchFail "adminPrint" . showText $ p


-----


adminProfanity :: HasCallStack => ActionFun
adminProfanity p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "profanity" f "profanity" p
  where
    f :: HasCallStack => [ProfRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminSearch :: HasCallStack => ActionFun
adminSearch p@AdviseNoArgs                          = advise p [ prefixAdminCmd "search" ] adviceASearchNoArgs
adminSearch   (WithArgs i mq cols (T.unwords -> a)) = do
    logPlaExecArgs (prefixAdminCmd "search") (pure a) i
    multiWrapSend mq cols =<< (middle (++) [""] <$> descMatchingSings <*> descMatchingRmNames) <$> getState
  where
    descMatchingSings ms =
      let idSings = views entTbl (map (_2 %~ view sing) . IM.toList) ms
      in "IDs with matching entity names:" : (noneOnNull . map (descMatch ms True) . getMatches $ idSings)
    descMatchingRmNames ms =
      let idNames = views rmTbl (map (_2 %~ view rmName) . IM.toList) ms
      in "Room IDs with matching room names:" : (noneOnNull . map (descMatch ms False) . getMatches $ idNames)
    getMatches = filter (views _2 (()!#) . snd) . map (second (applyRegex a))
    descMatch ms b (i', (x, y, z)) = T.concat [ padId . showText $ i'
                                              , " "
                                              , b |?| spcR . parensQuote . pp . getType i' $ ms
                                              , x
                                              , colorWith regexMatchColor y
                                              , z ]
adminSearch p = patternMatchFail "adminSearch" . showText $ p


applyRegex :: HasCallStack => Text -> Text -> (Text, Text, Text)
applyRegex searchTerm target = let (🍩) = (=~) `on` T.unpack in target 🍩 searchTerm |&| each %~ T.pack


-----


adminSecurity :: HasCallStack => ActionFun
adminSecurity p@AdviseNoArgs            = advise p [ prefixAdminCmd "security" ] adviceASecurityNoArgs
adminSecurity   (LowerNub i mq cols as) = withDbExHandler "adminSecurity" (getDbTblRecs "sec") >>= \case
  Just (recs :: [SecRec]) -> do
      logPlaExecArgs (prefixAdminCmd "security") as i
      multiWrapSend mq cols . intercalateDivider cols . concatMap (helper recs . capitalize . T.toLower) $ as
  Nothing   -> dbError mq cols
  where
    helper recs target = case filter ((target `T.isPrefixOf`) . (dbName :: SecRec -> Text)) recs of
      []      -> pure . pure . prd $ "No records found for " <> dblQuote target
      matches -> map mkSecReport matches
adminSecurity p = patternMatchFail "adminSecurity" . showText $ p


mkSecReport :: HasCallStack => SecRec -> [Text]
mkSecReport SecRec { .. } = [ "Name: "     <> dbName
                            , "Question: " <> dbQ
                            , "Answer: "   <> dbA ]


-----


adminSet :: HasCallStack => ActionFun
adminSet p@AdviseNoArgs = advise p [ prefixAdminCmd "set" ] adviceASetNoArgs
adminSet p@AdviseOneArg = advise p [ prefixAdminCmd "set" ] adviceASetNoSettings
adminSet   (WithArgs i mq cols (target:rest)) =
    helper |&| modifyState >=> \(toSelfMsgs, mTargetId, toTargetMsgs, logMsgs, fs) ->
        let ioHelper targetId = getState >>= \ms ->
                let f msg = (colorWith adminSetColor msg |&|) $ case getType targetId ms of
                        PCType  -> retainedMsg targetId ms
                        NpcType -> bcast . mkBcast targetId
                        t       -> patternMatchFail "adminSet f" . showText $ t
                in do logMsgs |#| logPla (prefixAdminCmd "set") i . g . slashes
                      unless (isIncognitoId i ms || targetId == i) . mapM_ f . dropBlanks $ toTargetMsgs
                      sequence_ fs
              where
                g = (parensQuote ("for ID " <> showText targetId) <>) . spcL
        in multiWrapSend mq cols toSelfMsgs >> maybeVoid ioHelper mTargetId
  where
    helper ms = case reads . T.unpack $ target :: [(Int, String)] of
      [(targetId, "")] | targetId < 0                -> sorryHelper sorryWtf
                       | not . hasType targetId $ ms -> sorry
                       | otherwise                   -> f targetId
      _ -> sorry
      where
        sorry       = sorryHelper . sorryParseId $ target
        sorryHelper = (ms, ) . (, Nothing, [], [], []) . pure
        f targetId  = maybe (sorryHelper sorryQuoteChars) g . procQuoteChars $ rest
          where
            g rest' = let (ms', toSelfMsgs, toTargetMsgs, logMsgs, fs) = foldl' (setHelper targetId)
                                                                                (ms, [], [], [], [])
                                                                                rest'
                      in (ms', (toSelfMsgs, Just targetId, toTargetMsgs, logMsgs, fs))
adminSet p = patternMatchFail "adminSet" . showText $ p


setHelper :: HasCallStack => Id
                          -> (MudState, [Text], [Text], [Text], Funs)
                          -> Text
                          -> (MudState, [Text], [Text], [Text], Funs)
setHelper targetId a@(ms, toSelfMsgs, _, _, _) arg = if
  | "+=" `T.isInfixOf` arg -> breakHelper AddAssign
  | "-=" `T.isInfixOf` arg -> breakHelper SubAssign
  | "="  `T.isInfixOf` arg -> breakHelper Assign
  | otherwise              -> sorry
  where
    breakHelper op = case T.breakOn (pp op) arg of ("", _) -> sorry
                                                   (_, "") -> sorry
                                                   pair    | l <- T.length . pp $ op
                                                           -> helper op . second (T.drop l) $ pair
    sorry = let msg = sorryParseArg arg
                f   = any (adviceASetInvalid `T.isInfixOf`) toSelfMsgs ?  (++ pure msg)
                                                                       :? (++ [ msg <> adviceASetInvalid ])
            in a & _2 %~ f
    helper op (T.toLower -> key, value) = findFullNameForAbbrev key keyNames |&| maybe notFound found
      where
        keyNames    = [ "entname"
                      , "sing"
                      , "plur"
                      , "entdesc"
                      , "entsmell"
                      , "sex"
                      , "st"
                      , "dx"
                      , "ht"
                      , "ma"
                      , "ps"
                      , "curhp"
                      , "curmp"
                      , "curpp"
                      , "curfp"
                      , "maxhp"
                      , "maxmp"
                      , "maxpp"
                      , "maxfp"
                      , "exp"
                      , "hand"
                      , "knownlangs"
                      , "mobrmdesc"
                      , "tempdesc"
                      , "mobsize"
                      , "corpseweight"
                      , "corpsevol"
                      , "corpsecapacity"
                      , "corpsedecompsecs"
                      , "following"
                      , "followers"
                      , "mygroup"
                      , "memberof"
                      , "race"
                      , "introduced"
                      , "linked"
                      , "skillpts" ] :: [Text]
        notFound    = appendMsg . sorryAdminSetKey $ key
        appendMsg m = a & _2 <>~ pure m
        found       = let t = getType targetId ms in \case
          "entname"          -> setEntMaybeTextHelper  t "entName"  "name"        entName  entName
          "sing"             -> setEntSingHelper       t
          "plur"             -> setEntTextHelper       t "plur"     "plural"      plur     plur
          "entdesc"          -> setEntTextHelper       t "entDesc"  "description" entDesc  entDesc
          "entsmell"         -> setEntMaybeTextHelper  t "entSmell" "smell"       entSmell entSmell
          "sex"              -> setMobSexHelper        t
          "st"               -> setMobAttribHelper     t "st" "ST" st st
          "dx"               -> setMobAttribHelper     t "dx" "DX" dx dx
          "ht"               -> setMobAttribHelper     t "ht" "HT" ht ht
          "ma"               -> setMobAttribHelper     t "ma" "MA" ma ma
          "ps"               -> setMobAttribHelper     t "ps" "PS" ps ps
          "curhp"            -> setMobXpsHelper        t "curHp" "HP"     (\i -> fst . getHps i) curHp
          "curmp"            -> setMobXpsHelper        t "curMp" "MP"     (\i -> fst . getMps i) curMp
          "curpp"            -> setMobXpsHelper        t "curPp" "PP"     (\i -> fst . getPps i) curPp
          "curfp"            -> setMobXpsHelper        t "curFp" "FP"     (\i -> fst . getFps i) curFp
          "maxhp"            -> setMobXpsHelper        t "maxHp" "max HP" (\i -> snd . getHps i) maxHp
          "maxmp"            -> setMobXpsHelper        t "maxMp" "max MP" (\i -> snd . getMps i) maxMp
          "maxpp"            -> setMobXpsHelper        t "maxPp" "max PP" (\i -> snd . getPps i) maxPp
          "maxfp"            -> setMobXpsHelper        t "maxFp" "max FP" (\i -> snd . getFps i) maxFp
          "exp"              -> setMobExpHelper        t
          "hand"             -> setMobHandHelper       t
          "knownlangs"       -> setMobKnownLangsHelper t
          "mobrmdesc"        -> setMobRmDescHelper     t
          "tempdesc"         -> setMobTempDescHelper   t
          "mobsize"          -> setMobSizeHelper       t
          "corpseweight"     -> setMobCorpseHelper     t "corpseWeight"   "corpse weight"   corpseWeight   corpseWeight
          "corpsevol"        -> setMobCorpseHelper     t "corpseVol"      "corpse volume"   corpseVol      corpseVol
          "corpsecapacity"   -> setMobCorpseHelper     t "corpseCapacity" "corpse capacity" corpseCapacity corpseCapacity
          "corpsedecompsecs" -> setMobCorpseHelper     t "corpseDecompSecs" "corpse decomp secs" corpseDecompSecs corpseDecompSecs
          "following"        -> setMobFollowingHelper  t
          "followers"        -> setMobInvHelper        t "followers" "followers have" (party.followers) (party.followers)
          "mygroup"          -> setMobInvHelper        t "myGroup"   "group has"      (party.myGroup  ) (party.myGroup  )
          "memberof"         -> setMobMemberOfHelper   t
          "race"             -> setPCRaceHelper        t
          "introduced"       -> setPCSingListHelper    t "introduced" "known names"  introduced introduced
          "linked"           -> setPCSingListHelper    t "linked"     "linked names" linked     linked
          "skillpts"         -> setPCSkillPtsHelper    t
          x                  -> patternMatchFail "setHelper helper found" (x :: Text)
        -----
        setEntMaybeTextHelper t k n getter setter
          | not . hasEnt $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set ", k, " to ", showMaybe x, mkDiffTxt isDiff, "." ]
                            prev     = view getter . getEnt targetId $ ms
                            isDiff   = x /= prev
                            toTarget = pure . T.concat $ [ "Your ", n, " has changed to ", showMaybe x, "." ]
                        in a & _1.entTbl.ind targetId.setter .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ ((isNpcPC targetId ms && isDiff) |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp k
        -----
        setEntSingHelper t
          | not . hasEnt $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "sing" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set sing to ", dblQuote x, mkDiffTxt isDiff, "." ]
                            prev     = getSing targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . prd $ "Your singular has changed to " <> dblQuote x
                        in a & _1.entTbl   .ind targetId.sing .~ x
                             & _1.pcSingTbl.at  prev .~ Nothing
                             & _1.pcSingTbl.at  x    ?~ targetId
                             & _2 <>~ toSelf
                             & _3 <>~ ((isNpcPC targetId ms && isDiff) |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "sing"
        -----
        setEntTextHelper t k n getter setter
          | not . hasEnt $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set ", k, " to ", dblQuote x, mkDiffTxt isDiff, "." ]
                            prev     = view getter . getEnt targetId $ ms
                            isDiff   = x /= prev
                            toTarget = pure . T.concat $ [ "Your ", n, " has changed to ", dblQuote x, "." ]
                        in a & _1.entTbl.ind targetId.setter .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ ((isNpcPC targetId ms && isDiff) |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp k
        -----
        setMobSexHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "sex" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set sex to ", pp x, mkDiffTxt isDiff, "." ]
                            prev     = getSex targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . prd $ "Your sex has changed to " <> pp x
                        in a & _1.mobTbl.ind targetId.sex .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "sex"
        -----
        setMobAttribHelper t k n getter setter
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> let prev                 = view getter . getMob targetId $ ms
                           addSubAssignHelper f = let new    = max1 $ prev `f` x
                                                      diff   = new - prev
                                                      toSelf = mkToSelfForInt k new diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ new
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> let new    = max1 x
                                                      diff   = new - prev
                                                      toSelf = mkToSelfForInt k new diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ new
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have gained ", commaShow diff,         " ", n, "." ]
                            | otherwise = pure . T.concat $ [ "You have lost ",   commaShow . abs $ diff, " ", n, "." ]
        -----
        setMobXpsHelper t k n f setter
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> let prev                 = f targetId ms
                           addSubAssignHelper g = let new    = prev `g` x
                                                      diff   = new - prev
                                                      toSelf = mkToSelfForInt k new diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ new
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> let diff   = x - prev
                                                      toSelf = mkToSelfForInt k x diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ x
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have gained ", commaShow diff,         " ", n, "." ]
                            | otherwise = pure . T.concat $ [ "You have lost ",   commaShow . abs $ diff, " ", n, "." ]
        -----
        setMobExpHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "exp" $ value
            Right x -> let prev                 = getExp targetId ms
                           addSubAssignHelper g = f $ max0 (prev `g` x)
                           f new                = let diff   = new - prev
                                                      toSelf = mkToSelfForInt "exp" new diff
                                                      a' | isPC targetId ms
                                                         , diff > 0
                                                         = a & _5 <>~ pure (awardExp diff (prefixAdminCmd "set") targetId)
                                                         | otherwise
                                                         = a & _1.mobTbl.ind targetId.exp .~ new
                                                  in a' & _2 <>~ toSelf
                                                        & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                        & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> f . max0 $ x
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have been awarded ", commaShow diff, " experience points." ]
                            | otherwise = pure . T.concat $ [ "You have lost ", commaShow . abs $ diff, " experience points." ]
        -----
        setMobHandHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "hand" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set hand to ", pp x, mkDiffTxt isDiff, "." ]
                            prev     = getHand targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . prd $ "Your handedness has changed to " <> pp x
                        in a & _1.mobTbl.ind targetId.hand .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "hand"
        -----
        setMobKnownLangsHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _              -> appendMsg . sorryAdminSetValue "knownLangs" $ value
            Right (nubSort -> x) ->
                let prev                 = getKnownLangs targetId ms
                    addSubAssignHelper f = let (new, toSelf, isDiff) = mkTupleForList prev f x mkToSelf
                                           in a & _1.mobTbl.ind targetId.knownLangs .~ new
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget new)
                                                & _4 <>~ (isDiff |?| toSelf)
                in case op of Assign    -> let toSelf = mkToSelf x isDiff
                                               isDiff = x /= prev
                                           in a & _1.mobTbl.ind targetId.knownLangs .~ x
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget x)
                                                & _4 <>~ (isDiff |?| toSelf)
                              AddAssign -> addSubAssignHelper (++)
                              SubAssign -> addSubAssignHelper (\\)
          where
            mkToSelf   x isDiff = pure . T.concat $ [ "Set knownLangs to ", ppList x, mkDiffTxt isDiff, "." ]
            mkToTarget x        = pure . prd $ "Your known languages have changed to " <> ppList x
        -----
        setMobRmDescHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "mobRmDesc" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set mobRmDesc to ", showMaybe x, mkDiffTxt isDiff, "." ]
                            prev     = getMobRmDesc targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . T.concat $ [ "Your room description has changed to ", showMaybe x, "." ]
                        in a & _1.mobTbl.ind targetId.mobRmDesc .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "mobRmDesc"
        -----
        setMobTempDescHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "tempDesc" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set tempDesc to ", showMaybe x, mkDiffTxt isDiff, "." ]
                            prev     = getTempDesc targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . T.concat $ [ "Your temporary character description has changed to "
                                                         , showMaybe x
                                                         , "." ]
                        in a & _1.mobTbl.ind targetId.tempDesc .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "tempDesc"
        -----
        setMobSizeHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "mobSize" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set mobSize to ", ppMaybe x, mkDiffTxt isDiff, "." ]
                            prev     = getMobSize targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . prd $ "Your size has changed to " <> ppMaybe x
                        in a & _1.mobTbl.ind targetId.mobSize .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "mobSize"
        -----
        setMobCorpseHelper t k n getter setter
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> let prev                 = view getter . getMob targetId $ ms
                           addSubAssignHelper f = let new    = max0 $ prev `f` x
                                                      diff   = new - prev
                                                      toSelf = mkToSelfForInt k new diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ new
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> let new    = max0 x
                                                      diff   = new - prev
                                                      toSelf = mkToSelfForInt k new diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ new
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff
              | diff > 0  = pure . T.concat $ [ "Your ", n, " has increased by ", commaShow diff,         "." ]
              | otherwise = pure . T.concat $ [ "Your ", n, " has decreased by ", commaShow . abs $ diff, "." ]
        -----
        setMobFollowingHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "following" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set following to ", descMaybeSingId x ms, mkDiffTxt isDiff, "." ]
                            prev     = getFollowing targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . T.concat $ [ "You are now following ", maybe none (`getSing` ms) x, "." ]
                        in a & _1.mobTbl.ind targetId.party.following .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "following"
        -----
        setMobInvHelper t k n getter setter
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _              -> appendMsg . sorryAdminSetValue k $ value
            Right (nubSort -> x) ->
                let prev                 = view getter . getMob targetId $ ms
                    addSubAssignHelper f = let (new, toSelf, isDiff) = mkTupleForList prev f x mkToSelf
                                           in a & _1.mobTbl.ind targetId.setter .~ new
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget new)
                                                & _4 <>~ (isDiff |?| toSelf)
                in case op of Assign    -> let toSelf = mkToSelf x isDiff
                                               isDiff = x /= prev
                                           in a & _1.mobTbl.ind targetId.setter .~ x
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget x)
                                                & _4 <>~ (isDiff |?| toSelf)
                              AddAssign -> addSubAssignHelper (++)
                              SubAssign -> addSubAssignHelper (\\)
          where
            mkToSelf   x isDiff = pure . T.concat $ [ "Set "
                                                    , k
                                                    , " to "
                                                    , mkValueTxt (`descSingId` ms) x
                                                    , mkDiffTxt isDiff
                                                    , "." ]
            mkToTarget x        = pure . T.concat $ [ "Your ", n, " changed to ", mkValueTxt (`getSing` ms) x, "." ]
            mkValueTxt f        = noneOnNull . commas . map f
        -----
        setMobMemberOfHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "memberOf" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set memberOf to ", descMaybeSingId x ms, mkDiffTxt isDiff, "." ]
                            prev     = getMemberOf targetId ms
                            isDiff   = x /= prev
                            toTarget = pure $ case x of
                                         Nothing -> "Your group membership has been set to none."
                                         Just i' -> "You are now a member of " <> getSing i' ms <> "'s group."
                        in a & _1.mobTbl.ind targetId.party.memberOf .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "following"
        -----
        setPCRaceHelper t
          | t /= PCType = sorryType
          | otherwise   = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "race" $ value
            Right x -> case op of
              Assign -> let toSelf   = pure . T.concat $ [ "Set race to ", pp x, mkDiffTxt isDiff, "." ]
                            prev     = getRace targetId ms
                            isDiff   = x /= prev
                            toTarget = pure . prd $ "Your race has changed to " <> pp x
                        in a & _1.pcTbl.ind targetId.race .~ x
                             & _2 <>~ toSelf
                             & _3 <>~ (isDiff |?| toTarget)
                             & _4 <>~ (isDiff |?| toSelf)
              _      -> sorryOp "race"
        -----
        setPCSingListHelper t k n getter setter
          | t /= PCType = sorryType
          | otherwise   = case eitherDecode value' of
            Left  _              -> appendMsg . sorryAdminSetValue k $ value
            Right (nubSort -> x) ->
                let prev                 = view getter . getPC targetId $ ms
                    addSubAssignHelper f = let (new, toSelf, isDiff) = mkTupleForList prev f x mkToSelf
                                           in a & _1.pcTbl.ind targetId.setter .~ new
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget new)
                                                & _4 <>~ (isDiff |?| toSelf)
                in case op of Assign    -> let toSelf = mkToSelf x isDiff
                                               isDiff = x /= prev
                                           in a & _1.pcTbl.ind targetId.setter .~ x
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget x)
                                                & _4 <>~ (isDiff |?| toSelf)
                              AddAssign -> addSubAssignHelper (++)
                              SubAssign -> addSubAssignHelper (\\)
          where
            mkToSelf   x isDiff = pure . T.concat $ [ "Set ", k, " to ", mkValueTxt x, mkDiffTxt isDiff, "." ]
            mkToTarget x        = pure . T.concat $ [ "Your ", n, " have changed to ", mkValueTxt x, "."     ]
            mkValueTxt          = noneOnNull . commas
        -----
        setPCSkillPtsHelper t
          | t /= PCType = sorryType
          | otherwise   = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "skillPts" $ value
            Right x -> let prev                 = getSkillPts targetId ms
                           addSubAssignHelper g = f . max0 $ prev `g` x
                           f new                = let diff   = new - prev
                                                      toSelf = mkToSelfForInt "skillPts" new diff
                                                  in a & _1.pcTbl.ind targetId.skillPts .~ new
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> f . max0 $ x
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have been awarded ", commaShow diff, " skill points." ]
                            | otherwise = pure . T.concat $ [ "You have lost ", commaShow . abs $ diff, " skill points." ]
        -----
        sorryType               = appendMsg . sorryAdminSetType $ targetId
        sorryOp                 = appendMsg . sorryAdminSetOp (pp op)
        value'                  = strictTextToLazyBS value
        mkDiffTxt isDiff        = not isDiff |?| spcL . parensQuote $ "no change"
        showMaybe Nothing       = none
        showMaybe (Just x)      = showText x
        mkToSelfForInt k v diff = pure . T.concat $ [ "Set ", k, " to ", commaShow v, " ", parensQuote diffTxt, "." ]
          where
            diffTxt = if | isZero diff -> "no change"
                         | diff > 0    -> "added "      <> commaShow diff
                         | otherwise   -> "subtracted " <> commaShow (abs diff)
        mkTupleForList prev f x g = let new    = nubSort $ prev `f` x
                                        toSelf = g new isDiff
                                        isDiff = new /= prev
                                    in (new, toSelf, isDiff)


-----


adminShutdown :: HasCallStack => ActionFun
adminShutdown (NoArgs' i mq    ) = shutdownHelper i mq Nothing
adminShutdown (Msg'    i mq msg) = shutdownHelper i mq . Just $ msg
adminShutdown p                  = patternMatchFail "adminShutdown" . showText $ p


shutdownHelper :: HasCallStack => Id -> MsgQueue -> Maybe Text -> MudStack ()
shutdownHelper i mq maybeMsg = getState >>= \ms ->
    let s    = getSing i ms
        rest = maybeMsg |&| maybe (prd . spcL . parensQuote $ "no message given") (("; message: " <>) . dblQuote)
    in do logPla     "shutdownHelper" i $ "initiating shutdown" <> rest
          massLogPla "shutdownHelper"   $ "closing connection due to server shutdown initiated by " <> s <> rest
          logNotice  "shutdownHelper"   $ "server shutdown initiated by "                           <> s <> rest
          massSend . colorWith shutdownMsgColor . fromMaybe dfltShutdownMsg $ maybeMsg
          writeMsg mq Shutdown


-----


adminSudoer :: HasCallStack => ActionFun
adminSudoer p@AdviseNoArgs                    = advise p [ prefixAdminCmd "sudoer" ] adviceASudoerNoArgs
adminSudoer   (OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let fn                  = "adminSudoer"
        SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to promote/demote"
        sorry               = (ms, ) . pure . sendFun
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      [] -> (ms, pure . sendFun $ sorryPCName strippedTarget |<>| hintASudoer)
      [targetId]
        | selfSing       <- getSing i ms
        , targetSing     <- getSing targetId ms
        , ia             <- isAdminId targetId ms
        , (verb, toFrom) <- ia ? ("demoted", "from") :? ("promoted", "to")
        , handleIncog    <- when (isIncognitoId targetId ms) . adminIncognito . mkActionParams targetId ms $ []
        , handlePeep     <-
            let peepingIds = getPeeping targetId ms
            in unless (()# peepingIds) . adminPeep . mkActionParams targetId ms . map (`getSing` ms) $ peepingIds
        , fs <- [ logNotice fn          . T.concat $ [ selfSing, spaced verb, targetSing, "." ]
                , logPla    fn i        . T.concat $ [ verb, " ",             targetSing, "." ]
                , logPla    fn targetId . T.concat $ [ verb, " by ",          selfSing,   "." ]
                , let msg = T.concat [ selfSing, " has ", verb, " you ", toFrom, " admin status." ]
                  in retainedMsg targetId ms . colorWith promoteDemoteColor $ msg
                , sendFun                           . T.concat $ [ "You have ",       verb, " ", targetSing, "." ]
                , bcastAdminsExcept [ i, targetId ] . T.concat $ [ selfSing, " has ", verb, " ", targetSing, "." ]
                , handleIncog
                , handlePeep ]
        -> if | targetId   == i        -> sorry sorrySudoerDemoteSelf
              | targetSing == "Root"   -> sorry sorrySudoerDemoteRoot
              | isAdHoc    targetId ms -> sorry sorrySudoerAdHoc
              | isSpiritId targetId ms -> sorry . sorrySudoerSpirit $ targetSing
              | otherwise              -> (upd ms [ plaTbl.ind targetId %~ setPlaFlag IsAdmin      (not ia)
                                                  , plaTbl.ind targetId %~ setPlaFlag IsTunedAdmin (not ia) ], fs)
      xs -> patternMatchFail fn . showText $ xs
adminSudoer p = advise p [] adviceASudoerExcessArgs


-----


adminSummon :: HasCallStack => ActionFun
adminSummon p@AdviseNoArgs                    = advise p [ prefixAdminCmd "summon" ] adviceASummonNoArgs
adminSummon   (OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the PC you wish to summon"
        idSings             = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
        destId              = getRmId i ms
        rn                  = getRmName destId ms
        destName            = rn |<>| parensQuote ("summoned by " <> s)
        s                   = getSing i ms
        sorry               = (ms, ) . pure . sendFun
        found (targetId@((`getRmId` ms) -> originId), targetSing)
          | targetSing == s       = sorry sorrySummonSelf
          | isAdminId targetId ms = sorry sorrySummonAdmin
          | isAdHoc   targetId ms = sorry sorrySummonAdHoc
          | destId == originId    = sorry . sorrySummonAlready $ targetSing
          | p   <- mkActionParams targetId ms []
          , res <- teleHelper p ms originId destId destName Nothing consLocPrefBcast sorry
          = res & _2 <>~ pure (logPla "adminSummon" i . T.concat $ [ "summoned ", targetSing, " to ", dblQuote rn, "." ])
        notFound = sorry . sorryPCNameLoggedIn $ strippedTarget
    in findFullNameForAbbrev strippedTarget idSings |&| maybe notFound found
adminSummon ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceASummonExcessArgs


-----


adminTeleId :: HasCallStack => ActionFun
adminTeleId p@AdviseNoArgs                    = advise p [ prefixAdminCmd "teleid" ] adviceATeleIdNoArgs
adminTeleId p@(OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The ID of the entity or room to which you want to teleport"
        teleport targetId  = let originId       = getRmId i ms
                                 (destId, desc) = locateHelper ms [] targetId
                                 destName       = mkNameTypeIdDesc targetId ms <> (()!# desc |?| (", " <> desc))
                                 msg            = thrice prd $ "Teleporting to " <> destName
                                 sorry          = (ms, ) . pure . multiSendFun . (msg :) . pure
                             in teleHelper p { args = [] } ms originId destId destName (Just msg) consLocPrefBcast sorry
        sorryParse = (ms, ) . pure . sendFun
    in case reads . T.unpack $ strippedTarget :: [(Int, String)] of
      [(targetId, "")]
        | targetId < 0                -> sorryParse sorryWtf
        | not . hasType targetId $ ms -> sorryParse . sorryParseId $ strippedTarget'
        | otherwise                   -> teleport targetId
      _                               -> sorryParse . sorryParseId $ strippedTarget'
adminTeleId ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceATeleIdExcessArgs


teleHelper :: HasCallStack => ActionParams
                           -> MudState
                           -> Id
                           -> Id
                           -> Text
                           -> Maybe Text
                           -> (Id -> [Broadcast] -> [Broadcast])
                           -> (Text -> (MudState, Funs))
                           -> (MudState, Funs)
teleHelper p@ActionParams { myId } ms originId destId destName mt f sorry =
    let g            = maybe strictId ((:) . (, pure myId) . nlnl) mt
        originDesig  = mkStdDesig myId ms Don'tCap
        originMobIds = myId `delete` desigIds originDesig
        destDesig    = mkSerializedNonStdDesig myId ms (getSing myId ms) A Don'tCap
        destMobIds   = views (invTbl.ind destId) (findMobIds ms) ms
        ms'          = ms & mobTbl.ind myId.rmId     .~ destId
                          & mobTbl.ind myId.lastRmId .~ originId
                          & invTbl.ind originId      %~ (myId `delete`)
                          & invTbl.ind destId        %~ addToInv ms (pure myId)
        bs           = map (first nlnl) [ (teleDescMsg,                             pure myId   )
                                        , (teleOriginMsg . serialize $ originDesig, originMobIds)
                                        , (teleDestMsg destDesig,                   destMobIds  ) ]
    in if | destId == originId   -> sorry sorryTeleAlready
          | destId == iWelcome   -> sorry sorryTeleWelcomeRm
          | destId == iLoggedOut -> sorry sorryTeleLoggedOutRm
          | otherwise            -> (ms', [ logPla "teleHelper" myId . prd $ "teleported to " <> destName
                                          , sendGmcpRmInfo Nothing myId ms'
                                          , bcastIfNotIncog myId . f myId . g $ bs
                                          , look p
                                          , rndmDos [ (calcProbTeleportDizzy   myId ms, mkExpAction "dizzy"   p)
                                                    , (calcProbTeleportShudder myId ms, mkExpAction "shudder" p) ] ])


-----


adminTelePC :: HasCallStack => ActionFun
adminTelePC p@AdviseNoArgs                    = advise p [ prefixAdminCmd "telepc" ] adviceATelePCNoArgs
adminTelePC p@(OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the PC to which you want to teleport"
        sorry               = (ms, ) . pure . sendFun
        idSings             = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
        originId            = getRmId i ms
        found (flip getRmId ms -> destId, targetSing)
          | targetSing == getSing i ms = sorry sorryTeleSelf
          | otherwise = teleHelper p { args = [] } ms originId destId targetSing Nothing consLocPrefBcast sorry
        notFound = sorry . sorryPCNameLoggedIn $ strippedTarget
    in findFullNameForAbbrev strippedTarget idSings |&| maybe notFound found
adminTelePC ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceATelePCExcessArgs


-----


adminTeleRm :: HasCallStack => ActionFun
adminTeleRm (NoArgs i mq cols) = logPlaExecArgs (prefixAdminCmd "telerm") [] i >> (multiWrapSend mq cols =<< mkTxt)
  where
    mkTxt  = views rmTeleNameTbl ((header :) . styleAbbrevs Don'tQuote . sort . IM.elems) <$> getState
    header = "You may teleport to the following rooms:"
adminTeleRm p@(OneArgLower i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. }        = mkSingleTarget mq cols target "The name of the room to which you want to teleport"
        sorry                      = (ms, ) . pure . sendFun
        originId                   = getRmId i ms
        found (destId, rmTeleName) =
            teleHelper p { args = [] } ms originId destId rmTeleName Nothing consLocPrefBcast sorry
        notFound = sorry . sorryTeleRmName $ strippedTarget'
    in (findFullNameForAbbrev strippedTarget' . views rmTeleNameTbl IM.toList $ ms) |&| maybe notFound found
adminTeleRm p = advise p [] adviceATeleRmExcessArgs


-----


adminTime :: HasCallStack => ActionFun
adminTime (NoArgs i mq cols) = do
    logPlaExec (prefixAdminCmd "time") i
    (ct, zt) <- liftIO $ (,) <$> formatTimeHelper `fmap` getCurrentTime <*> formatTimeHelper `fmap` getZonedTime
    multiWrapSend mq cols [ thrice prd "At the tone, the time will be", ct, zt ]
adminTime p = withoutArgs adminTime p


-----


adminTType :: HasCallStack => ActionFun
adminTType (NoArgs i mq cols) = (withDbExHandler "adminTType" . getDbTblRecs $ "ttype") >>= \case
  Just xs ->
    let grouped = groupBy ((==) `on` dbTType) xs
        folded  = foldr (\g -> (((dbTType . head) &&& (nubSort . map (dbHost :: TTypeRec -> Text))) g :)) [] grouped
        txtss   = [ uncurry (:) . first (<> t) $ pair
                  | pair@(_, hosts) <- folded, let l = length hosts
                                             , let t = T.concat [ ": ", showText l, " host", pluralize ("", "s") l ] ]
    in (logPlaExec (prefixAdminCmd "ttype") i >>) $ case intercalateDivider cols txtss of
          [] -> wrapSend      mq cols dbEmptyMsg
          ts -> multiWrapSend mq cols ts
  Nothing -> dbError mq cols
adminTType p = withoutArgs adminTType p


-----


adminTypo :: HasCallStack => ActionFun
adminTypo p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "typo" f "typo" p
  where
    f :: HasCallStack => [TypoRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminUptime :: HasCallStack => ActionFun
adminUptime (NoArgs i mq cols) = do
    logPlaExec (prefixAdminCmd "uptime") i
    send mq . nl =<< liftIO uptime |&| try >=> eitherRet (\res -> sendGenericErrorMsg mq cols >> logIOEx "adminUptime" res)
  where
    uptime = T.pack <$> readProcess "uptime" [] ""
adminUptime p = withoutArgs adminUptime p


-----


adminWhoIn :: HasCallStack => ActionFun
adminWhoIn = whoHelper LoggedIn "whoin"


whoHelper :: HasCallStack => LoggedInOrOut -> Text -> ActionFun
whoHelper inOrOut cn (NoArgs i mq cols) = do
    logPlaExecArgs (prefixAdminCmd cn) [] i
    pager i mq Nothing =<< [ concatMap (wrapIndent 20 cols) charListTxt | charListTxt <- mkCharListTxt inOrOut <$> getState ]
whoHelper inOrOut cn p@ActionParams { myId, args } =
    sequence_ [ logPlaExecArgs (prefixAdminCmd cn) args myId, dispMatches p 20 =<< mkCharListTxt inOrOut <$> getState ]


mkCharListTxt :: HasCallStack => LoggedInOrOut -> MudState -> [Text]
mkCharListTxt inOrOut ms =
    let is               = IM.keys . IM.filter predicate $ ms^.plaTbl
        (is', ss)        = unzip [ (i, s) | i <- is, let s = getSing i ms, then sortWith by s ]
        ias              = zip is' . styleAbbrevs Don'tQuote $ ss
        mkCharTxt (i, a) = let (s, r, l) = mkPrettySexRaceLvl i ms
                               name      = mkAnnotatedName i a
                           in T.concat [ padName name, padId . showText $ i, padSex s, padRace r, l ]
        nop              = length is
    in mkWhoHeader True ++ map mkCharTxt ias ++ (pure .  T.concat $ [ showText nop
                                                                    , spaced . pluralize ("person", "people") $ nop
                                                                    , pp inOrOut
                                                                    , "." ])
  where
    predicate           = case inOrOut of LoggedIn  -> isLoggedIn
                                          LoggedOut -> not . isLoggedIn
    mkAnnotatedName i a = let p      = getPla i ms
                              admin  = isAdmin     p |?| asterisk
                              incog  = isIncognito p |?| colorWith asteriskColor "@"
                              spirit = isSpirit    p |?| colorWith asteriskColor "$"
                          in T.concat [ a, admin, incog, spirit ]


-----


adminWhoOut :: HasCallStack => ActionFun
adminWhoOut = whoHelper LoggedOut "whoout"


-----


adminWire :: HasCallStack => ActionFun
adminWire p@AdviseNoArgs            = advise p [ prefixAdminCmd "wiretap" ] adviceAWireNoArgs
adminWire   (WithArgs i mq cols as) = views chanTbl IM.size <$> getState >>= \case
  0 -> informNoChans mq cols
  _ -> helper |&| modifyState >=> \(msgs, logMsgs) ->
           logMsgs |#| logPlaOut (prefixAdminCmd "wiretap") i >> multiWrapSend mq cols msgs
  where
    helper ms = let (ms', msgs) = foldl' helperWire (ms, []) as
                in (ms', (map fromEither &&& rights) msgs)
    helperWire (ms, msgs) a =
        let (ms', msg) = case reads . T.unpack $ a :: [(Int, String)] of
                           [(ci, "")] | ci < 0                                      -> sorry sorryWtf
                                      | views chanTbl ((ci `notElem`) . IM.keys) ms -> sorry . sorryParseChanId $ a
                                      | otherwise                                   -> checkAlreadyConn ci
                           _ -> sorry . sorryParseChanId $ a
            checkAlreadyConn ci | s <- getSing i  ms, c <- getChan ci ms = if views (chanConnTbl.at s) isJust c
              then sorry . sorryWireAlready $ c^.chanName
              else toggle ms ci s & _2 %~ Right
            sorry = (ms, ) . Left
        in (ms', msgs ++ pure msg)
    toggle ms ci s = let (cn, ss) = ms^.chanTbl.ind ci.to (chanName `fanView` chanWiretappers)
                     in if s `elem` ss
                       then ( ms & chanTbl.ind ci.chanWiretappers .~ s `delete` ss
                            , "You stop tapping the "  <> dblQuote cn <> " channel." )
                       else ( ms & chanTbl.ind ci.chanWiretappers <>~ pure s
                            , "You start tapping the " <> dblQuote cn <> " channel." )
adminWire p = patternMatchFail "adminWire" . showText $ p
