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
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.Misc
import Mud.Misc.Persist
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iRoot)
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut, massLogPla)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), first, second)
import Control.Concurrent.Async (asyncThreadId)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, _4, _5, at, both, each, to, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (validatePassword)
import Data.Aeson (eitherDecode)
import Data.Bits (zeroBits)
import Data.Char (isDigit, isLower, isUpper)
import Data.Either (rights)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List ((\\), delete, foldl', groupBy, intercalate, intersperse, nub, partition, sortBy)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid ((<>), Any(..), Sum(..), getSum)
import Data.Text (Text)
import Data.Time (FormatTime, TimeZone, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, getZonedTime, utcToLocalTime)
import Database.SQLite.Simple (FromRow, fromOnly)
import GHC.Conc (ThreadStatus(..), threadStatus)
import GHC.Exts (sortWith)
import Prelude hiding (exp, pi)
import qualified Data.IntMap.Lazy as IM (elems, filter, filterWithKey, keys, lookup, size, toList)
import qualified Data.Map.Lazy as M (elems, foldl, foldrWithKey, keys, size, toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T (putStrLn)
import System.Directory (getDirectoryContents)
import System.Process (readProcess)
import System.Time.Utils (renderSecs)
import Text.Regex.Posix ((=~))


{-# ANN module ("HLint: ignore Use ||" :: String) #-}


-----


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Cmds.Admin"


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


adminCmds :: [Cmd]
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
    , mkAdminCmd "date"       adminDate        True  "Display the current system date."
    , mkAdminCmd "discover"   adminDiscover    True  "Dump the discover database."
    , mkAdminCmd "examine"    adminExamine     True  "Display the properties of one or more IDs."
    , mkAdminCmd "experience" adminExp         True  "Dump the experience table."
    , mkAdminCmd "exself"     adminExamineSelf True  "Self-examination."
    , mkAdminCmd "hash"       adminHash        True  "Compare a plain-text password with a hashed password."
    , mkAdminCmd "host"       adminHost        True  "Display a report of connection statistics for one or more \
                                                     \players."
    , mkAdminCmd "incognito"  adminIncognito   True  "Toggle your incognito status."
    , mkAdminCmd "ip"         adminIp          True  "Display the server's IP addresses and listening port."
    , mkAdminCmd "kill"       adminKill        True  "Instantly kill one or more mobiles by ID."
    , mkAdminCmd "link"       adminLink        True  "Dump two-way links for one or more PCs, sorted by volume of messages in \
                                                     \descending order."
    , mkAdminCmd "locate"     adminLocate      True  "Locate one or more IDs."
    , mkAdminCmd "message"    adminMsg         True  "Send a message to a regular player."
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


mkAdminCmd :: Text -> ActionFun -> Bool -> CmdDesc -> Cmd
mkAdminCmd (prefixAdminCmd -> cn) f b cd = Cmd { cmdName           = cn
                                               , cmdPriorityAbbrev = Nothing
                                               , cmdFullName       = cn
                                               , cmdAction         = Action f b
                                               , cmdDesc           = cd }


-----


adminAdmin :: ActionFun
adminAdmin (NoArgs i mq cols) = getState >>= \ms ->
    let triples = sortBy (compare `on` view _2) [ (ai, as, isTuned) | ai <- getLoggedInAdminIds ms
                                                                    , let as      = getSing ai ms
                                                                    , let ap      = getPla  ai ms
                                                                    , let isTuned = isTunedAdmin ap ]
        ([self],   others   )  = partition (\x -> x^._1.to (== i)) triples
        (tunedIns, tunedOuts)  = partition (view _3) others
        styleds                = styleAbbrevs Don'tQuote . select _2 $ tunedIns
        others'                = zipWith (\triple styled -> triple & _2 .~ styled) tunedIns styleds ++ tunedOuts
        mkDesc (_, n, isTuned) = padName n <> tunedInOut isTuned
        descs                  = mkDesc self : map mkDesc others'
    in multiWrapSend mq cols descs >> logPlaExecArgs (prefixAdminCmd "admin") [] i
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
                  mkBsWithStyled is' = [ (formatChanMsg "Admin" (getStyled i') txt, pure i') | i' <- is' ]
              f bs = ioHelper s (concatMap format bs)
              ws   = wrapSend      mq cols
              mws  = multiWrapSend mq cols
          in case adminChanTargetify tunedIds tunedSings msg of
            Left errorMsg    -> ws errorMsg
            Right (Right bs) -> f bs . mkLogMsg $ bs
            Right (Left ())  -> case adminChanEmotify i ms tunedIds tunedSings msg of
              Left  errorMsgs  -> mws errorMsgs
              Right (Right bs) -> f bs . mkLogMsg $ bs
              Right (Left ())  -> case adminChanExpCmdify i ms tunedIds tunedSings msg of
                Left  errorMsg     -> ws errorMsg
                Right (bs, logMsg) -> f bs logMsg
      else wrapSend mq cols . sorryTunedOutOOCChan $ "admin"
  where
    getTunedAdminIds ms  = [ ai | ai <- getLoggedInAdminIds ms, isTunedAdminId ai ms ]
    mkLogMsg             = dropANSI . fst . head
    ioHelper s bs logMsg = bcastNl bs >> logHelper
      where
        logHelper = do
            logPlaOut (prefixAdminCmd "admin") i . pure $ logMsg
            ts <- liftIO mkTimestamp
            withDbExHandler_ "adminAdmin" . insertDbTblAdminChan . AdminChanRec ts s $ logMsg
adminAdmin p = patternMatchFail "adminAdmin" . showText $ p


-----


adminAlertExec :: ActionFun
adminAlertExec p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "alert_exec" f "alertexec" p
  where
    f :: [AlertExecRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


dumpCmdHelper :: (FromRow a) => Text -> ([a] -> MudStack ()) -> CmdName -> ActionFun
dumpCmdHelper tblName f cn (NoArgs i mq cols) = (withDbExHandler "dumpCmdHelper" . getDbTblRecs $ tblName) >>= \case
  Just xs -> f xs >> logPlaExec (prefixAdminCmd cn) i
  Nothing -> dbError mq cols
dumpCmdHelper tblName f cn p = withoutArgs (dumpCmdHelper tblName f cn) p


dumpDbTblHelper :: (Pretty a) => MsgQueue -> Cols -> [a] -> MudStack ()
dumpDbTblHelper mq cols [] = wrapSend mq cols dbEmptyMsg
dumpDbTblHelper mq cols xs = multiWrapSend mq cols . map pp $ xs


-----


adminAlertMsg :: ActionFun
adminAlertMsg p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "alert_msg" f "alertmsg" p
  where
    f :: [AlertMsgRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminAnnounce :: ActionFun
adminAnnounce p@AdviseNoArgs  = advise p [ prefixAdminCmd "announce" ] adviceAAnnounceNoArgs
adminAnnounce (Msg' i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    ok mq
    massSend . colorWith announceColor $ msg
    logPla    "adminAnnounce" i $       "announced "  <> dblQuote msg
    logNotice "adminAnnounce"   $ s <> " announced, " <> dblQuote msg
adminAnnounce p = patternMatchFail "adminAnnounce" . showText $ p


-----


adminAs :: ActionFun
adminAs p@(NoArgs' i mq    ) = advise p [ prefixAdminCmd "as" ] adviceAAsNoArgs    >> sendDfltPrompt mq i
adminAs p@(OneArg  i mq _ a) = advise p [ prefixAdminCmd "as" ] (adviceAAsNoCmd a) >> sendDfltPrompt mq i
adminAs (WithTarget i mq cols target rest) = getState >>= \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The target ID"
        as targetId         = let s = getSing targetId ms in case getType targetId ms of
          NpcType -> let npcMq        = getNpcMsgQueue targetId ms
                         notPossessed = do
                             ioHelper targetId s
                             liftIO . atomically . writeTQueue npcMq . ExternCmd mq cols $ rest
                         isPossessed pi = sorry $ if pi == i
                           then sorryAlreadyPossessing s
                           else sorryAlreadyPossessed  s . getSing pi $ ms
                     in maybe notPossessed isPossessed . getPossessor targetId $ ms
          PCType  | targetId == i                                            -> sorry sorryAsSelf
                  | isAdmin          . getPla targetId $ ms                  -> sorry sorryAsAdmin
                  | not . isLoggedIn . getPla targetId $ ms                  -> sorry . sorryLoggedOut $ s
                  | (targetMq, targetCols) <- getMsgQueueColumns targetId ms -> do
                      ioHelper targetId s
                      wrapSend targetMq targetCols asMsg
                      fakeClientInput targetMq rest
          _ -> sorry . sorryAsType $ s
        ioHelper targetId s = do
            sendFun . parensQuote $ "Executing as " <> aOrAnOnLower s <> "..."
            logPla "adminAs" i . T.concat $ [ "Executing "
                                            , dblQuote rest
                                            , " as "
                                            , aOrAnOnLower . descSingId targetId $ ms
                                            , "." ]
        sorry txt  = sendFun txt >> sendDfltPrompt mq i
        sorryParse = sorry . sorryParseId $ strippedTarget'
    in case reads . T.unpack $ strippedTarget :: [(Int, String)] of
      [(targetId, "")] | targetId < 0                                -> sorry sorryWtf
                       | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorryParse
                       | otherwise                                   -> as targetId
      _                                                              -> sorryParse
adminAs p = patternMatchFail "adminAs" . showText $ p


-----


adminBanHost :: ActionFun
adminBanHost (NoArgs i mq cols) = (withDbExHandler "adminBanHost" . getDbTblRecs $ "ban_host") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [BanHostRec]) >> logPlaExecArgs (prefixAdminCmd "banhost") [] i
  Nothing -> dbError mq cols
adminBanHost p@(AdviseOneArg a) = advise p [ prefixAdminCmd "banhost" ] . adviceABanHostNoReason $ a
adminBanHost (MsgWithTarget i mq cols (uncapitalize -> target) msg) = getState >>= \ms ->
    (withDbExHandler "adminBanHost" . isHostBanned $ target) >>= \case
      Nothing      -> dbError mq cols
      Just (Any b) -> let newStatus = not b in liftIO mkTimestamp >>= \ts -> do
          let banHost = BanHostRec ts target newStatus msg
          withDbExHandler_ "adminBanHost" . insertDbTblBanHost $ banHost
          notifyBan i mq cols (getSing i ms) target newStatus banHost
adminBanHost p = patternMatchFail "adminBanHost" . showText $ p


notifyBan :: (Pretty a) => Id -> MsgQueue -> Cols -> Sing -> Text -> Bool -> a -> MudStack ()
notifyBan i mq cols selfSing target newStatus x =
    let fn          = "notifyBan"
        (v, suffix) = newStatus ? ("banned", [ ": " <> pp x ]) :? ("unbanned", [ ": " <> pp x ])
    in do
        wrapSend mq cols   . T.concat $ [ "You have ",     v, " ", target ] ++ suffix
        bcastOtherAdmins i . T.concat $ [ selfSing, spaced v,      target ] ++ suffix
        logNotice fn       . T.concat $ [ selfSing, spaced v,      target ] ++ suffix
        logPla    fn i     . T.concat $ [                  v, " ", target ] ++ suffix


-----


adminBanPC :: ActionFun
adminBanPC (NoArgs i mq cols) = (withDbExHandler "adminBanPC" . getDbTblRecs $ "ban_pc") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [BanPCRec]) >> logPlaExecArgs (prefixAdminCmd "banpc") [] i
  Nothing -> dbError mq cols
adminBanPC p@(AdviseOneArg a) = advise p [ prefixAdminCmd "banpc" ] . adviceABanPCNoReason $ a
adminBanPC p@(MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let fn                  = "adminBanPC"
        SingleTarget { .. } = mkSingleTarget mq cols target "The name of the PC you wish to ban"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      []      -> sendFun $ sorryPCName strippedTarget <> " " <> hintABan
      [banId] -> let selfSing = getSing i     ms
                     pla      = getPla  banId ms
                 in if
                   | banId == i  -> sendFun sorryBanSelf
                   | isAdmin pla -> sendFun sorryBanAdmin
                   | otherwise   -> (withDbExHandler "adminBanPC" . isPCBanned $ strippedTarget) >>= \case
                     Nothing      -> dbError mq cols
                     Just (Any b) -> let newStatus = not b in liftIO mkTimestamp >>= \ts -> do
                         let rec = BanPCRec ts strippedTarget newStatus msg
                         withDbExHandler_ "adminBanPC" . insertDbTblBanPC $ rec
                         notifyBan i mq cols selfSing strippedTarget newStatus rec
                         when (newStatus && isLoggedIn pla) . adminBoot $ p { args = strippedTarget : T.words bannedMsg }
      xs      -> patternMatchFail fn . showText $ xs
adminBanPC p = patternMatchFail "adminBanPC" . showText $ p


-----


adminBoot :: ActionFun
adminBoot p@AdviseNoArgs                       = advise p [ prefixAdminCmd "boot" ] adviceABootNoArgs
adminBoot (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to boot"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      []       -> sendFun $ sorryPCName strippedTarget <> " " <> hintABoot
      [bootId] -> let selfSing = getSing i ms in if
                    | not . isLoggedIn . getPla bootId $ ms -> sendFun . sorryLoggedOut $ strippedTarget
                    | bootId == i -> sendFun sorryBootSelf
                    | bootMq <- getMsgQueue bootId ms, f <- ()# msg ? dfltMsg :? customMsg -> do
                        sendFun . prd $ "You have booted " <> strippedTarget
                        sendMsgBoot bootMq =<< f bootId strippedTarget selfSing
                        bcastAdminsExcept [ i, bootId ] . T.concat $ [ selfSing, " booted ", strippedTarget, "." ]
      xs       -> patternMatchFail "adminBoot" . showText $ xs
  where
    dfltMsg   bootId target' s = emptied $ do
        logPla "adminBoot dfltMsg"   i      $ T.concat [ "booted ", target', " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   bootId $ T.concat [ "booted by ", s,    " ", parensQuote "no message given", "." ]
    customMsg bootId target' s = do
        logPla "adminBoot customMsg" i      $ T.concat [ "booted ", target', "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" bootId $ T.concat [ "booted by ", s,    "; message: ", dblQuote msg ]
        unadulterated msg
adminBoot p = patternMatchFail "adminBoot" . showText $ p


-----


adminBug :: ActionFun
adminBug p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "bug" f "bug" p
  where
    f :: [BugRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminChan :: ActionFun
adminChan (NoArgs i mq cols) = getState >>= \ms -> case views chanTbl (map (mkChanReport i ms) . IM.elems) ms of
  []      -> informNoChans mq cols
  reports -> adminChanIOHelper i mq reports
adminChan (LowerNub i mq cols as) = getState >>= \ms ->
    let helper a = case reads . T.unpack $ a :: [(Int, String)] of
          [(ci, "")] | ci < 0                                -> pure sorryWtf
                     | ci `notElem` (ms^.chanTbl.to IM.keys) -> sorry
                     | otherwise                             -> mkChanReport i ms . getChan ci $ ms
          _                                                  -> sorry
          where
            sorry = pure . sorryParseChanId $ a
        reports = map helper as
    in case views chanTbl IM.size ms of 0 -> informNoChans mq cols
                                        _ -> adminChanIOHelper i mq reports
adminChan p = patternMatchFail "adminChan" . showText $ p


informNoChans :: MsgQueue -> Cols -> MudStack ()
informNoChans mq cols = wrapSend mq cols "No channels exist!"


adminChanIOHelper :: Id -> MsgQueue -> [[Text]] -> MudStack ()
adminChanIOHelper i mq reports = sequence_ [ pager i mq Nothing . intercalate [""] $ reports
                                           , logPlaExec (prefixAdminCmd "channel") i ]


-----


adminCount :: ActionFun
adminCount (NoArgs i mq cols) = do
    pager i mq Nothing . concatMap (wrapIndent 2 cols) =<< mkCountTxt
    logPlaExecArgs (prefixAdminCmd "count") [] i
adminCount p@ActionParams { myId, args } = do
    dispMatches p 2 =<< mkCountTxt
    logPlaExecArgs (prefixAdminCmd "count") args myId


mkCountTxt :: MudStack [Text]
mkCountTxt = map (uncurry mappend . second commaShow) <$> helper
  where
    helper = getState >>= \ms -> do
        let countType t = views typeTbl (IM.size . IM.filter (== t)) ms
            countWealth = views coinsTbl (f . mconcat . IM.elems) ms
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
        (noticeThrId, errorThrId) <- asks $ (both %~ asyncThreadId) . getLogAsyncs
        let plaLogThrIds = views plaLogTbl (map (asyncThreadId . fst) . IM.elems) ms
            otherThrIds  = views threadTbl M.keys ms
            threadIds    = noticeThrId : errorThrId : plaLogThrIds ++ otherThrIds
        noOfThreads <- length . filterThreads <$> mapM (liftIO . threadStatus) threadIds
        return [ ("Armor: ",        countType ArmType     )
               , ("Clothing: ",     countType ClothType   )
               , ("Containers: ",   countType ConType     )
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
    countHelps     = liftIO . mapM countFiles $ [ plaHelpCmdsDir, plaHelpTopicsDir, adminHelpCmdsDir, adminHelpTopicsDir ]
    countFiles dir = (length . dropIrrelevantFilenames <$> getDirectoryContents dir) `catch` handler
      where
        handler :: IOException -> IO Int
        handler = const . return $ 0
    filterThreads = filter $ \case ThreadRunning   -> True
                                   ThreadBlocked _ -> True
                                   _               -> False


-----


adminDate :: ActionFun
adminDate (NoArgs' i mq) = do
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
    logPlaExec (prefixAdminCmd "date") i
adminDate p = withoutArgs adminDate p


-----


adminDiscover :: ActionFun
adminDiscover p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "discover" f "discover" p
  where
    f :: [DiscoverRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminDispCmdList :: ActionFun
adminDispCmdList p@(LowerNub' i as) = dispCmdList adminCmds p >> logPlaExecArgs (prefixAdminCmd "?") as i
adminDispCmdList p                  = patternMatchFail "adminDispCmdList" . showText $ p


-----


adminExamine :: ActionFun -- TODO: ":examine 177" (Pidge) bombs.
adminExamine p@AdviseNoArgs          = advise p [ prefixAdminCmd "examine" ] adviceAExamineNoArgs
adminExamine (LowerNub i mq cols as) = getState >>= \ms ->
    let helper a = case reads . T.unpack $ a :: [(Int, String)] of
          [(targetId, "")] | targetId < 0                                -> pure sorryWtf
                           | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorry
                           | otherwise                                   -> examineHelper ms targetId
          _                                                              -> sorry
          where
            sorry = pure . sorryParseId $ a
    in do
        pager i mq Nothing . concatMap (wrapIndent 2 cols) . intercalateDivider cols . map helper $ as
        logPlaExecArgs (prefixAdminCmd "examine") as i
adminExamine p = patternMatchFail "adminExamine" . showText $ p


examineHelper :: MudState -> Id -> [Text]
examineHelper ms targetId = let t = getType targetId ms in helper t $ case t of
  ArmType      -> [ examineEnt, examineObj,   examineArm   ]
  ClothType    -> [ examineEnt, examineObj,   examineCloth ]
  ConType      -> [ examineEnt, examineObj,   examineInv,   examineCoins, examineCon ]
  FoodType     -> [ examineEnt, examineObj,   examineFood ]
  NpcType      -> [ examineEnt, examineInv,   examineCoins, examineEqMap, examineMob, examineNpc ]
  ObjType      -> [ examineEnt, examineObj ]
  PCType       -> [ examineEnt, examineInv,   examineCoins, examineEqMap, examineMob, examinePC, examinePla, examinePickPts ]
  RmType       -> [ examineInv, examineCoins, examineRm       ]
  VesselType   -> [ examineEnt, examineObj,   examineVessel   ]
  WpnType      -> [ examineEnt, examineObj,   examineWpn      ]
  WritableType -> [ examineEnt, examineObj,   examineWritable ]
  where
    helper t fs = let header = T.concat [ showText targetId, " ", parensQuote . pp $ t ]
                  in header : "" : concatMap (\f -> f targetId ms) fs


type ExamineHelper = Id -> MudState -> [Text]


examineArm :: ExamineHelper
examineArm i ms = let a = getArm i ms in [ "Type: " <> a^.armSub  .to pp
                                         , "AC: "   <> a^.armClass.to showText ]


examineCloth :: ExamineHelper
examineCloth i ms = let c = getCloth i ms in [ "Type: " <> pp c ]


examineCoins :: ExamineHelper
examineCoins i ms = let (map commaShow . coinsToList -> cs) = getCoins i ms in [ "Coins: " <> commas cs ]


examineCon :: ExamineHelper
examineCon i ms = let c = getCon i ms in [ "Is clothing: " <> c^.conIsCloth.to showText
                                         , T.concat [ "Volume/capacity: "
                                                    , showText . calcVol i $ ms
                                                    , " / "
                                                    , c^.conCapacity.to showText
                                                    , " "
                                                    , parensQuote $ (<> "%") . showText . calcConPerFull i $ ms ]
                                         , "Container flags: " <> (commas . dropBlanks . descFlags $ c) ]
  where
    descFlags c | c^.conFlags == zeroBits = none
                | otherwise               = let pairs = [(isCorpse, "corpse")]
                                            in [ f c |?| t | (f, t) <- pairs ]


examineEnt :: ExamineHelper
examineEnt i ms = let e = getEnt i ms in [ "Name: "           <> e^.entName .to (fromMaybe none)
                                         , "Sing: "           <> e^.sing    .to noneOnNull
                                         , "Plur: "           <> e^.plur    .to noneOnNull
                                         , "Description: "    <> e^.entDesc .to noneOnNull
                                         , "Smell: "          <> e^.entSmell.to (fromMaybe none)
                                         , "Entity flags: "   <> (commas . dropBlanks . descFlags $ e)
                                         , "Active effects: " <> descActiveEffects
                                         , "Paused effects: " <> descPausedEffects ]
  where
    descFlags e | e^.entFlags == zeroBits = none
                | otherwise               = let pairs = [(isInvis, "invisible")]
                                            in [ f e |?| t | (f, t) <- pairs ]
    descActiveEffects = descEffect getActiveEffects
    descPausedEffects = descEffect getPausedEffects
    descEffect f      = ppList . f i $ ms


ppList :: (Pretty a) => [a] -> Text
ppList = noneOnNull . commas . map pp


examineEqMap :: ExamineHelper
examineEqMap i ms = map helper . M.toList . getEqMap i $ ms
  where
    helper (slot, i') = bracketQuote (pp slot) <> " " <> descSingId i' ms


examineFood :: ExamineHelper
examineFood i ms =
    let f  = getFood i ms
        df = getDistinctFoodForFood f ms
    in [ "Distinct food ID: "    <> f^.foodId.to showText
       , "Eat description: "     <> f^.foodEatDesc
       , "Remaining mouthfuls: " <> f^.foodRemMouthfuls.to showText
       , "Distinct mouthfuls: "  <> df^.foodMouthfuls  .to showText ] ++ df^.foodEdibleEffects.to descEdibleEffects


descEdibleEffects :: EdibleEffects -> [Text]
descEdibleEffects (EdibleEffects d c) =
    [ "Digest effect list: "           <> maybe none descEffectList                           d
    , "Consumption effects amount: "   <> maybe none (views consumpAmt        showText      ) c
    , "Consumption effects interval: " <> maybe none (views consumpInterval   showText      ) c
    , "Consumption effect list: "      <> maybe none (views consumpEffectList descEffectList) c ]


descEffectList :: EffectList -> Text
descEffectList (EffectList xs) = commas . map helper $ xs
  where
    helper (Left  instaEff) = pp instaEff
    helper (Right eff     ) = pp eff


examineInv :: ExamineHelper
examineInv i ms = let is  = getInv i ms
                      txt = commas . map (`descSingId` ms) $ is
                  in [ "Contents: " <> noneOnNull txt ]


examineMob :: ExamineHelper
examineMob i ms =
    let m                  = getMob i ms
        showAttrib a       = showText (getBaseAttrib a i ms) <> " " <> (parensQuote . showText . calcEffAttrib a i $ ms)
        showPts x y        = m^.x.to showText <> " / " <> m^.y.to showText
        descSingIdHelper f = noneOnNull . commas . map (`descSingId` ms) . f i $ ms
    in [ "Sex: "              <> m^.sex.to pp
       , "ST: "               <> showAttrib St
       , "DX: "               <> showAttrib Dx
       , "HT: "               <> showAttrib Ht
       , "MA: "               <> showAttrib Ma
       , "PS: "               <> showAttrib Ps
       , "HP: "               <> showPts curHp maxHp
       , "MP: "               <> showPts curMp maxMp
       , "PP: "               <> showPts curPp maxPp
       , "FP: "               <> showPts curFp maxFp
       , "Exp: "              <> m^.exp .to commaShow
       , "Level: "            <> m^.lvl .to showText
       , "Handedness: "       <> m^.hand.to pp
       , "Know languages: "   <> m^.knownLangs.to ppList
       , "Room: "             <> let ri = m^.rmId in getRmName ri ms <> " " <> parensQuote (showText ri)
       , "Room description: " <> m^.mobRmDesc.to (fromMaybe none)
       , "Temp description: " <> m^.tempDesc .to (fromMaybe none)
       , "Corpse weight: "    <> m^.corpseWeight  .to commaShow
       , "Corpse volume: "    <> m^.corpseVol     .to commaShow
       , "Corpse capacity:"   <> m^.corpseCapacity.to commaShow
       , "Following: "        <> descMaybeId ms (getFollowing i ms)
       , "Followers: "        <> descSingIdHelper getFollowers
       , "My group: "         <> descSingIdHelper getMyGroup
       , "Member of: "        <> descMaybeId ms (getMemberOf i ms)
       , "Stomach: "          <> m^.stomach.to ppList
       , "Stomach ratio: "    <> let (mouths, size, perFull) = ( length . getStomach i $ ms
                                                               , calcStomachSize . getRace i $ ms
                                                               , calcStomachPerFull i ms ) & each %~ showText
                                 in T.concat [ mouths, " / ", size, " ", parensQuote $ perFull <> "%" ]
       , "Feeling map: "      <> let f tag feel = (tag <> " " <> pp feel :)
                                 in noneOnNull . commas . views feelingMap (M.foldrWithKey f []) $ m
       , encHelper i ms ]


encHelper :: Id -> MudState -> Text
encHelper i ms = let (w, maxEnc, encPer) = (calcWeight i ms, calcMaxEnc i ms, calcEncPer i ms) & each %~ showText
                 in T.concat [ "Weight/max enc: ", w, " / ", maxEnc, " ", parensQuote $ encPer <> "%" ]


examineNpc :: ExamineHelper
examineNpc i ms = [ "Possessor: " <> (descMaybeId ms . getPossessor i $ ms) ]


examineObj :: ExamineHelper
examineObj i ms = let o = getObj i ms in [ "Weight: " <> o^.objWeight.to commaShow
                                         , "Volume: " <> o^.objVol   .to commaShow ]


examinePC :: ExamineHelper
examinePC i ms = let p = getPC i ms in [ "Race: "         <> p^.race      .to pp
                                       , "Known names: "  <> p^.introduced.to (noneOnNull . commas)
                                       , "Links: "        <> p^.linked    .to (noneOnNull . commas)
                                       , "Skill points: " <> p^.skillPts  .to commaShow ]


examinePickPts :: ExamineHelper
examinePickPts i = pure . ("Pick pts: " <>) . views pickPtsTbl (maybe none commaShow . IM.lookup i)


examinePla :: ExamineHelper
examinePla i ms = let p = getPla i ms
                  in [ "Host: "              <> p^.currHostName.to (noneOnNull . T.pack)
                     , "Connect time: "      <> p^.connectTime .to (maybe none showText)
                     , "Player flags: "      <> (commas . dropBlanks . descFlags $ p)
                     , "Columns: "           <> p^.columns     .to showText
                     , "Lines: "             <> p^.pageLines   .to showText
                     , "Peepers: "           <> p^.peepers     .to helper
                     , "Peeping: "           <> p^.peeping     .to helper
                     , "Possessing: "        <> p^.possessing  .to (descMaybeId ms)
                     , "Retained messages: " <> p^.retainedMsgs.to (noneOnNull . slashes)
                     , "Last room: "         <> let f ri = getRmName ri ms <> " " <> parensQuote (showText ri)
                                                in p^.lastRmId.to (maybe none f)
                     , "Bonus time: "        <> p^.bonusTime.to (maybe none showText) ]
  where
    descFlags p | p^.plaFlags == zeroBits = none
                | otherwise               = let pairs = [ (isAdmin,            "admin"              )
                                                        , (isIncognito,        "incognito"          )
                                                        , (isSpirit,           "spirit"             )
                                                        , (isNotFirstAdminMsg, "not first admin msg")
                                                        , (isNotFirstMobSay,   "not first mob say"  )
                                                        , (isTunedAdmin,       "tuned admin"        )
                                                        , (isTunedQuestion,    "tuned question"     )
                                                        , (isShowingHp,        "showing HP"         )
                                                        , (isShowingMp,        "showing MP"         )
                                                        , (isShowingPp,        "showing PP"         )
                                                        , (isShowingFp,        "showing FP"         ) ]
                                            in [ f p |?| t | (f, t) <- pairs ]
    helper = noneOnNull . commas . map (`descSingId` ms)


examineRm :: ExamineHelper
examineRm i ms = let r = getRm i ms in [ "Name: "        <> r^.rmName
                                       , "Description: " <> r^.rmDesc.to xformNls
                                       , "Room flags: "  <> (commas . dropBlanks . descFlags $ r)
                                       , "Links: "       <> r^.rmLinks.to (noneOnNull . commas . map helper) ]
  where
    descFlags r | r^.rmFlags == zeroBits = none
                | otherwise              = none -- TODO: Room flags.
    helper = \case (StdLink    dir destId linkMove    ) -> f (pp dir) destId linkMove
                   (NonStdLink dir destId linkMove _ _) -> f dir      destId linkMove
      where
        f dir destId linkMove = spaces [ dir
                                       , "to"
                                       , getRmName destId ms
                                       , parensQuote (showText destId)
                                       , linkMove^.moveCost.to showText
                                       , linkMove^.moveTime.to commaShow ]


xformNls :: Text -> Text
xformNls = T.replace theNl (colorWith nlColor "\\n")


examineVessel :: ExamineHelper
examineVessel i ms = let v = getVessel i ms in
    [ "Max mouthfuls: "   <> v^.vesselMaxMouthfuls.to showText
    , "Vessel contents: " <> v^.vesselCont        .to (descCont v) ] ++ views vesselCont (maybe [] (descLiq . fst)) v
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


examineWpn :: ExamineHelper
examineWpn i ms = let w = getWpn i ms in [ "Type: "   <> w^.wpnSub.to pp
                                         , "Damage: " <> w^.wpnMinDmg.to showText <> " / " <> w^.wpnMaxDmg.to showText ]


examineWritable :: ExamineHelper
examineWritable i ms = let w = getWritable i ms in [ "Message: "   <> w^.writMessage.to (maybe none (xformNls . fst))
                                                   , "Language: "  <> w^.writMessage.to (maybe none (pp . snd))
                                                   , "Recipient: " <> w^.writRecip  .to (fromMaybe none) ]


-----


adminExamineSelf :: ActionFun
adminExamineSelf p@(NoArgs'' i) = adminExamine p { args = pure . showText $ i }
adminExamineSelf p              = withoutArgs adminExamineSelf p


-----


adminExp :: ActionFun
adminExp (NoArgs' i mq) = pager i mq Nothing mkReport >> logPlaExec (prefixAdminCmd "experience") i
  where
    mkReport = header ++ pure zero ++ (take 25 . map helper $ calcLvlExps)
    header   = [ "Level  Experience", T.replicate 17 "=" ]
    zero     = uncurry (<>) . first (pad 7) . dup $ "0"
    helper   = uncurry (<>) . (pad 7 . showText *** commaShow)
adminExp p = withoutArgs adminExp p


-----


adminHash :: ActionFun
adminHash p@AdviseNoArgs                      = advise p [ prefixAdminCmd "hash" ] adviceAHashNoArgs
adminHash p@(AdviseOneArg a                 ) = advise p [ prefixAdminCmd "hash" ] . adviceAHashNoHash $ a
adminHash   (WithArgs i mq cols [ pw, hash ]) = do
    wrapSend mq cols $ if uncurry validatePassword ((hash, pw) & both %~ T.encodeUtf8)
      then "It's a match!"
      else "The plain-text password does not match the hashed password."
    logPlaExec (prefixAdminCmd "hash") i
adminHash p = advise p [ prefixAdminCmd "hash" ] adviceAHashExcessArgs


-----


adminHost :: ActionFun
adminHost p@AdviseNoArgs          = advise p [ prefixAdminCmd "host" ] adviceAHostNoArgs
adminHost (LowerNub i mq cols as) = do
    ms          <- getState
    (now, zone) <- (,) <$> liftIO getCurrentTime <*> liftIO getCurrentTimeZone
    let helper target = let notFound = pure . sorryPCName $ target
                            found    = uncurry . mkHostReport ms now $ zone
                        in findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
    multiWrapSend mq cols . intercalate [""] . map (helper . capitalize . T.toLower) $ as
    logPlaExecArgs (prefixAdminCmd "host") as i
adminHost p = patternMatchFail "adminHost" . showText $ p


mkHostReport :: MudState -> UTCTime -> TimeZone -> Id -> Sing -> [Text]
mkHostReport ms now zone i s = (header ++) $ case getHostMap s ms of
  Nothing      -> pure . prd $ "There are no host records for " <> s
  Just hostMap | duration' <- ili |?| duration
               , total     <- M.foldl (\acc -> views secsConnected (+ acc)) 0 hostMap + getSum duration'
               , totalDesc <- "Grand total time connected: " <> renderIt total
               -> M.foldrWithKey helper [] hostMap ++ pure totalDesc
  where
    header = [ s <> ": "
             , "Currently logged " <> if ili
                 then T.concat [ "in from "
                               , T.pack . getCurrHostName i $ ms
                               , " "
                               , parensQuote . renderIt . getSum $ duration
                               , "." ]
                 else "out." ]
    ili       = isLoggedIn . getPla i $ ms
    renderIt  = T.pack . renderSecs
    duration  = Sum . round $ now `diffUTCTime` conTime
    conTime   = fromJust . getConnectTime i $ ms
    helper (T.pack -> host) r = (T.concat [ host
                                          , ": "
                                          , let n      = r^.noOfLogouts
                                                suffix = n > 1 |?| "s"
                                            in showText n <> " time" <> suffix
                                          , ", "
                                          , views secsConnected renderIt r
                                          , ", "
                                          , views lastLogout (showText . utcToLocalTime zone) r ] :)


-----


adminIncognito :: ActionFun
adminIncognito (NoArgs i mq cols) = modifyStateSeq $ \ms ->
    let s              = getSing i ms
        isIncog        = isIncognitoId i ms
        fs | isIncog   = [ wrapSend mq cols "You are no longer incognito."
                         , bcastOtherAdmins i $ s <> " is no longer incognito."
                         , logPla "adminIncognito helper fs" i "no longer incognito." ]
           | otherwise = [ wrapSend mq cols "You have gone incognito."
                         , bcastOtherAdmins i $ s <> " has gone incognito."
                         , logPla "adminIncognito helper fs" i "went incognito." ]
    in (ms & plaTbl.ind i %~ setPlaFlag IsIncognito (not isIncog), fs)
adminIncognito p = withoutArgs adminIncognito p


-----


adminIp :: ActionFun
adminIp (NoArgs i mq cols) = do
    ifList <- liftIO mkInterfaceList
    multiWrapSend mq cols [ "Interfaces: " <> ifList, prd $ "Listening on port " <> showText port ]
    logPlaExec (prefixAdminCmd "ip") i
adminIp p = withoutArgs adminIp p


-----


adminKill :: ActionFun
adminKill p@AdviseNoArgs          = advise p [ prefixAdminCmd "kill" ] adviceAKillNoArgs
adminKill (LowerNub i mq cols as) = getState >>= \ms ->
    let (is, fs) = foldl' (helper ms) ((,) mempty mempty) as
        logMsg   = prd $ "killing " <> commas [ getSing targetId ms <> " " <> parensQuote (showText targetId)
                                              | targetId <- is ]
        cn       = prefixAdminCmd "kill"
        s        = getSing i ms
    in do
        unless (()# is) . sequence_ $ [ logPla cn i logMsg
                                      , forM_ is . flip (logPla cn) . prd $ "killed by " <> s ]
        ok mq
        sequence_ fs
  where
    helper ms (is, fs) a =
        let (is', fs')  = case reads . T.unpack $ a :: [(Int, String)] of
                            [(targetId, "")] | targetId < 0                                -> h sorryWtf
                                             | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorry
                                             | otherwise                                   -> go targetId
                            _                                                              -> sorry
            h           = (,) mempty . pure . wrapSend mq cols
            sorry       = h . sorryParseId $ a
            go targetId | targetId == i                                  = h sorryAdminKillSelf
                        | getType targetId ms `elem` [ NpcType, PCType ] = (pure targetId, mkFuns)
                        | otherwise                                      = h . sorryAdminKillType $ targetId
              where
                mkFuns =
                    let d        = mkStdDesig targetId ms Don'tCap
                        toSelf   = mkBs (nl $ adminKillMsg "you are",              pure targetId               )
                        toOthers = mkBs (nl . adminKillMsg $ serialize d <> " is", targetId `delete` desigIds d)
                        mkBs     = pure . first (colorWith adminKillColor)
                    in [ bcast $ toSelf ++ toOthers, handleDeath targetId ]
        in (is ++ is', fs ++ fs')
adminKill p = patternMatchFail "adminKill" . showText $ p


-----


adminLink :: ActionFun
adminLink p@AdviseNoArgs          = advise p [ prefixAdminCmd "link" ] adviceALinkNoArgs
adminLink (LowerNub i mq cols as) = getState >>= \ms -> do
    let helper target =
            let notFound              = unadulterated . sorryPCName $ target
                found (_, targetSing) = (\case
                  [] -> header none
                  ss -> header . mkReport $ ss) <$> (liftIO . lookupTeleNames $ targetSing)
                  where
                    header          = (targetSing <> "'s two-way links:" :)
                    mkReport     ss | pairs <- sortBy (flip compare `on` fst) . mkCountSings $ ss
                                    = map (\(c, s) -> s <> " " <> parensQuote (showText c)) pairs
                    mkCountSings ss = [ (length g, s) | g@(s:_) <- sortGroup . map fromOnly $ ss ]
            in findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
    pager i mq Nothing . noneOnNull . intercalateDivider cols =<< forM as (helper . capitalize . T.toLower)
    logPlaExecArgs (prefixAdminCmd "link") as i
adminLink p = patternMatchFail "adminLink" . showText $ p


-----


adminLocate :: ActionFun
adminLocate p@AdviseNoArgs          = advise p [ prefixAdminCmd "locate" ] adviceALocateNoArgs
adminLocate (LowerNub i mq cols as) = getState >>= \ms ->
    let helper a = case reads . T.unpack $ a :: [(Int, String)] of
          [(targetId, "")] | targetId < 0                                -> sorryWtf
                           | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorryParseId a
                           | otherwise                                   -> locate targetId
          _                                                              -> sorryParseId a
          where
            locate targetId = let (_, desc) = locateHelper ms [] targetId
                              in mkNameTypeIdDesc targetId ms <> (()!# desc |?| (", " <> desc))
    in do
        multiWrapSend mq cols . intersperse "" . map helper $ as
        logPlaExecArgs (prefixAdminCmd "locate") as i
adminLocate p = patternMatchFail "adminLocate" . showText $ p


-----


adminMsg :: ActionFun
adminMsg p@AdviseNoArgs     = advise p [ prefixAdminCmd "message" ] adviceAMsgNoArgs
adminMsg p@(AdviseOneArg a) = advise p [ prefixAdminCmd "message" ] . adviceAMsgNoMsg $ a
adminMsg (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
    logMsgs |#| let f = uncurry . logPla $ "adminMsg" in mapM_ f
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
              | isLoggedIn targetPla, isIncognitoId i ms ->
                emptied . sendFun $ sorryMsgIncog
              | isLoggedIn targetPla ->
                  let (targetMq, targetCols) = getMsgQueueColumns targetId ms
                      adminSings             = map snd . filter f . mkAdminIdSingList $ ms
                      f (_, "Root")          = let rootPla = getPla iRoot ms
                                               in isLoggedIn rootPla && (not . isIncognito $ rootPla)
                      f _                    = True
                      me                     = head . filter g . styleAbbrevs Don'tQuote $ adminSings
                      g                      = (== s) . dropANSI
                      toTarget'              = quoteWith "__" me <> " " <> toTarget
                  in do
                      sendFun formatted
                      (multiWrapSend targetMq targetCols =<<) $ if isNotFirstAdminMsg targetPla
                        then unadulterated toTarget'
                        else [ toTarget' : hints | hints <- firstAdminMsg targetId s ]
                      dbHelper
              | otherwise -> do
                  multiSendFun [ formatted, parensQuote "Message retained." ]
                  retainedMsg targetId ms . mkRetainedMsgFromPerson s $ toTarget
                  dbHelper
              where
                targetPla = getPla targetId ms
                formatted = parensQuote ("to " <> targetSing) <> spaced (quoteWith "__" s) <> toSelf
                dbHelper  = do
                    ts <- liftIO mkTimestamp
                    withDbExHandler_ "admin_msg" . insertDbTblAdminMsg . AdminMsgRec ts s targetSing $ toSelf
                    return [ sentLogMsg, receivedLogMsg ]
                sentLogMsg     = (i,        T.concat [ "sent message to ", targetSing, ": ", toSelf   ])
                receivedLogMsg = (targetId, T.concat [ "received message from ", s,    ": ", toTarget ])
            ioHelper _ xs = patternMatchFail "adminMsg helper ioHelper" . showText $ xs
        in (findFullNameForAbbrev strippedTarget . mkPlaIdSingList $ ms) |&| maybe notFound found
adminMsg p = patternMatchFail "adminMsg" . showText $ p


firstAdminMsg :: Id -> Sing -> MudStack [Text]
firstAdminMsg i adminSing =
    modifyState $ (, [ "", hintAMsg adminSing ]) . (plaTbl.ind i %~ setPlaFlag IsNotFirstAdminMsg True)


-----


adminPassword :: ActionFun
adminPassword p@AdviseNoArgs                     = advise p [ prefixAdminCmd "password" ] adviceAPasswordNoArgs
adminPassword p@(AdviseOneArg a                ) = advise p [ prefixAdminCmd "password" ] . adviceAPasswordNoPw $ a
adminPassword p@(WithTarget i mq cols target pw)
  | length (T.words pw) > 1 = advise p [ prefixAdminCmd "password" ] adviceAPasswordExcessArgs
  | otherwise               = getState >>= \ms ->
      let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player whose password you wish to change"
          changePW = (withDbExHandler fn . liftIO . lookupPW $ strippedTarget) >>= \case
            Nothing           -> dbError mq cols
            Just (Just oldPW) -> do
                withDbExHandler_ fn . insertDbTblUnPw . UnPwRec strippedTarget $ pw
                sendFun $ strippedTarget <> "'s password has been changed."
                let msg      = T.concat [ getSing i ms, " has changed ", strippedTarget, "'s password" ]
                    oldPwMsg = prd $ " " <> parensQuote ("was " <> dblQuote oldPW)
                bcastOtherAdmins i . prd $ msg
                logPla fn i . T.concat $ [ "changed ", strippedTarget, "'s password", oldPwMsg ]
                logNotice fn $ msg <> oldPwMsg
            Just Nothing -> blowUp fn "password not found in database" strippedTarget
      in if
        | not . inRange (minPwLen, maxPwLen) . T.length $ pw -> sendFun sorryInterpNewPwLen
        | helper isUpper                                     -> sendFun sorryInterpNewPwUpper
        | helper isLower                                     -> sendFun sorryInterpNewPwLower
        | helper isDigit                                     -> sendFun sorryInterpNewPwDigit
        | otherwise -> case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
          []         -> sendFun $ sorryPCName strippedTarget <> " " <> hintAPassword
          [targetId] -> let targetPla = getPla targetId ms in if | targetId == i     -> sendFun sorryAdminPasswordSelf
                                                                 | isAdmin targetPla -> sendFun sorryAdminPasswordAdmin
                                                                 | otherwise         -> changePW
          xs         -> patternMatchFail "adminPassword" . showText $ xs
  where
    fn       = "adminPassword"
    helper f = ()# T.filter f pw
adminPassword p = patternMatchFail "adminPassword" . showText $ p


-----


adminMyChans :: ActionFun
adminMyChans p@AdviseNoArgs          = advise p [ prefixAdminCmd "mychannels" ] adviceAMyChansNoArgs
adminMyChans (LowerNub i mq cols as) = getState >>= \ms ->
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
      _ -> pager i mq Nothing allReports >> logPlaExec (prefixAdminCmd "mychannels") i
adminMyChans p = patternMatchFail "adminMyChans" . showText $ p


-----


adminPeep :: ActionFun
adminPeep p@AdviseNoArgs = advise p [ prefixAdminCmd "peep" ] adviceAPeepNoArgs
adminPeep (LowerNub i mq cols as) = do
    (msgs, unzip -> (logMsgsSelf, logMsgsOthers)) <- modifyState helper
    multiWrapSend mq cols msgs
    logPla "adminPeep" i . prd . slashes $ logMsgsSelf
    forM_ logMsgsOthers . uncurry . logPla $ "adminPeep"
  where
    helper ms =
        let s     = getSing i ms
            apiss = [ apis | apis@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
            peep target a@(pt, _, _) =
                let notFound = a & _2 %~ (sorryPCNameLoggedIn target :)
                    found (peepId@(flip getPla ms -> peepPla), peepSing) = if peepId `notElem` pt^.ind i.peeping
                      then if
                        | peepId == i     -> a & _2 %~ (sorryPeepSelf  :)
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
        in (ms & plaTbl .~ res^._1, (res^._2, res^._3))
adminPeep p = patternMatchFail "adminPeep" . showText $ p


-----


adminPersist :: ActionFun
adminPersist (NoArgs' i mq) = persist >> ok mq >> logPlaExec (prefixAdminCmd "persist") i
adminPersist p              = withoutArgs adminPersist p


-----


adminPossess :: ActionFun
adminPossess p@(NoArgs' i mq) = advise p [ prefixAdminCmd "possess" ] adviceAPossessNoArgs >> sendDfltPrompt mq i
adminPossess (OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The ID of the NPC you wish to possess"
        possess targetId    = if isNpc targetId ms
          then maybe canPossess can'tPossess . getPossessor targetId $ ms
          else sorry . sorryPossessType $ targetSing
          where
            targetSing      = getSing targetId ms
            can'tPossess pi = sorry . sorryAlreadyPossessed targetSing . getSing pi $ ms
            canPossess      = ( ms & plaTbl.ind i       .possessing   ?~ targetId
                                   & npcTbl.ind targetId.npcPossessor ?~ i
                              , [ sendFun . prd $ "You are now possessing " <> aOrAnOnLower targetSing
                                , sendDfltPrompt mq targetId
                                , logPla "adminPossess" i $ "started possessing "                 <>
                                                            aOrAnOnLower (descSingId targetId ms) <>
                                                            "." ] )
        sorry txt = (ms, [ sendFun txt, sendDfltPrompt mq i ])
    in case reads . T.unpack $ strippedTarget :: [(Int, String)] of
      [(targetId, "")]
        | targetId < 0                                -> sorry sorryWtf
        | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorry . sorryParseId $ strippedTarget'
        | otherwise                                   -> case getPossessing i ms of
          Nothing -> possess targetId
          Just pi -> sorry . sorryAlreadyPossessing . getSing pi $ ms
      _ -> sorry . sorryParseId $ strippedTarget'
adminPossess ActionParams { myId, plaMsgQueue, plaCols } = do
    wrapSend plaMsgQueue plaCols adviceAPossessExcessArgs
    sendDfltPrompt plaMsgQueue myId


-----


adminPrint :: ActionFun
adminPrint p@AdviseNoArgs  = advise p [ prefixAdminCmd "print" ] adviceAPrintNoArgs
adminPrint (Msg' i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    liftIO . T.putStrLn $ bracketQuote s <> " " <> colorWith printConsoleColor msg
    ok mq
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
adminPrint p = patternMatchFail "adminPrint" . showText $ p


-----


adminProfanity :: ActionFun
adminProfanity p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "profanity" f "profanity" p
  where
    f :: [ProfRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminSearch :: ActionFun
adminSearch p@AdviseNoArgs                        = advise p [ prefixAdminCmd "id" ] adviceASearchNoArgs
adminSearch (WithArgs i mq cols (T.unwords -> a)) = getState >>= \ms -> do
    multiWrapSend mq cols $ descMatchingSings ms ++ [""] ++ descMatchingRmNames ms
    logPlaExecArgs (prefixAdminCmd "search") (pure a) i
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
                                              , b |?| (parensQuote . pp . getType i' $ ms) <> " "
                                              , x
                                              , colorWith regexMatchColor y
                                              , z ]
adminSearch p = patternMatchFail "adminSearch" . showText $ p


applyRegex :: Text -> Text -> (Text, Text, Text)
applyRegex searchTerm target = let (🍩) = (=~) `on` T.unpack in target 🍩 searchTerm |&| each %~ T.pack


-----


adminSecurity :: ActionFun
adminSecurity p@AdviseNoArgs            = advise p [ prefixAdminCmd "security" ] adviceASecurityNoArgs
adminSecurity   (LowerNub i mq cols as) = (withDbExHandler "adminSecurity" . getDbTblRecs $ "sec") >>= \case
  Just (recs :: [SecRec]) -> do
      multiWrapSend mq cols . intercalateDivider cols . concatMap (helper recs . capitalize . T.toLower) $ as
      logPlaExecArgs (prefixAdminCmd "security") as i
  Nothing   -> dbError mq cols
  where
    helper recs target = case filter ((target `T.isPrefixOf`) . (dbName :: SecRec -> Text)) recs of
      []      -> pure . pure . prd $ "No records found for " <> dblQuote target
      matches -> map mkSecReport matches
adminSecurity p = patternMatchFail "adminSecurity" . showText $ p


mkSecReport :: SecRec -> [Text]
mkSecReport SecRec { .. } = [ "Name: "     <> dbName
                            , "Question: " <> dbQ
                            , "Answer: "   <> dbA ]


-----


adminSet :: ActionFun
adminSet p@AdviseNoArgs                       = advise p [ prefixAdminCmd "set" ] adviceASetNoArgs
adminSet p@(AdviseOneArg a                  ) = advise p [ prefixAdminCmd "set" ] . adviceASetNoSettings $ a
adminSet   (WithArgs i mq cols (target:rest)) = helper |&| modifyState >=> \(toSelfMsgs, mTargetId, toTargetMsgs, logMsgs, fs) -> do
    multiWrapSend mq cols toSelfMsgs
    let ioHelper targetId = getState >>= \ms -> do
            let f = case getType targetId ms of
                      PCType  -> retainedMsg targetId ms
                      NpcType -> bcast . mkBcast targetId
                      t       -> patternMatchFail "adminSet f" . showText $ t
            unless (isIncognitoId i ms || targetId == i) . forM_ (dropBlanks toTargetMsgs) $ f . colorWith adminSetColor
            sequence_ fs
            logMsgs |#| logPla (prefixAdminCmd "set") i . g . slashes
          where
            g = (parensQuote ("for ID " <> showText targetId) <>) . (" " <>)
    maybeVoid ioHelper mTargetId
  where
    helper ms = case reads . T.unpack $ target :: [(Int, String)] of
      [(targetId, "")] | targetId < 0                                -> sorryHelper sorryWtf
                       | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorry
                       | otherwise                                   -> f targetId
      _                                                              -> sorry
      where
        sorry       = sorryHelper . sorryParseId $ target
        sorryHelper = (ms, ) . (, Nothing, [], [], []) . pure
        f targetId  = maybe (sorryHelper sorryQuoteChars) g . procQuoteChars $ rest
          where
            g rest' = let (ms', toSelfMsgs, toTargetMsgs, logMsgs, fs) = foldl' (setHelper targetId) (ms, [], [], [], []) rest'
                      in (ms', (toSelfMsgs, Just targetId, toTargetMsgs, logMsgs, fs))
adminSet p = patternMatchFail "adminSet" . showText $ p


setHelper :: Id -> (MudState, [Text], [Text], [Text], Funs) -> Text -> (MudState, [Text], [Text], [Text], Funs)
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
                      , "exp"
                      , "hand"
                      , "knownlangs"
                      , "mobrmdesc"
                      , "tempdesc"
                      , "corpseweight"
                      , "corpsevol"
                      , "corpsecapacity"
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
          "entname"        -> setEntMaybeTextHelper  t "entName"  "name"        entName  entName
          "sing"           -> setEntTextHelper       t "sing"     "singular"    sing     sing
          "plur"           -> setEntTextHelper       t "plur"     "plural"      plur     plur
          "entdesc"        -> setEntTextHelper       t "entDesc"  "description" entDesc  entDesc
          "entsmell"       -> setEntMaybeTextHelper  t "entSmell" "smell"       entSmell entSmell
          "sex"            -> setMobSexHelper        t
          "st"             -> setMobAttribHelper     t "st" "ST" st st
          "dx"             -> setMobAttribHelper     t "dx" "DX" dx dx
          "ht"             -> setMobAttribHelper     t "ht" "HT" ht ht
          "ma"             -> setMobAttribHelper     t "ma" "MA" ma ma
          "ps"             -> setMobAttribHelper     t "ps" "PS" ps ps
          "curhp"          -> setMobCurHelper        t "curHp" "HP" getHps curHp
          "curmp"          -> setMobCurHelper        t "curMp" "MP" getMps curMp
          "curpp"          -> setMobCurHelper        t "curPp" "PP" getPps curPp
          "curfp"          -> setMobCurHelper        t "curFp" "FP" getFps curFp
          "exp"            -> setMobExpHelper        t
          "hand"           -> setMobHandHelper       t
          "knownlangs"     -> setMobKnownLangsHelper t
          "mobrmdesc"      -> setMobRmDescHelper     t
          "tempdesc"       -> setMobTempDescHelper   t
          "corpseweight"   -> setMobCorpseHelper     t "corpseWeight"   "corpse weight"   corpseWeight   corpseWeight
          "corpsevol"      -> setMobCorpseHelper     t "corpseVol"      "corpse volume"   corpseVol      corpseVol
          "corpsecapacity" -> setMobCorpseHelper     t "corpseCapacity" "corpse capacity" corpseCapacity corpseCapacity
          "following"      -> setMobFollowingHelper  t
          "followers"      -> setMobInvHelper        t "followers" "followers have" (party.followers) (party.followers)
          "mygroup"        -> setMobInvHelper        t "myGroup"   "group has"      (party.myGroup  ) (party.myGroup  )
          "memberof"       -> setMobMemberOfHelper   t
          "race"           -> setPCRaceHelper        t
          "introduced"     -> setPCSingListHelper    t "introduced" "known names"  introduced introduced
          "linked"         -> setPCSingListHelper    t "linked"     "linked names" linked     linked
          "skillpts"       -> setPCSkillPtsHelper    t
          x                -> patternMatchFail "setHelper helper found" (x :: Text)
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
                           addSubAssignHelper f = let x'     = max1 $ prev `f` x
                                                      diff   = x' - prev
                                                      toSelf = mkToSelfForInt k x' diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ x'
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> let x'     = max1 x
                                                      diff   = x' - prev
                                                      toSelf = mkToSelfForInt k x' diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ x'
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have gained ", commaShow diff,         " ", n, "." ]
                            | otherwise = pure . T.concat $ [ "You have lost ",   commaShow . abs $ diff, " ", n, "." ]
        -----
        setMobCurHelper t k n f setter
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> let (c, m)               = f targetId ms
                           addSubAssignHelper g = let c'     = (c `g` x) `min` m
                                                      diff   = c' - c
                                                      toSelf = mkToSelfForInt k c' diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ c'
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> let x'     = x `min` m
                                                      diff   = x' - c
                                                      toSelf = mkToSelfForInt k x' diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ x'
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have recovered ", commaShow diff,         " ", n, "." ]
                            | otherwise = pure . T.concat $ [ "You have lost ",      commaShow . abs $ diff, " ", n, "." ]
        -----
        setMobExpHelper t
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue "exp" $ value
            Right x -> let prev                 = getExp targetId ms
                           addSubAssignHelper g = f $ max0 (prev `g` x)
                           f x'                 = let diff   = x' - prev
                                                      toSelf = mkToSelfForInt "exp" x' diff
                                                      a' | isPC targetId ms
                                                         , diff > 0
                                                         = a & _5 <>~ pure (awardExp diff (prefixAdminCmd "set") targetId)
                                                         | otherwise
                                                         = a & _1.mobTbl.ind targetId.exp .~ x'
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
                    addSubAssignHelper f = let (x', toSelf, isDiff) = mkTupleForList prev f x mkToSelf
                                           in a & _1.mobTbl.ind targetId.knownLangs .~ x'
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget x')
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
        setMobCorpseHelper t k n getter setter
          | not . hasMob $ t = sorryType
          | otherwise        = case eitherDecode value' of
            Left  _ -> appendMsg . sorryAdminSetValue k $ value
            Right x -> let prev                 = view getter . getMob targetId $ ms
                           addSubAssignHelper f = let x'     = max0 $ prev `f` x
                                                      diff   = x' - prev
                                                      toSelf = mkToSelfForInt k x' diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ x'
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                       in case op of Assign    -> let x'     = max0 x
                                                      diff   = x' - prev
                                                      toSelf = mkToSelfForInt k x' diff
                                                  in a & _1.mobTbl.ind targetId.setter .~ x'
                                                       & _2 <>~ toSelf
                                                       & _3 <>~ (Sum diff |!| mkToTarget diff)
                                                       & _4 <>~ (Sum diff |!| toSelf)
                                     AddAssign -> addSubAssignHelper (+)
                                     SubAssign -> addSubAssignHelper (-)
          where
            mkToTarget diff | diff > 0  = pure . T.concat $ [ "You have gained ", commaShow diff,         " ", n, "." ]
                            | otherwise = pure . T.concat $ [ "You have lost ",   commaShow . abs $ diff, " ", n, "." ]
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
                    addSubAssignHelper f = let (x', toSelf, isDiff) = mkTupleForList prev f x mkToSelf
                                           in a & _1.mobTbl.ind targetId.setter .~ x'
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget x')
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
                    addSubAssignHelper f = let (x', toSelf, isDiff) = mkTupleForList prev f x mkToSelf
                                           in a & _1.pcTbl.ind targetId.setter .~ x'
                                                & _2 <>~ toSelf
                                                & _3 <>~ (isDiff |?| mkToTarget x')
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
                           f x'                 = let diff   = x' - prev
                                                      toSelf = mkToSelfForInt "skillPts" x' diff
                                                  in a & _1.pcTbl.ind targetId.skillPts .~ x'
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
        sorryType                = appendMsg . sorryAdminSetType $ targetId
        sorryOp                  = appendMsg . sorryAdminSetOp (pp op)
        value'                   = strictTextToLazyBS value
        mkDiffTxt isDiff         = not isDiff |?| (" " <> parensQuote "no change")
        showMaybe Nothing        = none
        showMaybe (Just x)       = showText x
        mkToSelfForInt k v diff = pure . T.concat $ [ "Set ", k, " to ", commaShow v, " ", parensQuote diffTxt, "." ]
          where
            diffTxt = if | diff == 0 -> "no change"
                         | diff >  0 -> "added "      <> commaShow diff
                         | otherwise -> "subtracted " <> commaShow (abs diff)
        mkTupleForList prev f x g = let x'     = nubSort $ prev `f` x
                                        toSelf = g x' isDiff
                                        isDiff = x' /= prev
                                    in (x', toSelf, isDiff)


-----


adminShutdown :: ActionFun
adminShutdown (NoArgs' i mq    ) = shutdownHelper i mq Nothing
adminShutdown (Msg'    i mq msg) = shutdownHelper i mq . Just $ msg
adminShutdown p                  = patternMatchFail "adminShutdown" . showText $ p


shutdownHelper :: Id -> MsgQueue -> Maybe Text -> MudStack ()
shutdownHelper i mq maybeMsg = getState >>= \ms ->
    let s    = getSing i ms
        rest = maybeMsg |&| maybe (prd $ " " <> parensQuote "no message given") (("; message: " <>) . dblQuote)
    in do
        massSend . colorWith shutdownMsgColor . fromMaybe dfltShutdownMsg $ maybeMsg
        logPla     "shutdownHelper" i $ "initiating shutdown" <> rest
        massLogPla "shutdownHelper"   $ "closing connection due to server shutdown initiated by " <> s <> rest
        logNotice  "shutdownHelper"   $ "server shutdown initiated by "                           <> s <> rest
        writeMsg mq Shutdown


-----


adminSudoer :: ActionFun
adminSudoer p@AdviseNoArgs                  = advise p [ prefixAdminCmd "sudoer" ] adviceASudoerNoArgs
adminSudoer (OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let fn                  = "adminSudoer helper"
        SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to promote/demote"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      [] -> (ms, pure . sendFun $ sorryPCName strippedTarget <> " " <> hintASudoer)
      [targetId]
        | selfSing       <- getSing i ms
        , targetSing     <- getSing targetId ms
        , ia             <- isAdminId targetId ms
        , (verb, toFrom) <- ia ? ("demoted", "from") :? ("promoted", "to")
        , handleIncog    <- when (isIncognitoId targetId ms) . adminIncognito . mkActionParams targetId ms $ []
        , handlePeep     <-
            let peepingIds = getPeeping targetId ms
            in unless (()# peepingIds) . adminPeep . mkActionParams targetId ms . map (`getSing` ms) $ peepingIds
        , fs <- [ retainedMsg targetId ms . colorWith promoteDemoteColor . T.concat $ [ selfSing
                                                                                      , " has "
                                                                                      , verb
                                                                                      , " you "
                                                                                      , toFrom
                                                                                      , " admin status." ]
                , sendFun                           . T.concat $ [ "You have ",       verb, " ", targetSing, "." ]
                , bcastAdminsExcept [ i, targetId ] . T.concat $ [ selfSing, " has ", verb, " ", targetSing, "." ]
                , logNotice fn                      . T.concat $ [ selfSing, spaced verb,        targetSing, "." ]
                , logPla    fn i                    . T.concat $ [ verb, " ",                    targetSing, "." ]
                , logPla    fn targetId             . T.concat $ [ verb, " by ",                 selfSing,   "." ]
                , handleIncog
                , handlePeep ]
        -> if | targetId   == i      -> (ms, pure . sendFun $ sorrySudoerDemoteSelf)
              | targetSing == "Root" -> (ms, pure . sendFun $ sorrySudoerDemoteRoot)
              | otherwise            -> (ms & plaTbl.ind targetId %~ setPlaFlag IsAdmin      (not ia)
                                            & plaTbl.ind targetId %~ setPlaFlag IsTunedAdmin (not ia), fs)
      xs -> patternMatchFail "adminSudoer helper" . showText $ xs
adminSudoer p = advise p [] adviceASudoerExcessArgs


-----


adminSummon :: ActionFun
adminSummon p@AdviseNoArgs                  = advise p [ prefixAdminCmd "summon" ] adviceASummonNoArgs
adminSummon (OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the PC you wish to summon"
        idSings             = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
        destId              = getRmId i ms
        rn                  = getRmName destId ms
        destName            = rn <> " " <> parensQuote ("summoned by " <> s)
        s                   = getSing i ms
        sorry               = (ms, ) . pure . sendFun
        found (targetId@((`getRmId` ms) -> originId), targetSing)
          | targetSing == s       = sorry sorrySummonSelf
          | isAdminId targetId ms = sorry sorrySummonAdmin
          | destId == originId    = sorry . sorrySummonAlready $ targetSing
          | p   <- mkActionParams targetId ms []
          , res <- teleHelper p ms originId destId destName Nothing consLocPrefBcast
          = res & _2 <>~ pure (logPla "adminSummon" i . T.concat $ [ "summoned ", targetSing, " to ", dblQuote rn, "." ])
        notFound = sorry . sorryPCNameLoggedIn $ strippedTarget
    in findFullNameForAbbrev strippedTarget idSings |&| maybe notFound found
adminSummon ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceASummonExcessArgs


-----


adminTeleId :: ActionFun
adminTeleId p@AdviseNoArgs                    = advise p [ prefixAdminCmd "teleid" ] adviceATeleIdNoArgs
adminTeleId p@(OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The ID of the entity or room to which you want to \
                                                            \teleport"
        teleport targetId  =
            let (destId, desc) = locateHelper ms [] targetId
                destName       = mkNameTypeIdDesc targetId ms <> (()!# desc |?| (", " <> desc))
                notice         = "Teleporting to " <> destName <> "..."
                originId       = getRmId i ms
                sorry          = (ms, ) . pure . multiSendFun . (notice :) . pure
            in if | destId == originId   -> sorry sorryTeleAlready
                  | destId == iLoggedOut -> sorry sorryTeleLoggedOutRm
                  | otherwise            ->
                      teleHelper p { args = [] } ms originId destId destName (Just notice) consLocPrefBcast
        sorryParse = (ms, ) . pure . sendFun
    in case reads . T.unpack $ strippedTarget :: [(Int, String)] of
      [(targetId, "")]
        | targetId < 0                                -> sorryParse sorryWtf
        | targetId `notElem` (ms^.typeTbl.to IM.keys) -> sorryParse . sorryParseId $ strippedTarget'
        | otherwise                                   -> teleport targetId
      _                                               -> sorryParse . sorryParseId $ strippedTarget'
adminTeleId ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceATeleIdExcessArgs


teleHelper :: ActionParams
           -> MudState
           -> Id
           -> Id
           -> Text
           -> Maybe Text
           -> (Id -> [Broadcast] -> [Broadcast])
           -> (MudState, Funs)
teleHelper p@ActionParams { myId } ms originId destId destName mt f =
    let g            = maybe strictId (\t -> ((nlnl t, pure myId) :)) mt
        originDesig  = mkStdDesig myId ms Don'tCap
        originMobIds = myId `delete` desigIds originDesig
        s            = fromJust . desigEntSing $ originDesig
        destDesig    = mkSerializedNonStdDesig myId ms s A Don'tCap
        destMobIds   = findMobIds ms $ ms^.invTbl.ind destId
        ms'          = ms & mobTbl.ind myId.rmId .~ destId
                          & invTbl.ind originId  %~ (myId `delete`)
                          & invTbl.ind destId    %~ addToInv ms (pure myId)
    in (ms', [ bcastIfNotIncog myId . f myId . g $ [ (nlnl   teleDescMsg,                             pure myId   )
                                                   , (nlnl . teleOriginMsg . serialize $ originDesig, originMobIds)
                                                   , (nlnl . teleDestMsg               $ destDesig,   destMobIds  ) ]
             , look p
             , logPla "telehelper" myId . prd $ "teleported to " <> dblQuote destName
             , rndmDos [ (calcProbTeleDizzy   myId ms, mkExpAction "dizzy"   p)
                       , (calcProbTeleShudder myId ms, mkExpAction "shudder" p) ] ])


-----


adminTelePC :: ActionFun
adminTelePC p@AdviseNoArgs                    = advise p [ prefixAdminCmd "telepc" ] adviceATelePCNoArgs
adminTelePC p@(OneArgNubbed i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the PC to which you want to teleport"
        idSings             = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
        originId            = getRmId i ms
        found (flip getRmId ms -> destId, targetSing)
          | targetSing == getSing i ms = (ms, pure .  sendFun $ sorryTeleSelf)
          | destId     == originId     = (ms, pure .  sendFun $ sorryTeleAlready)
          | otherwise = teleHelper p { args = [] } ms originId destId targetSing Nothing consLocPrefBcast
        notFound = (ms, pure . sendFun . sorryPCNameLoggedIn $ strippedTarget)
    in findFullNameForAbbrev strippedTarget idSings |&| maybe notFound found
adminTelePC ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceATelePCExcessArgs


-----


adminTeleRm :: ActionFun
adminTeleRm (NoArgs i mq cols) = (multiWrapSend mq cols =<< mkTxt) >> logPlaExecArgs (prefixAdminCmd "telerm") [] i
  where
    mkTxt  = views rmTeleNameTbl ((header :) . styleAbbrevs Don'tQuote . IM.elems) <$> getState
    header = "You may teleport to the following rooms:"
adminTeleRm p@(OneArgLower i mq cols target) = modifyStateSeq $ \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the room to which you want to teleport"
        originId            = getRmId i ms
        found (destId, rmTeleName)
          | destId == originId = (ms, pure . sendFun $ sorryTeleAlready)
          | otherwise          = teleHelper p { args = [] } ms originId destId rmTeleName Nothing consLocPrefBcast
        notFound               = (ms, pure . sendFun . sorryTeleRmName $ strippedTarget')
    in (findFullNameForAbbrev strippedTarget' . views rmTeleNameTbl IM.toList $ ms) |&| maybe notFound found
adminTeleRm p = advise p [] adviceATeleRmExcessArgs


-----


adminTime :: ActionFun
adminTime (NoArgs i mq cols) = do
    (ct, zt) <- liftIO $ (,) <$> formatThat `fmap` getCurrentTime <*> formatThat `fmap` getZonedTime
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
    logPlaExec (prefixAdminCmd "time") i
  where
    formatThat :: (FormatTime a) => a -> Text
    formatThat = T.pack . formatTime defaultTimeLocale "%Z: %F %T"
adminTime p = withoutArgs adminTime p


-----


adminTType :: ActionFun
adminTType (NoArgs i mq cols) = (withDbExHandler "adminTType" . getDbTblRecs $ "ttype") >>= \case
  Just xs ->
    let grouped = groupBy ((==) `on` dbTType) xs
        folded  = foldr (\g -> ((dbTType . head $ g, nubSort . map (dbHost :: TTypeRec -> Text) $ g) :)) [] grouped
        txtss   = [ uncurry (:) . first (<> t) $ pair | pair@(_, hosts) <- folded
                                                      , let l = length hosts
                                                      , let t = T.concat [ ": "
                                                                         , showText l
                                                                         , " host"
                                                                         , pluralize ("", "s") l ] ]
    in (>> logPlaExec (prefixAdminCmd "ttype") i) $ case intercalateDivider cols txtss of
          [] -> wrapSend      mq cols dbEmptyMsg
          ts -> multiWrapSend mq cols ts


  Nothing -> dbError mq cols
adminTType p = withoutArgs adminTType p


-----


adminTypo :: ActionFun
adminTypo p@ActionParams { plaMsgQueue, plaCols } = dumpCmdHelper "typo" f "typo" p
  where
    f :: [TypoRec] -> MudStack ()
    f = dumpDbTblHelper plaMsgQueue plaCols


-----


adminUptime :: ActionFun
adminUptime (NoArgs i mq cols) = do
    send mq . nl =<< liftIO uptime |&| try >=> eitherRet ((sendGenericErrorMsg mq cols >>) . logIOEx "adminUptime")
    logPlaExec (prefixAdminCmd "uptime") i
  where
    uptime = T.pack <$> readProcess "uptime" [] ""
adminUptime p = withoutArgs adminUptime p


-----


adminWhoIn :: ActionFun
adminWhoIn = whoHelper LoggedIn "whoin"


whoHelper :: LoggedInOrOut -> Text -> ActionFun
whoHelper inOrOut cn (NoArgs i mq cols) = do
    pager i mq Nothing =<< [ concatMap (wrapIndent 20 cols) charListTxt | charListTxt <- mkCharListTxt inOrOut <$> getState ]
    logPlaExecArgs (prefixAdminCmd cn) [] i
whoHelper inOrOut cn p@ActionParams { myId, args } =
    (dispMatches p 20 =<< mkCharListTxt inOrOut <$> getState) >> logPlaExecArgs (prefixAdminCmd cn) args myId


mkCharListTxt :: LoggedInOrOut -> MudState -> [Text]
mkCharListTxt inOrOut ms =
    let is               = IM.keys . IM.filter predicate $ ms^.plaTbl
        (is', ss)        = unzip [ (i, s) | i <- is, let s = getSing i ms, then sortWith by s ]
        ias              = zip is' . styleAbbrevs Don'tQuote $ ss
        mkCharTxt (i, a) = let (s, r, l) = mkPrettifiedSexRaceLvl i ms
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
    mkAnnotatedName i a = let p     = getPla i ms
                              admin = isAdmin p     |?| asterisk
                              incog = isIncognito p |?| colorWith asteriskColor "@"
                          in a <> admin <> incog


-----


adminWhoOut :: ActionFun
adminWhoOut = whoHelper LoggedOut "whoout"


-----


adminWire :: ActionFun
adminWire p@AdviseNoArgs          = advise p [ prefixAdminCmd "wiretap" ] adviceAWireNoArgs
adminWire (WithArgs i mq cols as) = views chanTbl IM.size <$> getState >>= \case
  0 -> informNoChans mq cols
  _ -> helper |&| modifyState >=> \(msgs, logMsgs) ->
           multiWrapSend mq cols msgs >> logMsgs |#| logPlaOut (prefixAdminCmd "wiretap") i
  where
    helper ms = let (ms', msgs) = foldl' helperWire (ms, []) as
                in (ms', (map fromEither msgs, rights msgs))
    helperWire (ms, msgs) a =
        let (ms', msg) = case reads . T.unpack $ a :: [(Int, String)] of
                           [(ci, "")] | ci < 0                                -> sorry sorryWtf
                                      | ci `notElem` (ms^.chanTbl.to IM.keys) -> sorry . sorryParseChanId $ a
                                      | otherwise                             -> checkAlreadyConn ci
                           _                                                  -> sorry . sorryParseChanId $ a
            checkAlreadyConn ci | s <- getSing i  ms, c <- getChan ci ms = if views (chanConnTbl.at s) isJust c
              then sorry . sorryWireAlready $ c^.chanName
              else toggle ms ci s & _2 %~ Right
            sorry = (ms, ) . Left
        in (ms', msgs ++ pure msg)
    toggle ms ci s = let (cn, ss) = ms^.chanTbl.ind ci.to ((view chanName *** view chanWiretappers) . dup)
                     in if s `elem` ss
                       then ( ms & chanTbl.ind ci.chanWiretappers .~ s `delete` ss
                            , "You stop tapping the "  <> dblQuote cn <> " channel." )
                       else ( ms & chanTbl.ind ci.chanWiretappers <>~ pure s
                            , "You start tapping the " <> dblQuote cn <> " channel." )
adminWire p = patternMatchFail "adminWire" . showText $ p
