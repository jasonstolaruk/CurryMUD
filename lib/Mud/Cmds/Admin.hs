{-# LANGUAGE FlexibleContexts, LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RecordWildCards, TransformListComp, TupleSections, ViewPatterns #-}

module Mud.Cmds.Admin (adminCmds) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Pla
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.LocPref
import Mud.Misc.Persist
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Msgs
import Mud.Util.List hiding (headTail)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut, massLogPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, at, both, to, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (^.))
import Control.Monad ((>=>), forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isLetter)
import Data.Either (isLeft)
import Data.Function (on)
import Data.List ((\\), delete, intercalate, intersperse, nub, partition, sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>), Any(..), Sum(..), getSum)
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, getZonedTime, utcToLocalTime)
import Data.Tuple (swap)
import GHC.Exts (sortWith)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (elems, empty, filter, foldlWithKey', keys, map, mapWithKey, toList)
import qualified Data.Map.Lazy as M (foldl, foldrWithKey)
import qualified Data.Set as S (filter, map, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import System.Process (readProcess)
import System.Time.Utils (renderSecs)


default (Int)


-----


{-# ANN module ("HLint: ignore Use ||" :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Admin"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Admin"


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Admin"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Admin"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Admin"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Admin"


logPlaOut :: CmdName -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.Admin"


massLogPla :: T.Text -> T.Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds.Admin"


-- ==================================================


adminCmds :: [Cmd]
adminCmds =
    [ mkAdminCmd "?"         adminDispCmdList "Display or search this command list."
    , mkAdminCmd "admin"     adminAdmin       "Tune the admin channel in/out, or send a message on the admin channel."
    , mkAdminCmd "banhost"   adminBanHost     "Dump the banned hostname database, or ban/unban a host."
    , mkAdminCmd "banplayer" adminBanPlayer   "Dump the banned player database, or ban/unban a player."
    , mkAdminCmd "announce"  adminAnnounce    "Send a message to all players."
    , mkAdminCmd "boot"      adminBoot        "Boot a player, optionally with a custom message."
    , mkAdminCmd "bug"       adminBug         "Dump the bug database."
    , mkAdminCmd "date"      adminDate        "Display the current system date."
    , mkAdminCmd "host"      adminHost        "Display a report of connection statistics for one or more players."
    , mkAdminCmd "incognito" adminIncognito   "Toggle your incognito status."
    , mkAdminCmd "ip"        adminIp          "Display the server's IP addresses and listening port."
    , mkAdminCmd "message"   adminMsg         "Send a message to a regular player."
    , mkAdminCmd "peep"      adminPeep        "Start or stop peeping one or more players."
    , mkAdminCmd "persist"   adminPersist     "Persist the world (save the current world state to disk)."
    , mkAdminCmd "print"     adminPrint       "Print a message to the server console."
    , mkAdminCmd "profanity" adminProfanity   "Dump the profanity database."
    , mkAdminCmd "shutdown"  adminShutdown    "Shut down CurryMUD, optionally with a custom message."
    , mkAdminCmd "sudoer"    adminSudoer      "Toggle a player's admin status."
    , mkAdminCmd "telepla"   adminTelePla     "Teleport to a given player."
    , mkAdminCmd "telerm"    adminTeleRm      "Display a list of rooms to which you may teleport, or teleport to a \
                                              \given room."
    , mkAdminCmd "time"      adminTime        "Display the current system time."
    , mkAdminCmd "typo"      adminTypo        "Dump the typo database."
    , mkAdminCmd "uptime"    adminUptime      "Display the system uptime."
    , mkAdminCmd "whoin"     adminWhoIn       "Display or search a list of all the people that are currently logged in."
    , mkAdminCmd "whoout"    adminWhoOut      "Display or search a list of all the people that are currently logged \
                                              \out." ]


mkAdminCmd :: T.Text -> Action -> CmdDesc -> Cmd
mkAdminCmd (prefixAdminCmd -> cn) act cd = Cmd { cmdName           = cn
                                               , cmdPriorityAbbrev = Nothing
                                               , cmdFullName       = cn
                                               , action            = act
                                               , cmdDesc           = cd }


prefixAdminCmd :: T.Text -> CmdName
prefixAdminCmd = prefixCmd adminCmdChar


-----


-- TODO: Help.
adminAdmin :: Action
adminAdmin (NoArgs i mq cols) = getState >>= \ms ->
    let triples = sortBy (compare `on` view _2) [ (ai, as, isTuned) | ai <- getLoggedInAdminIds ms
                                                                    , let as      = getSing ai ms
                                                                    , let ap      = getPla  ai ms
                                                                    , let isTuned = getPlaFlag IsTunedAdmin ap ]
        ([self], others) = partition (\x -> x^._1.to (== i)) triples
        styleds          = styleAbbrevs Don'tBracket . map (view _2) $ others
        others'          = zipWith (\triple styled -> triple & _2 .~ styled) others styleds
        mkDesc (_, n, isTuned) = padName n <> (isTuned ? "tuned in" :? "tuned out")
        descs                  = mkDesc self : map mkDesc others'
    in multiWrapSend mq cols descs >> logPlaExecArgs (prefixAdminCmd "admin") [] i
adminAdmin (Msg i mq cols msg) = getState >>= \ms ->
    if getPlaFlag IsTunedAdmin . getPla i $ ms
      then case getTunedAdminIds ms of
        [_]      -> sorryNoOneListening mq cols "admin"
        tunedIds ->
          let tunedSings         = map (`getSing` ms) tunedIds
              getStyled targetId = let styleds = styleAbbrevs Don'tBracket $ getSing targetId ms `delete` tunedSings
                                   in head . filter ((== s) . dropANSI) $ styleds
              s                  = getSing i ms
              format (txt, is)   = if i `elem` is
                then (formatAdminChanMsg s txt, pure i) : mkBsWithStyled (i `delete` is)
                else mkBsWithStyled is
                where
                  mkBsWithStyled is' = [ (formatAdminChanMsg (getStyled i') txt, pure i') | i' <- is' ]
          in case emotify i ms tunedIds tunedSings msg of
            Left  errorMsgs  -> multiWrapSend mq cols errorMsgs
            Right (Right bs) -> let logMsg = dropANSI . fst . head $ bs
                                in ioHelper s (concatMap format bs) logMsg
            Right (Left ()) -> case expCmdify i ms tunedIds tunedSings msg of
              Left  errorMsg     -> wrapSend mq cols errorMsg
              Right (bs, logMsg) -> ioHelper s (concatMap format bs) logMsg
      else sorryNotTuned mq cols "admin"
  where
    getTunedAdminIds ms = [ ai | ai <- getLoggedInAdminIds ms, getPlaFlag IsTunedAdmin . getPla ai $ ms ]
    ioHelper s bs logMsg = bcastNl bs >> logHelper
      where
        logHelper = do
            logPlaOut (prefixAdminCmd "admin") i . pure $ logMsg
            ts <- liftIO mkTimestamp
            withDbExHandler_ "adminAdmin" . insertDbTblAdminChan . AdminChanRec ts s $ logMsg
adminAdmin p = patternMatchFail "adminAdmin" [ showText p ]


emotify :: Id -> MudState -> Inv -> [Sing] -> T.Text -> Either [T.Text] (Either () [Broadcast])
emotify i ms tunedIds tunedSings msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | or [ (T.head . head $ ws) `elem` ("[<" :: String)
       , "]." `T.isSuffixOf` last ws
       , ">." `T.isSuffixOf` last ws ]  = Left . pure $ "Sorry, but you can't open or close your message with brackets."
  | msg == T.singleton emoteChar <> "." = Left . pure $ "He don't."
  | c == emoteChar = fmap Right . procEmote i ms tunedIds tunedSings . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right . Left $ ()


procEmote :: Id -> MudState -> Inv -> [Sing] -> Args -> Either [T.Text] [Broadcast]
procEmote _ _ _ _ as | any (`elem` yous) . map (T.dropAround (not . isLetter) . T.toLower) $ as = Left . pure $ advice
  where
    advice = T.concat [ "Sorry, but you can't use a form of the word "
                      , dblQuote "you"
                      , " in an emote. Instead, you must specify who you wish to target using "
                      , dblQuote etc
                      , ", as in "
                      , quoteColor
                      , dblQuote . T.concat $ [ cn
                                              , "slowly turns her head to look directly at "
                                              , etc
                                              , "taro" ]
                      , dfltColor
                      , "." ]
    cn  = prefixAdminCmd "admin" <> " " <> T.singleton emoteChar
    etc = T.singleton emoteTargetChar
procEmote i ms tunedIds tunedSings as =
    let s                       = getSing i ms
        xformed                 = xformArgs True as
        xformArgs _      []     = []
        xformArgs isHead (x:xs) = (: xformArgs False xs) $ if
          | x == enc            -> mkRight . dup3 $ s
          | x == enc's          -> mkRight . dup3 $ s <> "'s"
          | enc `T.isInfixOf` x -> Left . adviceEnc $ cn
          | x == etc            -> Left . adviceEtc $ cn
          | T.take 1 x == etc   -> isHead ? Left adviceEtcHead :? (procTarget . T.tail $ x)
          | etc `T.isInfixOf` x -> Left . adviceEtc $ cn
          | isHead, hasEnc      -> mkRight . dup3 . capitalizeMsg $ x
          | isHead              -> mkRight . dup3 $ s <> " " <> x
          | otherwise           -> mkRight . dup3 $ x
    in case filter isLeft xformed of
      [] -> let (toSelf, toTargets, toOthers) = unzip3 . map fromRight $ xformed
                targetIds = nub . foldr extractIds [] $ toTargets
                extractIds [ForNonTargets _           ] acc = acc
                extractIds (ForTarget     _ targetId:_) acc = targetId : acc
                extractIds (ForTargetPoss _ targetId:_) acc = targetId : acc
                extractIds xs                           _   = patternMatchFail "procEmote extractIds" [ showText xs ]
                msgMap  = foldr (\targetId -> at targetId .~ Just []) IM.empty targetIds
                msgMap' = foldr consWord msgMap toTargets
                consWord [ ForNonTargets word                           ] = IM.map (word :)
                consWord [ ForTarget     p targetId, ForNonTargets word ] = selectiveCons p targetId False word
                consWord [ ForTargetPoss p targetId, ForNonTargets word ] = selectiveCons p targetId True  word
                consWord xs = const . patternMatchFail "procEmote consWord" $ [ showText xs ]
                selectiveCons p targetId isPoss word = IM.mapWithKey helper
                  where
                    helper k v = let targetSing = getSing k ms |&| (isPoss ? (<> "'s") :? id)
                                 in (: v) $ if k == targetId
                                   then T.concat [ emoteTargetColor, targetSing, dfltColor, p ]
                                   else word
                toTargetBs = IM.foldlWithKey' helper [] msgMap'
                  where
                    helper acc k = (: acc) . (formatMsg *** pure) . (, k)
                formatMsg = bracketQuote . punctuateMsg . T.unwords
            in Right $ (formatMsg toSelf, pure i) : (formatMsg toOthers, tunedIds \\ (i : targetIds)) : toTargetBs
      advices -> Left . intersperse "" . map fromLeft . nub $ advices
  where
    cn              = prefixAdminCmd "admin" <> " " <> T.singleton emoteChar
    enc             = T.singleton emoteNameChar
    enc's           = enc <> "'s"
    etc             = T.singleton emoteTargetChar
    mkRight         = Right . mkForNonTargets
    mkForNonTargets = _2 %~ (pure . ForNonTargets)
    hasEnc          = any (`elem` [ enc, enc's ]) as
    procTarget word = let punc = "!\"),./:;?" :: String in
        case swap . (both %~ T.reverse) . T.span (`elem` punc) . T.reverse $ word of
          ("",   _) -> Left . adviceEtc $ cn
          ("'s", _) -> Left adviceEtcEmptyPoss
          (w,    p) ->
            let (isPoss, target) = ("'s" `T.isSuffixOf` w ? (True, T.dropEnd 2) :? (False, id)) & _2 %~ (w |&|)
                notFound = Left . sorryAdminName $ target
                found targetSing@(addSuffix isPoss p -> targetSing') =
                    let targetId = head . filter ((== targetSing) . (`getSing` ms)) $ tunedIds
                    in Right ( targetSing'
                             , [ mkEmoteWord isPoss p targetId, ForNonTargets targetSing' ]
                             , targetSing' )
            in findFullNameForAbbrev (capitalize target) (getSing i ms `delete` tunedSings) |&| maybe notFound found
    addSuffix   isPoss p = (<> p) . (isPoss ? (<> "'s") :? id)
    mkEmoteWord isPoss   = isPoss ? ForTargetPoss :? ForTarget


sorryAdminName :: T.Text -> T.Text
sorryAdminName n = "There is no admin by the name of " <>
                   (dblQuote . capitalize $ n)         <>
                   " currently tuned in to the admin channel."


formatAdminChanMsg :: T.Text -> T.Text -> T.Text
formatAdminChanMsg n msg = T.concat [ underlineANSI
                                    , parensQuote "Admin"
                                    , noUnderlineANSI
                                    , " "
                                    , n
                                    , ": "
                                    , msg ]


expCmdify :: Id -> MudState -> Inv -> [Sing] -> T.Text -> Either T.Text ([Broadcast], T.Text)
expCmdify i ms tunedIds tunedSings msg@(T.words -> ws@(headTail . head -> (c, rest)))
  | msg == T.singleton expCmdChar <> "." = Left "He don't."
  | c == expCmdChar = fmap format . procExpCmd i ms tunedIds tunedSings . (tail ws |&|) $ if ()# rest
    then id
    else (rest :)
  | otherwise = Right (pure (msg, tunedIds), msg)
  where
    format xs = xs & _1 %~ map (_1 %~ angleBracketQuote)
                   & _2 %~ angleBracketQuote


procExpCmd :: Id -> MudState -> Inv -> [Sing] -> Args -> Either T.Text ([Broadcast], T.Text)
procExpCmd _ _ _ _ (_:_:_:_) = Left "An expressive command sequence may not be more than 2 words long."
procExpCmd i ms tunedIds tunedSings (unmsg -> [ cn, target ]) =
    let cns = S.toList . S.map (\(ExpCmd n _) -> n) $ expCmdSet
    in findFullNameForAbbrev cn cns |&| maybe notFound found
  where
    found match =
        let [ExpCmd _ ct] = S.toList . S.filter (\(ExpCmd cn' _) -> cn' == match) $ expCmdSet
        in case ct of
          NoTarget toSelf toOthers -> if ()# target
            then Right ( (format Nothing toOthers, i `delete` tunedIds) : mkBroadcast i toSelf
                       , toSelf )
            else Left $ "The " <> dblQuote match <> " expressive command cannot be used with a target."
          HasTarget toSelf toTarget toOthers -> if ()# target
            then Left $ "The " <> dblQuote match <> " expressive command requires a single target."
            else case findTarget of
              Nothing -> Left . sorryAdminName $ target
              Just n  -> let targetId = getIdForPCSing n ms
                         in Right ( (colorizeYous . format Nothing $ toTarget, pure targetId              ) :
                                    (format (Just n) toOthers,                 tunedIds \\ [ i, targetId ]) :
                                    (mkBroadcast i . format (Just n) $ toSelf)
                                  , toSelf )
          Versatile toSelf toOthers toSelfWithTarget toTarget toOthersWithTarget -> if ()# target
            then Right ( (format Nothing toOthers, i `delete` tunedIds) : mkBroadcast i toSelf
                       , toSelf )
            else case findTarget of
              Nothing -> Left . sorryAdminName $ target
              Just n  -> let targetId = getIdForPCSing n ms
                         in Right ( (colorizeYous . format Nothing $ toTarget, pure targetId              ) :
                                    (format (Just n) toOthersWithTarget,       tunedIds \\ [ i, targetId ]) :
                                    (mkBroadcast i . format (Just n) $ toSelfWithTarget)
                                  , toSelfWithTarget )
    notFound   = Left $ "There is no expressive command by the name of " <> dblQuote cn <> "."
    findTarget = findFullNameForAbbrev (capitalize target) $ getSing i ms `delete` tunedSings
    format maybeTargetSing =
        let substitutions = [ ("%", s), ("^", heShe), ("&", hisHer), ("*", himHerself) ]
        in replace (substitutions ++ maybe [] (pure . ("@", )) maybeTargetSing)
    s                           = getSing i ms
    (heShe, hisHer, himHerself) = mkPros . getSex i $ ms
    colorizeYous                = T.unwords . map helper . T.words
      where
        helper w = let (a, b) = T.break isLetter w
                       (c, d) = T.span  isLetter b
                   in T.toLower c `elem` yous ? (a <> quoteWith' (emoteTargetColor, dfltColor) c <> d) :? w
procExpCmd _ _ _ _ as = patternMatchFail "procExpCmd" as


-----


adminAnnounce :: Action
adminAnnounce p@AdviseNoArgs = advise p [ prefixAdminCmd "announce" ] advice
  where
    advice = T.concat [ "You must provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "announce" <> " CurryMUD will be shutting down for maintenance in 30 \
                                                  \minutes"
                      , dfltColor
                      , "." ]
adminAnnounce (Msg' i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    ok mq
    massSend $ announceColor <> msg <> dfltColor
    logPla    "adminAnnounce" i $       "announced "  <> dblQuote msg
    logNotice "adminAnnounce"   $ s <> " announced, " <> dblQuote msg
adminAnnounce p = patternMatchFail "adminAnnounce" [ showText p ]


-----


adminBanHost :: Action
adminBanHost (NoArgs i mq cols) = (withDbExHandler "adminBanHost" . getDbTblRecs $ "ban_host") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [BanHostRec]) >> logPlaExecArgs (prefixAdminCmd "banhost") [] i
  Nothing -> sorryDbEx mq cols
adminBanHost p@(AdviseOneArg a) = advise p [ prefixAdminCmd "banhost" ] advice
  where
    advice = T.concat [ "Please also provide a reason, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "banhost " <> a <> " used by Taro"
                      , dfltColor
                      , "." ]
adminBanHost (MsgWithTarget i mq cols (uncapitalize -> target) msg) = getState >>= \ms ->
    (withDbExHandler "adminBanHost" . isHostBanned $ target) >>= \case
      Nothing      -> sorryDbEx mq cols
      Just (Any b) -> let newStatus = not b in liftIO mkTimestamp >>= \ts -> do
          let banHost = BanHostRec ts target newStatus msg
          withDbExHandler_ "adminBanHost" . insertDbTblBanHost $ banHost
          notifyBan i mq cols (getSing i ms) target newStatus banHost
adminBanHost p = patternMatchFail "adminBanHost" [ showText p ]


dumpDbTblHelper :: (Pretty a) => MsgQueue -> Cols -> [a] -> MudStack ()
dumpDbTblHelper mq cols [] = wrapSend mq cols "The database is empty."
dumpDbTblHelper mq cols xs = multiWrapSend mq cols . map pp $ xs


notifyBan :: (Pretty a) => Id -> MsgQueue -> Cols -> Sing -> T.Text -> Bool -> a -> MudStack ()
notifyBan i mq cols selfSing target newStatus x =
    let fn          = "notifyBan"
        (v, suffix) = newStatus ? ("banned", [ ": " <> pp x ]) :? ("unbanned", [ ": " <> pp x ])
    in do
        wrapSend mq cols   . T.concat $ [ "You have ",   v, " ",    target ] ++ suffix
        bcastOtherAdmins i . T.concat $ [ selfSing, " ", v, " ",    target ] ++ suffix
        logNotice fn       . T.concat $ [ selfSing, " ", v, " ",    target ] ++ suffix
        logPla    fn i     . T.concat $ [                v, " ",    target ] ++ suffix


-----


adminBanPlayer :: Action
adminBanPlayer (NoArgs i mq cols) = (withDbExHandler "adminBanPlayer" . getDbTblRecs $ "ban_pla") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [BanPlaRec]) >> logPlaExecArgs (prefixAdminCmd "banpla") [] i
  Nothing -> sorryDbEx mq cols
adminBanPlayer p@(AdviseOneArg a) = advise p [ prefixAdminCmd "banplayer" ] advice
  where
    advice = T.concat [ "Please also provide a reason, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "banplayer " <> a <> " for harassing hanako"
                      , dfltColor
                      , "." ]
adminBanPlayer p@(MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let fn = "adminBanPlayer"
        SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to ban"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      []      -> sendFun . T.concat $ [ "There is no PC by the name of "
                                      , dblQuote strippedTarget
                                      , ". "
                                      , parensQuote "Note that you must specify the full PC name of the player you \
                                                    \wish to ban." ]
      [banId] -> let selfSing = getSing i     ms
                     pla      = getPla  banId ms
                 in if
                   | banId == i             -> sendFun "You can't ban yourself."
                   | getPlaFlag IsAdmin pla -> sendFun "You can't ban an admin."
                   | otherwise -> (withDbExHandler "adminBanPlayer" . isPlaBanned $ strippedTarget) >>= \case
                     Nothing      -> sorryDbEx mq cols
                     Just (Any b) -> let newStatus = not b in liftIO mkTimestamp >>= \ts -> do
                         let banPla = BanPlaRec ts strippedTarget newStatus msg
                         withDbExHandler_ "adminBanPlayer" . insertDbTblBanPla $ banPla
                         notifyBan i mq cols selfSing strippedTarget newStatus banPla
                         when (newStatus && isLoggedIn pla)
                              (adminBoot p { args = strippedTarget : T.words "You have been banned from CurryMUD!" })
      xs      -> patternMatchFail fn [ showText xs ]
adminBanPlayer p = patternMatchFail "adminBanPlayer" [ showText p ]


-----


adminBoot :: Action
adminBoot p@AdviseNoArgs = advise p [ prefixAdminCmd "boot" ] "Please specify the full PC name of the player you wish \
                                                              \to boot, optionally followed by a custom message."
adminBoot (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to boot"
    in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
      []       -> sendFun . T.concat $ [ "There is no PC by the name of "
                                       , dblQuote strippedTarget
                                       , ". "
                                       , parensQuote "Note that you must specify the full PC name of the player you \
                                                     \wish to boot." ]
      [bootId] -> let selfSing = getSing i ms in if
                    | not . isLoggedIn . getPla bootId $ ms -> sendFun $ strippedTarget <> " is not logged in."
                    | bootId == i -> sendFun "You can't boot yourself."
                    | bootMq <- getMsgQueue bootId ms, f <- ()# msg ? dfltMsg :? customMsg -> do
                        wrapSend mq cols $ "You have booted " <> strippedTarget <> "."
                        sendMsgBoot bootMq =<< f bootId strippedTarget selfSing
                        bcastAdminsExcept [ i, bootId ] . T.concat $ [ selfSing, " booted ", strippedTarget, "." ]
      xs       -> patternMatchFail "adminBoot" [ showText xs ]
  where
    dfltMsg   bootId target' s = emptied $ do
        logPla "adminBoot dfltMsg"   i      $ T.concat [ "booted ", target', " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   bootId $ T.concat [ "booted by ", s,    " ", parensQuote "no message given", "." ]
    customMsg bootId target' s = do
        logPla "adminBoot customMsg" i      $ T.concat [ "booted ", target', "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" bootId $ T.concat [ "booted by ", s,    "; message: ", dblQuote msg ]
        unadulterated msg
adminBoot p = patternMatchFail "adminBoot" [ showText p ]


-----


adminBug :: Action
adminBug (NoArgs i mq cols) = (withDbExHandler "adminBug" . getDbTblRecs $ "bug") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [BugRec]) >> logPlaExec (prefixAdminCmd "bug") i
  Nothing -> sorryDbEx mq cols
adminBug p = withoutArgs adminBug p


-----


adminDate :: Action
adminDate (NoArgs' i mq) = do
    send mq . nlnl . T.pack . formatTime defaultTimeLocale "%A %B %d" =<< liftIO getZonedTime
    logPlaExec (prefixAdminCmd "date") i
adminDate p = withoutArgs adminDate p


-----


adminDispCmdList :: Action
adminDispCmdList p@(LowerNub' i as) = dispCmdList adminCmds p >> logPlaExecArgs (prefixAdminCmd "?") as i
adminDispCmdList p                  = patternMatchFail "adminDispCmdList" [ showText p ]


-----


adminHost :: Action
adminHost p@AdviseNoArgs = advise p [ prefixAdminCmd "host" ] "Please specify the PC name(s) of one or more players \
                                                              \whose host statistics you would like to see."
adminHost (LowerNub i mq cols as) = do
    ms          <- getState
    (now, zone) <- (,) <$> liftIO getCurrentTime <*> liftIO getCurrentTimeZone
    let (f, guessWhat) | any hasLocPref as = (stripLocPref, sorryMsg)
                       | otherwise         = (id,           ""      )
        g = ()# guessWhat ? id :? (guessWhat :)
        helper target =
            let notFound = [ "There is no PC by the name of " <> dblQuote target <> "." ]
                found    = uncurry (mkHostReport ms now zone)
            in findFullNameForAbbrev target (mkAdminPlaIdSingList ms) |&| maybe notFound found
    multiWrapSend mq cols . g . intercalate [""] . map (helper . capitalize . f) $ as
    logPlaExec (prefixAdminCmd "host") i
  where
    sorryMsg = sorryIgnoreLocPrefPlur "The PC names of the players whose host statistics you would like to see"
adminHost p = patternMatchFail "adminHost" [ showText p ]


mkHostReport :: MudState -> UTCTime -> TimeZone -> Id -> Sing -> [T.Text]
mkHostReport ms now zone i s = (header ++) $ case getHostMap s ms of
  Nothing      -> [ "There are no host records for " <> s <> "." ]
  Just hostMap | dur       <- ili |?| duration
               , total     <- M.foldl (\acc -> views secsConnected (+ acc)) 0 hostMap + getSum dur
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


adminIncognito :: Action
adminIncognito (NoArgs i mq cols) = modifyState helper >>= sequence_
  where
    helper ms = let s           = getSing i ms
                    isIncognito = getPlaFlag IsIncognito . getPla i $ ms
                    fs | isIncognito = [ wrapSend mq cols "You are no longer incognito."
                                       , bcastOtherAdmins i $ s <> " is no longer incognito."
                                       , logPla "adminIncognito helper fs" i "no longer incognito." ]
                       | otherwise   = [ wrapSend mq cols "You have gone incognito."
                                       , bcastOtherAdmins i $ s <> " has gone incognito."
                                       , logPla "adminIncognito helper fs" i "went incognito." ]
                in (ms & plaTbl.ind i %~ setPlaFlag IsIncognito (not isIncognito), fs)
adminIncognito p = withoutArgs adminIncognito p


-----


adminIp :: Action
adminIp (NoArgs i mq cols) = do
    ifList <- liftIO mkInterfaceList
    multiWrapSend mq cols [ "Interfaces: " <> ifList <> ".", "Listening on port " <> showText port <> "." ]
    logPlaExec (prefixAdminCmd "ip") i
adminIp p = withoutArgs adminIp p


-----


adminMsg :: Action
adminMsg p@AdviseNoArgs = advise p [ prefixAdminCmd "message" ] advice
  where
    advice = T.concat [ "Please specify the PC name of a regular player followed by a message, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "message" <> " taro thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminMsg p@(AdviseOneArg a) = advise p [ prefixAdminCmd "message" ] advice
  where
    advice = T.concat [ "Please also provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "message " <> a <> " thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminMsg (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
    logMsgs |#| let f = uncurry (logPla "adminMsg") in mapM_ f
  where
    helper ms =
        let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the message recipient"
            s                   = getSing i ms
            targetMsg           = mkRetainedMsgFromPerson s msg
            notFound            = emptied . sendFun $ "There is no regular player by the name of " <>
                                                      dblQuote strippedTarget                      <>
                                                      "."
            found (targetId, targetSing) = let targetPla = getPla targetId ms in if
              | isLoggedIn targetPla, getPlaFlag IsIncognito . getPla i $ ms ->
                emptied . sendFun $ "You can't send a message to a player who is logged in while you are incognito."
              | isLoggedIn targetPla ->
                let sentLogMsg     = (i,        T.concat [ "sent message to "
                                                         , targetSing
                                                         , ": "
                                                         , dblQuote msg ])
                    receivedLogMsg = (targetId, T.concat [ "received message from "
                                                         , s
                                                         , ": "
                                                         , dblQuote msg ])
                in do
                    sendFun . T.concat $ [ "You send ", targetSing, ": ", dblQuote msg ]
                    let (targetMq, targetCols) = getMsgQueueColumns targetId ms
                        f                      = multiWrapSend targetMq targetCols . fstList T.tail
                    (f =<<) $ if getPlaFlag IsNotFirstAdminMsg targetPla
                      then unadulterated targetMsg
                      else [ targetMsg : hints | hints <- firstAdminMsg targetId s ]
                    return [ sentLogMsg, receivedLogMsg ]
              | otherwise -> do
                  multiWrapSend mq cols . consSorry $ [ T.concat [ "You send ", targetSing, ": ", dblQuote msg ]
                                                      , parensQuote "Message retained." ]
                  retainedMsg targetId ms targetMsg
                  let sentLogMsg     = ( i
                                       , T.concat [ "sent message to ", targetSing, ": ", dblQuote msg ] )
                      receivedLogMsg = ( targetId
                                       , T.concat [ "received message from ", s,    ": ", dblQuote msg ] )
                  return [ sentLogMsg, receivedLogMsg ]
        in (findFullNameForAbbrev strippedTarget . mkPlaIdSingList $ ms) |&| maybe notFound found
adminMsg p = patternMatchFail "adminMsg" [ showText p ]


firstAdminMsg :: Id -> Sing -> MudStack [T.Text]
firstAdminMsg i adminSing = modifyState $ (, msg) . (plaTbl.ind i %~ setPlaFlag IsNotFirstAdminMsg True)
  where
    msg = [ "", T.concat [ hintANSI
                         , "Hint:"
                         , noHintANSI
                         , " the above is a message from "
                         , adminSing
                         , ", a CurryMUD administrator. To reply, type "
                         , quoteColor
                         , dblQuote $ "admin " <> uncapitalize adminSing <> " msg"
                         , dfltColor
                         , ", where "
                         , quoteColor
                         , dblQuote "msg"
                         , dfltColor
                         , " is the message you want to send to "
                         , adminSing
                         , "." ] ]


-----


adminPeep :: Action
adminPeep p@AdviseNoArgs = advise p [ prefixAdminCmd "peep" ] "Please specify the PC name(s) of one or more players \
                                                              \you wish to start or stop peeping."
adminPeep (LowerNub i mq cols as) = do
    (msgs, unzip -> (logMsgsSelf, logMsgsOthers)) <- modifyState helper
    multiWrapSend mq cols msgs
    logPla "adminPeep" i . (<> ".") . slashes $ logMsgsSelf
    forM_ logMsgsOthers $ uncurry (logPla "adminPeep")
  where
    helper ms =
        let s     = getSing i ms
            apiss = [ apis | apis@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
            (f, guessWhat) | any hasLocPref as = (stripLocPref, sorryMsg)
                           | otherwise         = (id,           ""      )
            g = ()# guessWhat ? id :? (guessWhat :)
            peep target a@(pt, _, _) =
                let notFound = a & _2 %~ ("No PC by the name of " <> dblQuote target <> " is currently logged in." :)
                    found (peepId@(flip getPla ms -> peepPla), peepSing) = if peepId `notElem` pt^.ind i.peeping
                      then if
                        | peepId == i                -> a & _2 %~ ("You can't peep yourself." :)
                        | getPlaFlag IsAdmin peepPla -> a & _2 %~ ("You can't peep an admin." :)
                        | otherwise                  ->
                          let pt'     = pt & ind i     .peeping %~ (peepId :)
                                           & ind peepId.peepers %~ (i      :)
                              msg     = "You are now peeping " <> peepSing <> "."
                              logMsgs = [("started peeping " <> peepSing, (peepId, s <> " started peeping."))]
                          in a & _1 .~ pt' & _2 %~ (msg :) & _3 <>~ logMsgs
                      else let pt'     = pt & ind i     .peeping %~ (peepId `delete`)
                                            & ind peepId.peepers %~ (i      `delete`)
                               msg     = "You are no longer peeping " <> peepSing <> "."
                               logMsgs = [("stopped peeping " <> peepSing, (peepId, s <> " stopped peeping."))]
                           in a & _1 .~ pt' & _2 %~ (msg :) & _3 <>~ logMsgs
                in findFullNameForAbbrev target apiss |&| maybe notFound found
            res = foldr (peep . capitalize . f) (ms^.plaTbl, [], []) as
        in (ms & plaTbl .~ res^._1, (res^._2.to g, res^._3))
    sorryMsg = sorryIgnoreLocPrefPlur "The PC names of the players you wish to start or stop peeping"
adminPeep p = patternMatchFail "adminPeep" [ showText p ]


-----


adminPersist :: Action
adminPersist (NoArgs' i mq) = persist >> ok mq >> logPlaExec (prefixAdminCmd "persist") i
adminPersist p              = withoutArgs adminPersist p


-----


adminPrint :: Action
adminPrint p@AdviseNoArgs = advise p [ prefixAdminCmd "print" ] advice
  where
    advice = T.concat [ "You must provide a message to print to the server console, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "print" <> " is anybody home?"
                      , dfltColor
                      , "." ]
adminPrint (Msg' i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    liftIO . T.putStrLn . T.concat $ [ bracketQuote s, " ", printConsoleColor, msg, dfltColor ]
    ok mq
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
adminPrint p = patternMatchFail "adminPrint" [ showText p ]


-----


adminProfanity :: Action
adminProfanity (NoArgs i mq cols) = (withDbExHandler "adminProfanity" . getDbTblRecs $ "profanity") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [ProfRec]) >> logPlaExec (prefixAdminCmd "profanity") i
  Nothing -> sorryDbEx mq cols
adminProfanity p = withoutArgs adminProfanity p


-----


adminShutdown :: Action
adminShutdown (NoArgs' i mq    ) = shutdownHelper i mq Nothing
adminShutdown (Msg'    i mq msg) = shutdownHelper i mq . Just $ msg
adminShutdown p                  = patternMatchFail "adminShutdown" [ showText p ]


shutdownHelper :: Id -> MsgQueue -> Maybe T.Text -> MudStack ()
shutdownHelper i mq maybeMsg = getState >>= \ms ->
    let s    = getSing i ms
        rest = maybeMsg |&| maybe (" " <> parensQuote "no message given" <> ".") (("; message: " <>) . dblQuote)
    in do
        massSend $ shutdownMsgColor <> fromMaybe dfltShutdownMsg maybeMsg <> dfltColor
        logPla     "shutdownHelper" i $ "initiating shutdown" <> rest
        massLogPla "shutdownHelper"   $ "closing connection due to server shutdown initiated by " <> s <> rest
        logNotice  "shutdownHelper"   $ "server shutdown initiated by "                           <> s <> rest
        liftIO . atomically . writeTQueue mq $ Shutdown


-----


adminSudoer :: Action
adminSudoer p@AdviseNoArgs = advise p [ prefixAdminCmd "sudoer" ] "Please specify the full PC name of the player you \
                                                                  \wish to promote/demote."
adminSudoer (OneArgNubbed i mq cols target) = modifyState helper >>= sequence_
  where
    helper ms =
      let fn                  = "adminSudoer helper"
          SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player you wish to promote/demote"
      in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == strippedTarget ] of
        [] -> (ms, [ let msg = T.concat [ "There is no PC by the name of "
                                        , dblQuote strippedTarget
                                        , ". "
                                        , parensQuote "Note that you must specify the full PC name of the player you  \
                                                      \wish to promote/demote." ]
                             in sendFun msg ])
        [targetId]
          | selfSing       <- getSing i ms
          , targetSing     <- getSing targetId ms
          , isAdmin        <- getPlaFlag IsAdmin . getPla targetId $ ms
          , (verb, toFrom) <- isAdmin ? ("demoted", "from") :? ("promoted", "to")
          , handleIncog    <- let act = adminIncognito . mkActionParams targetId ms $ []
                              in when (getPlaFlag IsIncognito . getPla targetId $ ms) act
          , handlePeep     <- let peepingIds = getPeeping targetId ms
                                  act        = adminPeep . mkActionParams targetId ms . map (`getSing` ms) $ peepingIds
                              in unless (()# peepingIds) act
          , fs <- [ retainedMsg targetId ms           . T.concat $ [ promoteDemoteColor
                                                                   , selfSing
                                                                   , " has "
                                                                   , verb
                                                                   , " you "
                                                                   , toFrom
                                                                   , " admin status."
                                                                   , dfltColor ]
                  , sendFun                           . T.concat $ [ "You have ",       verb, " ", targetSing, "." ]
                  , bcastAdminsExcept [ i, targetId ] . T.concat $ [ selfSing, " has ", verb, " ", targetSing, "." ]
                  , logNotice fn                      . T.concat $ [ selfSing, " ",     verb, " ", targetSing, "." ]
                  , logPla    fn i                    . T.concat $ [ verb, " ",    targetSing, "." ]
                  , logPla    fn targetId             . T.concat $ [ verb, " by ", selfSing,   "." ]
                  , handleIncog
                  , handlePeep ]
          -> if | targetId   == i      -> (ms, [ sendFun "You can't demote yourself." ])
                | targetSing == "Root" -> (ms, [ sendFun "You can't demote Root."     ])
                | otherwise            -> (ms & plaTbl.ind targetId %~ setPlaFlag IsAdmin      (not isAdmin)
                                              & plaTbl.ind targetId %~ setPlaFlag IsTunedAdmin (not isAdmin), fs)
        xs -> patternMatchFail "adminSudoer helper" [ showText xs ]
adminSudoer (ActionParams { plaMsgQueue, plaCols }) =
    wrapSend plaMsgQueue plaCols "Sorry, but you can only promote/demote one player at a time."


-----


adminTelePla :: Action
adminTelePla p@AdviseNoArgs = advise p [ prefixAdminCmd "telepla" ] "Please specify the PC name of the player to which \
                                                                    \you want to teleport."
adminTelePla p@(OneArgNubbed i mq cols target) = modifyState helper >>= sequence_
  where
    helper ms =
        let SingleTarget { .. } = mkSingleTarget mq cols target "The PC name of the player to which you want to \
                                                                \teleport"
            idSings             = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
            originId            = getRmId i ms
            found (flip getRmId ms -> destId, targetSing)
              | targetSing == getSing i ms = (ms, [ sendFun "You can't teleport to yourself." ])
              | destId     == originId     = (ms, [ sendFun "You're already there!"           ])
              | otherwise = teleHelper i ms p { args = [] } originId destId targetSing consSorryBroadcast
            notFound     = (ms, pure sorryInvalid)
            sorryInvalid = sendFun $ "No PC by the name of " <> dblQuote strippedTarget <> " is currently logged in."
        in findFullNameForAbbrev strippedTarget idSings |&| maybe notFound found
adminTelePla (ActionParams { plaMsgQueue, plaCols }) = wrapSend plaMsgQueue plaCols "Please specify a single PC name."


teleHelper :: Id
           -> MudState
           -> ActionParams
           -> Id
           -> Id
           -> T.Text
           -> (Id -> [Broadcast] -> [Broadcast])
           -> (MudState, [MudStack ()])
teleHelper i ms p originId destId name f =
    let originDesig = mkStdDesig i ms Don'tCap
        originPCIds = i `delete` pcIds originDesig
        s           = fromJust . stdPCEntSing $ originDesig
        destDesig   = mkSerializedNonStdDesig i ms s A Don'tCap
        destPCIds   = findPCIds ms $ ms^.invTbl.ind destId
        ms'         = ms & pcTbl .ind i.rmId   .~ destId
                         & invTbl.ind originId %~ (i `delete`)
                         & invTbl.ind destId   %~ (sortInv ms . (++ pure i))
        msgAtOrigin = nlnl $ "There is a soft audible pop as " <> serialize originDesig <> " vanishes in a jarring \
                             \flash of white light."
        msgAtDest   = nlnl $ "There is a soft audible pop as " <> destDesig             <> " appears in a \
                             \jarring flash of white light."
        desc        = nlnl   "You are instantly transported in a blinding flash of white light. For a brief moment you \
                             \are overwhelmed with vertigo accompanied by a confusing sensation of nostalgia."
    in (ms', [ bcastIfNotIncog i . f i $ [ (desc, pure i), (msgAtOrigin, originPCIds), (msgAtDest, destPCIds) ]
             , look p
             , rndmDos [ (calcProbTeleVomit   i ms, mkExpAction "vomit"   p)
                       , (calcProbTeleShudder i ms, mkExpAction "shudder" p) ]
             , logPla "telehelper" i $ "teleported to " <> dblQuote name <> "." ])


-----


adminTeleRm :: Action
adminTeleRm (NoArgs i mq cols) = (multiWrapSend mq cols =<< mkTxt) >> logPlaExecArgs (prefixAdminCmd "telerm") [] i
  where
    mkTxt  = views rmTeleNameTbl ((header :) . styleAbbrevs Don'tBracket . IM.elems) <$> getState
    header = "You may teleport to the following rooms:"
adminTeleRm p@(OneArg i mq cols target) = modifyState helper >>= sequence_
  where
    helper ms =
        let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the room to which you want to teleport"
            originId            = getRmId i ms
            found (destId, rmTeleName)
              | destId == originId = (ms, [ sendFun "You're already there!" ])
              | otherwise          = teleHelper i ms p { args = [] } originId destId rmTeleName consSorryBroadcast
            notFound     = (ms, pure sorryInvalid)
            sorryInvalid = sendFun . T.concat $ [ dblQuote strippedTarget'
                                                , " is not a valid room name. Type "
                                                , quoteColor
                                                , dblQuote . prefixAdminCmd $ "telerm"
                                                , dfltColor
                                                , " with no arguments to get a list of valid room names." ]
        in (findFullNameForAbbrev strippedTarget' . views rmTeleNameTbl IM.toList $ ms) |&| maybe notFound found
adminTeleRm p = advise p [] advice
  where
    advice = T.concat [ "Please provide one argument: the name of the room to which you'd like to teleport, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "telerm" <> " lounge"
                      , dfltColor
                      , "." ]


-----


adminTime :: Action
adminTime (NoArgs i mq cols) = do
    (ct, zt) <- liftIO $ (,) <$> formatThat `fmap` getCurrentTime <*> formatThat `fmap` getZonedTime
    multiWrapSend mq cols [ "At the tone, the time will be...", ct, zt ]
    logPlaExec (prefixAdminCmd "time") i
  where
    formatThat (T.words . showText -> wordy@(headLast -> (date, zone))) =
        let time = T.init . T.dropWhileEnd (/= '.') . head . tail $ wordy in T.concat [ zone, ": ", date, " ", time ]
adminTime p = withoutArgs adminTime p


-----


adminTypo :: Action
adminTypo (NoArgs i mq cols) = (withDbExHandler "adminTypo" . getDbTblRecs $ "typo") >>= \case
  Just xs -> dumpDbTblHelper mq cols (xs :: [TypoRec]) >> logPlaExec (prefixAdminCmd "typo") i
  Nothing -> sorryDbEx mq cols
adminTypo p = withoutArgs adminTypo p


-----


adminUptime :: Action
adminUptime (NoArgs i mq cols) = do
    send mq . nl =<< liftIO uptime |&| try >=> eitherRet ((sendGenericErrorMsg mq cols >>) . logIOEx "adminUptime")
    logPlaExec (prefixAdminCmd "uptime") i
  where
    uptime = T.pack <$> readProcess "uptime" [] ""
adminUptime p = withoutArgs adminUptime p


-----


adminWhoIn :: Action
adminWhoIn = whoHelper LoggedIn "whoin"


whoHelper :: LoggedInOrOut -> T.Text -> Action
whoHelper inOrOut cn (NoArgs i mq cols) = do
    pager i mq =<< [ concatMap (wrapIndent 20 cols) charListTxt | charListTxt <- mkCharListTxt inOrOut <$> getState ]
    logPlaExecArgs (prefixAdminCmd cn) [] i
  where
whoHelper inOrOut cn p@(ActionParams { plaId, args }) =
    (dispMatches p 20 =<< mkCharListTxt inOrOut <$> getState) >> logPlaExecArgs (prefixAdminCmd cn) args plaId


mkCharListTxt :: LoggedInOrOut -> MudState -> [T.Text]
mkCharListTxt inOrOut ms = let is               = IM.keys . IM.filter predicate $ ms^.plaTbl
                               (is', ss)        = unzip [ (i, s) | i <- is, let s = getSing i ms, then sortWith by s ]
                               ias              = zip is' . styleAbbrevs Don'tBracket $ ss
                               mkCharTxt (i, a) = let (s, r, l) = mkPrettifiedSexRaceLvl i ms
                                                      name      = mkAnnotatedName i a
                                                  in T.concat [ padName name
                                                              , padSex  s
                                                              , padRace r
                                                              , l ]
                               nop              = length is
                           in mkWhoHeader ++ map mkCharTxt ias ++ [ T.concat [ showText nop
                                                                             , " "
                                                                             , pluralize ("person", "people") nop
                                                                             , " "
                                                                             , showText inOrOut
                                                                             , "." ] ]
  where
    predicate           = case inOrOut of LoggedIn  -> isLoggedIn
                                          LoggedOut -> not . isLoggedIn
    mkAnnotatedName i a = let p     = getPla i ms
                              admin = getPlaFlag IsAdmin     p |?| asterisk
                              incog = getPlaFlag IsIncognito p |?| (asteriskColor <> "@" <> dfltColor)
                          in a <> admin <> incog


-----


adminWhoOut :: Action
adminWhoOut = whoHelper LoggedOut "whoout"
