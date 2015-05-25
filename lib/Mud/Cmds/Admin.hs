{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, TransformListComp, TupleSections, ViewPatterns #-}

module Mud.Cmds.Admin (adminCmds) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Pla
import Mud.Cmds.Util.Abbrev
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
import Mud.Misc.ANSI
import Mud.Misc.Persist
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Msgs
import Mud.Util.List
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logIOEx, logNotice, logPla, logPlaExec, logPlaExecArgs, massLogPla)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Lens (_1, _2, _3, views)
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (^.))
import Control.Monad ((>=>), forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, getZonedTime)
import Data.Time.Format (formatTime)
import GHC.Exts (sortWith)
import Prelude hiding (pi)
import System.Directory (doesFileExist)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import qualified Data.IntMap.Lazy as IM (elems, filter, keys, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, readFile)


default (Int)


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


massLogPla :: T.Text -> T.Text -> MudStack ()
massLogPla = L.massLogPla "Mud.Cmds.Admin"


-- ==================================================


adminCmds :: [Cmd]
adminCmds =
    [ mkAdminCmd "?"         adminDispCmdList "Display or search this command list."
    , mkAdminCmd "admin"     adminAdmin       "Toggle a player's admin status."
    , mkAdminCmd "announce"  adminAnnounce    "Send a message to all players."
    , mkAdminCmd "boot"      adminBoot        "Boot a player, optionally with a custom message."
    , mkAdminCmd "bug"       adminBug         "Dump the bug log."
    , mkAdminCmd "date"      adminDate        "Display the current system date."
    , mkAdminCmd "incognito" adminIncognito   "Toggle your incognito status."
    , mkAdminCmd "peep"      adminPeep        "Start or stop peeping one or more players."
    , mkAdminCmd "persist"   adminPersist     "Persist the world (save the current world state to disk)."
    , mkAdminCmd "print"     adminPrint       "Print a message to the server console."
    , mkAdminCmd "profanity" adminProfanity   "Dump the profanity log."
    , mkAdminCmd "retained"  adminRetained    "Send a retained message to a player."
    , mkAdminCmd "shutdown"  adminShutdown    "Shut down CurryMUD, optionally with a custom message."
    , mkAdminCmd "telepla"   adminTelePla     "Teleport to a given player."
    , mkAdminCmd "telerm"    adminTeleRm      "Display a list of rooms to which you may teleport, or teleport to a \
                                              \given room."
    , mkAdminCmd "tell"      adminTell        "Send a message to a player."
    , mkAdminCmd "time"      adminTime        "Display the current system time."
    , mkAdminCmd "typo"      adminTypo        "Dump the typo log."
    , mkAdminCmd "uptime"    adminUptime      "Display the system uptime."
    , mkAdminCmd "whoin"     adminWhoIn       "Display or search a list of the characters who are currently logged in."
    , mkAdminCmd "whoout"    adminWhoOut      "Display or search a list of the characters who are currently logged \
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


adminAdmin :: Action
adminAdmin p@AdviseNoArgs = advise p [ prefixAdminCmd "admin" ] "Please specify the full PC name of the player you \
                                                                \wish to promote/demote."
adminAdmin (OneArgNubbed i mq cols (capitalize -> target)) = modifyState helper >>= sequence_
  where
    helper ms = let fn = "adminAdmin helper" in case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == target ] of
      []         -> (ms, [ wrapSend mq cols $ "There is no PC by the name of " <> dblQuote target <> ". (Note that you \
                                              \must specify the full PC name of the player you wish to \
                                              \promote/demote.)" ])
      [targetId] -> let selfSing       = getSing i ms
                        targetSing     = getSing targetId ms
                        isAdmin        = getPlaFlag IsAdmin . getPla targetId $ ms
                        mkRetained msg = retainedMsg targetId ms $ T.concat [ promoteDemoteColor
                                                                            , selfSing
                                                                            , msg
                                                                            , dfltColor ]
                        fs = if isAdmin
                          then [ mkRetained " has demoted you from admin status."
                               , wrapSend  mq cols     $ "You have demoted "      <> targetSing <> "."
                               , logPla    fn i        $ "demoted "               <> targetSing <> "."
                               , logPla    fn targetId $ "demoted by "            <> selfSing   <> "."
                               , logNotice fn          $ selfSing <> " demoted "  <> targetSing <> "." ]
                          else [ mkRetained " has promoted you to admin status."
                               , wrapSend  mq cols     $ "You have promoted "     <> targetSing <> "."
                               , logPla    fn i        $ "promoted "              <> targetSing <> "."
                               , logPla    fn targetId $ "promoted by "           <> selfSing   <> "."
                               , logNotice fn          $ selfSing <> " promoted " <> targetSing <> "." ]
                    in if
                      | targetId == i        -> (ms, [ wrapSend mq cols "You can't demote yourself." ])
                      | targetSing == "Root" -> (ms, [ wrapSend mq cols "You can't demote Root."     ])
                      | otherwise            -> (ms & plaTbl.ind targetId %~ setPlaFlag IsAdmin (not isAdmin), fs)
      xs         -> patternMatchFail "adminAdmin helper" [ showText xs ]
adminAdmin (ActionParams { plaMsgQueue, plaCols }) =
    wrapSend plaMsgQueue plaCols "Sorry, but you can only promote/demote one player at a time."


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
adminAnnounce (Msg i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    ok mq
    massSend $ announceColor <> msg <> dfltColor
    logPla    "adminAnnounce" i $       "announced "  <> dblQuote msg
    logNotice "adminAnnounce"   $ s <> " announced, " <> dblQuote msg
adminAnnounce p = patternMatchFail "adminAnnounce" [ showText p ]


-----


adminBoot :: Action
adminBoot p@AdviseNoArgs = advise p [ prefixAdminCmd "boot" ] "Please specify the full PC name of the player you wish \
                                                              \to boot, followed optionally by a custom message."
adminBoot (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    case [ pi | pi <- views pcTbl IM.keys ms, getSing pi ms == target ] of
      []       -> wrapSend mq cols $ "There is no PC by the name of " <> dblQuote target <> ". (Note that you must \
                                     \specify the full PC name of the player you wish to boot.)"
      [bootId] -> let selfSing = getSing i ms in if
                    | not . isLoggedIn . getPla bootId $ ms -> wrapSend mq cols $ target <> " is not logged in."
                    | bootId == i -> wrapSend mq cols "You can't boot yourself."
                    | otherwise   -> let bootMq = getMsgQueue bootId ms
                                         f      = T.null msg ? dfltMsg :? customMsg
                                     in ok mq >> (sendMsgBoot bootMq =<< f bootId selfSing)
      xs       -> patternMatchFail "adminBoot" [ showText xs ]
  where
    dfltMsg   bootId s = emptied $ do
        logPla "adminBoot dfltMsg"   i      $ T.concat [ "booted ", target, " ", parensQuote "no message given", "." ]
        logPla "adminBoot dfltMsg"   bootId $ T.concat [ "booted by ", s,   " ", parensQuote "no message given", "." ]
    customMsg bootId s = do
        logPla "adminBoot customMsg" i      $ T.concat [ "booted ", target, "; message: ", dblQuote msg ]
        logPla "adminBoot customMsg" bootId $ T.concat [ "booted by ", s,   "; message: ", dblQuote msg ]
        return . Just $ msg
adminBoot p = patternMatchFail "adminBoot" [ showText p ]


-----


adminBug :: Action
adminBug (NoArgs i mq cols) =
    dumpLog mq cols bugLogFile ("bug", "bugs") >> logPlaExec (prefixAdminCmd "bug") i
adminBug p = withoutArgs adminBug p


dumpLog :: MsgQueue -> Cols -> FilePath -> BothGramNos -> MudStack ()
dumpLog mq cols logFile (s, p) = send mq =<< helper
  where
    helper  = liftIO readLog |$| try >=> eitherRet handler
    readLog = mIf (doesFileExist logFile)
                  (return . multiWrapNl   cols . T.lines =<< T.readFile logFile)
                  (return . wrapUnlinesNl cols $ "No " <> p <> " have been logged.")
    handler e = do
        fileIOExHandler "dumpLog" e
        return . wrapUnlinesNl cols $ "Unfortunately, the " <> s <> " log could not be retrieved."


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


adminIncognito :: Action
adminIncognito (NoArgs i mq cols) = modifyState helper >>= sequence_
  where
    helper ms = let isIncognito = getPlaFlag IsIncognito . getPla i $ ms
                    fs          = if isIncognito
                      then [ wrapSend mq cols "You are no longer incognito."
                           , logPla "adminIncognito helper" i "no longer incognito." ]
                      else [ wrapSend mq cols "You have gone incognito."
                           , logPla "adminIncognito helper" i "went incognito." ]
                in (ms & plaTbl.ind i %~ setPlaFlag IsIncognito (not isIncognito), fs)
adminIncognito p = withoutArgs adminIncognito p


-----


adminPeep :: Action
adminPeep p@AdviseNoArgs = advise p [ prefixAdminCmd "peep" ] "Please specify one or more PC names of the player(s) \
                                                              \you wish to start or stop peeping."
adminPeep (LowerNub i mq cols (map capitalize -> as)) = do
    (msgs, unzip -> (logMsgsSelf, logMsgsOthers)) <- modifyState helper
    multiWrapSend mq cols msgs
    logPla "adminPeep" i . (<> ".") . slashes $ logMsgsSelf
    forM_ logMsgsOthers $ uncurry (logPla "adminPeep")
  where
    helper ms =
        let s     = getSing i ms
            apiss = [ apis | apis@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
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
                in maybe notFound found . findFullNameForAbbrev target $ apiss
            res = foldr peep (ms^.plaTbl, [], []) as
        in (ms & plaTbl .~ res^._1, (res^._2, res^._3))
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
                      , dblQuote $ prefixAdminCmd "print" <> " Is anybody home?"
                      , dfltColor
                      , "." ]
adminPrint (Msg i mq msg) = getState >>= \ms -> let s = getSing i ms in do
    liftIO . T.putStrLn . T.concat $ [ bracketQuote s, " ", printConsoleColor, msg, dfltColor ]
    ok mq
    logPla    "adminPrint" i $       "printed "  <> dblQuote msg
    logNotice "adminPrint"   $ s <> " printed, " <> dblQuote msg
adminPrint p = patternMatchFail "adminPrint" [ showText p ]


-----


adminProfanity :: Action
adminProfanity (NoArgs i mq cols) =
    dumpLog mq cols profanityLogFile ("profanity", "profanities") >> logPlaExec (prefixAdminCmd "profanity") i
adminProfanity p = withoutArgs adminProfanity p


-----


adminRetained :: Action
adminRetained p@AdviseNoArgs = advise p [ prefixAdminCmd "retained" ] advice
  where
    advice = T.concat [ "Please specify the PC name of a player followed by a message, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "retained" <> " taro thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminRetained p@(AdviseOneArg a) = advise p [ prefixAdminCmd "retained" ] advice
  where
    advice = T.concat [ "Please also provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "retained " <> a <> " thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminRetained (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
    unless (null logMsgs) . forM_ logMsgs . uncurry $ logPla (prefixAdminCmd "retained")
  where
    helper ms =
        let s         = getSing i ms
            targetMsg = T.concat [ T.singleton retainedFromAdminMarker
                                 , bracketQuote s
                                 , " "
                                 , adminTellColor
                                 , msg
                                 , dfltColor ]
            notFound  = emptied . wrapSend mq cols $ "There is no PC by the name of " <> dblQuote target <> "."
            found (targetId, targetSing) = let targetPla = getPla targetId ms in if
              | targetId == i -> emptied . wrapSend mq cols $ "You talk to yourself."
              | isLoggedIn targetPla, getPlaFlag IsIncognito . getPla i $ ms, not . getPlaFlag IsAdmin $ targetPla ->
                emptied . wrapSend mq cols $ "When the recipient of your message is logged in and you are incognito, \
                                             \the recipient must be an administrator."
              | isLoggedIn targetPla ->
                let sentLogMsg     = (i,        T.concat [ "sent message to ", targetSing, ": ", dblQuote msg ])
                    receivedLogMsg = (targetId, T.concat [ "received message from ", s,    ": ", dblQuote msg ])
                in do
                    wrapSend mq cols . T.concat $ [ "You send ", targetSing, ": ", dblQuote msg ]
                    (retainedMsg targetId ms =<<) $ if getPlaFlag IsNotFirstAdminTell targetPla
                      then return targetMsg
                      else [ T.intercalate "\n" $ targetMsg : hints | hints <- firstAdminTell targetId s ]
                    return [ sentLogMsg, receivedLogMsg ]
              | otherwise -> do
                  multiWrapSend mq cols [ T.concat [ "You send ", targetSing, ": ", dblQuote msg ]
                                        , parensQuote "Message retained." ]
                  retainedMsg targetId ms targetMsg
                  let sentLogMsg     = ( i
                                       , T.concat [ "sent retained message to ", targetSing, ": ", dblQuote msg ] )
                      receivedLogMsg = ( targetId
                                       , T.concat [ "received retained message from ", s,    ": ", dblQuote msg ] )
                  return [ sentLogMsg, receivedLogMsg ]
        in maybe notFound found . findFullNameForAbbrev target . mkAdminPlaIdSingList $ ms
adminRetained p = patternMatchFail "adminRetained" [ showText p ]


-----


adminShutdown :: Action
adminShutdown (NoArgs' i mq) = shutdownHelper i mq Nothing
adminShutdown (Msg i mq msg) = shutdownHelper i mq . Just $ msg
adminShutdown p              = patternMatchFail "adminShutdown" [ showText p ]


shutdownHelper :: Id -> MsgQueue -> Maybe T.Text -> MudStack ()
shutdownHelper i mq maybeMsg = getState >>= \ms ->
    let s    = getSing i ms
        rest = maybe (" " <> parensQuote "no message given" <> ".") (("; message: " <>) . dblQuote) maybeMsg
    in do
        massSend $ shutdownMsgColor <> fromMaybe dfltShutdownMsg maybeMsg <> dfltColor
        logPla     "adminShutdown" i $ "initiating shutdown" <> rest
        massLogPla "adminShutdown"   $ "closing connection due to server shutdown initiated by " <> s <> rest
        logNotice  "adminShutdown"   $ "server shutdown initiated by "                           <> s <> rest
        liftIO . atomically . writeTQueue mq $ Shutdown


-----



adminTelePla :: Action
adminTelePla p@AdviseNoArgs = advise p [ prefixAdminCmd "telepla" ] "Please specify the PC name of the player to which \
                                                                    \you want to teleport."
adminTelePla p@(OneArgNubbed i mq cols (capitalize -> target)) = modifyState helper >>= sequence_
  where
    helper ms =
        let idSings  = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
            originId = getRmId i ms
            found (flip getRmId ms -> destId, targetSing)
              | targetSing == getSing i ms = (ms, [ wrapSend mq cols "You can't teleport to yourself." ])
              | destId     == originId     = (ms, [ wrapSend mq cols "You're already there!"           ])
              | otherwise                  = teleHelper i ms p { args = [] } originId destId targetSing
            notFound = (ms, [sorryInvalid])
        in maybe notFound found . findFullNameForAbbrev target $ idSings
    sorryInvalid = wrapSend mq cols $ "No PC by the name of " <> dblQuote target <> " is currently logged in."
adminTelePla (ActionParams { plaMsgQueue, plaCols }) = wrapSend plaMsgQueue plaCols "Please specify a single PC name."


teleHelper :: Id -> MudState -> ActionParams -> Id -> Id -> T.Text -> (MudState, [MudStack ()])
teleHelper i ms p originId destId name =
    let originDesig = mkStdDesig i ms Don'tCap
        originPCIds = i `delete` pcIds originDesig
        s           = fromJust . stdPCEntSing $ originDesig
        destDesig   = mkSerializedNonStdDesig i ms s A Don'tCap
        destPCIds   = findPCIds ms $ ms^.invTbl.ind destId
        ms'         = ms & pcTbl .ind i.rmId   .~ destId
                         & invTbl.ind originId %~ (i `delete`)
                         & invTbl.ind destId   %~ (sortInv ms . (++ [i]))
        msgAtOrigin = nlnl $ "There is a soft audible pop as " <> serialize originDesig <> " suddenly vanishes in a \
                             \jarring flash of white light."
        msgAtDest   = nlnl $ "There is a soft audible pop as " <> destDesig             <> " suddenly appears in a \
                             \jarring flash of white light."
        desc        = nlnl   "You are instantly transported in a blinding flash of white light. For a brief moment you \
                             \are overwhelmed with vertigo accompanied by a confusing sensation of nostalgia."
        in (ms', [ unless (getPlaFlag IsIncognito . getPla i $ ms) . bcast $ [ (msgAtOrigin, originPCIds)
                                                                             , (msgAtDest,   destPCIds  ) ]
                 , bcast . mkBroadcast i $ desc
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
        let originId = getRmId i ms
            found (destId, rmTeleName)
              | destId == originId = (ms, [ wrapSend mq cols "You're already there!" ])
              | otherwise          = teleHelper i ms p { args = [] } originId destId rmTeleName
            notFound = (ms, [sorryInvalid])
        in maybe notFound found . findFullNameForAbbrev target . views rmTeleNameTbl IM.toList $ ms
    sorryInvalid = wrapSend mq cols . T.concat $ [ dblQuote target
                                                 , " is not a valid room name. Type "
                                                 , quoteColor
                                                 , dblQuote . prefixAdminCmd $ "telerm"
                                                 , dfltColor
                                                 , " with no arguments to get a list of valid room names." ]
adminTeleRm p = advise p [] advice
  where
    advice = T.concat [ "Please provide one argument: the name of the room to which you'd like to teleport, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "telerm" <> " lounge"
                      , dfltColor
                      , "." ]


-----


adminTell :: Action
adminTell p@AdviseNoArgs = advise p [ prefixAdminCmd "tell" ] advice
  where
    advice = T.concat [ "Please specify the PC name of a player followed by a message, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "tell" <> " taro thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminTell p@(AdviseOneArg a) = advise p [ prefixAdminCmd "tell" ] advice
  where
    advice = T.concat [ "Please also provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ prefixAdminCmd "tell " <> a <> " thank you for reporting the bug you found"
                      , dfltColor
                      , "." ]
adminTell (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
    unless (null logMsgs) . forM_ logMsgs . uncurry $ logPla (prefixAdminCmd "tell")
  where
    helper ms =
        let s        = getSing i ms
            idSings  = [ idSing | idSing@(api, _) <- mkAdminPlaIdSingList ms, isLoggedIn . getPla api $ ms ]
            notFound = emptied . wrapSend mq cols $ "No PC by the name of " <> dblQuote target <> " is currently \
                                                    \logged in."
            found (tellId@(flip getPla ms -> tellPla), tellSing)
              | tellId == i = emptied . wrapSend mq cols $ "You talk to yourself."
              | getPlaFlag IsIncognito . getPla i $ ms
              , not . getPlaFlag IsAdmin $ tellPla =
                  emptied . wrapSend mq cols $ "You can only send messages to other administrators while incognito."
              | tellMq         <- getMsgQueue tellId ms
              , tellCols       <- tellPla^.columns
              , targetMsg      <- T.concat [ bracketQuote s, " ", adminTellColor, msg, dfltColor ]
              , sentLogMsg     <- (i,      T.concat [ "sent message to ", tellSing, ": ", dblQuote msg ])
              , receivedLogMsg <- (tellId, T.concat [ "received message from ", s,  ": ", dblQuote msg ]) = do
                  wrapSend mq cols . T.concat $ [ "You send ", tellSing, ": ", dblQuote msg ]
                  if getPlaFlag IsNotFirstAdminTell tellPla
                    then wrapSend      tellMq tellCols targetMsg
                    else multiWrapSend tellMq tellCols =<< [ targetMsg : hints | hints <- firstAdminTell tellId s ]
                  return [ sentLogMsg, receivedLogMsg ]
        in maybe notFound found . findFullNameForAbbrev target $ idSings
adminTell p = patternMatchFail "adminTell" [ showText p ]


firstAdminTell :: Id -> Sing -> MudStack [T.Text]
firstAdminTell tellId adminSing = modifyState $ (, msg) . (plaTbl.ind tellId %~ setPlaFlag IsNotFirstAdminTell True)
  where
    msg = [ "", T.concat [ hintANSI
                         , "Hint:"
                         , noHintANSI
                         , " the above is a message from "
                         , adminSing
                         , ", a CurryMUD administrator. To reply, type "
                         , quoteColor
                         , dblQuote $ "admin " <> adminSing <> " msg"
                         , dfltColor
                         , ", where "
                         , quoteColor
                         , dblQuote "msg"
                         , dfltColor
                         , " is the message you want to send to "
                         , adminSing
                         , "." ] ]


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
adminTypo (NoArgs i mq cols) = dumpLog mq cols typoLogFile ("typo", "typos") >> logPlaExec (prefixAdminCmd "typo") i
adminTypo p                  = withoutArgs adminTypo p


-----


adminUptime :: Action
adminUptime (NoArgs i mq cols) = do
    send mq . nl =<< liftIO uptime |$| try >=> eitherRet ((sendGenericErrorMsg mq cols >>) . logIOEx "adminUptime")
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
                               mkCharTxt (i, a) = let (pp *** pp -> (s, r)) = getSexRace i ms
                                                      name                  = mkAnnotatedName i a
                                                  in T.concat [ pad 15 name, pad 7 s, pad 10 r ]
                           in map mkCharTxt ias ++ [ T.concat [ mkNumOfCharsTxt is, " ", showText inOrOut, "." ] ]
  where
    predicate           = case inOrOut of LoggedIn  -> isLoggedIn
                                          LoggedOut -> not . isLoggedIn
    mkAnnotatedName i a = let p = getPla i ms
                              admin | getPlaFlag IsAdmin p     = asteriskColor <> "*" <> dfltColor
                                    | otherwise                = ""
                              incog | getPlaFlag IsIncognito p = asteriskColor <> "@" <> dfltColor
                                    | otherwise                = ""
                          in a <> admin <> incog
    mkNumOfCharsTxt (length -> nop) | nop == 1  = "1 character"
                                    | otherwise = showText nop <> " characters"


-----


adminWhoOut :: Action
adminWhoOut = whoHelper LoggedOut "whoout"
