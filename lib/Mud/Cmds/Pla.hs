{-# OPTIONS_GHC -fno-warn-type-defaults -Wno-redundant-constraints #-}
{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, TupleSections, TypeFamilies, ViewPatterns #-}

module Mud.Cmds.Pla ( getRecordUptime
                    , getUptime
                    , look
                    , mkNonStdRmLinkCmds
                    , mkRacialLangCmds
                    , noOfNpcCmds
                    , noOfPlaCmds
                    , noOfSpiritCmds
                    , npcCmds
                    , plaCmds
                    , ready
                    , showDate
                    , showMotd
                    , spiritCmds ) where

import           Mud.Cmds.ExpCmds
import           Mud.Cmds.Msgs.Advice
import           Mud.Cmds.Msgs.CmdDesc
import           Mud.Cmds.Msgs.Dude
import           Mud.Cmds.Msgs.Hint
import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Msgs.Sorry
import           Mud.Cmds.Util.Abbrev
import           Mud.Cmds.Util.EmoteExp.EmoteExp
import           Mud.Cmds.Util.EmoteExp.TwoWayEmoteExp
import           Mud.Cmds.Util.Misc
import           Mud.Cmds.Util.Pla
import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.ActionParams.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Coins
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Lang
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Data.State.Util.Random
import           Mud.Interp.Misc
import           Mud.Interp.MultiLine
import           Mud.Interp.Pause
import           Mud.Misc.ANSI
import           Mud.Misc.CurryTime
import           Mud.Misc.Database
import           Mud.Misc.LocPref
import qualified Mud.Misc.Logging as L (logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import           Mud.Misc.Misc
import           Mud.Misc.NameResolution
import           Mud.TheWorld.Liqs
import           Mud.TheWorld.Zones.AdminZoneIds (iRoot)
import           Mud.TheWorld.Zones.WarehouseIds (iPidge)
import           Mud.Threads.Act
import           Mud.Threads.LightTimer
import           Mud.Threads.Misc
import           Mud.Threads.SpiritTimer
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.FilePaths
import           Mud.TopLvlDefs.Misc
import           Mud.TopLvlDefs.Padding
import           Mud.TopLvlDefs.Telnet.Chars
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Weights
import           Mud.Util.List hiding (headTail)
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Padding
import           Mud.Util.Quoting
import           Mud.Util.Text
import           Mud.Util.Wrapping

import           Control.Arrow ((***), (&&&), first, second)
import           Control.Exception.Lifted (catch, try)
import           Control.Lens (_1, _2, _3, _4, at, both, each, to, view, views)
import           Control.Lens.Operators ((%~), (&), (+~), (-~), (.~), (<>~), (?~), (^.))
import           Control.Monad ((>=>), foldM, forM, forM_, guard, join, mplus, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Crypto.BCrypt (validatePassword)
import           Data.Bits (setBit, zeroBits)
import           Data.Bool (bool)
import           Data.Char (isDigit, isLetter, isLower, isSpace, isUpper)
import           Data.Either (lefts, partitionEithers)
import           Data.Function (on)
import           Data.Int (Int64)
import qualified Data.IntMap.Strict as IM ((!), keys)
import           Data.Ix (inRange)
import           Data.List ((\\), delete, foldl', intercalate, intersperse, nub, partition, sort, sortBy, unfoldr)
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M ((!), elems, filter, foldrWithKey, keys, lookup, null, singleton, size, toList)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>), All(..), Sum(..))
import qualified Data.Set as S (filter, toList)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T (readFile)
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Tuple (swap)
import           GHC.Stack (HasCallStack)
import           Prelude hiding (log, pi)
import           System.Clock (Clock(..), TimeSpec(..), getTime)
import           System.Console.ANSI (ColorIntensity(..), clearScreenCode)
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath ((</>))
import           System.Time.Utils (renderSecs)


default (Int, Double)


-----


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Cmds.Pla"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Pla"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Pla"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Pla"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Pla"


logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.Pla"


-- ==================================================


mkPlaCmds :: HasCallStack => Id -> MudState -> [Cmd]
mkPlaCmds i = sort . (plaCmds ++) . mkRacialLangCmds i


plaCmds :: HasCallStack => [Cmd]
plaCmds = regularCmds ++ priorityAbbrevCmds ++ expCmds


regularCmds :: HasCallStack => [Cmd]
regularCmds = map (uncurry4 mkRegularCmd) regularCmdTuples


-- TODO: "buy" and "sell".
-- TODO: "shout". Consider indoor vs. outdoor. Update the "communication" help topic.
-- TODO: Spirits can see in the dark.
regularCmdTuples :: HasCallStack => [(CmdFullName, ActionFun, Bool, CmdDesc)]
regularCmdTuples =
    [ ("?",          plaDispCmdList,     True,  cmdDescDispCmdList)
    , ("about",      about,              True,  cmdDescAbout)
    , ("admin",      admin,              True,  cmdDescAdmin)
    , ("bonus",      bonus,              True,  cmdDescBonus)
    , ("bug",        bug,                True,  cmdDescBug)
    , ("d",          go "d",             True,  cmdDescGoDown)
    , ("e",          go "e",             True,  cmdDescGoEast)
    , ("eat",        eat,                False, cmdDescEat)
    , ("empty",      emptyAction,        True,  cmdDescEmpty)
    , ("equipment",  equip,              True,  cmdDescEquip)
    , ("expressive", expCmdList,         True,  cmdDescExpCmdList)
    , ("extinguish", extinguish,         True,  cmdDescExtinguish)
    , ("feeling",    feeling,            True,  cmdDescFeeling)
    , ("light",      light,              True,  cmdDescLight)
    , ("listen",     listen,             True,  cmdDescListen)
    , ("lookself",   lookSelf,           True,  cmdDescLookSelf)
    , ("moles",      cmdNotFoundAction,  True,  "")
    , ("molest",     alertExec "molest", True,  "")
    , ("n",          go "n",             True,  cmdDescGoNorth)
    , ("ne",         go "ne",            True,  cmdDescGoNortheast)
    , ("newchannel", newChan,            True,  "Create one or more new telepathic channels.")
    , ("nw",         go "nw",            True,  cmdDescGoNorthwest)
    , ("password",   password,           False, "Change your password.")
    , ("question",   question,           True,  "Ask/answer newbie questions " <> plusRelatedMsg)
    , ("qui",        quitCan'tAbbrev,    True,  "")
    , ("quit",       quit,               False, cmdDescQuit)
    , ("rap",        cmdNotFoundAction,  True,  "")
    , ("rape",       alertExec "rape",   True,  "")
    , ("razzl",      cmdNotFoundAction,  True,  "")
    , ("razzle",     razzle,             True,  "")
    , ("read",       readAction,         True,  cmdDescRead)
    , ("refuel",     refuel,             True,  cmdDescRefuel)
    , ("remove",     remove,             True,  cmdDescRemove)
    , ("s",          go "s",             True,  cmdDescGoSouth)
    , ("sacrifice",  sacrifice,          False, "Sacrifice a corpse using a holy symbol.")
    , ("se",         go "se",            True,  cmdDescGoSoutheast)
    , ("security",   security,           True,  "View or change your security Q&A.")
    , ("set",        setAction,          True,  cmdDescSet)
    , ("sw",         go "sw",            True,  cmdDescGoSouthwest)
    , ("take",       getAction,          True,  cmdDescGet)
    , ("tune",       tune,               True,  cmdDescTune)
    , ("typo",       typo,               True,  cmdDescTypo)
    , ("u",          go "u",             True,  cmdDescGoUp)
    , ("unlink",     unlink,             True,  cmdDescUnlink)
    , ("uptime",     uptime,             True,  cmdDescUptime)
    , ("w",          go "w",             True,  cmdDescGoWest)
    , ("whoami",     whoAmI,             True,  cmdDescWhoAmI)
    , ("zoom",       zoom,               True,  cmdDescZoom) ]


mkRegularCmd :: HasCallStack => CmdFullName -> ActionFun -> Bool -> CmdDesc -> Cmd
mkRegularCmd cfn f b cd = Cmd { cmdName           = cfn
                              , cmdPriorityAbbrev = Nothing
                              , cmdFullName       = cfn
                              , cmdAction         = Action f b
                              , cmdDesc           = cd }


priorityAbbrevCmds :: HasCallStack => [Cmd]
priorityAbbrevCmds = concatMap (uncurry5 mkPriorityAbbrevCmd) priorityAbbrevCmdTuples


priorityAbbrevCmdTuples :: HasCallStack => [(CmdFullName, CmdPriorityAbbrevTxt, ActionFun, Bool, CmdDesc)]
priorityAbbrevCmdTuples =
    [ ("bars",        "b",   bars,           True,  cmdDescBars)
    , ("channel",     "c",   chan,           True,  "Send a message on a telepathic channel " <> plusRelatedMsg)
    , ("clear",       "cl",  clear,          True,  cmdDescClear)
    , ("color",       "col", color,          True,  cmdDescColor)
    , ("connect",     "co",  connect,        True,  "Connect one or more people to a telepathic channel.")
    , ("date",        "da",  date,           True,  cmdDescDate)
    , ("description", "de",  description,    False, cmdDescDescription)
    , ("disconnect",  "di",  disconnect,     True,  "Disconnect one or more people from a telepathic channel.")
    , ("drink",       "dr",  drink,          False, cmdDescDrink)
    , ("drop",        "dro", dropAction,     True,  cmdDescDrop)
    , ("emote",       "em",  emote,          True,  cmdDescEmote)
    , ("exits",       "ex",  exits,          True,  cmdDescExits)
    , ("fill",        "f",   fill,           True,  cmdDescFill)
    , ("get",         "g",   getAction,      True,  cmdDescGet)
    , ("give",        "gi",  give,           True,  cmdDescGive)
    , ("help",        "h",   help,           True,  cmdDescHelp)
    , ("intro",       "in",  intro,          True,  "Display a list of the people who have introduced themselves to \
                                                    \you, or introduce yourself to one or more people.")
    , ("inventory",   "i",   inv,            True,  cmdDescInv)
    , ("leave",       "le",  leave,          True,  "Sever your connections to one or more telepathic channels.")
    , ("link",        "li",  link,           True,  cmdDescLink)
    , ("look",        "l",   look,           True,  cmdDescLook)
    , ("motd",        "m",   motd,           True,  cmdDescMotd)
    , ("put",         "p",   putAction,      True,  cmdDescPut)
    , ("ready",       "r",   ready,          True,  cmdDescReady)
    , ("roomdesc",    "ro",  roomDesc,       True,  cmdDescRoomDesc)
    , ("say",         "sa",  say,            True,  cmdDescSay CommonLang)
    , ("show",        "sh",  showAction,     True,  cmdDescShow)
    , ("smell",       "sm",  smell,          True,  cmdDescSmell)
    , ("stats",       "st",  stats,          True,  cmdDescStats)
    , ("stop",        "sto", stop,           True,  cmdDescStop)
    , ("taste",       "ta",  taste,          True,  cmdDescTaste)
    , ("telepathy",   "t",   tele,           True,  cmdDescTelepathy)
    , ("tempdesc",    "te",  tempDescAction, True,  cmdDescTempDesc)
    , ("time",        "ti",  time,           True,  cmdDescTime)
    , ("unready",     "un",  unready,        True,  cmdDescUnready)
    , ("whisper",     "whi", whisper,        True,  cmdDescWhisper)
    , ("who",         "wh",  who,            True,  cmdDescWho) ]


mkPriorityAbbrevCmd :: HasCallStack => CmdFullName -> CmdPriorityAbbrevTxt -> ActionFun -> Bool -> CmdDesc -> [Cmd]
mkPriorityAbbrevCmd cfn cpat f b cd = unfoldr helper (T.init cfn) ++ [ Cmd { cmdName           = cfn
                                                                           , cmdPriorityAbbrev = Just cpat
                                                                           , cmdFullName       = cfn
                                                                           , cmdAction         = Action f b
                                                                           , cmdDesc           = cd } ]
  where
    helper ""                      = Nothing
    helper abbrev | abbrev == cpat = Just (mkExplicitAbbrevCmd, "")
                  | otherwise      = Just (mkExplicitAbbrevCmd, T.init abbrev)
      where
        mkExplicitAbbrevCmd = Cmd { cmdName           = abbrev
                                  , cmdPriorityAbbrev = Nothing
                                  , cmdFullName       = cfn
                                  , cmdAction         = Action f b
                                  , cmdDesc           = "" }


noOfPlaCmds :: HasCallStack => Int
noOfPlaCmds = length regularCmdTuples + length priorityAbbrevCmdTuples + length langsNoCommon


-----


spiritCmds :: HasCallStack => [Cmd]
spiritCmds = spiritRegularCmds ++ spiritPriorityAbbrevCmds


spiritRegularCmds :: HasCallStack => [Cmd]
spiritRegularCmds = sort . map (uncurry4 mkRegularCmd) $ spiritRegularCmdTuples


spiritRegularCmdTuples :: HasCallStack => [(CmdFullName, ActionFun, Bool, CmdDesc)]
spiritRegularCmdTuples =
    [ ("?",        spiritDispCmdList, True,  cmdDescDispCmdList)
    , ("about",    about,             True,  cmdDescAbout)
    , ("admin",    admin,             True,  cmdDescAdmin)
    , ("bonus",    bonus,             True,  cmdDescBonus)
    , ("bug",      bug,               True,  cmdDescBug)
    , ("d",        go "d",            True,  cmdDescGoDown)
    , ("e",        go "e",            True,  cmdDescGoEast)
    , ("feeling",  feeling,           True,  cmdDescFeeling)
    , ("lookself", lookSelf,          True,  cmdDescLookSelf)
    , ("n",        go "n",            True,  cmdDescGoNorth)
    , ("ne",       go "ne",           True,  cmdDescGoNortheast)
    , ("nw",       go "nw",           True,  cmdDescGoNorthwest)
    , ("qui",      quitCan'tAbbrev,   True,  "")
    , ("quit",     quit,              False, cmdDescQuit)
    , ("s",        go "s",            True,  cmdDescGoSouth)
    , ("se",       go "se",           True,  cmdDescGoSoutheast)
    , ("set",      setAction,         True,  cmdDescSet)
    , ("sw",       go "sw",           True,  cmdDescGoSouthwest)
    , ("tune",     tune,              True,  cmdDescTune)
    , ("typo",     typo,              True,  cmdDescTypo)
    , ("u",        go "u",            True,  cmdDescGoUp)
    , ("unlink",   unlink,            True,  cmdDescUnlink)
    , ("uptime",   uptime,            True,  cmdDescUptime)
    , ("w",        go "w",            True,  cmdDescGoWest)
    , ("whoami",   whoAmI,            True,  cmdDescWhoAmI)
    , ("zoom",     zoom,              True,  cmdDescZoom) ]


spiritPriorityAbbrevCmds :: HasCallStack => [Cmd]
spiritPriorityAbbrevCmds = concatMap (uncurry5 mkPriorityAbbrevCmd) spiritPriorityAbbrevCmdTuples


spiritPriorityAbbrevCmdTuples :: HasCallStack => [(CmdFullName, CmdPriorityAbbrevTxt, ActionFun, Bool, CmdDesc)]
spiritPriorityAbbrevCmdTuples =
    [ ("bars",      "b",   bars,  True,  cmdDescBars)
    , ("clear",     "cl",  clear, True,  cmdDescClear)
    , ("color",     "col", color, True,  cmdDescColor)
    , ("date",      "da",  date,  True,  cmdDescDate)
    , ("exits",     "ex",  exits, True,  cmdDescExits)
    , ("help",      "h",   help,  True,  cmdDescHelp)
    , ("link",      "li",  link,  True,  cmdDescLink)
    , ("look",      "l",   look,  True,  cmdDescLook)
    , ("motd",      "m",   motd,  True,  cmdDescMotd)
    , ("stats",     "st",  stats, True,  cmdDescStats)
    , ("stop",      "sto", stop,  True,  cmdDescStop)
    , ("telepathy", "t",   tele,  True,  cmdDescTelepathy)
    , ("time",      "ti",  time,  True,  cmdDescTime)
    , ("who",       "wh",  who,   True,  cmdDescWho) ]


noOfSpiritCmds :: HasCallStack => Int
noOfSpiritCmds = length spiritRegularCmdTuples + length spiritPriorityAbbrevCmdTuples


-----


{-
NPC commands must conform to the following rules:
* Messages should not be sent to the executor of the command in the form of broadcasts (otherwise they will be
erroneously decorated with "toNpcColor".)
* Given the above, "toSelf" messages should be explicitly subjected to "parseInBands" (as necessary) before being sent
to the executor (via "wrapSend" or a related function). Log messages may likewise need to be subjected to
"parseInBandsSuffix", depending on their content.
* When an NPC executes a command, that NPC's name should be represented as a "Desig" in any broadcasts sent to others.
-}


mkNpcCmds :: HasCallStack => Id -> MudState -> [Cmd]
mkNpcCmds i = sort . (npcCmds ++) . mkRacialLangCmds i


npcCmds :: HasCallStack => [Cmd]
npcCmds = npcRegularCmds ++ npcPriorityAbbrevCmds ++ expCmds


npcRegularCmds :: HasCallStack => [Cmd]
npcRegularCmds = map (uncurry4 mkRegularCmd) npcRegularCmdTuples


npcRegularCmdTuples :: HasCallStack => [(CmdFullName, ActionFun, Bool, CmdDesc)]
npcRegularCmdTuples =
    [ ("?",          npcDispCmdList, True,  cmdDescDispCmdList)
    , (".",          npcAsSelf,      False, "Execute a command as your admin PC.")
    , ("d",          go "d",         True,  cmdDescGoDown)
    , ("e",          go "e",         True,  cmdDescGoEast)
    , ("eat",        eat,            False, cmdDescEat)
    , ("empty",      emptyAction,    True,  cmdDescEmpty)
    , ("equipment",  equip,          True,  cmdDescEquip)
    , ("expressive", expCmdList,     True,  cmdDescExpCmdList)
    , ("extinguish", extinguish,     True,  cmdDescExtinguish)
    , ("feeling",    feeling,        True,  cmdDescFeeling)
    , ("light",      light,          True,  cmdDescLight)
    , ("listen",     listen,         True,  cmdDescListen)
    , ("lookself",   lookSelf,       True,  cmdDescLookSelf)
    , ("n",          go "n",         True,  cmdDescGoNorth)
    , ("ne",         go "ne",        True,  cmdDescGoNortheast)
    , ("nw",         go "nw",        True,  cmdDescGoNorthwest)
    , ("read",       readAction,     True,  cmdDescRead)
    , ("refuel",     refuel,         True,  cmdDescRefuel)
    , ("remove",     remove,         True,  cmdDescRemove)
    , ("s",          go "s",         True,  cmdDescGoSouth)
    , ("se",         go "se",        True,  cmdDescGoSoutheast)
    , ("sw",         go "sw",        True,  cmdDescGoSouthwest)
    , ("taste",      taste,          True,  cmdDescTaste)
    , ("u",          go "u",         True,  cmdDescGoUp)
    , ("w",          go "w",         True,  cmdDescGoWest)
    , ("zoom",       zoom,           True,  cmdDescZoom) ]


npcPriorityAbbrevCmds :: HasCallStack => [Cmd]
npcPriorityAbbrevCmds = concatMap (uncurry5 mkPriorityAbbrevCmd) npcPriorityAbbrevCmdTuples


npcPriorityAbbrevCmdTuples :: HasCallStack => [(CmdFullName, CmdPriorityAbbrevTxt, ActionFun, Bool, CmdDesc)]
npcPriorityAbbrevCmdTuples =
    [ ("bars",        "b",   bars,           True,  cmdDescBars)
    , ("clear",       "c",   clear,          True,  cmdDescClear)
    , ("date",        "da",  date,           True,  cmdDescDate)
    , ("description", "de",  description,    False, cmdDescDescription)
    , ("drink",       "dr",  drink,          False, cmdDescDrink)
    , ("drop",        "dro", dropAction,     True,  cmdDescDrop)
    , ("emote",       "em",  emote,          True,  cmdDescEmote)
    , ("exits",       "ex",  exits,          True,  cmdDescExits)
    , ("exorcise",    "exo", npcExorcise,    False, "Stop being possessed.")
    , ("fill",        "f",   fill,           True,  cmdDescFill)
    , ("get",         "g",   getAction,      True,  cmdDescGet)
    , ("give",        "gi",  give,           True,  cmdDescGive)
    , ("inventory",   "i",   inv,            True,  cmdDescInv)
    , ("look",        "l",   look,           True,  cmdDescLook)
    , ("put",         "p",   putAction,      True,  cmdDescPut)
    , ("ready",       "r",   ready,          True,  cmdDescReady)
    , ("roomdesc",    "ro",  roomDesc,       True,  cmdDescRoomDesc)
    , ("say",         "sa",  say,            True,  cmdDescSay CommonLang)
    , ("show",        "sh",  showAction,     True,  cmdDescShow)
    , ("smell",       "sm",  smell,          True,  cmdDescSmell)
    , ("stats",       "st",  stats,          True,  cmdDescStats)
    , ("stop",        "sto", stop,           True,  cmdDescStop)
    , ("tempdesc",    "te",  tempDescAction, True,  cmdDescTempDesc)
    , ("time",        "ti",  time,           True,  cmdDescTime)
    , ("unready",     "un",  unready,        True,  cmdDescUnready)
    , ("whisper",     "whi", whisper,        True,  cmdDescWhisper)
    , ("whoami",      "wh",  whoAmI,         True,  "Confirm who " <> parensQuote "and what" <> " you are.") ]


noOfNpcCmds :: HasCallStack => Int
noOfNpcCmds = length npcRegularCmdTuples + length npcPriorityAbbrevCmdTuples + length langsNoCommon


-----


mkRacialLangCmds :: HasCallStack => Id -> MudState -> [Cmd]
mkRacialLangCmds i ms | tuples  <- map mkRegCmdTuple langsNoCommon
                      , matches <- map snd . filter (isKnownLang i ms . fst) $ tuples
                      = sort . map (uncurry4 mkRegularCmd) $ matches
  where
    mkRegCmdTuple l = (l, (pp l, actionFunForLang l, True, cmdDescSay l))


actionFunForLang :: HasCallStack => Lang -> ActionFun
actionFunForLang = \case CommonLang    -> undefined
                         DwarfLang     -> dwarvish
                         ElfLang       -> elvish
                         FelinoidLang  -> felinoidean
                         HobbitLang    -> hobbitish
                         HumanLang     -> hominal
                         LagomorphLang -> lagomorphean
                         NymphLang     -> naelyni
                         VulpenoidLang -> vulpenoidean


-----


about :: HasCallStack => ActionFun
about (NoArgs i mq cols) = do
    logPlaExec "about" i
    helper |&| try >=> eitherRet ((sendGenericErrorMsg mq cols >>) . fileIOExHandler "about")
  where
    helper = multiWrapSend mq cols =<< liftIO [ T.lines cont | file <- mkMudFilePath aboutFileFun, cont <- T.readFile file ]
about p = withoutArgs about p


-----


admin :: HasCallStack => ActionFun
admin p@(NoArgs'' _                        ) = adminList p
admin p@AdviseOneArg                         = advise p ["admin"] adviceAdminNoMsg
admin   (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
    logMsgs |#| let f = uncurry . logPla $ "admin" in mapM_ f
  where
    helper ms =
        let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the administrator you wish to message"
            s                   = getSing i ms
            notFound            = emptied . sendFun . sorryAdminName $ strippedTarget
            found      (adminId, _        ) | adminId == i = emptied . sendFun $ sorryAdminChanSelf
            found pair@(adminId, adminSing) = case emotifyTwoWay "admin" i ms adminId msg of
              Left  errorMsgs  -> emptied . multiSendFun $ errorMsgs
              Right (Right bs) -> ioHelper pair bs
              Right (Left  ()) -> case expCmdifyTwoWay i ms adminId adminSing msg of
                Left  errorMsg -> emptied . sendFun $ errorMsg
                Right bs       -> ioHelper pair bs
            ioHelper (adminId, adminSing) [ fst -> toSelf, fst -> toAdmin ] = do
                if getAll . mconcat $ [ All . isLoggedIn $ adminPla
                                      , not (isAdminId i ms) |?| All . not . isIncognito $ adminPla ]
                  then sendFun formatted
                  else multiSendFun [ formatted, parensQuote "Message retained." ]
                retainedMsg adminId ms . mkRetainedMsgFromPerson s $ toAdmin
                ts <- liftIO mkTimestamp
                withDbExHandler_ "admin_msg" . insertDbTblAdminMsg . AdminMsgRec ts s adminSing $ toSelf
                return [ sentLogMsg, receivedLogMsg ]
              where
                adminPla  = getPla adminId ms
                formatted = parensQuote ("to " <> adminSing) <> spaced (quoteWith "__" s) <> toSelf
                sentLogMsg     = (i,       T.concat [ "sent message to ", adminSing, ": ", toSelf  ])
                receivedLogMsg = (adminId, T.concat [ "received message from ", s,   ": ", toAdmin ])
            ioHelper _ xs      = pmf "admin helper ioHelper" xs
            filterRoot idSings | isAdminId i ms = idSings
                               | otherwise      = isAwake iRoot ms ? idSings :? filter ((/= iRoot) . fst) idSings -- Root's ID can change.
        in (findFullNameForAbbrev strippedTarget . filterRoot . mkAdminIdSingList $ ms) |&| maybe notFound found
admin p = pmf "admin" p


adminList :: HasCallStack => ActionFun
adminList (NoArgs i mq cols) = sequence_ [ logPlaExecArgs "admin" [] i, multiWrapSend mq cols =<< helper =<< getState ]
  where
    helper ms = let p            = getPla i ms
                    singSuffixes = sortBy (compare `on` fst) [ second ((" logged " <>) . mkSuffix) pair
                                                             | (swap -> pair) <- mkAdminIdSingList ms ]
                    mkSuffix ai  = if isAdmin p && isIncognitoId ai ms
                      then inOut (isLoggedIn . getPla ai $ ms) |<>| parensQuote "incognito"
                      else inOut . isAwake ai $ ms
                    singSuffixes' = onFalse (isAdmin p) (filter f) singSuffixes
                      where
                        f (a, b) | a == "Root" = b == " logged in"
                                 | otherwise   = otherwise
                    combineds = [ padName abbrev <> suffix
                                | (_, suffix) <- singSuffixes'
                                | abbrev      <- styleAbbrevs Don'tQuote . map fst $ singSuffixes' ]
                in ()!# combineds ? return combineds :? unadulterated sorryNoAdmins
adminList p = pmf "adminList" p


-----


alertExec :: HasCallStack => CmdName -> ActionFun
alertExec cn (NoArgs      i mq cols         ) = alertExecHelper i mq cols cn "" ""
alertExec cn (OneArgLower i mq cols a       ) = alertExecHelper i mq cols cn a  a
alertExec cn (WithArgs    i mq cols as@(a:_)) = alertExecHelper i mq cols cn a . spaces $ as
alertExec _  p                                = pmf "alertExec" p


alertExecHelper :: HasCallStack => Id -> MsgQueue -> Cols -> CmdName -> Text -> Text -> MudStack ()
alertExecHelper i mq cols cn target args = do
    ms <- getState
    ts <- liftIO mkTimestamp
    let s          = getSing i ms
        targetSing = alertExecFindTargetSing i ms target
        msg        = T.concat [ s, " attempted to execute ", dblQuote cn, targetingMsg targetSing, " ", argsMsg, "." ]
        outIds     = (getIdForRoot ms `delete`) $ getAdminIds ms \\ getLoggedInAdminIds ms
        rec        = AlertExecRec Nothing ts s cn targetSing args
    logNotice fn   msg
    logPla    fn i msg
    sendCmdNotFound i mq cols
    bcastAdmins msg
    forM_ outIds (\adminId -> retainedMsg adminId ms . mkRetainedMsgFromPerson s $ msg)
    withDbExHandler_ fn . insertDbTblAlertExec $ rec
  where
    fn                      = "alertExecHelper"
    targetingMsg targetSing = targetSing |!| (" targeting " <> targetSing)
    argsMsg | ()# args      = "with no arguments"
            | otherwise     = "with the following arguments: " <> dblQuote args


alertExecFindTargetSing :: HasCallStack => Id -> MudState -> Text -> Text
alertExecFindTargetSing _ _  ""     = ""
alertExecFindTargetSing i ms target = let (_, _, inRms) = sortArgsInvEqRm InRm . pure $ target
                                          invCoins      = first (i `delete`) . getVisibleInvCoins (getRmId i ms) $ ms
                                          (eiss, _)     = uncurry (resolveRmInvCoins i ms inRms) invCoins
                                      in ()!# invCoins |?| case eiss of []           -> ""
                                                                        (Right is:_) -> commas . map (`getSing` ms) $ is
                                                                        _            -> ""


-----


bars :: HasCallStack => ActionFun
bars (NoArgs i mq cols) = getState >>= \ms ->
    let mkBars = map (uncurry . mkBar . calcBarLen $ cols) . mkPtPairs i $ ms
    in logPlaExecArgs "bars" [] i >> multiWrapSend mq cols mkBars
bars (LowerNub i mq cols as) = getState >>= \ms ->
    let mkBars  = case second nub . partitionEithers . map f $ as of
                    (x:xs, []     ) -> x |<>| hint : xs
                    ([],   barTxts) -> barTxts
                    (x:xs, barTxts) -> middle (++) mMempty barTxts $ x |<>| hint : xs
        f a = case filter ((a `T.isPrefixOf`) . fst) . mkPtPairs i $ ms of
          []        -> Left . sorryParseArg $ a
          (match:_) -> Right . uncurry (mkBar . calcBarLen $ cols) $ match
    in logPlaExecArgs "bars" as i >> multiWrapSend mq cols mkBars
  where
    hint = T.concat [ "Please specify any of the following: "
                    , commas . map dblQuote $ [ "hp", "mp", "pp" ]
                    , ", or "
                    , dblQuote "fp"
                    , "." ]
bars p = pmf "bars" p


mkBar :: HasCallStack => Int -> Text -> (Int, Int) -> Text
mkBar x txt (c, m) = let ratio  = c `divide` m
                         greens = round $ fromIntegral x * ratio
                         reds   = x - greens
                     in T.concat [ T.toUpper txt
                                 , ": "
                                 , colorWith greenBarColor . T.replicate greens $ " "
                                 , colorWith redBarColor   . T.replicate reds   $ " "
                                 , " "
                                 , showTxt . round $ 100 * ratio
                                 , "%" ]


mkPtPairs :: HasCallStack => Id -> MudState -> [(Text, (Int, Int))]
mkPtPairs i ms = let (hps, mps, pps, fps) = getPts i ms
                 in [ ("hp", hps), ("mp", mps), ("pp", pps), ("fp", fps) ]


-----


bonus :: HasCallStack => ActionFun
bonus p@AdviseNoArgs              = advise p ["bonus"] adviceBonusNoArgs
bonus   (OneArgLower i mq cols a) = getState >>= \ms ->
    let (f, guessWhat)  | hasLocPref a = (stripLocPref, sorryBonusIgnore)
                        | otherwise    = (id,           ""              )
        a'                             = capitalize . T.toLower . f $ a
        s                              = getSing i ms
        intros                         = getIntroduced i ms
        bonusHelper _   | a' == s      = sorry sorryBonusSelf
        bonusHelper now                = case filter (a' `T.isPrefixOf`) intros of
          []           -> sorry . sorryBonusName $ a'
          [targetSing] ->
              let targetId = getIdForPCSing targetSing ms
                  x        = calcBonus targetId ms
                  bs       = pure (prd $ "You give a bonus to " <> targetSing, pure i)
              in if isAdminId targetId ms
                then sorry sorryBonusAdmin
                else fmap2 getAll (canBonus targetSing) >>= \case
                  Just True  -> do let msg = T.concat [ "giving a bonus of ", commaShow x, " exp to ", targetSing, "." ]
                                   logPla "bonus bonusHelper" i msg
                                   bcastNl . onFalse (()# guessWhat) ((guessWhat, pure i) :) $ bs
                                   retainedMsg targetId ms . colorWith bonusColor . mkToTarget $ targetId
                                   awardExp x ("bonus from " <> s) targetId
                                   tweak $ plaTbl.ind i.bonusTime ?~ now
                                   ts <- liftIO mkTimestamp
                                   withDbExHandler_ "bonus bonusHelper" . insertDbTblBonus . BonusRec ts s targetSing $ x
                  Just False -> sorry . sorryBonusCount $ targetSing
                  Nothing    -> unit
          xs -> pmf "bonus bonusHelper" xs
        canBonus targetSing = (withDbExHandler "bonus canBonus" . lookupBonusesFromTo s $ targetSing) >>= \case
          Just c  -> unadulterated . All $ c < maxBonuses
          Nothing -> emptied . dbError mq $ cols
        mkToTarget targetId | s `elem` getIntroduced targetId ms = g s
                            | otherwise                          = g "Someone"
          where
            g = (<> " has given you bonus experience points for outstanding role-playing.")
    in if getLvl i ms <= 2
      then sorry sorryBonusLvl
      else liftIO getCurrentTime >>= \now -> case getBonusTime i ms of
        Nothing -> bonusHelper now
        Just bt -> round (now `diffUTCTime` bt) < bonusDelay ? sorry sorryBonusTime :? bonusHelper now
  where
    sorry = wrapSend mq cols
bonus p = advise p ["bonus"] adviceBonusExcessArgs


-----


bug :: HasCallStack => ActionFun
bug p@AdviseNoArgs = advise p ["bug"] adviceBugNoArgs
bug p              = bugTypoLogger p BugLog


-----


chan :: HasCallStack => ActionFun
chan (NoArgs i mq cols) = getState >>= \ms ->
    let (chanNames, chanTunings) = mkChanNamesTunings i ms
        helper names tunings     = let txts = mkChanTxts
                                   in (()!# txts ? txts :? none) |&| ("Telepathic channels:" :)
          where
            mkChanTxts = [ padChanName n <> tunedInOut t | n <- names | t <- tunings ]
    in do logPlaExecArgs "chan" [] i
          multiWrapSend mq cols . helper (styleAbbrevs Don'tQuote chanNames) $ chanTunings
chan (OneArg i mq cols a@(T.toLower -> a')) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryChanName $ a
        found match =
            let (cn, c)                  = getMatchingChanWithName match cns cs
                ((_, isTuned):_, others) = partition ((== s) . fst) $ c^.chanConnTbl.to M.toList
                (linkeds, nonLinkeds)    = partition (views _1 (isLinked ms . (i, ))) . filter f . map mkTriple $ others
                f                        = views _1 (`isAwake` ms)
            in mapM (updateRndmName i . view _1) nonLinkeds >>= \rndmNames ->
                let combo       = linkeds ++ zipWith (\rndmName -> _2 .~ rndmName) rndmNames nonLinkeds
                    (ins, outs) = partition (view _3) . sortBy (compare `on` view _2) $ combo
                    styleds     = styleAbbrevs Don'tQuote . select _2 $ ins
                    ins'        = zipWith (\styled -> _2 .~ styled) styleds ins
                    g (_, n, isTuned') = let n' = isRndmName n ? underline n :? n in padName n' <> tunedInOut isTuned'
                    combo'             = ins' ++ outs
                in if isTuned
                  then do
                      let msgs              = ()!# combo' ? map g combo' :? pure "You are the only person connected."
                          affixChanName txt = parensQuote cn |<>| txt
                      logPla "chan" i . affixChanName . commas $ [ getSing i' ms <> " is " <> tunedInOut isTuned'
                                                                 | (i', _, isTuned') <- combo' ]
                      multiWrapSend mq cols $ "Channel " <> dblQuote cn <> ":" : msgs
                  else wrapSend mq cols . sorryTunedOutICChan $ cn
        (cs, cns, s)           = mkChanBindings i ms
        mkTriple (s', isTuned) = (getIdForPCSing s' ms, s', isTuned)
    in findFullNameForAbbrev a' (map T.toLower cns) |&| maybe notFound found
chan (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryChanName $ target
        found match = let (cn, c) = getMatchingChanWithName match cns cs in if
          | views chanConnTbl (not . (M.! s)) c -> wrapSend mq cols . sorryTunedOutICChan $ cn
          | isIncognitoId i ms                  -> wrapSend mq cols . sorryChanIncog $ "a telepathic"
          | otherwise                           -> getChanStyleds i c ms >>= \triples -> if ()# triples
            then wrapSend mq cols . sorryChanNoOneListening . dblQuote $ cn
            else let getStyled targetId = view _3 . head . filter (views _1 (== i)) <$> getChanStyleds targetId c ms
                     format (txt, is)   = if i `elem` is
                                            then ((formatChanMsg cn s txt, pure i) :) <$> mkBsWithStyled (i `delete` is)
                                            else mkBsWithStyled is
                       where
                         mkBsWithStyled is' = mapM getStyled is' >>= \styleds ->
                             return [ (formatChanMsg cn styled txt, pure i') | i' <- is' | styled <- styleds ]
                     ioHelper (expandEmbeddedIdsToSings ms -> expanded) bs = do
                         let expanded' = parensQuote cn |<>| expanded
                         logPlaOut "chan" i . pure $ expanded'
                         bcastNl =<< expandEmbeddedIds ms cc bs
                         sendToWiretappers expanded
                         alertMsgHelper i "chan" expanded'
                         ts <- liftIO mkTimestamp
                         withDbExHandler_ "chan" . insertDbTblChan . ChanRec ts (c^.chanId) cn s $ expanded
                     sendToWiretappers tappedMsg =
                         let cn' = colorWith wiretapColor . spaced . parensQuote $ cn
                             is  = c^.chanWiretappers.to (map (`getIdForPCSing` ms))
                             is' = filter (isLoggedIn . (`getPla` ms)) is
                         in bcastNl . pure $ (T.concat [ cn', " ", s, ": ", tappedMsg ], is')
                     cc   = ChanContext "chan" (Just cn) False
                     f bs = let msg' = dropANSI . fst . head $ bs in ioHelper msg' =<< g bs
                     g    = concatMapM format
                     ws   = wrapSend      mq cols
                     mws  = multiWrapSend mq cols
                 in case targetify i cc triples msg of
                   Left  errorMsg   -> ws errorMsg
                   Right (Right bs) -> f bs
                   Right (Left  ()) -> case emotify i ms cc triples msg of
                     Left  errorMsgs  -> mws errorMsgs
                     Right (Right bs) -> f bs
                     Right (Left  ()) -> case expCmdify i ms cc triples msg of
                       Left  errorMsg   -> ws errorMsg
                       Right (bs, msg') -> ioHelper msg' =<< g bs
        (cs, cns, s) = mkChanBindings i ms
    in findFullNameForAbbrev (T.toLower target) (map T.toLower cns) |&| maybe notFound found
chan p = pmf "chan" p


-----


clear :: HasCallStack => ActionFun
clear (NoArgs' i mq) = sequence_ [ logPlaExec "clear" i, send mq . T.pack $ clearScreenCode ]
clear p              = withoutArgs clear p


-----


color :: HasCallStack => ActionFun
color (NoArgs' i mq) = sequence_ [ logPlaExec "color" i, send mq . nl . T.concat $ msg ]
  where
    msg = [ nl . T.concat $ [ mkColorDesc fg bg, colorWith ansi . spaced $  "CurryMUD" ]
          | fgc <- colors, bgc <- colors, fgc /= bgc
          , let fg = (Dull, fgc), let bg = (Dull, bgc), let ansi = mkColorANSI fg bg ] ++ other
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName                                         = padColorName . showTxt . snd
    other = [ nl . T.concat $ [ pad 19 "Blinking",   blink     . spaced $ "CurryMUD" ]
            , nl . T.concat $ [ pad 19 "Underlined", underline . spaced $ "CurryMUD" ] ]
color p = withoutArgs color p


-----


cmdNotFoundAction :: HasCallStack => ActionFun
cmdNotFoundAction ActionParams { .. } = sendCmdNotFound myId plaMsgQueue plaCols


-----


connect :: HasCallStack => ActionFun
connect p@AdviseNoArgs         = advise p ["connect"] adviceConnectNoArgs
connect p@AdviseOneArg         = advise p ["connect"] adviceConnectNoChan
connect p@(Lower i mq cols as) = getState >>= \ms ->
    let getIds = map (`getIdForPCSing` ms)
        next   = if isIncognitoId i ms
          then wrapSend mq cols . sorryIncog $ "connect"
          else connectHelper i (mkLastArgWithNubbedOthers as) |&| modifyState >=> \(ms', pair) -> case pair of
            ([Left msg], Nothing) -> bcastNl . mkBcast i $ msg
            (res,        Just ci)
              | (sorryMsgs, targetSings) <- partitionEithers res
              , sorryBs   <- [ (msg, pure i) | msg <- sorryMsgs ]
              , targetIds <- getIds targetSings
              , c         <- getChan ci ms'
              , cn        <- c^.chanName
              , otherIds  <- let f = (\\ (i : targetIds)) . filter (`isAwake` ms') . getIds . M.keys . M.filter id
                             in views chanConnTbl f c
              , toTargets <- (T.concat [ getSing i ms', " has connected you to the ", dblQuote cn, " channel." ], targetIds)
              , toSelf    <- (focusingInnateMsg <>) $ case targetSings of
                [one] -> T.concat [ "you connect ", one, " to the ", dblQuote cn, " channel." ]
                _     -> T.concat [ "you connect the following people to the "
                                  , dblQuote cn
                                  , " channel: "
                                  , commas targetSings
                                  , "." ]
              -> do logPla "connect" i $ "connected to " <> dblQuote cn <> ": " <> commas targetSings
                    toOthers <- mkToOthers ms' otherIds targetIds cn
                    bcastNl $ toTargets : toOthers ++ (()!# targetSings |?| mkBcast i toSelf) ++ sorryBs
                    connectBlink targetIds ms'
            xs -> pmf "connect" xs
    in checkActing p ms (Right "connect a person to a telepathic channel") (pure Sacrificing) next
  where
    mkToOthers ms otherIds targetIds cn = do
        namesForMe      <- mapM (getRelativePCName ms . (, i)) otherIds
        namesForTargets <- mapM (\otherId -> mapM (getRelativePCName ms . (otherId, )) targetIds) otherIds
        let f i' me = map (\n -> (T.concat [ me, " has connected ", n, " to the ", dblQuote cn, " channel." ], pure i'))
        return . concat . zipWith3 f otherIds namesForMe $ namesForTargets
    connectBlink targetIds ms = forM_ targetIds $ \targetId ->
        rndmDo_ (calcProbConnectBlink targetId ms) . mkExpAction "blink" . mkActionParams targetId ms $ []
connect p = pmf "connect" p


-----


date :: HasCallStack => ActionFun
date (NoArgs i mq cols) = logPlaOut "date" i =<< showDate mq cols
date p                  = withoutArgs date p


showDate :: HasCallStack => MsgQueue -> Cols -> MudStack [Text]
showDate mq cols = liftIO getCurryTime >>= \CurryTime { .. } ->
    let msgs = [ T.concat [ "It's the "
                          , mkOrdinal curryDayOfWeek
                          , " day "
                          , parensQuote . ppWeekdayForDayOfWeek $ curryDayOfWeek
                          , " of the "
                          , mkOrdinal curryWeek
                          , " week of the "
                          , mkOrdinal curryMonth
                          , " month "
                          , parensQuote . ppMonthForMonthNum $ curryMonth
                          , " of the year "
                          , showTxt curryYear
                          , "." ]
               , prd $ "The date is " <> T.intercalate "-" (map showTxt [ curryMonth, curryDayOfMonth, curryYear ]) ]
    in ((>>) <$> multiWrapSend mq cols <*> return) msgs


-----


description :: HasCallStack => ActionFun
description (NoArgs i mq cols) = getEntDesc i <$> getState >>= \desc -> do
    wrapSend1Nl    mq cols "Your description is:"
    wrapSend       mq cols desc
    promptChangeIt mq cols
    setInterp i . Just $ interpConfirmDescChange
description p = withoutArgs description p


interpConfirmDescChange :: HasCallStack => Interp
interpConfirmDescChange cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> getServerSettings >>= \s -> do blankLine mq
                                               send mq . T.unlines . parseWrapXform s cols $ descRules
                                               pause i mq . Just . descHelper s i mq $ cols
  Just False -> neverMind i mq
  Nothing    -> promptRetryYesNo mq cols
  where
    descRules = T.concat [ lSpcs, rulesIntroMsg, " ", violationMsg, quoteWith nlTxt descRulesMsg, descRule5 ]
interpConfirmDescChange _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


descHelper :: HasCallStack => ServerSettings -> Id -> MsgQueue -> Cols -> MudStack ()
descHelper s i mq cols = sequence_ [ writeMsg mq . InacSecs $ maxInacSecsCompose
                                   , send mq . nl . T.unlines . parseWrapXform s cols $ enterDescMsg
                                   , setInterp i . Just . interpMutliLine f $ [] ]
  where
    f desc = case spaces . dropBlanks . map T.strip $ desc of
      ""    -> neverMind i mq
      desc' -> do multiWrapSend  mq cols [ "", "You entered:", desc' ]
                  wrapSendPrompt mq cols $ "Keep this description? " <> mkYesNoChoiceTxt
                  setInterp i . Just . interpConfirmDesc $ desc'


interpConfirmDesc :: HasCallStack => Text -> Interp
interpConfirmDesc desc cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> getState >>= \ms -> do
      logPla "description" i . prd $ "changing description to " <> dblQuote desc
      tweak $ entTbl.ind i.entDesc .~ desc
      ok mq
      sendDfltPrompt mq i
      resetInterp i
      resetInacTimer
      let poss = mkPossPro . getSex i $ ms
      bcastAdminsExcept (pure i) . T.concat $ [ descSingId i ms, " has changed ", poss, " description." ]
  Just False -> resetInacTimer >> neverMind i mq
  Nothing    -> promptRetryYesNo mq cols
  where
    resetInacTimer = writeMsg mq . InacSecs $ maxInacSecs
interpConfirmDesc _ _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


-----


disconnect :: HasCallStack => ActionFun
disconnect p@AdviseNoArgs         = advise p ["disconnect"] adviceDisconnectNoArgs
disconnect p@AdviseOneArg         = advise p ["disconnect"] adviceDisconnectNoChan
disconnect p@(Lower i mq cols as) = getState >>= \ms ->
    let getIds = map (`getIdForPCSing` ms)
        next   = if isIncognitoId i ms
          then wrapSend mq cols . sorryIncog $ "disconnect"
          else getAllChanIdNames i ms >>= \idNamesTbl ->
              disconnectHelper i (mkLastArgWithNubbedOthers as) idNamesTbl |&| modifyState >=> \case
                ([Left msg], Nothing) -> bcastNl . mkBcast i $ msg
                (res,        Just ci)
                  | (sorryMsgs, idSingNames)              <- partitionEithers res
                  , (targetIds, targetSings, targetNames) <- unzip3 idSingNames
                  , sorryBs   <- [ (msg, pure i) | msg <- sorryMsgs ]
                  , c         <- getChan ci ms
                  , cn        <- c^.chanName
                  , otherIds  <- let f = (\\ (i : targetIds)) . filter (`isAwake` ms) . getIds . M.keys . M.filter id
                                 in views chanConnTbl f c
                  , toTargets <- ( "Someone has severed your telepathic connection to the " <> dblQuote cn <> " channel."
                                 , targetIds )
                  , toSelf    <- (focusingInnateMsg <>) $ case targetNames of
                    [n] -> T.concat [ "you disconnect ", format n, " from the ", dblQuote cn, " channel." ]
                    _   -> T.concat [ "you disconnect the following people from the "
                                    , dblQuote cn
                                    , " channel: "
                                    , commas . map format $ targetNames
                                    , "." ] -> do
                      toOthers <- mkToOthers ms otherIds targetIds cn
                      bcastNl $ toTargets : toOthers ++ (()!# targetNames |?| mkBcast i toSelf) ++ sorryBs
                      targetSings |#| let msg = T.concat [ "disconnected from ", dblQuote cn, ": ", commas targetSings ]
                                      in const . logPla "disconnect" i $ msg
                xs -> pmf "disconnect" xs
    in checkActing p ms (Right "disconnect a person to a telepathic channel") (pure Sacrificing) next
  where
    format n = isRndmName n ? underline n :? n
    mkToOthers ms otherIds targetIds cn = do
        namesForMe      <- mapM (getRelativePCName ms . (, i)) otherIds
        namesForTargets <- mapM (\otherId -> mapM (getRelativePCName ms . (otherId, )) targetIds) otherIds
        let f i' me = map g
              where
                g n = (T.concat [ me, " has disconnected ", n, " from the ", dblQuote cn, " channel." ], pure i')
        return . concat . zipWith3 f otherIds namesForMe $ namesForTargets
disconnect p = pmf "disconnect" p


-----


drink :: HasCallStack => ActionFun
drink p@(NoArgs' i mq                   ) = advise p ["drink"] adviceDrinkNoArgs   >> sendDfltPrompt mq i
drink p@(OneArg  i mq _    _            ) = advise p ["drink"] adviceDrinkNoVessel >> sendDfltPrompt mq i
drink p@(Lower   i mq cols [amt, target]) = getState >>= \ms ->
    let f = mkRndmVector >>= \v -> helper v |&| modifyState >=> sequence_
    in checkActing p ms (Left Drinking) [ Drinking, Eating, Sacrificing ] f
  where
    helper v ms | amt `T.isPrefixOf` "all" || amt == T.singleton allChar = next maxBound
                | otherwise = case reads . T.unpack $ amt :: [(Int, String)] of
                  [(x, "")] | x <= 0    -> sorry sorryDrinkMouthfuls
                            | otherwise -> next x
                  _                     -> sorry . sorryParseMouthfuls $ amt
      where
        sorry  = (ms, ) . (: pure (sendDfltPrompt mq i)) . wrapSend mq cols
        next x =
            let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv . pure $ target
                (ri, myInvCoins)       = (getRmId `fanUncurry` getInvCoins) (i, ms)
                rmInvCoins             = first (i `delete`) . getVisibleInvCoins ri $ ms
                maybeHooks             = lookupHooks i ms "drink"
                -----
                drinkInv =
                    let (eiss, _)    = uncurry (resolveMobInvCoins i ms inInvs) myInvCoins
                        f [targetId] = case (getSing `fanUncurry` getType) (targetId, ms) of
                          (s, VesselType) -> maybe (sorry . sorryDrinkEmpty $ s) (g s) . getVesselCont targetId $ ms
                          (s, _         ) -> sorry . sorryDrinkType $ s
                          where
                            g _ _      | fst (calcStomachAvailSize i ms) <= 0 = sorry sorryFull
                            g s (l, _) | db <- DrinkBundle { drinkerId       = i
                                                           , drinkerMq       = mq
                                                           , drinkerCols     = cols
                                                           , drinkVesselId   = Just targetId
                                                           , drinkVesselSing = s
                                                           , drinkLiq        = l
                                                           , drinkAmt        = x }
                                       = (ms, pure . startAct i Drinking . drinkAct $ db)
                        f _ = sorry sorryDrinkExcessTargets
                    in case eiss of []      -> sorry sorryDrinkCoins
                                    (eis:_) -> either sorry f eis
                -----
                drinkRm =
                    let hookArg = head inRms <> T.cons hookArgDelimiter (showTxt x)
                    in case ((()!#) *** (()!#)) (rmInvCoins, maybeHooks) of
                      (True,  False) -> sorry sorryDrinkRmNoHooks
                      (False, True ) ->
                          let (inRms', (ms', _, bs, logMsgs), fs) = procHooks i ms v "drink" . pure $ hookArg
                              sorryMsgs                           = inRms' |!| pure sorryDrinkEmptyRmWithHooks
                          in (ms', [ logMsgs |#| logPla "drink" i . prd . slashes
                                   , when (()!# sorryMsgs) $ multiWrapSend mq cols sorryMsgs >> sendDfltPrompt mq i
                                   , bcastIfNotIncogNl i bs
                                   , sequence_ fs ])
                      (True,  True ) ->
                          let (inRms', (ms', _, bs, logMsgs), fs) = procHooks i ms v "drink" . pure $ hookArg
                          in if ()# inRms'
                            then (ms', [ logMsgs |#| logPla "drink" i . prd . slashes
                                       , bcastIfNotIncogNl i bs
                                       , sequence_ fs ])
                            else sorry . sorryDrinkRmWithHooks . head $ inRms
                      a -> pmf "drink helper next drinkRm" a
            in if | ()!# inEqs                      -> sorry sorryDrinkInEq
                  | ()!# inInvs,    ()#  myInvCoins -> sorry dudeYourHandsAreEmpty
                  | ()!# inInvs,    ()!# myInvCoins -> drinkInv
                  | ()# rmInvCoins, ()#  maybeHooks -> sorry sorryDrinkEmptyRmNoHooks
                  | otherwise                       -> drinkRm
drink p = advise p ["drink"] adviceDrinkExcessArgs


-----


dropAction :: HasCallStack => ActionFun -- TODO: Continue from here with "VerbObj".
dropAction p@AdviseNoArgs     = advise p ["drop"] adviceDropNoArgs
dropAction p@(LowerNub' i as) = genericAction p helper "drop"
  where
    helper ms = let (inInvs, inEqs, inRms)      = sortArgsInvEqRm InInv as
                    sorryInEq                   = inEqs |!| sorryDropInEq
                    sorryInRm                   = inRms |!| sorryDropInRm
                    invCoins                    = getInvCoins i ms
                    d                           = mkStdDesig  i ms DoCap
                    ri                          = getRmId     i ms
                    (eiss, ecs)                 = uncurry (resolveMobInvCoins i ms inInvs) invCoins
                    tuple                       = foldl' (helperDropEitherInv i d i ri) (ms, [], [], []) eiss
                    (ms', toSelfs, bs, logMsgs) = helperGetDropEitherCoins i d Drop i ri tuple ecs
                    f | ()!# invCoins = (ms', (dropBlanks $ [ sorryInEq, sorryInRm ] ++ toSelfs, bs, logMsgs))
                      | otherwise     = genericSorry ms dudeYourHandsAreEmpty
                in genericCheckActing i ms (Right "drop an item") [ Drinking, Sacrificing ] f
dropAction p = pmf "dropAction" p


-----


dwarvish :: HasCallStack => ActionFun
dwarvish = sayHelper DwarfLang


-----


eat :: HasCallStack => ActionFun
eat p@(NoArgs' i mq                   ) = advise p ["eat"] adviceEatNoArgs >> sendDfltPrompt mq i
eat p@(OneArg  i mq _    _            ) = advise p ["eat"] adviceEatNoFood >> sendDfltPrompt mq i
eat p@(Lower   i mq cols [amt, target]) = getState >>= \ms ->
    checkActing p ms (Left Eating) [ Drinking, Eating, Sacrificing ] (helper |&| modifyState >=> sequence_)
  where
    helper ms | amt `T.isPrefixOf` "all" || amt == T.singleton allChar = next maxBound
              | otherwise = case reads . T.unpack $ amt :: [(Int, String)] of
                [(x, "")] | x <= 0    -> sorry sorryEatMouthfuls
                          | otherwise -> next x
                _                     -> sorry . sorryParseMouthfuls $ amt
      where
        sorry  = (ms, ) . (: pure (sendDfltPrompt mq i)) . wrapSend mq cols
        next x = let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv . pure $ target
                     myInvCoins             = getInvCoins i ms
                     eatInv                 =
                         let (eiss, _)    = uncurry (resolveMobInvCoins i ms inInvs) myInvCoins
                             f [targetId] = case (getSing `fanUncurry` getType) (targetId, ms) of
                               (s, FoodType) | fst (calcStomachAvailSize i ms) <= 0 -> sorry sorryFull
                                             | eb <- EatBundle { eaterId     = i
                                                               , eaterMq     = mq
                                                               , eaterCols   = cols
                                                               , eatFoodId   = targetId
                                                               , eatFoodSing = s
                                                               , eatFood     = getFood targetId ms
                                                               , eatAmt      = x }
                                             -> (ms, pure . startAct i Eating . eatAct $ eb)
                               (s, _       ) -> sorry . sorryEatType $ s
                             f _ = sorry sorryEatExcessTargets
                         in case eiss of []      -> sorry sorryEatCoins
                                         (eis:_) -> either sorry f eis
                 in if | ()!# inEqs      -> sorry sorryEatInEq
                       | ()!# inRms      -> sorry sorryEatInRm
                       | ()#  myInvCoins -> sorry dudeYourHandsAreEmpty
                       | otherwise       -> eatInv
eat p = advise p ["eat"] adviceEatExcessArgs


-----


elvish :: HasCallStack => ActionFun
elvish = sayHelper ElfLang


-----


emote :: HasCallStack => ActionFun
emote p@AdviseNoArgs                                                       = advise p ["emote"] adviceEmoteNoArgs
emote p@ActionParams { args }   | any (`elem` yous) . map T.toLower $ args = advise p ["emote"] adviceYouEmote
emote   (WithArgs i mq cols as) = getState >>= \ms ->
    let d                       = mkStdDesig i ms DoCap
        ser                     = serialize d
        d'                      = d { desigCap = Don'tCap }
        ser'                    = serialize d'
        xformed                 = xformArgs True as
        xformArgs _      []     = []
        xformArgs isHead [x]    | (h, t) <- headTail x, h == emoteNameChar, all isPunc . T.unpack $ t
                                = pure . mkRightForNonTargets $ expandEnc isHead & each <>~ t
        xformArgs isHead (x:xs) = (: xformArgs False xs) $ if
          | x == enc             -> mkRightForNonTargets . expandEnc $ isHead
          | x == enc's           -> mkRightForNonTargets $ expandEnc isHead & each <>~ "'s"
          | enc `T.isInfixOf` x  -> Left . adviceEnc $ "emote "
          | x == etc             -> Left . adviceEtc $ "emote "
          | T.take 1 x == etc    -> isHead ? Left adviceEtcHead :? procTarget ms (T.tail x)
          | etc `T.isInfixOf` x  -> Left . adviceEtc $ "emote "
          | isHead, hasEnc as    -> mkRightForNonTargets $ dup3 x  & each %~ capitalizeMsg
          | isHead, x' <- spcL x -> mkRightForNonTargets $ dup3 x' & _1   %~ (mkMyName True <>)
                                                                   & _2   %~ (ser           <>)
                                                                   & _3   %~ (ser           <>)
          | otherwise            -> mkRightForNonTargets . dup3 $ x
        expandEnc isHead = (isHead ? dup ser :? dup ser') |&| uncurry (mkMyName isHead, , )
        mkMyName  isHead = onTrue isHead capitalize . onTrue (isNpc i ms) theOnLower . getSing i $ ms
    in case lefts xformed of
      []      -> let (msg, toOthers, targetIds, toTargetBs) = happyTimes ms xformed
                     toSelf                                 = parseInBands       i ms msg
                     logMsg                                 = parseInBandsSuffix i ms msg
                 in do logPlaOut "emote" i . pure $ logMsg
                       wrapSend mq cols toSelf
                       bcastIfNotIncogNl i $ (toOthers, desigIds d \\ (i : targetIds)) : toTargetBs
                       alertMsgHelper i "emote" logMsg
      advices -> multiWrapSend mq cols . nub $ advices
  where
    procTarget ms word =
        case swap . (both %~ T.reverse) . T.span isPunc . T.reverse $ word of
          ("",   _) -> Left . adviceEtc $ "emote "
          ("'s", _) -> Left adviceEtcBlankPoss
          (w,    p) ->
            let (isPoss, target) = ("'s" `T.isSuffixOf` w ? (True, T.dropEnd 2) :? (False, id)) & _2 %~ (w |&|)
                invCoins         = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
            in if ()!# invCoins
              then case singleArgInvEqRm InRm target of
                (InInv, _      ) -> sorry sorryEmoteTargetInInv
                (InEq,  _      ) -> sorry sorryEmoteTargetInEq
                (InRm,  target') -> case uncurry (resolveRmInvCoins i ms . pure $ target') invCoins of
                  (_,                  [Left [msg]]) -> Left msg
                  (_,                  Right _:_   ) -> sorry sorryEmoteTargetCoins
                  ([Left  msg       ], _           ) -> Left msg
                  ([Right (_:_:_)   ], _           ) -> Left sorryEmoteExcessTargets
                  ([Right [targetId]], _           ) | targetSing <- getSing targetId ms ->
                    if not (isNpcPla targetId ms)
                      then Left . sorryEmoteTargetType $ targetSing
                      else let targetDesig = addSuffix isPoss p . serialize . mkStdDesig targetId ms $ Don'tCap
                           in Right ( targetDesig
                                    , [ mkEmoteWord isPoss p targetId, ForNonTargets targetDesig ]
                                    , targetDesig )
                  x -> pmf "emote procTarget" x
              else Left sorryNoOneHere
    addSuffix   isPoss p = (<> p) . onTrue isPoss (<> "'s")
    mkEmoteWord isPoss   = isPoss ? ForTargetPoss :? ForTarget
    sorry t              = Left . quoteWith' (t, sorryEmoteTargetRmOnly) $ " "
emote p = pmf "emote" p


-----


emptyAction :: HasCallStack => ActionFun
emptyAction p@AdviseNoArgs            = advise p ["empty"] adviceEmptyNoArgs
emptyAction   (LowerNub i mq cols as) = helper |&| modifyState >=> \(toSelfs, bs, logSings) -> do
    logSings |#| logPla "emptyAction" i . prd . ("emptied " <>) . commas
    multiWrapSend mq cols toSelfs
    bcastIfNotIncogNl i bs
  where
    helper ms =
        let (inInvs, inEqs, inRms)       = sortArgsInvEqRm InInv as
            (sorryInEq, sorryInRm)       = (inEqs |!| sorryEmptyInEq, inRms |!| sorryEmptyInRm)
            invCoins                     = getInvCoins i ms
            d                            = mkStdDesig  i ms DoCap
            (eiss, ecs)                  = uncurry (resolveMobInvCoins i ms inInvs) invCoins
            (ms', toSelfs, bs, logSings) = foldl' (helperEmptyEitherInv d) (ms, [], [], []) eiss
            toSelfs'                     = dropBlanks $ [ sorryInEq, sorryInRm, ecs |!| sorryEmptyCoins ] ++ toSelfs
        in genericCheckActing i ms (Right "empty a vessel") [ Attacking, Drinking, Sacrificing ] $ if ()!# invCoins
          then (ms', (toSelfs', bs, logSings))
          else genericSorry ms dudeYourHandsAreEmpty
    helperEmptyEitherInv d a = \case
      Left  msg -> a & _2 <>~ pure msg
      Right is  -> foldl' f a is
      where
        f a'@(ms, _, _, _) targetId =
            let s            = getSing targetId ms
                poss         = mkPossPro . getSex i $ ms
                t            = T.concat [ serialize d, " empties the contents of ", poss, " ", s, "." ]
                alreadyEmpty = a' & _2 <>~ pure (sorryEmptyAlready s)
                emptyIt      = a' & _1 .~  (ms & vesselTbl.ind targetId.vesselCont .~ Nothing)
                                  & _2 <>~ pure (prd $ "You empty the contents of the " <> s)
                                  & _3 <>~ pure (t, i `delete` desigIds d)
                                  & _4 <>~ pure s
            in case getType targetId ms of
              VesselType -> maybe alreadyEmpty (const emptyIt) . getVesselCont targetId $ ms
              ConType    -> a' & _2 <>~ pure sorryEmptyCon
              CorpseType -> a' & _2 <>~ pure sorryEmptyCorpse
              _          -> a' & _2 <>~ pure (sorryEmptyType s)
emptyAction p = pmf "emptyAction" p


-----


equip :: HasCallStack => ActionFun
equip (NoArgs   i mq cols   ) = getState >>= \ms -> send mq . nl . mkEqDesc i cols ms i (getSing i ms) $ PlaType
equip (LowerNub i mq cols as) = getState >>= \ms ->
    let em@(M.elems -> is) = getEqMap i ms in send mq $ if ()!# em
      then let (inInvs, inEqs, inRms)            = sortArgsInvEqRm InEq as
               (gecrs, miss, rcs)                = resolveEntCoinNames i ms inEqs is mempty
               eiss                              = zipWith (curry procGecrMisMobEq) gecrs miss
               invDesc                           = concatMapTxt helperEitherInv eiss
               helperEitherInv (Left  msg)       = wrapUnlinesNl cols msg
               helperEitherInv (Right targetIds) = nl . mkEntDescs i cols ms $ targetIds
               coinsDesc                         = rcs |!| wrapUnlinesNl cols sorryEquipCoins
           in T.concat [ inInvs |!| sorryInInv, inRms |!| sorryInRm, invDesc, coinsDesc ]
      else wrapUnlinesNl cols dudeYou'reNaked
  where
    sorryInInv = wrapUnlinesNl cols . sorryEquipInvLook EquipCmd $ InvCmd
    sorryInRm  = wrapUnlinesNl cols . sorryEquipInvLook EquipCmd $ LookCmd
equip p = pmf "equip" p


-----


exits :: HasCallStack => ActionFun
exits (NoArgs i mq cols) = sequence_ [ logPlaExec "exits" i, send mq . nl . mkExitsSummary cols . getMobRm i =<< getState ]
exits p                  = withoutArgs exits p


-----


expCmdList :: HasCallStack => ActionFun
expCmdList (NoArgs i mq cols) = do
    logPlaExecArgs "expressive" [] i
    pager i mq Nothing . concatMap (wrapIndent cmdNamePadding cols) . mkExpCmdListTxt i =<< getState
expCmdList (LowerNub i mq cols as) = do logPlaExecArgs "expressive" as i
                                        dispMatches i mq cols cmdNamePadding Isn'tRegex as . mkExpCmdListTxt i =<< getState
expCmdList p                       = pmf "expCmdList" p


mkExpCmdListTxt :: HasCallStack => Id -> MudState -> [Text]
mkExpCmdListTxt i ms =
    let cmdNames       = [ cmdName cmd | cmd <- mkPlaCmds i ms ]
        styledCmdNames = styleAbbrevs Don'tQuote cmdNames
    in concatMap mkExpCmdTxt [ (styled, head matches) | (cn, styled) <- zip cmdNames styledCmdNames
                                                      , let matches = findMatches cn
                                                      , length matches == 1 ]
  where
    findMatches cn = S.toList . S.filter (\(ExpCmd ecn _ _) -> ecn == cn) $ expCmdSet
    mkExpCmdTxt (styled, ExpCmd ecn ect mrd) = dropEmpties $ case ect of
      (NoTarget  toSelf _  ) -> [ paddedName <> mkInitialTxt  ecn <> toSelf
                                , mobRmDescHelper ]
      (HasTarget toSelf _ _) -> [ paddedName <> mkInitialTxt (ecn <> " hanako") <> T.replace "@" "Hanako" toSelf
                                , mobRmDescHelper ]
      (Versatile toSelf _ toSelfWithTarget _ _) -> [ paddedName <> mkInitialTxt ecn <> toSelf
                                                   , indent                          <>
                                                     mkInitialTxt (ecn <> " hanako") <>
                                                     T.replace "@" "Hanako" toSelfWithTarget
                                                   , mobRmDescHelper ]
      where
        paddedName         = padCmdName styled
        mkInitialTxt input = colorWith quoteColor input <> spaced (colorWith arrowColor "->")
        indent             = T.replicate cmdNamePadding (T.singleton indentFiller)
        mobRmDescHelper    = let ecn' = colorWith quoteColor ecn in case mrd of
          Nothing    -> ""
          (Just "" ) -> (indent <>) . parensQuote $ ecn' <> " clears room description."
          (Just txt) -> (indent <>) . parensQuote $ T.concat [ ecn', " sets room description to ", dblQuote txt, "." ]


-----


extinguish :: HasCallStack => ActionFun
extinguish p@(LowerNub' i as) = getState >>= \ms ->
    let f = genericActionWithFuns p helper "extinguish"
    in checkActing p ms (Right "extinguish a torch or lamp") [ Attacking, Drinking, Sacrificing ] f
  where
    helper _ ms = let (inInvs, inEqs, inRms) = sortArgsInvEqRm InEq as
                      sorryInRm              = inRms |!| sorryExtinguishInRm
                      (invCoins, eqMap)      = ((,) <$> uncurry getInvCoins <*> uncurry getEqMap) (i, ms)
                      d                      = mkStdDesig i ms DoCap
                      (gecrs, miss, rcs)     = resolveEntCoinNames i ms inEqs (M.elems eqMap) mempty
                      eiss                   = zipWith (curry procGecrMisMobEq) gecrs miss
                      (eiss', rcs')          = uncurry (resolveMobInvCoins i ms inInvs) invCoins
                      sorrys                 = [ ()!# rcs || ()!# rcs' |?| sorryExtinguishCoins, sorryInRm ]
                      extinguisher           = foldl' (helperExtinguishEitherInv i d)
                                                      (ms, (dropBlanks sorrys, [], [], []))
                      sorry                  = genericSorryWithFuns ms
                  in if | ()# invCoins, ()# eqMap -> sorry dudeYou'reScrewed
                        | ()# as,       ()# eqMap -> sorry dudeYou'reNaked
                        | ()# as                  -> case findLitReadiedLight ms eqMap of
                                                       Nothing -> sorry sorryExtinguishLight
                                                       Just i' -> extinguisher . pure $ (Right . pure $ i', TheEq)
                        | otherwise               -> extinguisher $ zip eiss (repeat TheEq) ++ zip eiss' (repeat TheInv)
    findLitReadiedLight ms eqMap = (mplus <$> (LHandS |&|) <*> (RHandS |&|)) g
      where
        g slot = case slot `M.lookup` eqMap of Nothing -> Nothing
                                               Just i' | getLightIsLit i' ms -> Just i' | otherwise -> Nothing
extinguish p = pmf "extinguish" p


-----


feeling :: HasCallStack => ActionFun
feeling (NoArgs i mq cols) = spiritHelper i a b
  where
    a ms = let txts = f . dropEmpties $ [ g i ms | g <- [ mkHpDesc
                                                        , mkMpDesc
                                                        , mkPpDesc
                                                        , mkFpDesc
                                                        , mkEffStDesc
                                                        , mkEffDxDesc
                                                        , mkEffHtDesc
                                                        , mkEffMaDesc
                                                        , mkEffPsDesc
                                                        , mkFullDesc ] ] ++ mkFeelingDescs i ms
           in sequence_ [ logPla "feeling" i . dropANSI . slashes $ txts, multiWrapSend mq cols txts ]
    f ts = ()# ts ? pure "You feel fine." :? ts
    b    = const . wrapSend mq cols $ msg
    msg  = "You can still feel emotions, and your sense of self remains intact. At the same time, you are entirely \
           \detached from your body. The whole experience is quite surreal."
feeling p = withoutArgs feeling p


mkFeelingDescs :: HasCallStack => Id -> MudState -> [Text]
mkFeelingDescs i ms = M.foldrWithKey helper [] . getFeelingMap i $ ms
  where
    helper tag (Feeling fv _ _) = (getFeelingFun tag ms fv :)


-----


felinoidean :: HasCallStack => ActionFun
felinoidean = sayHelper FelinoidLang


-----


fill :: HasCallStack => RmActionFun
fill p@AdviseNoArgs  = advise p [] adviceFillNoArgs
fill p@AdviseOneArg  = advise p [] adviceFillNoSource
fill p@(Lower' i as) = getState >>= \ms ->
    checkActing p ms (Right "fill a vessel") [ Attacking, Drinking, Sacrificing ] . genericActionWithFuns p helper $ "fill"
  where
    helper v ms =
        let b@LastArgIsTargetBindings { .. } = mkLastArgIsTargetBindings i ms as
            maybeHooks                       = lookupHooks i ms "fill"
            sorry                            = genericSorryWithFuns ms
        in if ()# srcInvCoins
          then sorry dudeYourHandsAreEmpty
          else case singleArgInvEqRm InInv targetArg of
            (InInv, target) ->
                let (eiss, _)    = uncurry (resolveMobInvCoins i ms . pure $ target) srcInvCoins
                    f [targetId] | getType targetId ms /= VesselType = sorry . sorryFillSourceType . getSing targetId $ ms
                                 | otherwise                         = fillHelper i ms b targetId
                    f _          = sorry sorryFillExcessSources
                in case eiss of []      -> sorry sorryFillSourceCoins
                                (eis:_) -> either sorry f eis
            (InEq, _     ) -> sorry sorryFillSourceEq
            (InRm, target)
              | ()# rmInvCoins, ()# maybeHooks -> sorry sorryFillEmptyRmNoHooks
              | otherwise                      ->
                  let hookArg = T.intercalate (T.singleton hookArgDelimiter) $ target : otherArgs
                  in case ((()!#) *** (()!#)) (rmInvCoins, maybeHooks) of
                    (True,  False) -> sorry sorryFillRmNoHooks
                    (False, True ) ->
                        let (otherArgs', (ms', toSelfs, bs, logMsgs), fs) = procHooks i ms v "fill" . pure $ hookArg
                            sorryMsgs                                     = otherArgs' |!| pure sorryFillEmptyRmWithHooks
                        in (ms', (dropBlanks $ sorryMsgs ++ toSelfs, bs, logMsgs, fs))
                    (True,  True ) ->
                        let (otherArgs', (ms', toSelfs, bs, logMsgs), fs) = procHooks i ms v "fill" . pure $ hookArg
                        in if ()# otherArgs'
                          then (ms', (toSelfs, bs, logMsgs, fs))
                          else sorry . sorryFillRmWithHooks $ target
                    a -> pmf "fill helper" a
fill p = pmf "fill" p


-----


getAction :: HasCallStack => ActionFun
getAction p@AdviseNoArgs         = advise p ["get"] adviceGetNoArgs
getAction p@(Lower i mq cols as) = getState >>= \ms -> case reverse as of
  (_:"from":_:_) -> wrapSend mq cols hintGet
  _              -> let f = genericActionWithFuns p helper "get"
                    in checkActing p ms (Right "pick up an item") [ Drinking, Sacrificing ] f
  where
    helper v ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm . nub $ as
            sorrys                 = dropEmpties [ inInvs |!| sorryGetInInv, inEqs |!| sorryGetInEq ]
            d                      = mkStdDesig i ms DoCap
            ri                     = getRmId i ms
            invCoins               = first (i `delete`) . getVisibleInvCoins ri $ ms
        in case ((()!#) *** (()!#)) (invCoins, lookupHooks i ms "get") of
          (False, False) -> (ms, (pure sorryGetEmptyRmNoHooks, [], [], []))
          -----
          (True,  False) -> let (ms', (toSelfs, bs, logMsgs)) = invCoinsHelper ms inRms d ri invCoins
                            in (ms', (sorrys ++ toSelfs, bs, logMsgs, []))
          -----
          (False, True ) -> let (inRms', (ms', toSelfs, bs, logMsgs), fs) = procHooks i ms v "get" inRms
                                sorrys' = sorrys ++ (inRms' |!| pure sorryGetEmptyRmWithHooks)
                            in (ms', (sorrys' ++ toSelfs, bs, logMsgs, fs))
          -----
          (True,  True ) ->
            let (inRms', (ms', hooksToSelfs, hooksBs, hooksLogMsgs), fs) = procHooks i ms v "get" inRms
                (ms'', (invCoinsToSelfs, invCoinsBs, invCoinsLogMsgs))   = invCoinsHelper ms' inRms' d ri invCoins
            in (ms'', ( concat [ sorrys, hooksToSelfs, invCoinsToSelfs ]
                      , hooksBs      ++ invCoinsBs
                      , hooksLogMsgs ++ invCoinsLogMsgs
                      , fs ))
    invCoinsHelper ms args d ri invCoins =
        let (eiss, ecs)                     = uncurry (resolveRmInvCoins i ms args) invCoins
            (ms',  toSelfs,  bs,  logMsgs ) = foldl' (helperGetEitherInv       i d     ri)  (ms,  [],      [], []     ) eiss
            (ms'', toSelfs', bs', logMsgs') =         helperGetDropEitherCoins i d Get ri i (ms', toSelfs, bs, logMsgs) ecs
        in (ms'', (toSelfs', bs', logMsgs'))
getAction p = pmf "getAction" p


-----


give :: HasCallStack => ActionFun
give p@AdviseNoArgs  = advise p ["give"] adviceGiveNoArgs
give p@AdviseOneArg  = advise p ["give"] adviceGiveNoName
give p@(Lower' i as) = genericAction p helper "give"
  where
    helper ms =
        let b@LastArgIsTargetBindings { targetArg } = mkLastArgIsTargetBindings i ms as
            f    = genericCheckActing i ms (Right "give an item to a person") [ Drinking, Sacrificing ] next
            next = case singleArgInvEqRm InRm targetArg of
              (InInv, _     ) -> genericSorry ms sorryGiveToInv
              (InEq,  _     ) -> genericSorry ms sorryGiveToEq
              (InRm,  target) -> shuffleGive i ms b { targetArg = target }
        in maybe f (genericSorry ms) . emptyInvChecks $ b
    emptyInvChecks LastArgIsTargetBindings { srcInvCoins, rmInvCoins } =
        ( ()# srcInvCoins |?| Just dudeYourHandsAreEmpty
        , ()# rmInvCoins  |?| Just sorryNoOneHere ) |&| uncurry mplus
give p = pmf "give" p


-----


go :: HasCallStack => Text -> ActionFun
go dir p@(NoArgs i mq cols) = getState >>= \ms ->
    let f = bool (wrapSend mq cols exhaustedMsg) (tryMove i mq cols dir) . (> 0) . fst . getFps i $ ms
    in checkActing p ms (Right "move") (pure Sacrificing) f
go dir p = withoutArgs (go dir) p


tryMove :: HasCallStack => Id -> MsgQueue -> Cols -> Text -> MudStack ()
tryMove i mq cols dir = helper |&| modifyState >=> \case Left  msg          -> wrapSend mq cols msg
                                                         Right (bs, logMsg) -> do logPla "tryMove" i logMsg
                                                                                  sendGmcpRmInfo Nothing i =<< getState
                                                                                  look . ActionParams i mq cols $ []
                                                                                  bcastIfNotIncog i bs
  where
    helper ms = let { originId = getRmId i ms; originRm = getRm originId ms } in case findExit originRm dir of
      Nothing -> (ms, Left sorry)
      Just (linkTxt, destId, moveCost, maybeOriginMsg, maybeDestMsg) ->
          let originDesig  = mkStdDesig i ms DoCap
              s            = getSing    i ms
              originMobIds = i `delete` desigIds originDesig
              destMobIds   = findMobIds ms . getInv destId $ ms
              ms'          = upd ms [ mobTbl.ind i.rmId      .~ destId
                                    , mobTbl.ind i.lastRmId  .~ originId
                                    , mobTbl.ind i.curFp     %~ moveCostHelper
                                    , mobTbl.ind i.mobRmDesc .~ Nothing
                                    , invTbl.ind originId    %~ (i `delete`)
                                    , invTbl.ind destId      %~ addToInv ms (pure i) ]
              msgAtOrigin  = nlnl $ case maybeOriginMsg of
                               Nothing  -> T.concat [ serialize originDesig, spaced verb, expandLinkName dir, "." ]
                               Just msg -> T.replace "%" (serialize originDesig) msg
              msgAtDest    = let destDesig = mkSerializedNonStdDesig i ms s A DoCap in nlnl $ case maybeDestMsg of
                               Nothing  -> T.concat [ destDesig, " arrives from ", expandOppLinkName dir, "." ]
                               Just msg -> T.replace "%" destDesig msg
              logMsg       = let { fromTxt = showRm originId originRm; toTxt = showRm destId . getRm destId $ ms }
                             in T.concat [ "moved ", linkTxt, " from room ", fromTxt, " to room ", toTxt, "." ]
              moveCostHelper x | uncurry (||) . (isAdminId `fanUncurry` isSpiritId) $ (i, ms) = x
                               | otherwise = subtract moveCost x
          in (ms', Right (not (isSpiritId i ms) |?| [ (msgAtOrigin, originMobIds), (msgAtDest, destMobIds) ], logMsg))
    sorry     = dir `elem` stdLinkNames ? sorryGoExit :? sorryGoParseDir dir
    verb      | dir == "u"              = "goes"
              | dir == "d"              = "heads"
              | dir `elem` stdLinkNames = "leaves"
              | otherwise               = "enters"
    showRm ri = ((|<>|) <$> showTxt . fst <*> views rmName parensQuote . snd) . (ri, )


findExit :: HasCallStack => Rm -> LinkName -> Maybe (Text, Id, MoveCost, Maybe Text, Maybe Text)
findExit rm ln = case views rmLinks (filter isValid) rm of
  []     -> Nothing
  (rl:_) -> Just . ((,,,,) <$> getLinkName <*> getDestId <*> getMoveCost <*> getOriginMsg <*> getDestMsg) $ rl
  where
    isValid      StdLink    { .. } = ln == linkDirToCmdName _slDir
    isValid      NonStdLink { .. } = ln `T.isPrefixOf` _nslName
    getLinkName  StdLink    { .. } = showTxt _slDir
    getLinkName  NonStdLink { .. } = _nslName
    getDestId    StdLink    { .. } = _slDestId
    getDestId    NonStdLink { .. } = _nslDestId
    getMoveCost  StdLink    { .. } = _slCost
    getMoveCost  NonStdLink { .. } = _nslCost
    getOriginMsg NonStdLink { .. } = Just _nslOriginMsg
    getOriginMsg _                 = Nothing
    getDestMsg   NonStdLink { .. } = Just _nslDestMsg
    getDestMsg   _                 = Nothing


-- The following 3 functions are in this module because they reference "go" (which references "look")...
mkNonStdRmLinkCmds :: HasCallStack => Rm -> [Cmd]
mkNonStdRmLinkCmds (view rmLinks -> rls) = [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


mkCmdForRmLink :: HasCallStack => RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) =
    Cmd { cmdName = cn, cmdPriorityAbbrev = Nothing, cmdFullName = cn, cmdAction = Action (go cn) True, cmdDesc = "" }


mkCmdNameForRmLink :: HasCallStack => RmLink -> Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _slDir
                                               NonStdLink { .. } -> _nslName


 -----


help :: HasCallStack => ActionFun
help (NoArgs i mq cols) = liftIO (T.readFile =<< mkMudFilePath rootHeplFileFun) |&| try >=> either handler helper
  where
    handler e          = fileIOExHandler "help" e >> wrapSend mq cols helpRootErrorMsg
    helper rootHelpTxt = ((,) <$> getState <*> getServerSettings) >>= \(ms, s) -> do
        logPla "help" i "reading the root help file."
        let (is, ia, ls) = mkHelpTriple i ms
        hs <- sortBy (compare `on` helpName) <$> liftIO (mkHelpData ls is ia)
        let zipped                 = zip (styleAbbrevs Don'tQuote [ helpName h | h <- hs ]) hs
            (cmdNames, topicNames) = partition (isCmdHelp . snd) zipped & both %~ (formatHelpNames . mkHelpNames)
            helpTxt                = T.concat [ nl rootHelpTxt
                                              , nl "COMMANDS:"
                                              , nl cmdNames
                                              , nl "TOPICS:"
                                              , topicNames
                                              , ia |?| footnote ]
        pager i mq Nothing . parseHelpTxt s (mkPlaCmds i ms) cols $ helpTxt
    mkHelpNames zipped    = [ padHelpTopic . (styled <>) $ isAdminHelp h |?| asterisk | (styled, h) <- zipped ]
    formatHelpNames names = let wordsPerLine = cols `div` helpTopicPadding
                            in T.unlines . map T.concat . chunksOf wordsPerLine $ names
    footnote              = nlPrefix $ asterisk <> " indicates help that is available only to administrators."
help (LowerNub i mq cols as) = ((,) <$> getState <*> getServerSettings) >>= \(ms, s) -> do
    let (is, ia, ls) = mkHelpTriple i ms
    hs <- liftIO . mkHelpData ls is $ ia
    (map (parseHelpTxt s (mkPlaCmds i ms) cols) -> helpTxts, dropBlanks -> hns) <- unzip <$> forM as (getHelpByName cols hs)
    hns |#| logPla "help" i . prd . ("reading help on: " <>) . commas
    pager i mq Nothing . intercalateDivider cols $ helpTxts
help p = pmf "help" p


mkHelpTriple :: HasCallStack => Id -> MudState -> (Bool, Bool, [Lang])
mkHelpTriple i ms = (isSpiritId i ms, isAdminId i ms, getKnownLangs i ms)


mkHelpData :: HasCallStack => [Lang] -> Bool -> Bool -> IO [Help]
mkHelpData ls is ia = do
    let f dir = [ (dir', dropIrrelevantFiles . sort $ cont) | dir' <- mkMudFilePath dir
                                                            , cont <- getDirectoryContents dir' ]
    [  (plaHelpCmdsDir,     plaHelpCmdNames    )
     , (plaHelpTopicsDir,   plaHelpTopicNames  )
     , (adminHelpCmdsDir,   adminHelpCmdNames  )
     , (adminHelpTopicsDir, adminHelpTopicNames) ] <- mapM f [ plaHelpCmdsDirFun
                                                             , plaHelpTopicsDirFun
                                                             , adminHelpCmdsDirFun
                                                             , adminHelpTopicsDirFun ]
    let phcs = [ Help { helpName     = T.pack x
                      , helpFilePath = plaHelpCmdsDir </> x
                      , isCmdHelp    = True
                      , isAdminHelp  = False } | x <- filterLangCmds plaHelpCmdNames ]
        phts = [ Help { helpName     = T.pack x
                      , helpFilePath = plaHelpTopicsDir </> x
                      , isCmdHelp    = False
                      , isAdminHelp  = False } | x <- plaHelpTopicNames ]
        shcs = [ Help { helpName     = T.pack x
                      , helpFilePath = plaHelpCmdsDir </> x
                      , isCmdHelp    = True
                      , isAdminHelp  = False } | x <- filterSpiritCmds plaHelpCmdNames ]
        ahcs = [ Help { helpName     = T.pack $ adminCmdChar : x
                      , helpFilePath = adminHelpCmdsDir </> x
                      , isCmdHelp    = True
                      , isAdminHelp  = True } | x <- adminHelpCmdNames ]
        ahts = [ Help { helpName     = T.pack x
                      , helpFilePath = adminHelpTopicsDir </> x
                      , isCmdHelp    = False
                      , isAdminHelp  = True } | x <- adminHelpTopicNames ]
    return $ (is ? shcs :? phcs) ++ phts ++ (guard ia >> ahcs ++ ahts)
  where
    filterLangCmds = filter helper
      where
        helper (T.pack -> cn)
          | allRacialSays <- map mkCmdNameForLang langsNoCommon
          , myRacialSays  <- map mkCmdNameForLang ls
          , cn `elem` allRacialSays
          = cn `elem` myRacialSays
          | otherwise = otherwise
    filterSpiritCmds cns = [ cn | cn <- cns, let scns = map cmdName spiritCmds, T.pack cn `elem` scns ]


parseHelpTxt :: HasCallStack => ServerSettings -> [Cmd] -> Cols -> Text -> [Text]
parseHelpTxt s cmds cols txt = [ procCmdTokens . xformLeadingSpaceChars . expandDividers cols $ t | t <- parseWrap s cols txt ]
  where
    procCmdTokens (T.uncons -> Just (x, xs)) | x == cmdToken = helper
      where
        helper                = let (cn, rest) = T.break isSpace xs
                                in maybe xs (<> rest) . lookup cn . map f . mkCmdTriplesForStyling $ cmds
        f a@(_,  Nothing,  _) = dropSnd a
        f   (cn, Just cpa, _) = let a = cpa `T.stripPrefix` cn
                                in (cn, ) . uncurry (<>) . first (colorWith abbrevColor) . maybe (cn, "") (cpa, ) $ a
    procCmdTokens l = l


getHelpByName :: HasCallStack => Cols -> [Help] -> HelpName -> MudStack (Text, Text)
getHelpByName cols hs name = findFullNameForAbbrev name [ (id &&& helpName) h | h <- hs ] |&| maybe sorry found
  where
    sorry                                      = return (sorryHelpName name, "")
    found (helpFilePath -> hf, dblQuote -> hn) = (,) <$> readHelpFile hf hn <*> return hn
    readHelpFile hf hn                         = liftIO (T.readFile hf) |&| try >=> eitherRet handler
      where
        handler e = do
            fileIOExHandler "getHelpByName readHelpFile" e
            return . wrapUnlines cols . helpFileErrorMsg $ hn


-----


hobbitish :: HasCallStack => ActionFun
hobbitish = sayHelper HobbitLang


-----


hominal :: HasCallStack => ActionFun
hominal = sayHelper HumanLang


-----


intro :: HasCallStack => ActionFun
intro (NoArgs i mq cols) = getState >>= \ms -> let intros = getIntroduced i ms in if ()# intros
  then let introsTxt = "No one has introduced themselves to you yet."
       in logPlaOut "intro" i (pure introsTxt) >> wrapSend mq cols introsTxt
  else let introsTxt = commas intros in do logPla "intro" i $ "known names: " <> introsTxt
                                           multiWrapSend mq cols [ "You know the following names:", introsTxt ]
intro p@(LowerNub i mq cols as) = getState >>= \ms ->
    checkActing p ms (Right "introduce yourself") [ Attacking, Drinking, Sacrificing ] $ if isIncognitoId i ms
      then wrapSend mq cols . sorryIncog $ "intro"
      else helper |&| modifyState >=> \(map fromClassifiedBcast . sort -> bs, logMsgs, intro'dIds) -> do
        logMsgs |#| logPla "intro" i . slashes
        bcast bs
        mapM_ (awardExp 50 (getSing i ms <> " introduced")) intro'dIds
  where
    helper ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorryInInv = inInvs |!| mkNTB sorryIntroInInv
            sorryInEq  = inEqs  |!| mkNTB sorryIntroInEq
            invCoins@(first (i `delete`) -> invCoins') = getMobRmVisibleInvCoins i ms
            (eiss, ecs) = uncurry (resolveRmInvCoins i ms inRms) invCoins'
            ris         = fst invCoins
            (pt, cbs,  logMsgs, intro'dIds) = foldl' (helperIntroEitherInv ms ris) (ms^.pcTbl, [], [], []) eiss
            (    cbs', logMsgs'           ) = foldl' helperIntroEitherCoins        (cbs, logMsgs         ) ecs
        in if ()!# invCoins'
          then (ms & pcTbl .~ pt, (sorryInInv ++ sorryInEq ++ cbs', logMsgs', intro'dIds))
          else (ms,               (mkNTB sorryIntroNoOneHere,       [],       []        ))
    mkNTB                                           = mkNTBcast i . nlnl
    helperIntroEitherInv _  _   a (Left msg       ) = ()# msg ? a :? (a & _2 <>~ mkNTB msg)
    helperIntroEitherInv ms ris a (Right targetIds) = foldl' tryIntro a targetIds
      where
        tryIntro a'@(pt, _, _, _) targetId = let targetSing = getSing targetId ms in case getType targetId ms of
          PlaType -> let s           = getSing i ms
                         targetDesig = serialize . mkStdDesig targetId ms $ Don'tCap
                         msg         = prd $ "You introduce yourself to " <> targetDesig
                         logMsg      = prd $ "Introduced to " <> targetSing
                         srcMsg      = nlnl msg
                         is          = findMobIds ms ris
                         srcDesig    = StdDesig { desigDoExpandSing = False
                                                , desigEntName      = mkUnknownPCEntName i ms
                                                , desigCap          = DoCap
                                                , desigId           = i
                                                , desigIds          = is }
                         himHerself  = mkReflexPro . getSex i $ ms
                         targetMsg   = nlnl . T.concat $ [ parseInBands targetId ms . serialize $ srcDesig
                                                         , " introduces "
                                                         , himHerself
                                                         , " to you as "
                                                         , colorWith knownNameColor s
                                                         , "." ]
                         othersMsg   = nlnl . T.concat $ [ serialize srcDesig { desigDoExpandSing = True }
                                                         , " introduces "
                                                         , himHerself
                                                         , " to "
                                                         , targetDesig
                                                         , "." ]
                         cbs         = [ NonTargetBcast (srcMsg,    pure i               )
                                       , TargetBcast    (targetMsg, pure targetId        )
                                       , NonTargetBcast (othersMsg, is \\ [ i, targetId ]) ]
                     in if s `elem` pt^.ind targetId.introduced
                       then let sorry = nlnl . sorryIntroAlready $ targetDesig
                            in a' & _2 <>~ mkNTBcast i sorry
                       else a' & _1.ind targetId.introduced %~ (sort . (s :))
                               & _2 <>~ cbs
                               & _3 <>~ pure logMsg
                               & _4 %~  (targetId :)
          _       -> let b = head . mkNTB . sorryIntroType $ targetSing
                     in a' & _2 %~ (`appendIfUnique` b)
    helperIntroEitherCoins a (Left msgs)   = a & _1 <>~ (mkNTBcast i . T.concat $ [ nlnl msg | msg <- msgs ])
    helperIntroEitherCoins a Right {}      = let cb = head . mkNTB $ sorryIntroCoin
                                             in first (`appendIfUnique` cb) a
    fromClassifiedBcast (TargetBcast    b) = b
    fromClassifiedBcast (NonTargetBcast b) = b
intro p = pmf "intro" p


-----


inv :: HasCallStack => ActionFun
inv (NoArgs   i mq cols   ) = getState >>= \ms@(getSing i -> s) -> send mq . nl . mkInvCoinsDesc i cols ms i $ s
inv (LowerNub i mq cols as) = getState >>= \ms ->
    let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
        invCoins               = getInvCoins i ms
        (eiss, ecs)            = uncurry (resolveMobInvCoins i ms inInvs) invCoins
        invDesc                = concatMapTxt (helperEitherInv ms) eiss
        coinsDesc              = concatMapTxt helperEitherCoins    ecs
    in send mq $ if ()!# invCoins
      then T.concat [ inEqs |!| sorryInEq, inRms |!| sorryInRm, invDesc, coinsDesc ]
      else wrapUnlinesNl cols dudeYourHandsAreEmpty
  where
    helperEitherInv _  (Left  msg ) = wrapUnlinesNl cols msg
    helperEitherInv ms (Right is  ) = nl . mkEntDescs i cols ms $ is
    helperEitherCoins  (Left  msgs) = multiWrapNl cols . intersperse "" $ msgs
    helperEitherCoins  (Right c   ) = nl . mkCoinsDesc cols $ c
    sorryInEq                       = wrapUnlinesNl cols . sorryEquipInvLook InvCmd $ EquipCmd
    sorryInRm                       = wrapUnlinesNl cols . sorryEquipInvLook InvCmd $ LookCmd
inv p = pmf "inv" p


-----


lagomorphean :: HasCallStack => ActionFun
lagomorphean = sayHelper LagomorphLang


-----


leave :: HasCallStack => ActionFun
leave p@AdviseNoArgs                     = advise p ["leave"] adviceLeaveNoArgs
leave p@(WithArgs i mq cols (nub -> as)) =
    let next = helper |&| modifyState >=> \(ms, chanId_name_isDels, sorryMsgs) ->
          let s                              = getSing i ms
              (chanIds, chanNames, chanRecs) = foldl' unzipper ([], [], []) chanId_name_isDels
              unzipper acc (ci, cn, isDel)
                | isDel     = acc & _2 <>~ pure cn
                                  & _3 <>~ (pure . ChanRec "" ci cn s . asteriskQuote $ "Channel deleted.")
                | otherwise = acc & _1 <>~ pure ci
                                  & _2 <>~ pure cn
              toSelfMsgs = mkLeaveMsg chanNames
              msgs       = ()# sorryMsgs ? toSelfMsgs :? sorryMsgs ++ (toSelfMsgs |!| "" : toSelfMsgs)
              f bs ci    | c        <- getChan ci ms
                         , cn       <- c^.chanName
                         , g        <- filter (`isAwake` ms) . map (`getIdForPCSing` ms) . M.keys . M.filter id
                         , otherIds <- views chanConnTbl g c
                         = (bs ++) <$> forM otherIds (\i' -> [ (leftChanMsg n cn, pure i')
                                                             | n <- getRelativePCName ms (i', i) ])
          in do chanNames |#| logPla "leave" i . commas
                multiWrapSend mq cols msgs
                bcastNl =<< foldM f [] chanIds
                ts <- liftIO mkTimestamp
                withDbExHandler_ "leave" . forM_ chanRecs $ \cr -> insertDbTblChan cr { dbTimestamp = ts }
    in getState >>= \ms -> checkActing p ms (Right "sever a connection") (pure Sacrificing) next
  where
    helper ms = let (ms', chanId_name_isDels, sorryMsgs) = foldl' f (ms, [], []) as
                in (ms', (ms', chanId_name_isDels, sorryMsgs))
      where
        f triple a@(T.toLower -> a') =
            let notFound     = triple & _3 <>~ pure (sorryChanName a)
                found match  = let (cn, c) = getMatchingChanWithName match cns cs
                                   ci      = c^.chanId
                                   isDel   = views chanConnTbl ((== 1) . M.size) c
                               in (triple & _2 <>~ pure (ci, cn, isDel) |&|) $ if isDel
                                 then _1.chanTbl.at  ci                  .~ Nothing
                                 else _1.chanTbl.ind ci.chanConnTbl.at s .~ Nothing
                (cs, cns, s) = mkChanBindings i ms
            in findFullNameForAbbrev a' (map T.toLower cns) |&| maybe notFound found
    mkLeaveMsg []     = []
    mkLeaveMsg ns@[_] = pure    . mkMsgHelper False $ ns
    mkLeaveMsg ns     = T.lines . mkMsgHelper True  $ ns
    mkMsgHelper isPlur (map dblQuote -> ns) =
        T.concat [ focusingInnateMsg
                 , "you sever your telepathic connection"
                 , sOnTrue isPlur
                 , " to the "
                 , isPlur ? (nl "following channels:" <> commas ns) :? (head ns <> " channel")
                 , "." ]
leave p = pmf "leave" p


-----


light :: HasCallStack => ActionFun
light p@AdviseNoArgs            = advise p ["light"] adviceLightNoArgs
light p@(OneArgLower' _ a     ) = lightUp p a Nothing
light p@(WithArgs _ _ _ [a, b]) = lightUp p a . Just $ b
light p                         = advise p ["light"] adviceLightExcessArgs


lightUp :: HasCallStack => ActionParams -> Text -> Maybe Text -> MudStack ()
lightUp p@(WithArgs i _ _ _) lightArg fireArg = getState >>= \ms ->
    let f = genericActionWithFuns p helper "light"
    in checkActing p ms (Right "light a torch or lamp") [ Attacking, Drinking, Sacrificing ] f
  where
    helper _ ms =
        let (invCoins@(is, _), eqMap) = (getInvCoins `fanUncurry` getEqMap) (i, ms)
            (inInvs, inEqs, inRms)    = sortArgsInvEqRm InEq . pure $ lightArg
            (gecrs, miss, _)          = resolveEntCoinNames i ms inEqs (M.elems eqMap) mempty
            eqEiss                    = zipWith (curry procGecrMisMobEq) gecrs miss
            (invEiss, _)              = uncurry (resolveMobInvCoins i ms inInvs) invCoins
            f [lightId]               = either sorry (g lightId) procFireArg
            f _                       = sorry sorryLightExcessLights
            g lightId [fireId]        = if
              | getType lightId ms /= LightType -> sorry . sorryLightLightType $ lightSing
              | getLightIsLit lightId ms        -> sorry . sorryLightLit $ lightSing
              | secs <= 0                       -> sorry $ case sub of Torch    -> sorryLightTorchSecs
                                                                       (Lamp _) -> sorryLightLampSecs
              | otherwise ->
                  let (fireType, fireSing) = ((,) <$> uncurry getType <*> uncurry getSing) (fireId, ms)
                      toSelf    = prd $ "You light the " <> lightSing
                      d         = mkStdDesig i ms DoCap
                      isInInv   = lightId `elem` is
                      bs        = let t1 = isInInv ? aOrAn lightSing :? (mkPossPro (getSex i ms) |<>| lightSing)
                                      t2 = isInInv |?| spcL (parensQuote "carried")
                                  in pure (T.concat [ serialize d, " lights ", t1, t2, "." ], i `delete` desigIds d)
                      logMsg    = let t = spcL . parensQuote . ("in " <>) $ (isInInv ? "inventory" :? "readied equipment")
                                  in prd $ "lighting " <> aOrAn lightSing <> t
                      res       = ( ms & lightTbl.ind lightId.lightIsLit .~ True
                                  , (pure toSelf, bs, pure logMsg, pure . startLightTimer $ lightId) )
                      sorryFire = sorry . sorryLightFireType $ fireSing
                  in case fireType of ObjType   | fireSing /= "tinderbox"         -> sorryFire
                                                | otherwise                       -> res
                                      LightType | fireId == lightId               -> sorry . sorryLightSelf      $ lightSing
                                                | not . getLightIsLit fireId $ ms -> sorry . sorryLightFireUnlit $ fireSing
                                                | otherwise                       -> res
                                      _                                           -> sorryFire
              where
                (lightSing, sub, secs) = ((,,) <$> uncurry getSing <*> uncurry getLightSub <*> uncurry getLightSecs)
                                         (lightId, ms)
            g _ _ = sorry sorryLightExcessFires
            procFireArg = case fireArg of
              Nothing   -> let h i' = ((&&) <$> (== ObjType) . uncurry getType <*> (== "tinderbox") . uncurry getSing)
                                      (i', ms)
                           in case filter h is of (x:_) -> Right . pure $ x
                                                  []    -> Left sorryLightTinderbox
              Just fire -> let (inInvs', inEqs', inRms') = sortArgsInvEqRm InInv . pure $ fire
                               (invEiss', _)             = uncurry (resolveMobInvCoins i ms inInvs') invCoins
                               (gecrs', miss', _)        = resolveEntCoinNames i ms inEqs' (M.elems eqMap) mempty
                               eqEiss'                   = zipWith (curry procGecrMisMobEq) gecrs' miss'
                           in if | ()!# inInvs', ()# invCoins -> Left dudeYourHandsAreEmpty
                                 | ()!# inEqs',  M.null eqMap -> Left dudeYou'reNaked
                                 | ()!# inRms'                -> Left sorryLightFireInRm
                                 | otherwise                  -> case (eqEiss', invEiss') of (eis:_, []   ) -> eis
                                                                                             ([],    eis:_) -> eis
                                                                                             _              -> Left sorryLightFireCoins
            sorry = genericSorryWithFuns ms
        in if | ()#  invCoins, ()# eqMap    -> sorry dudeYou'reScrewed
              | ()!# inInvs,   ()# invCoins -> sorry dudeYourHandsAreEmpty
              | ()!# inEqs,    M.null eqMap -> sorry dudeYou'reNaked
              | ()!# inRms                  -> sorry sorryLightInRm
              | h <- either sorry f         -> case (eqEiss, invEiss) of (eis:_, []   ) -> h eis
                                                                         ([],    eis:_) -> h eis
                                                                         _              -> sorry sorryLightCoins
lightUp p _ _ = pmf "lightUp" p


-----


link :: HasCallStack => ActionFun
link (NoArgs i mq cols) = do
    ms  <- getState
    res <- helperLinkUnlink ms i mq cols
    flip maybeVoid res $ \(meLinkedToOthers, othersLinkedToMe, twoWays) ->
        let msgs                         = intercalate mMempty . dropEmpties $ [ twoWays       |!| twoWayMsgs
                                                                               , oneWaysFromMe |!| oneWayFromMeMsgs
                                                                               , oneWaysToMe   |!| oneWayToMeMsgs ]
            (oneWaysFromMe, oneWaysToMe) = ((,) <$> (meLinkedToOthers \\) <*> (othersLinkedToMe \\)) twoWays
            twoWayMsgs       = [ "Two-way links:",                mkSingsList True  twoWays       ]
            oneWayFromMeMsgs = [ "One-way links from your mind:", mkSingsList False oneWaysFromMe ]
            oneWayToMeMsgs   = [ "One-way links to your mind:",   mkSingsList False oneWaysToMe   ]
            mkSingsList doStyle ss = let (awakes, asleeps) = sortAwakesAsleeps ss
                                     in commas $ onTrue doStyle (styleAbbrevs Don'tQuote) awakes ++ asleeps
            sortAwakesAsleeps      = foldr sorter mempties
            sorter linkSing acc    =
                let linkId   = head . filter ((== linkSing) . (`getSing` ms)) $ ms^.pcTbl.to IM.keys
                    f lens x = acc & lens %~ (x' :)
                      where
                        x' = case view (at linkSing) . getTeleLinkTbl i $ ms of
                          Nothing  -> x
                          Just val -> val ? x :? x |<>| parensQuote "tuned out"
                in linkSing |&| (isAwake linkId ms ? f _1 :? f _2)
        in do logPla "link" i . slashes . dropBlanks $ [ twoWays       |!| "Two-way: "         <> commas twoWays
                                                       , oneWaysFromMe |!| "One-way from me: " <> commas oneWaysFromMe
                                                       , oneWaysToMe   |!| "One-way to me: "   <> commas oneWaysToMe ]
              multiWrapSend mq cols msgs
link p@(LowerNub i mq cols as) = getState >>= \ms -> if
  | isIncognitoId i ms -> wrapSend mq cols . sorryIncog $ "link"
  | isSpiritId    i ms -> wrapSend mq cols   sorryLinkSpirit
  | otherwise          -> let f                   = helper |&| modifyState >=> g
                              g (bs, logMsgs, fs) = logMsgs |#| (logPla "link" i . slashes) >> bcast bs >> sequence_ fs
                          in checkActing p ms (Right "establish a link") (pure Sacrificing) f
  where
    helper ms = let (inInvs, inEqs, inRms)  = sortArgsInvEqRm InRm as
                    sorryInInv              = inInvs |!| (mkBcast i . nlnl $ sorryLinkInInv)
                    sorryInEq               = inEqs  |!| (mkBcast i . nlnl $ sorryLinkInEq )
                    invCoins                = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
                    (eiss, ecs)             = uncurry (resolveRmInvCoins i ms inRms) invCoins
                    (ms', bs,  logMsgs, fs) = foldl' helperLinkEitherInv   (ms, [], [], []) eiss
                    (     bs', logMsgs'   ) = foldl' helperLinkEitherCoins (bs, logMsgs)    ecs
                in if ()!# invCoins
                  then (ms', (sorryInInv ++ sorryInEq ++ bs',        logMsgs', fs))
                  else (ms,  (mkBcast i . nlnl $ sorryLinkNoOneHere, [],       []))
    helperLinkEitherInv a (Left  sorryMsg ) = ()# sorryMsg ? a :? (a & _2 <>~ (mkBcast i . nlnl $ sorryMsg))
    helperLinkEitherInv a (Right targetIds) = foldl' tryLink a targetIds
      where
        tryLink a'@(ms, _, _, _) targetId = let targetSing = getSing targetId ms in case getType targetId ms of
          PlaType ->
            let (srcIntros, targetIntros) = f getIntroduced
                (srcLinks,  targetLinks ) = f getLinked
                f g                       = ((i |&|) &&& (targetId |&|)) (uncurry g . (, ms))
                s                         = getSing i ms
                targetDesig               = serialize . mkStdDesig targetId ms $ Don'tCap
                srcMsg    = nlnl . T.concat $ [ focusingInnateMsg
                                              , "you establish a telepathic connection from your mind to "
                                              , targetSing
                                              , "'s mind."
                                              , twoWayMsg ]
                twoWayMsg = isTwoWay |?| " This completes the psionic circuit."
                isTwoWay  = targetSing `elem` srcLinks
                logMsg    = T.concat [ "Established a ", oneTwoWay, " link with ", targetSing, "." ]
                oneTwoWay | isTwoWay  = "two-way"
                          | otherwise = "one-way"
                targetMsg = nlnl . T.concat $ [ "You sense an ephemeral blip in your psionic energy field as "
                                              , colorWith knownNameColor s
                                              , " establishes a telepathic connection from "
                                              , mkPossPro . getSex i $ ms
                                              , " mind to yours."
                                              , twoWayMsg ]
                bs            = [ (srcMsg, pure i), (targetMsg, pure targetId) ]
                msgHelper txt = a' & _2 <>~ mkBcast i (nlnl txt)
            in if | targetSing `notElem` srcIntros    -> msgHelper . sorryLinkIntroTarget       $ targetDesig
                  | s          `notElem` targetIntros -> msgHelper . sorryLinkIntroSelf         $ targetSing
                  | s             `elem` targetLinks  -> msgHelper . sorryLinkAlready oneTwoWay $ targetDesig
                  | not . hasPp i ms $ 10             -> msgHelper . sorryPp $ "link with " <> targetDesig
                  | otherwise                         ->
                      let g a'' | isTwoWay  = a''
                                | otherwise = a'' & _1.rndmNamesMstrTbl.ind i       .at targetSing .~ Nothing
                                                  & _1.rndmNamesMstrTbl.ind targetId.at s          .~ Nothing
                      in g $ a' & _1.pcTbl.ind targetId.linked %~ (sort . (s :))
                                & _1.teleLinkMstrTbl.ind i       .at targetSing ?~ True
                                & _1.teleLinkMstrTbl.ind targetId.at s          ?~ True
                                & _1.mobTbl         .ind i       .curPp         -~ 10
                                & _2 <>~ bs
                                & _3 <>~ pure logMsg
                                & _4 <>~ [ action, awardExp 100 ("linked by " <> targetSing) targetId ]
          _  -> let b = (nlnl . sorryLinkType $ targetSing, pure i)
                in a' & _2 %~ (`appendIfUnique` b)
          where
            action = rndmDo_ (calcProbLinkFlinch targetId ms) . mkExpAction "flinch" . mkActionParams targetId ms $ []
    helperLinkEitherCoins a (Left msgs) = a & _1 <>~ (mkBcast i . T.concat $ [ nlnl msg | msg <- msgs ])
    helperLinkEitherCoins a Right {}    = let b = (nlnl sorryLinkCoin, pure i) in first (`appendIfUnique` b) a
link p = pmf "link" p


-----


listen :: HasCallStack => ActionFun
listen (NoArgs i mq cols) = getState >>= \ms -> let humMsgs = mkHumMsgs ms
                                                    ts      = views rmListen (maybe a b) . getMobRm i $ ms
                                                    a       = ()# humMsgs ? pure noSoundMsg :? humMsgs
                                                    b       = onFalse (()# humMsgs) (++ humMsgs) . pure
                                                in multiWrapSend mq cols ts >> logPlaExec "listen" i
  where
    mkHumMsgs ms     = concatMap (helper ms) [ (\i' -> M.elems . getEqMap i', (<> " in your readied equipment"))
                                             , (getInv,                       (<> " in your inventory"        ))
                                             , (getMobRmInv,                  (<> " on the ground"            )) ]
    helper ms (f, g) = foldr (\i' acc -> maybe acc (: acc) . mkMaybeHumMsg i ms i' $ g) [] . uncurry f $ (i, ms)
listen p = withoutArgs listen p


-----


look :: HasCallStack => ActionFun
look (NoArgs i mq cols) = getState >>= \ms ->
    let ri        = getRmId i  ms
        r         = getRm   ri ms
        top       = fillerToSpcs . multiWrap cols $ theRmName : theRmDesc
        theRmName = views rmName (underline . quoteWith filler) r
        theRmDesc = views rmDesc formatRmDesc r
        bottom    = [ mkExitsSummary cols r, mkRmInvCoinsDesc i cols ms ri ]
    in send mq . nl . T.concat $ top : bottom
  where
    filler       = T.singleton indentFiller
    formatRmDesc = map (T.replicate rmDescIndentAmt filler <>) . T.lines
look (LowerNub i mq cols as) = mkRndmVector >>= \v ->
    helper v |&| modifyState >=> \(toSelf, bs, hookLogMsg, maybeTargetDesigs, fs) -> do
        ms <- getState
        let mkLogMsgForDesigs targetDesigs | targetSings <- [ parseInBandsSuffix i ms . serialize $ targetDesig
                                                            | targetDesig <- targetDesigs ]
                                           = "looking at " <> commas targetSings
            logMsg = T.intercalate " / " . dropBlanks $ [ maybeEmp mkLogMsgForDesigs maybeTargetDesigs, hookLogMsg ]
        logMsg |#| logPla "look" i . prd
        send mq toSelf
        unless (isSpiritId i ms) . bcastIfNotIncogNl i $ bs
        sequence_ fs
  where
    helper v ms =
        let invCoins               = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
            (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorry                  = T.concat [ inInvs |!| sorryInInv, inEqs |!| sorryInEq ]
            sorryInInv             = wrapUnlinesNl cols . sorryEquipInvLook LookCmd $ InvCmd
            sorryInEq              = wrapUnlinesNl cols . sorryEquipInvLook LookCmd $ EquipCmd
        in case ((()!#) *** (()!#)) (invCoins, lookupHooks i ms "look") of
          (False, False) -> (ms, (wrapUnlinesNl cols sorryLookEmptyRmNoHooks, [], "", Nothing, []))
          -----
          (True,  False) -> let (toSelf, bs, maybeDesigs) = invCoinsHelper ms inRms invCoins
                            in (ms, (sorry <> toSelf, bs, "", maybeDesigs, []))
          -----
          (False, True ) -> let (inRms', (ms', toSelf, bs, logMsg), fs) = hooksHelper ms v inRms
                                sorry' = sorry <> (inRms' |!| wrapUnlinesNl cols sorryLookEmptyRmWithHooks)
                            in (ms', (sorry' <> toSelf, bs, logMsg, Nothing, fs))
          -----
          (True,  True ) -> let (inRms', (ms', hooksToSelf, hooksBs, logMsg), fs) = hooksHelper ms v inRms
                                (invCoinsToSelf, invCoinsBs, maybeDesigs)         = invCoinsHelper ms' inRms' invCoins
                            in (ms', ( sorry <> hooksToSelf <> invCoinsToSelf
                                     , hooksBs ++ invCoinsBs
                                     , logMsg
                                     , maybeDesigs
                                     , fs ))
    -----
    invCoinsHelper ms args invCoins =
        let (eiss, ecs)  = uncurry (resolveRmInvCoins i ms args) invCoins
            invDesc      = concatMapTxt (helperLookEitherInv ms) eiss
            coinsDesc    = concatMapTxt helperLookEitherCoins    ecs
            selfDesig    = mkStdDesig i ms DoCap
            selfDesig'   = serialize selfDesig
            is           = i `delete` desigIds selfDesig
            targetDesigs = [ mkStdDesig targetId ms Don'tCap | targetId <- extractMobIdsFromEiss ms eiss ]
            mkBsForTarget targetDesig acc =
                let targetId = desigId targetDesig
                    toTarget = (nl $ selfDesig' <> " looks at you.", pure targetId)
                    toOthers = ( nl . T.concat $ [ selfDesig', " looks at ", serialize targetDesig, "." ]
                               , targetId `delete` is)
                in toTarget : toOthers : acc
        in (invDesc <> coinsDesc, foldr mkBsForTarget [] targetDesigs, targetDesigs |!| Just targetDesigs)
    helperLookEitherInv _  (Left  msg ) = wrapUnlinesNl cols msg
    helperLookEitherInv ms (Right is  ) = nl . mkEntDescs i cols ms $ is
    helperLookEitherCoins  (Left  msgs) = multiWrapNl cols . intersperse "" $ msgs
    helperLookEitherCoins  (Right c   ) = nl . mkCoinsDesc cols $ c
    -----
    hooksHelper ms v args = procHooks i ms v "look" args & _2._2 %~ (T.unlines . map (multiWrap cols . T.lines))
                                                         & _2._4 %~ slashes
look p = pmf "look" p


-----


lookSelf :: HasCallStack => ActionFun
lookSelf (NoArgs i mq cols) = spiritHelper i a b
  where
    a ms = send mq . nl . mkEntDesc iPidge cols ms . dupSecond (`getEnt` ms) $ i
    b    = const . wrapSend mq cols $ "You are an invisible spirit, detached from your body and floating."
lookSelf p = withoutArgs lookSelf p


-----


motd :: HasCallStack => ActionFun
motd (NoArgs i mq cols) = logPlaExec "motd" i >> showMotd mq cols
motd p                  = withoutArgs motd p


showMotd :: HasCallStack => MsgQueue -> Cols -> MudStack ()
showMotd mq cols = send mq =<< helper
  where
    helper    = liftIO readMotd |&| try >=> eitherRet handler
    readMotd  = [ frame cols . multiWrap cols . T.lines . colorizeFileTxt motdColor $ cont
                | file <- mkMudFilePath motdFileFun
                , cont <- T.readFile file ]
    handler e = fileIOExHandler "showMotd" e >> return (wrapUnlinesNl cols motdErrorMsg)


-----


naelyni :: HasCallStack => ActionFun
naelyni = sayHelper NymphLang


-----


newChan :: HasCallStack => ActionFun
newChan p@AdviseNoArgs                     = advise p ["newchannel"] adviceNewChanNoArgs
newChan p@(WithArgs i mq cols (nub -> as)) = getState >>= \ms ->
    checkActing p ms (Right "create a new telepathic channel") (pure Sacrificing) next
  where
    next = helper |&| modifyState >=> \(unzip -> (newChanNames, chanRecs), sorryMsgs) ->
        let (sorryMsgs', otherMsgs) = (intersperse "" sorryMsgs, mkNewChanMsg newChanNames)
            msgs | ()# otherMsgs    = sorryMsgs'
                 | otherwise        = otherMsgs ++ (sorryMsgs' |!| "" : sorryMsgs')
        in do newChanNames |#| logPla "newChan" i . commas
              multiWrapSend mq cols msgs
              ts <- liftIO mkTimestamp
              let f cr = withDbExHandler_ "newChan" . insertDbTblChan $ (cr { dbTimestamp = ts } :: ChanRec)
              mapM_ f chanRecs
    helper ms = let s                              = getSing i ms
                    (ms', newChanNames, sorryMsgs) = foldl' (f s) (ms, [], []) as
                in (ms', (newChanNames, sorryMsgs))
      where
        f s triple@(ms', _, _) a@(T.toLower -> a')
          | T.length a > maxChanNameLen =
              let msg = "a channel name may not be more than " <> showTxt maxChanNameLen <> " characters long"
              in sorryNewChanName a msg `sorry` triple
          | T.any isNG a = let x = sorryNewChanName a "a channel name may only contain alphabetic letters and digits"
                           in x `sorry` triple
          | a' `elem` illegalNames = sorryNewChanName a "this name is reserved or already in use" `sorry` triple
          | a' `elem` map T.toLower myChanNames
          , match <- head . filter ((== a') . T.toLower) $ myChanNames = sorryNewChanExisting match `sorry` triple
          | not . hasPp i ms' $ 5 = sorryPp ("create a new channel named " <> dblQuote a) `sorry` triple
          | otherwise = let ci = views chanTbl (head . (enumFrom 0 \\) . IM.keys) $ triple^._1
                            c  = Chan ci a (M.singleton s True) []
                            cr = ChanRec "" ci a s . asteriskQuote $ "New channel created."
                        in triple & _1.chanTbl.at ci      ?~ c
                                  & _1.mobTbl.ind i.curPp -~ 5
                                  & _2 <>~ pure (a, cr)
        sorry msg    = _3 <>~ pure msg
        isNG c       = not $ isLetter c || isDigit c
        illegalNames = [ "admin", "all", "question" ] ++ pcNames
        pcNames      = views pcTbl (map (uncapitalize . (`getSing` ms)) . IM.keys) ms
        myChanNames  = select chanName . getPCChans i $ ms
    mkNewChanMsg []     = []
    mkNewChanMsg ns@[_] = pure    . mkMsgHelper False $ ns
    mkNewChanMsg ns     = T.lines . mkMsgHelper True  $ ns
    mkMsgHelper isPlur (map dblQuote -> ns) = T.concat [ focusingInnateMsg
                                                       , "you create a "
                                                       , isPlur |?| "group of "
                                                       , "telepathic network"
                                                       , sOnTrue isPlur
                                                       , " to which others may be connected. To "
                                                       , isPlur ? "these " :? "this "
                                                       , dblQuote . ("channel" <>) . sOnTrue $ isPlur
                                                       , " you assign the "
                                                       , isPlur |?| "following "
                                                       , "name"
                                                       , isPlur ? nl "s:" <> commas ns :? spcL (head ns)
                                                       , "." ]
newChan p = pmf "newChan" p


-----


npcAsSelf :: HasCallStack => ActionFun
npcAsSelf p = execIfPossessed p "." npcAsSelfHelper


npcAsSelfHelper :: HasCallStack => Id -> ActionFun
npcAsSelfHelper _ p@(NoArgs'  i mq     ) = advise p [] adviceAsSelfNoArgs >> sendDfltPrompt mq i
npcAsSelfHelper _   (WithArgs i mq _ as) = do logPlaExecArgs "." as i
                                              writeMsg mq . AsSelf . nl . T.unwords $ as
npcAsSelfHelper _ p                      = pmf "npcAsSelfHelper" p


-----


npcDispCmdList :: HasCallStack => ActionFun
npcDispCmdList p@(LowerNub' i as) = sequence_ [ logPlaExecArgs "?" as i, flip dispCmdList p . mkNpcCmds i =<< getState ]
npcDispCmdList p                  = pmf "npcDispCmdList" p


-----


npcExorcise :: HasCallStack => ActionFun
npcExorcise p = execIfPossessed p "exorcise" npcExorciseHelper


npcExorciseHelper :: HasCallStack => Id -> ActionFun
npcExorciseHelper pi (NoArgs i mq cols) = getState >>= \ms -> do
    logPla "npcExorciseHelper" i . prd $ "no longer possessing " <> aOrAnOnLower (descSingId i ms)
    tweaks [ plaTbl.ind pi.possessing .~ Nothing, npcTbl.ind i.npcPossessor .~ Nothing ]
    wrapSend mq cols . prd $ "You stop possessing " <> aOrAnOnLower (getSing i ms)
    sendDfltPrompt mq pi
    sendGmcpRmInfo Nothing pi ms
npcExorciseHelper pi p = withoutArgs (npcExorciseHelper pi) p


-----


plaDispCmdList :: HasCallStack => ActionFun
plaDispCmdList p@(LowerNub' i as) = sequence_ [ logPlaExecArgs "?" as i, flip dispCmdList p . mkPlaCmds i =<< getState ]
plaDispCmdList p                  = pmf "plaDispCmdList" p


-----


putAction :: HasCallStack => ActionFun
putAction p@AdviseNoArgs  = advise p ["put"] advicePutNoArgs
putAction p@AdviseOneArg  = advise p ["put"] advicePutNoCon
putAction p@(Lower' i as) = getState >>= \ms ->
    let f = genericActionWithFuns p helper "put"
    in checkActing p ms (Right "put an item into a container") [ Drinking, Sacrificing ] f
  where
    helper v ms =
      let LastArgIsTargetBindings { .. } = mkLastArgIsTargetBindings i ms as
          shuffler target b is f         =
              let (ms', (toSelfs, bs, logMsgs)) = shufflePut i ms srcDesig target b otherArgs is srcInvCoins f
              in (ms', (toSelfs, bs, logMsgs, []))
      in case singleArgInvEqRm InInv targetArg of
        (InInv, target) -> shuffler target False srcInvCoins procGecrMisMobInv
        (InEq,  _     ) -> genericSorryWithFuns ms . sorryConInEq $ Put
        (InRm,  target) ->
            let invCoinsHelper = shuffler target True rmInvCoins procGecrMisRm
                f hooks g      = case filter ((dropPrefixes target `elem`) . hookTriggers) hooks of
                                   []      -> g
                                   matches -> hooksHelper otherArgs matches
            in case (()!# rmInvCoins, lookupHooks i ms "put") of
              (False, Nothing   ) -> genericSorryWithFuns ms sorryNoConHere
              (True,  Nothing   ) -> invCoinsHelper
              (False, Just hooks) -> f hooks . genericSorryWithFuns ms . sorryPutEmptyRmWithHooks $ target
              (True,  Just hooks) -> f hooks invCoinsHelper
      where
        hooksHelper args matches =
            let h@Hook { hookName }                  = head matches
                (_, (ms', toSelfs, bs, logMsgs), fs) = getHookFun hookName ms i h v (args, (ms, [], [], []), [])
            in (ms', (toSelfs, bs, logMsgs, fs))
putAction p = pmf "putAction" p


-----


password :: HasCallStack => ActionFun
password (NoArgs i mq _) = do
    sendPrompt mq $ telnetHideInput <> "Current password:"
    setInterp i . Just $ interpCurrPW
password p = withoutArgs password p


interpCurrPW :: HasCallStack => Interp
interpCurrPW cn (WithArgs i mq cols as)
  | ()# cn || ()!# as = pwSorryHelper i mq cols sorryInterpPW
  | otherwise         = getState >>= fmap join . withDbExHandler "interpCurrPW" . lookupPW . getSing i >>= \case
    Nothing -> pwSorryHelper i mq cols sorryInterpPW
    Just pw -> if uncurry validatePassword ((pw, cn) & both %~ TE.encodeUtf8)
      then do blankLine        mq
              multiWrapSend1Nl mq cols . pwMsg $ "Please choose a new password."
              sendPrompt       mq "New password:"
              setInterp i . Just . interpNewPW $ pw
      else pwSorryHelper i mq cols sorryInterpPW
interpCurrPW _ p = pmf "interpCurrPW" p


pwSorryHelper :: HasCallStack => Id -> MsgQueue -> Cols -> Text -> MudStack ()
pwSorryHelper i mq cols msg = do
    send mq telnetShowInput
    blankLine mq
    wrapSend mq cols msg
    sendDfltPrompt mq i
    resetInterp i


interpNewPW :: HasCallStack => Text -> Interp
interpNewPW oldPW cn (NoArgs i mq cols)
  | not . inRange (minPwLen, maxPwLen) . T.length $ cn = pwSorryHelper i mq cols sorryInterpNewPwLen
  | helper isUpper                                     = pwSorryHelper i mq cols sorryInterpNewPwUpper
  | helper isLower                                     = pwSorryHelper i mq cols sorryInterpNewPwLower
  | helper isDigit                                     = pwSorryHelper i mq cols sorryInterpNewPwDigit
  | otherwise = do
      sendPrompt mq "Verify password:"
      setInterp i . Just . interpVerifyNewPW oldPW $ cn
  where
    helper f = ()# T.filter f cn
interpNewPW _ _ ActionParams { .. } = pwSorryHelper myId plaMsgQueue plaCols sorryInterpNewPwExcessArgs


interpVerifyNewPW :: HasCallStack => Text -> Text -> Interp
interpVerifyNewPW oldPW pass cn (NoArgs i mq cols)
  | cn == pass = getSing i <$> getState >>= \s -> do
      logPla "interpVerifyNewPW" i . prd $ "password changed " <> parensQuote ("was " <> dblQuote oldPW)
      withDbExHandler_ "interpVerifyNewPW" . insertDbTblUnPw . UnPwRec s $ pass
      send mq telnetShowInput
      blankLine mq
      wrapSend  mq cols $ "Password changed. " <> pwWarningTxt
      sendDfltPrompt mq i
      resetInterp i
  | otherwise = pwSorryHelper i mq cols sorryInterpNewPwMatch
interpVerifyNewPW _ _ _ p = pmf "interpVerifyNewPW" p


-----


question :: HasCallStack => ActionFun
question (NoArgs' i mq) = getState >>= \ms ->
    let (plaIds,    adminIds) = (getLoggedInPlaIds ms, getNonIncogLoggedInAdminIds ms) & both %~ (i `delete`)
        (linkedIds, otherIds) = partition (isLinked ms . (i, )) plaIds
    in mapM (updateRndmName i) otherIds >>= \rndmNames ->
           let rndms   = zip3 otherIds rndmNames . repeat $ False
               linkeds = [ (li, getSing li ms, isAdminId li ms) | li <- linkedIds ]
               admins  = [ (ai, getSing ai ms, True           ) | ai <- adminIds  ]
               (tunedIns, tunedOuts) =
                 let xs = rndms ++ nubSort (linkeds ++ admins)
                 in partition (views _1 (`isTunedQuestionId` ms)) . sortBy (compare `on` view _2) $ xs
               styleds = styleAbbrevs Don'tQuote . select _2 $ tunedIns
               combo   = map f $ zipWith (\styled -> _2 .~ styled) styleds tunedIns ++ tunedOuts
                 where
                  f (i', n, ia) | ia           = (i', n <> asterisk)
                                | isRndmName n = (i', underline n  )
                                | otherwise    = (i', n            )
               mkDesc (i', n) = pad (succ namePadding) n <> tunedInOut (isTunedQuestionId i' ms)
               descs          = mkDesc (i, getSing i ms <> (isAdminId i ms |?| asterisk)) : map mkDesc combo
               descs'         = "Question channel:" : descs
           in logPlaExecArgs "question" [] i >> pager i mq Nothing descs'
question (Msg i mq cols msg) = getState >>= \ms -> if
  | not . isTunedQuestionId i $ ms -> wrapSend mq cols . sorryTunedOutOOCChan $ "question"
  | isIncognitoId i ms             -> wrapSend mq cols . sorryChanIncog $ "the question"
  | otherwise                      -> getQuestionStyleds i ms >>= \triples -> if ()# triples
    then wrapSend mq cols . sorryChanNoOneListening $ "question"
    else let ioHelper (expandEmbeddedIdsToSings ms -> logMsg) bs = do
                 logPlaOut "question" i . pure $ logMsg
                 bcastNl =<< expandEmbeddedIds ms questionChanContext bs
                 alertMsgHelper i "question" logMsg
                 ts <- liftIO mkTimestamp
                 withDbExHandler_ "question" . insertDbTblQuestion . QuestionRec ts s $ logMsg
             s    = getSing i ms
             f bs = ioHelper (dropANSI . fst . head $ bs) =<< g bs
             g    = concatMapM (formatQuestion i ms)
             ws   = wrapSend      mq cols
             mws  = multiWrapSend mq cols
          in case targetify i questionChanContext triples msg of
            Left  errorMsg   -> ws errorMsg
            Right (Right bs) -> f bs
            Right (Left  ()) -> case emotify i ms questionChanContext triples msg of
              Left  errorMsgs  -> mws errorMsgs
              Right (Right bs) -> f bs
              Right (Left  ()) -> case expCmdify i ms questionChanContext triples msg of
                Left  errorMsg     -> ws errorMsg
                Right (bs, logMsg) -> ioHelper logMsg =<< g bs
question p = pmf "question" p


-----


quit :: HasCallStack => ActionFun -- TODO: Can you quit while attacking?
quit (NoArgs i mq cols) = logPlaExec "quit" i >> mIf (isSpiritId i <$> getState)
    (throwWaitSpiritTimer i)
    (wrapSend mq cols sleepMsg >> writeMsg mq Quit)
quit ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceQuitExcessArgs


-----


quitCan'tAbbrev :: HasCallStack => ActionFun
quitCan'tAbbrev (NoArgs _ mq cols) = wrapSend mq cols sorryQuitCan'tAbbrev
quitCan'tAbbrev p                  = withoutArgs quitCan'tAbbrev p


-----


razzle :: HasCallStack => ActionFun
razzle p@(WithArgs i mq cols [ "dazzle", "root", "beer" ]) = mIf (hasRazzledId i <$> getState)
    (cmdNotFoundAction p)
    (do logPlaExec "razzle" i
        modifyStateSeq $ \ms ->
            let et  = EntTemplate (Just "flask")
                                  "large potion flask" ""
                                  "This glass flask complete with cork stopper is the ideal vessel for potion storage \
                                  \and transportation."
                                  Nothing
                                  zeroBits
                ot  = ObjTemplate potionFlaskLrgWeight
                                  potionFlaskLrgVol
                                  Nothing Nothing Nothing
                                  (setBit zeroBits . fromEnum $ IsBiodegradable)
                vt  = VesselTemplate (Just (potTinnitusLiq, maxBound)) Nothing
                res = dropFst . newVessel (ms & plaTbl.ind i %~ setPlaFlag HasRazzled True) et ot vt $ i
                f   = bcastOtherAdmins i . prd $ getSing i ms <> " has executed " <> dblQuote "razzle dazzle root beer"
            in second (++ pure f) res
        wrapSend mq cols "A potion flask materializes in your hands.")
razzle p = cmdNotFoundAction p


-----


readAction :: HasCallStack => ActionFun
readAction p@AdviseNoArgs            = advise p ["read"] adviceReadNoArgs
readAction p@(LowerNub i mq cols as) = (,) <$> getState <*> mkRndmVector >>= \(ms, v) ->
    let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
        sorryInEq              = inEqs |!| wrapUnlinesNl cols sorryReadInEq
        sorryInRm              = inRms |!| wrapUnlinesNl cols sorryReadNoHooks
        d                      = mkStdDesig  i ms DoCap
        invCoins               = getInvCoins i ms
        helper                 = case ((()!#) *** (()!#)) (invCoins, lookupHooks i ms "read") of
          (False, False) -> let sorry = inInvs |!| wrapUnlinesNl cols dudeYourHandsAreEmpty
                            in send mq . T.concat $ [ sorry, sorryInEq, sorryInRm ]
          -----
          (True,  False) -> let sorry = sorryInEq <> sorryInRm
                            in ioHelper (invCoinsHelper ms inInvs d invCoins & _1 %~ (sorry <>))
          -----
          (False, True ) -> let sorry = (inInvs |!| wrapUnlinesNl cols dudeYourHandsAreEmpty) <> sorryInEq
                                (inRms', (_, toSelfs, bs, logMsgs), fs) = procHooks i ms v "read" inRms
                                sorry' = sorry <> wrapper (map sorryReadWithHooks inRms')
                            in ioHelper (sorry' <> wrapper toSelfs, bs, logMsgs) >> sequence_ fs
          -----
          (True,  True ) ->
            let (inRms', (_, hooksToSelfs, hooksBs, hooksLogMsgs), fs) = procHooks i ms v "read" inRms
                sorry = sorryInEq <> wrapper (map sorryReadWithHooks inRms')
                (invCoinsToSelf, invCoinsBs, invCoinsLogMsgs) = invCoinsHelper ms inInvs d invCoins
            in do
                ioHelper ( T.concat [ sorry, wrapper hooksToSelfs, invCoinsToSelf ]
                                    , hooksBs      ++ invCoinsBs
                                    , hooksLogMsgs ++ invCoinsLogMsgs )
                sequence_ fs
    in checkActing p ms (Right "read") [ Attacking, Drinking, Sacrificing ] helper
  where
    invCoinsHelper ms args d invCoins =
        let (eiss, ecs) = uncurry (resolveMobInvCoins i ms args) invCoins
            a           = foldl' helperEitherInv ("", [], []) eiss
        in foldl' helperEitherCoins a ecs
      where
        helperEitherInv   acc (Left  msg ) = acc & _1 <>~ wrapUnlinesNl cols msg
        helperEitherInv   acc (Right is  ) = readHelper i cols ms d acc is
        helperEitherCoins acc (Left  msgs) = acc & _1 <>~ (multiWrapNl cols . intersperse "" $ msgs)
        helperEitherCoins acc (Right _   ) = acc & _1 <>~ wrapUnlinesNl cols sorryReadCoins
    ioHelper (toSelf, bs, logMsgs) = do logMsgs |#| logPla "read" i . prd . slashes
                                        send mq toSelf
                                        bcastIfNotIncogNl i bs
    wrapper = T.unlines . map (multiWrap cols . T.lines)
readAction p = pmf "readAction" p


-----


ready :: HasCallStack => ActionFun
ready p@AdviseNoArgs     = advise p ["ready"] adviceReadyNoArgs
ready p@(LowerNub' i as) = genericAction p helper "ready"
  where
    helper ms =
        let (inInvs, inEqs, inRms)    = sortArgsInvEqRm InInv as
            sorryInEq                 = inEqs |!| sorryReadyInEq
            sorryInRm                 = inRms |!| sorryReadyInRm
            sorryCoins                = rcs   |!| sorryReadyCoins
            invCoins@(is, _)          = getInvCoins i ms
            d                         = mkStdDesig  i ms DoCap
            (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols i ms inInvs is mempty
            eiss                      = zipWith (curry procGecrMisReady) gecrs miss
            (et, it, toSelfs, bs, logMsgs) =
                foldl' (helperReady i ms d) (ms^.eqTbl, ms^.invTbl, [], [], []) . zip eiss $ mrols
        in genericCheckActing i ms (Right "ready an item") [ Drinking, Sacrificing ] $ if ()!# invCoins
          then (ms & eqTbl .~ et & invTbl .~ it, ( dropBlanks $ [ sorryInEq, sorryInRm, sorryCoins ] ++ toSelfs
                                                 , bs
                                                 , logMsgs ))
          else genericSorry ms dudeYourHandsAreEmpty
ready p = pmf "ready" p


helperReady :: HasCallStack => Id
                            -> MudState
                            -> Desig
                            -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                            -> (Either Text Inv, Maybe RightOrLeft)
                            -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
helperReady _ _  _ a (Left  msg,       _   ) = a & _3 <>~ pure msg
helperReady i ms d a (Right targetIds, mrol) = foldl' (readyDispatcher i ms d mrol) a targetIds


readyDispatcher :: HasCallStack => Id
                                -> MudState
                                -> Desig
                                -> Maybe RightOrLeft
                                -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                                -> Id
                                -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyDispatcher i ms d mrol a targetId =
    helper |&| either (\msg -> a & _3 <>~ pure msg) (`uncurry7` (i, ms, d, mrol, a, targetId, targetSing))
  where
    helper = case getType targetId ms of
      ArmType        -> Right readyArm
      ClothType      -> Right readyCloth
      ConType        -> getConIsCloth targetId ms ? Right readyCloth :? sorry
      HolySymbolType | getHolySymbolGodName targetId ms == Rhayk -> Left sorryReadyHolySymbolRhayk
                     | otherwise                                 -> sorry
      LightType      -> Right readyLight
      WpnType        -> Right readyWpn
      _             -> sorry
    sorry      = Left . sorryReadyType $ targetSing
    targetSing = getSing targetId ms


-- Readying clothing:


readyCloth :: HasCallStack => Id
                           -> MudState
                           -> Desig
                           -> Maybe RightOrLeft
                           -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                           -> Id
                           -> Sing
                           -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyCloth i ms d mrol a@(et, _, _, _, _) clothId clothSing | em <- et IM.! i, cloth <- getCloth clothId ms =
  case mrol |&| maybe (getAvailClothSlot i ms cloth em) (getDesigClothSlot ms clothSing cloth em) of
      Left  msg  -> a & _3 <>~ pure msg
      Right slot -> moveReadiedItem i a slot clothId . mkReadyClothMsgs slot $ cloth
  where
    mkReadyClothMsgs (pp -> slot) = \case
      Earring  -> wearMsgs
      NoseRing -> putOnMsgs i d clothSing
      Necklace -> putOnMsgs i d clothSing
      Bracelet -> wearMsgs
      Ring     -> slideMsgs
      Backpack -> putOnMsgs i d clothSing
      _        -> donMsgs   i d clothSing
      where
        wearMsgs   = (   T.concat [ "You wear the ",  clothSing, " on your ", slot, "." ]
                     , ( T.concat [ serialize d, " wears ",  aOrAn clothSing, " on ", poss, " ", slot, "." ]
                       , otherPCIds ) )
        slideMsgs  = (   T.concat [ "You slide the ", clothSing, " on your ", slot, "." ]
                     , ( T.concat [ serialize d, " slides ", aOrAn clothSing, " on ", poss, " ", slot, "." ]
                       , otherPCIds) )
        poss       = mkPossPro . getSex i $ ms
        otherPCIds = i `delete` desigIds d


getAvailClothSlot :: HasCallStack => Id -> MudState -> Cloth -> EqMap -> Either Text Slot
getAvailClothSlot i ms cloth em | sexy <- getSex i ms, h <- getHand i ms =
    maybe (Left sorry) Right $ case cloth of
      Earring  -> getEarringSlotForSex sexy `mplus` (getEarringSlotForSex . otherSex $ sexy)
      NoseRing -> findAvailSlot em noseRingSlots
      Necklace -> findAvailSlot em necklaceSlots
      Bracelet -> getBraceletSlotForHand h  `mplus` (getBraceletSlotForHand . otherHand $ h)
      Ring     -> getRingSlot sexy h
      _        -> maybeSingleSlot em . clothToSlot $ cloth
  where
    getEarringSlotForSex sexy = findAvailSlot em $ case sexy of
      Male   -> lEarringSlots
      Female -> rEarringSlots
      NoSex  -> lEarringSlots
    getBraceletSlotForHand h  = findAvailSlot em $ case h of
      RHand  -> lBraceletSlots
      LHand  -> rBraceletSlots
      NoHand -> lBraceletSlots
    getRingSlot sexy h        = findAvailSlot em $ case sexy of
      Male   -> case h of
        RHand  -> [ RingLRS, RingLIS, RingRRS, RingRIS, RingLMS, RingRMS, RingLPS, RingRPS ]
        LHand  -> [ RingRRS, RingRIS, RingLRS, RingLIS, RingRMS, RingLMS, RingRPS, RingLPS ]
        NoHand -> noSexHand
      Female -> case h of
        RHand  -> [ RingLRS, RingLIS, RingRRS, RingRIS, RingLPS, RingRPS, RingLMS, RingRMS ]
        LHand  -> [ RingRRS, RingRIS, RingLRS, RingLIS, RingRPS, RingLPS, RingRMS, RingLMS ]
        NoHand -> noSexHand
      NoSex  -> noSexHand
    noSexHand =   [ RingRIS, RingRMS, RingRRS, RingRPS, RingLIS, RingLMS, RingLRS, RingLPS ]
    sorry | cloth `elem` enumFromTo Earring Ring               = sorryReadyClothFull      . pp $ cloth
          | cloth `elem` [ Skirt, Dress, Backpack, Cloak ]     = sorryReadyAlreadyWearing . pp $ cloth
          | ci <- em M.! clothToSlot cloth, s <- getSing ci ms = sorryReadyAlreadyWearing s


otherSex :: Sex -> Sex
otherSex Male   = Female
otherSex Female = Male
otherSex NoSex  = Female


rEarringSlots, lEarringSlots, noseRingSlots, necklaceSlots, rBraceletSlots, lBraceletSlots :: [Slot]
rEarringSlots  = [ EarringR1S, EarringR2S ]
lEarringSlots  = [ EarringL1S, EarringL2S ]
noseRingSlots  = [ NoseRing1S, NoseRing2S ]
necklaceSlots  = Necklace1S  `enumFromTo` Necklace2S
rBraceletSlots = BraceletR1S `enumFromTo` BraceletR3S
lBraceletSlots = BraceletL1S `enumFromTo` BraceletL3S


getDesigClothSlot :: HasCallStack => MudState -> Sing -> Cloth -> EqMap -> RightOrLeft -> Either Text Slot
getDesigClothSlot ms clothSing cloth em rol
  | cloth `elem` [ NoseRing, Necklace ] ++ enumFromTo Shirt Cloak = sorryRol
  | isRingRol rol, cloth /= Ring                                  = sorryRol
  | cloth == Ring, not . isRingRol $ rol                          = Left ringHelp
  | otherwise = case cloth of
    Earring  -> findSlotFromList rEarringSlots  lEarringSlots  |&| maybe (Left sorryEarring ) Right
    Bracelet -> findSlotFromList rBraceletSlots lBraceletSlots |&| maybe (Left sorryBracelet) Right
    Ring     -> M.lookup slotFromRol em |&| maybe (Right slotFromRol)
                                                  (Left . sorryReadyAlreadyWearingRing slotFromRol . (`getSing` ms))
    _        -> pmf "getDesigClothSlot" cloth
  where
    findSlotFromList rs ls = findAvailSlot em $ case rol of
      R -> rs
      L -> ls
      _ -> pmf "getDesigClothSlot findSlotFromList" rol
    getSlotFromList  rs ls = head $ case rol of
      R -> rs
      L -> ls
      _ -> pmf "getDesigClothSlot getSlotFromList"  rol
    sorryRol         = Left . sorryReadyRol clothSing $ rol
    sorryEarring     = sorryReadyClothFullOneSide cloth . getSlotFromList rEarringSlots  $ lEarringSlots
    sorryBracelet    = sorryReadyClothFullOneSide cloth . getSlotFromList rBraceletSlots $ lBraceletSlots
    slotFromRol      = fromRol rol :: Slot


-- Readying weapons:


readyWpn :: HasCallStack => Id
                         -> MudState
                         -> Desig
                         -> Maybe RightOrLeft
                         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                         -> Id
                         -> Sing
                         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyWpn i ms d mrol a@(et, _, _, _, _) wpnId wpnSing | em <- et IM.! i, wpn <- getWpn wpnId ms, sub <- wpn^.wpnSub =
    if not . isSlotAvail em $ BothHandsS
      then sorry . sorryReadyHandsFull $ wpnSing
      else case mrol |&| maybe (getAvailHandSlot ms i wpnSing em) (getDesigHandSlot ms wpnSing em) of
        Left  msg  -> sorry msg
        Right slot -> case sub of
          OneHanded -> let readyMsgs = (   T.concat [ "You wield the ", wpnSing, " with your ", pp slot, "." ]
                                       , ( T.concat [ serialize d
                                                    , " wields "
                                                    , aOrAn wpnSing
                                                    , " with "
                                                    , poss
                                                    , " "
                                                    , pp slot
                                                    , "." ]
                                         , otherPCIds ) )
                       in moveReadiedItem i a slot wpnId readyMsgs
          TwoHanded
            | all (isSlotAvail em) [ RHandS, LHandS ] ->
                let readyMsgs = ( "You wield the " <> wpnSing <> " with both hands."
                                , ( T.concat [ serialize d, " wields ", aOrAn wpnSing, " with both hands." ]
                                  , otherPCIds ) )
                in moveReadiedItem i a BothHandsS wpnId readyMsgs
            | otherwise -> sorry . sorryReadyWpnHands $ wpnSing
  where
    sorry msg  = a & _3 <>~ pure msg
    poss       = mkPossPro . getSex i $ ms
    otherPCIds = i `delete` desigIds d


getAvailHandSlot :: HasCallStack => MudState -> Id -> Sing -> EqMap -> Either Text Slot
getAvailHandSlot ms i s em = let h@(otherHand -> oh) = getHand i ms in
    (findAvailSlot em . map getSlotForHand $ [ h, oh ]) |&| maybe (Left . sorryReadyHandsFull $ s) Right
  where
    getSlotForHand h = case h of RHand  -> RHandS
                                 LHand  -> LHandS
                                 NoHand -> RHandS


getDesigHandSlot :: HasCallStack => MudState -> Sing -> EqMap -> RightOrLeft -> Either Text Slot
getDesigHandSlot ms wpnSing em rol
  | isRingRol rol = Left . sorryReadyWpnRol $ wpnSing
  | otherwise     = M.lookup desigSlot em |&| maybe (Right desigSlot) (Left . sorry)
  where
    desigSlot = case rol of R -> RHandS
                            L -> LHandS
                            _ -> pmf "getDesigHandSlot desigSlot" rol
    sorry i   = sorryReadyHand (getSing i ms) desigSlot


-- Readying armor:


readyArm :: HasCallStack => Id
                         -> MudState
                         -> Desig
                         -> Maybe RightOrLeft
                         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                         -> Id
                         -> Sing
                         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyArm i ms d mrol a@(et, _, _, _, _) armId armSing | em <- et IM.! i, sub <- getArmSub armId ms =
    case mrol |&| maybe (getAvailArmSlot ms sub em) sorry of
      Left  msg  -> a & _3 <>~ pure msg
      Right slot -> moveReadiedItem i a slot armId . mkReadyArmMsgs $ sub
  where
    sorry          = Left . sorryReadyRol armSing
    mkReadyArmMsgs = ((i, d, armSing) |&|) . uncurry3 . \case
      Head   -> putOnMsgs
      Hands  -> putOnMsgs
      Feet   -> putOnMsgs
      Shield -> mkReadyMsgs "ready" "readies"
      _      -> donMsgs


getAvailArmSlot :: HasCallStack => MudState -> ArmSub -> EqMap -> Either Text Slot
getAvailArmSlot ms (armSubToSlot -> slot) em = maybeSingleSlot em slot |&| maybe (Left sorry) Right
  where
    sorry | i <- em M.! slot = sorryReadyAlreadyWearing . getSing i $ ms


-- Readying light sources:


readyLight :: HasCallStack => Id
                           -> MudState
                           -> Desig
                           -> Maybe RightOrLeft
                           -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                           -> Id
                           -> Sing
                           -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyLight i ms d mrol a@(et, _, _, _, _) lightId lightSing | em <- et IM.! i =
    if not . isSlotAvail em $ BothHandsS
      then sorry . sorryReadyLight $ lightSing
      else case mrol |&| maybe (getAvailHandSlot ms i lightSing em) (getDesigHandSlot ms lightSing em) of
        Left  msg  -> sorry msg
        Right slot -> let readyMsgs = (   T.concat [ "You hold the ", lightSing, " in your ", pp slot, "." ]
                                      , ( T.concat [ serialize d
                                                   , " holds "
                                                   , aOrAn lightSing
                                                   , " in "
                                                   , poss
                                                   , " "
                                                   , pp slot
                                                   , "." ]
                                        , i `delete` desigIds d ) )
                       in moveReadiedItem i a slot lightId readyMsgs
  where
    sorry msg = a & _3 <>~ pure msg
    poss      = mkPossPro . getSex i $ ms


-----


refuel :: HasCallStack => RmActionFun
refuel p@AdviseNoArgs                     = advise p [] adviceRefuelNoArgs
refuel p@AdviseOneArg                     = advise p [] adviceRefuelNoSource
refuel p@(Lower i mq cols [lamp, vessel]) = getState >>= \ms ->
    checkActing p ms (Right "refuel a lamp") [ Attacking, Drinking, Sacrificing ] helper
  where
    helper = modifyStateSeq $ \ms ->
        let (inInvs, inEqs, _) = sortArgsInvEqRm InInv . pure $ lamp
            (invCoins, eqMap)  = (getInvCoins `fanUncurry` getEqMap) (i, ms)
            sorry     = (ms, ) . pure . wrapSend mq cols
            refuelInv = let (eiss, _) = uncurry (resolveMobInvCoins i ms inInvs) invCoins
                        in procEiss eiss
            refuelEq  = let (gecrs, miss, _) = resolveEntCoinNames i ms inEqs (M.elems eqMap) mempty
                            eiss             = zipWith (curry procGecrMisMobEq) gecrs miss
                        in procEiss eiss
            procEiss eiss     = case eiss of []      -> sorry sorryRefuelLampCoins
                                             (eis:_) -> either sorry refueler eis
            refueler [lampId] = case (getSing `fanUncurry` getType) (lampId, ms) of
              (s, LightType) | (Lamp maxLampSecs)       <- getLightSub lampId ms
                             , (inInvs', inEqs', inRms) <- sortArgsInvEqRm InInv . pure $ vessel
                             -> if | ()!# inEqs' -> sorry sorryRefuelVesselInEq
                                   | ()!# inRms  -> sorry sorryRefuelVesselInRm
                                   | otherwise   -> let (eiss, _) = uncurry (resolveMobInvCoins i ms inInvs') invCoins
                                                    in case eiss of []      -> sorry sorryRefuelVesselCoins
                                                                    (eis:_) -> either sorry (refuelerHelper s maxLampSecs) eis
              (s, _        ) -> sorry . sorryRefuelLampType $ s
              where
                refuelerHelper lampSing maxLampSecs [vesselId] | vesselSing <- getSing vesselId ms = case getVesselCont vesselId ms of
                  Nothing -> sorry . sorryRefuelVesselEmpty $ vesselSing
                  Just (liq, mouths)
                    | liq == oilLiq ->
                      let secs          = getLightSecs lampId ms
                          vesselSecs    = mouths * calcLampSecsPerMouthfulOfOil
                          lampAvailSecs = maxLampSecs - secs
                      in if | secs < maxLampSecs -> if vesselSecs >= lampAvailSecs
                              then -- There is enough oil in the vessel to fill the lamp.
                                   let vesselRemSecs = vesselSecs - lampAvailSecs
                                       mouths'       = floor $ vesselRemSecs `divide` calcLampSecsPerMouthfulOfOil
                                       newCont | vesselRemSecs == 0 = Nothing
                                               | otherwise          = Just (liq, mouths')
                                       toSelf  = T.concat [ "You refuel the "
                                                          , lampSing
                                                          , " with the oil from the "
                                                          , vesselSing
                                                          , "." ]
                                   in ( ms & lightTbl .ind lampId  .lightSecs  .~ maxLampSecs
                                           & vesselTbl.ind vesselId.vesselCont .~ newCont
                                      , ioHelper ms lampSing vesselSing toSelf False )
                              else -- There is not enough oil in the vessel to fill the lamp.
                                   let toSelf = T.concat [ "You empty the contents of the "
                                                         , vesselSing
                                                         , " into the "
                                                         , lampSing
                                                         , "." ]
                                   in ( ms & lightTbl .ind lampId  .lightSecs  +~ vesselSecs
                                           & vesselTbl.ind vesselId.vesselCont .~ Nothing
                                      , ioHelper ms lampSing vesselSing toSelf True )
                            | otherwise -> sorry . sorryRefuelAlready $ lampSing
                    | otherwise -> sorry . sorryRefuelLiq $ vesselSing
                refuelerHelper _ _ _ = sorry sorryRefuelExcessVessels
            refueler _ = sorry sorryRefuelExcessLamps
        in if | ()#  invCoins -> sorry dudeYourHandsAreEmpty
              | ()!# inInvs   -> refuelInv
              | ()!# inEqs    -> refuelEq
              | otherwise     -> sorry sorryRefuelLampInRm
    ioHelper ms lampSing vesselSing toSelf b =
        let bs     = pure (T.concat [ serialize d
                                    , " refuels "
                                    , aOrAn lampSing
                                    , " with the oil from "
                                    , aOrAn vesselSing
                                    , "." ], i `delete` desigIds d)
            d      = mkStdDesig i ms DoCap
            logMsg = T.concat [ "refueled "
                              , aOrAn lampSing
                              , " with "
                              , aOrAn vesselSing
                              , b |?| ", emptying it"
                              , "." ]
        in [ wrapSend mq cols toSelf
           , bcastIfNotIncogNl i bs
           , logPla "refuel helper refuelerHelper" i logMsg ]
refuel p = advise p ["refuel"] adviceRefuelExcessArgs


-----


remove :: HasCallStack => ActionFun
remove p@AdviseNoArgs  = advise p ["remove"] adviceRemoveNoArgs
remove p@AdviseOneArg  = advise p ["remove"] adviceRemoveNoCon
remove p@(Lower' i as) = genericAction p helper "remove"
  where
    helper ms =
      let LastArgIsTargetBindings { .. } = mkLastArgIsTargetBindings i ms as
      in case singleArgInvEqRm InInv targetArg of
        (InInv, target) -> shuffleRem i ms srcDesig target False otherArgs srcInvCoins procGecrMisMobInv
        (InEq,  _     ) -> genericSorry ms . sorryConInEq $ Rem
        (InRm,  target) ->
          let f | ()# rmInvCoins = genericSorry ms sorryNoConHere
                | otherwise      = shuffleRem i ms srcDesig target True otherArgs rmInvCoins procGecrMisRm
          in genericCheckActing i ms (Right "remove an item from a container") [ Drinking, Sacrificing ] f
remove p = pmf "remove" p


-----


roomDesc :: HasCallStack => ActionFun
roomDesc (NoArgs i mq cols) = do logPla "roomDesc" i "clearing room description."
                                 tweak $ mobTbl.ind i.mobRmDesc .~ Nothing
                                 wrapSend mq cols "Your room description has been cleared."
roomDesc (WithArgs i mq cols (T.unwords -> desc@(dblQuote -> desc'))) = if T.length desc > maxMobRmDescLen
  then wrapSend mq cols $ "A room description cannot exceed " <> showTxt maxMobRmDescLen <> " characters in length."
  else do logPla "roomDesc" i . prd $ "setting room description to " <> desc'
          tweak $ mobTbl.ind i.mobRmDesc ?~ desc
          wrapSend mq cols . prd $ "Your room description has been set to " <> desc'
roomDesc p = pmf "roomDesc" p


-----


sacrifice :: HasCallStack => ActionFun
sacrifice p@(NoArgs i mq cols) = getState >>= \ms ->
    case (findHolySymbolGodName `fanUncurry` findCorpseIdInMobRm) (i, ms) of
      (Just gn, Just ci) -> sacrificeHelper p ci gn
      (Nothing, Just _ ) -> sorry sorrySacrificeHolySymbol
      (Just _,  Nothing) -> sorry sorrySacrificeCorpse
      (Nothing, Nothing) -> sorry sorrySacrificeHolySymbolCorpse
  where
    sorry msg = wrapSend mq cols msg >> sendDfltPrompt mq i
sacrifice p@(OneArgLower i mq cols a) = getState >>= \ms -> case findCorpseIdInMobRm i ms of
  Nothing -> sorry sorrySacrificeCorpse
  Just ci -> either id (sacrificeHelper p ci) . sacrificeHolySymbol i mq cols a $ ms
  where
    sorry msg = wrapSend mq cols msg >> sendDfltPrompt mq i
sacrifice p@(Lower i mq cols [a, b]) = getState >>= \ms -> case sacrificeHolySymbol i mq cols a ms of
  Left  f  -> f
  Right gn -> either id (flip (sacrificeHelper p) gn) . sacrificeCorpse i mq cols b $ ms
sacrifice p = advise p ["sacrifice"] adviceSacrificeExcessArgs


findHolySymbolGodName :: HasCallStack => Id -> MudState -> Maybe GodName
findHolySymbolGodName i ms =
    (`getHolySymbolGodName` ms) <$> (listToMaybe . filter (`isHolySymbol` ms) . getInv i $ ms)


findCorpseIdInMobRm :: HasCallStack => Id -> MudState -> Maybe Id
findCorpseIdInMobRm i ms = listToMaybe . filter ((== CorpseType) . (`getType` ms)) . getMobRmInv i $ ms


sacrificeHolySymbol :: HasCallStack => Id -> MsgQueue -> Cols -> Text -> MudState -> Either Fun GodName
sacrificeHolySymbol i mq cols a ms =
    let invCoins    = getInvCoins i ms
        next target = let pair@(eiss, _) = uncurry (resolveMobInvCoins i ms . pure $ target) invCoins
                      in if ((&&) <$> ((()!#) . fst) <*> ((()!#) . snd)) pair
                        then sorry sorrySacrificeHolySymbolExcessTargets
                        else case eiss of
                          []      -> sorry sorrySacrificeHolySymbolCoins
                          (eis:_) -> case eis of
                            Left  msg        -> sorry msg
                            Right [targetId] -> if isHolySymbol targetId ms
                                                  then Right . getHolySymbolGodName targetId $ ms
                                                  else sorry . sorrySacrificeHolySymbolType . getSing targetId $ ms
                            Right _          -> sorry sorrySacrificeHolySymbolExcessTargets
    in case singleArgInvEqRm InInv a of (InInv, target) -> next target
                                        (InEq,  _     ) -> sorry sorrySacrificeHolySymbolInEq
                                        (InRm,  _     ) -> sorry sorrySacrificeHolySymbolInRm
  where
    sorry msg = Left $ wrapSend mq cols msg >> sendDfltPrompt mq i


sacrificeCorpse :: HasCallStack => Id -> MsgQueue -> Cols -> Text -> MudState -> Either Fun Id
sacrificeCorpse i mq cols a ms =
    let invCoins    = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
        next target = let pair@(eiss, _) = uncurry (resolveRmInvCoins i ms . pure $ target) invCoins
                      in if ((&&) <$> ((()!#) . fst) <*> ((()!#) . snd)) pair
                        then sorry sorrySacrificeCorpseExcessTargets
                        else case eiss of
                          []      -> sorry sorrySacrificeCorpseCoins
                          (eis:_) -> case eis of
                            Left  msg        -> sorry msg
                            Right [targetId] -> let (targetSing, t) = (getSing `fanUncurry` getType) (targetId, ms)
                                                in if t == CorpseType
                                                  then Right targetId
                                                  else sorry . sorrySacrificeCorpseType $ targetSing
                            Right _          -> sorry sorrySacrificeCorpseExcessTargets
    in case singleArgInvEqRm InRm a of (InInv, _     ) -> sorry sorrySacrificeCorpseInInv
                                       (InEq,  _     ) -> sorry sorrySacrificeCorpseInEq
                                       (InRm,  target) -> next target
  where
    sorry msg = Left $ wrapSend mq cols msg >> sendDfltPrompt mq i


-----


say :: HasCallStack => ActionFun
say = sayHelper CommonLang


sayHelper :: HasCallStack => Lang -> ActionFun
sayHelper l p@AdviseNoArgs                    = advise p [ mkCmdNameForLang l ] . adviceSayNoArgs $ l
sayHelper l p@(WithArgs i mq cols args@(a:_)) = getState >>= \ms ->
    let f | isIncognitoId i ms         = wrapSend mq cols . sorryIncog . mkCmdNameForLang $ l
          | T.head a == adverbOpenChar = case parseAdverb . T.unwords $ args of
            Left  msg                    -> adviseHelper msg
            Right (adverb, rest@(T.words -> rs@(head -> r)))
              | T.head r == sayToChar, T.length r > 1 -> if length rs > 1
                then sayTo (Just adverb) (T.tail rest) |&| modifyState >=> ioHelper ms
                else adviseHelper . adviceSayToNoUtterance $ l
              | otherwise -> ioHelper ms =<< simpleSayHelper ms (Just adverb) rest
          | T.head a == sayToChar, T.length a > 1 = if length args > 1
            then sayTo Nothing (T.tail . T.unwords $ args) |&| modifyState >=> ioHelper ms
            else adviseHelper . adviceSayToNoUtterance $ l
          | otherwise = ioHelper ms =<< simpleSayHelper ms Nothing (T.unwords args)
    in checkActing p ms (Right "speak") [ Drinking, Sacrificing ] f
  where
    adviseHelper                = advise p [ mkCmdNameForLang l ]
    parseAdverb (T.tail -> msg) = case T.break (== adverbCloseChar) msg of
      (_,   "")            -> Left . adviceAdverbCloseChar      $ l
      ("",  _ )            -> Left . adviceBlankAdverb          $ l
      (" ", _ )            -> Left . adviceBlankAdverb          $ l
      (_,   x ) | x == acl -> Left . adviceSayAdverbNoUtterance $ l
      (adverb, right)      -> Right (adverb, T.drop 2 right)
    sayTo maybeAdverb (T.words -> (target:rest@(r:_))) ms =
        let d        = mkStdDesig i ms DoCap
            invCoins = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
        in if ()!# invCoins
          then case singleArgInvEqRm InRm target of
            (InInv, _      ) -> sorry sorrySayInInv
            (InEq,  _      ) -> sorry sorrySayInEq
            (InRm,  target') -> case uncurry (resolveRmInvCoins i ms . pure $ target') invCoins of
              (_,                  [Left [msg]]) -> sorry msg
              (_,                  Right _:_   ) -> sorry sorrySayCoins
              ([Left  msg       ], _           ) -> sorry msg
              ([Right (_:_:_)   ], _           ) -> sorry sorrySayExcessTargets
              ([Right [targetId]], _           ) ->
                  let targetDesig = serialize . mkStdDesig targetId ms $ Don'tCap
                  in if isNpcPla targetId ms
                    then parseRearAdverb |&| either sorry (sayToHelper d targetId targetDesig)
                    else sorry . sorrySayTargetType . getSing targetId $ ms
              x -> pmf "sayHelper sayTo" x
          else sorry sorrySayNoOneHere
      where
        sorry           = (ms, ) . (, [], "") . pure
        parseRearAdverb = case maybeAdverb of
          Just adverb                          -> Right (spcR adverb, "", formatMsg . T.unwords $ rest)
          Nothing | T.head r == adverbOpenChar -> case parseAdverb . T.unwords $ rest of
                      Right (adverb, rest') -> Right ("", spcL adverb, formatMsg rest')
                      Left  msg             -> Left  msg
                  | otherwise -> Right ("", "", formatMsg . T.unwords $ rest)
        sayToHelper d targetId targetDesig (frontAdv, rearAdv, msg) =
            let inLang           = mkInLangTxtForLang l
                toSelfMsg        = T.concat [ "You say ", frontAdv, "to ", targetDesig, inLang, rearAdv, ", ", msg ]
                toTargetMsg      | isKnownLang targetId ms l
                                 = T.concat [ serialize d, " says ", frontAdv, "to you", inLang, rearAdv, ", ", msg ]
                                 | otherwise
                                 = T.concat [ serialize d, " says something ", frontAdv, "to you", inLang, rearAdv, "." ]
                toTargetBcast    = (nl toTargetMsg, pure targetId)
                mkToOthersMsg i' | isKnownLang i' ms l
                                 = T.concat [ serialize d
                                            , " says "
                                            , frontAdv
                                            , "to "
                                            , targetDesig
                                            , inLang
                                            , rearAdv
                                            , ", "
                                            , msg ]
                                 | otherwise
                                 = T.concat [ serialize d
                                            , " says something "
                                            , frontAdv
                                            , "to "
                                            , targetDesig
                                            , inLang
                                            , rearAdv
                                            , "." ]
                toOthersBcasts   = [ (nl . mkToOthersMsg $ i', pure i') | i' <- desigIds d \\ [ i, targetId ] ]
                f                | isNpc i        ms = (, [])
                                 | isNpc targetId ms = firstMobSay i
                                 | otherwise         = (, [])
                (pt, hints)      = ms^.plaTbl.to f
            in (ms & plaTbl .~ pt, ( onFalse (()# hints) (++ hints) . pure $ toSelfMsg
                                   , toTargetBcast : toOthersBcasts
                                   , toSelfMsg ))
    sayTo _ msg _ = pmf "sayHelper sayTo" msg
    formatMsg     = dblQuote . capitalizeMsg . punctuateMsg
    ioHelper ms triple@(x:xs, _, _) | (toSelfs, bs, logMsg) <- triple & _1 .~ parseInBands       i ms x : xs
                                                                      & _3 %~ parseInBandsSuffix i ms
                                    = do multiWrapSend mq cols toSelfs
                                         bcastIfNotIncogNl i bs
                                         logMsg |#| logPlaOut (mkCmdNameForLang l) i . pure
                                         logMsg |#| alertMsgHelper i (mkCmdNameForLang l)
    ioHelper _  triple              = pmf "sayHelper ioHelper" triple
    simpleSayHelper ms (maybeEmp spcL -> adverb) (formatMsg -> msg) =
        return $ let d                = mkStdDesig i ms DoCap
                     inLang           = mkInLangTxtForLang l
                     toSelfMsg        = T.concat [ "You say", inLang, adverb, ", ", msg ]
                     mkToOthersMsg i' | isKnownLang i' ms l = T.concat [ serialize d, " says", adverb, inLang, ", ", msg ]
                                      | otherwise           = T.concat [ serialize d
                                                                       , " says something"
                                                                       , adverb
                                                                       , inLang
                                                                       , "." ]
                     toOthersBcasts   = [ (nl . mkToOthersMsg $ i', pure i') | i' <- i `delete` desigIds d ]
                 in (pure toSelfMsg, toOthersBcasts, toSelfMsg)
sayHelper _ p = pmf "sayHelper" p


firstMobSay :: HasCallStack => Id -> PlaTbl -> (PlaTbl, [Text])
firstMobSay i pt | pt^.ind i.to isNotFirstMobSay = (pt, [])
                 | otherwise                     = (pt & ind i %~ setPlaFlag IsNotFirstMobSay True, [ "", hintSay ])


-----


security :: HasCallStack => ActionFun
security (NoArgs i mq cols) = getSing i <$> getState >>= \s ->
    withDbExHandler "security" (lookupSec s) >>= \case Just []   -> securityHelper i mq cols
                                                       Just recs -> securityChange . last $ recs
                                                       Nothing   -> dbError mq cols
  where
    securityChange SecRec { dbQ, dbA } = do multiWrapSend mq cols [ "You have set your security Q&A as follows:"
                                                                  , "Question: " <> dbQ
                                                                  , "Answer: "   <> dbA ]
                                            promptChangeIt mq cols
                                            setInterp i . Just $ interpConfirmSecurityChange
security p = withoutArgs security p


securityHelper :: HasCallStack => Id -> MsgQueue -> Cols -> MudStack ()
securityHelper i mq cols = do multiWrapSend mq cols $ securityWarn ++ mMempty ++ securityQs
                              promptSecurity mq
                              setInterp i . Just $ interpSecurityNum


securityWarn :: [Text]
securityWarn = [ "IMPORTANT: Resetting your password will require the assistance of a CurryMUD administrator. The \
                 \administrator will prompt you to answer your chosen security question, and will manually verify your \
                 \answer. Which is to say, administrators have access to security Q&A: do NOT provide, in your answer, \
                 \any personal information you do not want an administrator to know!"
               , "If you choose to provide your email address (option #4 below), a new password can simply be emailed \
                 \to you." ]


securityQs :: [Text]
securityQs = [ "Please choose your security question from the following options:"
             , "1) What was the name of your elementary/primary school?"
             , "2) What was the last name of your first grade teacher?"
             , "3) Where were you on New Year's 2000?"
             , "4) What is your email address?"
             , "5) Create your own question."
             , "6) Cancel." ]


promptSecurity :: HasCallStack => MsgQueue -> MudStack ()
promptSecurity = flip sendPrompt "Which will you choose? [1-6]"


interpSecurityNum :: HasCallStack => Interp
interpSecurityNum cn (NoArgs i mq cols) = case cn of
  "1" -> helper "What was the name of your elementary/primary school?"
  "2" -> helper "What was the last name of your first grade teacher?"
  "3" -> helper "Where were you on New Year's 2000?"
  "4" -> helper "What is your email address?"
  "5" -> securityCreateQHelper i mq cols
  "6" -> neverMind i mq
  ""  -> neverMind i mq
  _   -> retrySecurityNum mq cols
  where
    helper q = sequence_ [ promptAnswer mq, setInterp i . Just . interpSecurityA $ q ]
interpSecurityNum _ ActionParams { .. } = retrySecurityNum plaMsgQueue plaCols


promptAnswer :: HasCallStack => MsgQueue -> MudStack ()
promptAnswer = flip sendPrompt "Answer:"


retrySecurityNum :: HasCallStack => MsgQueue -> Cols -> MudStack ()
retrySecurityNum mq cols = do multiWrapSend mq cols $ "Invalid choice." : "" : securityQs
                              promptSecurity mq


interpSecurityA :: HasCallStack => Text -> Interp
interpSecurityA q "" (NoArgs _ mq cols     ) = do wrapSend mq cols $ "Please answer the question, " <> dblQuote q
                                                  promptAnswer mq
interpSecurityA q cn (WithArgs i mq cols as) = securitySetHelper i mq cols q . T.unwords $ cn : as
interpSecurityA _ _  p                       = pmf "interpSecurityA" p


securitySetHelper :: HasCallStack => Id -> MsgQueue -> Cols -> Text -> Text -> MudStack ()
securitySetHelper i mq cols q a = getSing i <$> getState >>= \s -> do
    logPla "securitySetHelper" i "setting security Q&A."
    withDbExHandler_ "interpSecurityA" . insertDbTblSec . SecRec s q $ a
    wrapSend mq cols "Thank you! Your security Q&A has been set."
    sendDfltPrompt mq i
    resetInterp i


interpConfirmSecurityChange :: HasCallStack => Interp
interpConfirmSecurityChange cn (NoArgs i mq cols) = case yesNoHelper cn of
  Just True  -> blankLine mq >> securityHelper i mq cols
  Just False -> neverMind i mq
  Nothing    -> promptRetryYesNo mq cols
interpConfirmSecurityChange _ ActionParams { plaMsgQueue, plaCols } = promptRetryYesNo plaMsgQueue plaCols


securityCreateQHelper :: HasCallStack => Id -> MsgQueue -> Cols -> MudStack ()
securityCreateQHelper i mq cols = do send mq . nlPrefix . nl . T.unlines $ info
                                     sendPrompt mq "Enter your question:"
                                     setInterp i . Just $ interpSecurityCreateQ
  where
    info = "OK. Ideally, your security Q&A should be:" : concatMap (wrapIndent 2 cols) rest
    rest = [ "* Memorable. You don't want to forget your answer."
           , "* Unvarying. Choose a question for which the answer will not change over time."
           , "* Safe. Your answer should not be easily guessed or researched by your friends and acquaintances who \
             \play CurryMUD." ]


interpSecurityCreateQ :: HasCallStack => Interp
interpSecurityCreateQ "" (NoArgs'  i mq     ) = neverMind i mq
interpSecurityCreateQ cn (WithArgs i mq _ as) = do
    promptAnswer mq
    setInterp i . Just . interpSecurityCreateA $ T.unwords $ cn : as
interpSecurityCreateQ _ p = pmf "interpSecurityCreateQ" p


interpSecurityCreateA :: HasCallStack => Text -> Interp
interpSecurityCreateA _ "" (NoArgs'  i mq        ) = neverMind i mq
interpSecurityCreateA q cn (WithArgs i mq cols as) = securitySetHelper i mq cols q . T.unwords $ cn : as
interpSecurityCreateA _ _  p                       = pmf "interpSecurityCreateA" p


-----


setAction :: HasCallStack => ActionFun
setAction (NoArgs i mq cols) = getState >>= \ms ->
    let (styleAbbrevs Don'tQuote -> names, values) = unzip . mkSettingPairs i $ ms
    in logPlaExecArgs "set" [] i >> multiWrapSend mq cols [ padSettingName (n <> ": ") <> v | n <- names | v <- values ]
setAction (Lower i mq cols as) = helper |&| modifyState >=> \(msgs, logMsgs) ->
    logMsgs |#| logPlaOut "set" i >> multiWrapSend mq cols msgs
  where
    helper ms = let (p, msgs, logMsgs) = foldl' (helperSettings i ms) (getPla i ms, [], []) as
                in (ms & plaTbl.ind i .~ p, (msgs, logMsgs))
setAction p = pmf "setAction" p


-----


showAction :: HasCallStack => ActionFun
showAction p@AdviseNoArgs         = advise p ["show"] adviceShowNoArgs
showAction p@AdviseOneArg         = advise p ["show"] adviceShowNoName
showAction p@(Lower i mq cols as) = getState >>= \ms ->
  let next = if isIncognitoId i ms
        then wrapSend mq cols . sorryIncog $ "show"
        else let eqMap      = getEqMap    i ms
                 invCoins   = getInvCoins i ms
                 rmInvCoins = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
             in if
               | ()# eqMap, ()# invCoins -> wrapSend mq cols dudeYou'reScrewed
               | ()# rmInvCoins          -> wrapSend mq cols sorryNoOneHere
               | otherwise               -> case singleArgInvEqRm InRm . last $ as of
                 (InInv, _     ) -> wrapSend mq cols $ sorryShowTarget "item in your inventory"         <> tryThisInstead
                 (InEq,  _     ) -> wrapSend mq cols $ sorryShowTarget "item in your readied equipment" <> tryThisInstead
                 (InRm,  target) ->
                   let argsWithoutTarget                    = init $ case as of [_, _] -> as
                                                                                _      -> (++ pure target) . nub . init $ as
                       (targetGecrs, targetMiss, targetRcs) = uncurry (resolveEntCoinNames i ms . pure $ target) rmInvCoins
                   in if ()# targetMiss && ()!# targetRcs
                     then wrapSend mq cols . sorryShowTarget $ "coin"
                     else case procGecrMisRm . head . zip targetGecrs $ targetMiss of
                       Left  msg        -> wrapSend mq cols msg
                       Right [targetId] ->
                         let d         = mkStdDesig i ms DoCap
                             theTarget = IdSingTypeDesig { theId    = targetId
                                                         , theSing  = getSing targetId ms
                                                         , theType  = getType targetId ms
                                                         , theDesig = serialize . mkStdDesig targetId ms $ Don'tCap }
                             (inInvs, inEqs, inRms)         = sortArgsInvEqRm InInv argsWithoutTarget
                             (invToSelfs, invBs, invLogMsg) = inInvs |!| showInv ms d invCoins inInvs theTarget
                             (eqToSelfs,  eqBs,  eqLogMsg ) = inEqs  |!| showEq  ms d eqMap    inEqs  theTarget
                             sorryRmMsg                     = inRms  |!| sorryShowInRm
                         in if theType theTarget `notElem` [ NpcType, PlaType ]
                           then wrapSend mq cols . sorryShowTarget . theSing $ theTarget
                           else do
                               let logMsg = slashes . dropBlanks $ [ invLogMsg |!| parensQuote "inv" |<>| invLogMsg
                                                                   , eqLogMsg  |!| parensQuote "eq"  |<>| eqLogMsg ]
                               logMsg |#| logPla "show" i . (T.concat [ "showing to "
                                                                      , theSing theTarget
                                                                      , ": " ] <>)
                               multiWrapSend mq cols . dropBlanks $ sorryRmMsg : [ parseInBands i ms msg
                                                                                 | msg <- invToSelfs ++ eqToSelfs ]
                               bcastNl $ invBs ++ eqBs
                       Right _ -> wrapSend mq cols sorryShowExcessTargets
  in checkActing p ms (Right "show an item to another person") [ Attacking, Drinking, Sacrificing ] next
  where
    tryThisInstead = " Try showing something to someone in your current room."
    showInv ms d invCoins inInvs IdSingTypeDesig { .. }
      | ()!# invCoins =
          let (eiss, ecs)                         = uncurry (resolveMobInvCoins i ms inInvs) invCoins
              showInvHelper                       = foldl' helperEitherInv mempty eiss
              helperEitherInv acc (Left  msg    ) = acc & _1 <>~ pure msg
              helperEitherInv acc (Right itemIds) = acc & _1 <>~ mkToSelfMsgs itemIds
                                                        & _2 <>~ mkBs
                                                        & _3 <>~ pure mkLogMsg
                where
                  mkBs     = mkToTargetBs itemIds ++ mkToOthersBs itemIds
                  mkLogMsg = commas . map (`getSing` ms) $ itemIds
              mkToSelfMsgs itemIds = [ T.concat [ "You show the ", n, " to ", theDesig, "." ]
                                     | itemId <- itemIds
                                     , let n = if getType itemId ms == CorpseType
                                                 then mkCorpseAppellation i ms itemId
                                                 else getSing itemId ms ]
              mkToTargetBs = map f
                where
                  f itemId = let (n, t) | getType itemId ms == CorpseType
                                        , ca <- mkCorpseAppellation theId ms itemId
                                        = (ca, expandCorpseTxt ca . getCorpseDesc itemId $ ms)
                                        | otherwise = (getSing `fanUncurry` getEntDesc) (itemId, ms)
                             in ( T.concat [ serialize d
                                           , " shows you "
                                           , underline . aOrAn $ n
                                           , " "
                                           , parensQuote "carried"
                                           , nl ":"
                                           , t ]
                                , pure theId )
              mkToOthersBs itemIds = concatMap f $ desigIds d \\ [ i, theId ]
                where
                  f targetId = foldl' g [] itemIds
                    where
                      g acc itemId = let n | getType itemId ms == CorpseType = mkCorpseAppellation targetId ms itemId
                                           | otherwise                       = getSing itemId ms
                                     in acc ++ pure ( T.concat [ serialize d
                                                               , " shows "
                                                               , aOrAn n
                                                               , " "
                                                               , parensQuote "carried"
                                                               , " to "
                                                               , theDesig
                                                               , "." ]
                                                    , pure targetId )
              -----
              (canCoins, can'tCoinMsgs) = distillEcs ecs
              showCoinsHelper           = ( can'tCoinMsgs     ++ pure mkToSelfCoinsMsg
                                          , mkToTargetCoinsBs ++ mkToOthersCoinsBs )
              coinTxt           = mkCoinTxt canCoins
              mkToSelfCoinsMsg  = coinTxt |!|                 T.concat   [ "You show "
                                                                         , coinTxt
                                                                         , " to "
                                                                         , theDesig
                                                                         , "." ]
              mkToTargetCoinsBs = coinTxt |!| mkBcast theId . T.concat $ [ serialize d
                                                                         , " shows you "
                                                                         , underline coinTxt
                                                                         , "." ]
              mkToOthersCoinsBs = coinTxt |!| [(T.concat [ serialize d
                                                         , " shows "
                                                         , aCoinSomeCoins canCoins
                                                         , " to "
                                                         , theDesig
                                                         , "." ], desigIds d \\ [ i, theId ])]
          in let (toSelfMsgs, bs, logMsgs)  = showInvHelper
                 (toSelfCoinsMsgs, coinsBs) = showCoinsHelper
             in (toSelfMsgs ++ toSelfCoinsMsgs, bs ++ coinsBs, slashes . dropEmpties $ [ slashes logMsgs, coinTxt ])
      | otherwise = (pure dudeYourHandsAreEmpty, , ) mempty mempty
    showEq ms d eqMap inEqs IdSingTypeDesig { .. }
      | ()!# eqMap =
          let (gecrs, miss, rcs)                  = resolveEntCoinNames i ms inEqs (M.elems eqMap) mempty
              eiss                                = zipWith (curry procGecrMisMobEq) gecrs miss
              showEqHelper                        = foldl' helperEitherInv mempty eiss
              helperEitherInv acc (Left  msg    ) = acc & _1 <>~ pure msg
              helperEitherInv acc (Right itemIds) = acc & _1 <>~ mkToSelfMsgs itemIds
                                                        & _2 <>~ mkBs
                                                        & _3 <>~ pure mkLogMsg
                where
                  mkBs     = mkToTargetBs itemIds ++ mkToOthersBs itemIds
                  mkLogMsg = commas . map (`getSing` ms) $ itemIds
              mkToSelfMsgs     itemIds = [ T.concat [ "You show the ", getSing itemId ms, " to ", theDesig, "." ]
                                         | itemId <- itemIds ]
              mkToTargetBs     itemIds = [ ( T.concat [ serialize d
                                                      , " shows you "
                                                      , underline . aOrAn . getSing itemId $ ms
                                                      , let t = descSlotForId i ms itemId eqMap in t |!| spcL t
                                                      , nl ":"
                                                      , getEntDesc itemId ms ]
                                           , pure theId )
                                         | itemId <- itemIds ]
              mkToOthersBs     itemIds = [ ( T.concat [ serialize d
                                                      , " shows "
                                                      , aOrAn . getSing itemId $ ms
                                                      , let t = descSlotForId i ms itemId eqMap in t |!| spcL t
                                                      , " to "
                                                      , theDesig
                                                      , "." ]
                                           , desigIds d \\ [ i, theId ] )
                                         | itemId <- itemIds ]
              -----
              showCoinsInEqHelper = rcs |!| sorryEquipCoins
          in let (toSelfMsgs, bs, logMsgs) = showEqHelper in (showCoinsInEqHelper : toSelfMsgs, bs, slashes logMsgs)
      | otherwise = (pure dudeYou'reNaked, , ) mempty mempty
showAction p = pmf "showAction" p


-----


smell :: HasCallStack => ActionFun
smell (NoArgs i mq cols) = getState >>= \ms ->
    let corpseMsgs = mkCorpseMsgs ms
        ts         = views rmSmell (maybe a b) . getMobRm i $ ms
        a          = ()# corpseMsgs ? pure noSmellMsg :? corpseMsgs
        b          = onFalse (()# corpseMsgs) (++ corpseMsgs) . pure
    in do logPlaExec "smell" i
          multiWrapSend mq cols ts
          let d = mkStdDesig i ms DoCap
          bcastIfNotIncogNl i . pure . ((<> " smells the air.") . serialize &&& (i `delete`) . desigIds) $ d
  where
    mkCorpseMsgs ms = concatMap (helper ms) [ (getInv, f "carried"), (getMobRmInv, f "on the ground") ]
      where
        f t = let t' = spcL . parensQuote $ t in (<> t')
    helper ms (f, g) = foldr (\i' acc -> maybe acc (: acc) . mkMaybeCorpseSmellMsg i ms i' $ g) [] . uncurry f $ (i, ms)
smell p@(OneArgLower i mq cols a) = getState >>= \ms ->
    let (invCoins, eqMap) = (getInvCoins `fanUncurry` getEqMap) (i, ms)
        rmInvCoins = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
        maybeHooks = lookupHooks i ms "smell"
        d          = mkStdDesig  i ms DoCap
        f          = if and [ ()# invCoins, ()# eqMap, ()# rmInvCoins, ()# maybeHooks ]
                       then sorry sorrySmellNothingToSmell
                       else case singleArgInvEqRm InInv a of
                         (InInv, target) | ()# invCoins                   -> sorry dudeYourHandsAreEmpty
                                         | otherwise                      -> smellInv ms d invCoins target
                         (InEq,  target) | ()# eqMap                      -> sorry dudeYou'reNaked
                                         | otherwise                      -> smellEq ms d eqMap target
                         (InRm,  target) | ()# rmInvCoins, ()# maybeHooks -> sorry sorrySmellEmptyRmNoHooks
                                         | otherwise                      -> smellRm ms d rmInvCoins maybeHooks target
    in checkActing p ms (Right "smell an item") [ Drinking, Eating, Sacrificing ] f
  where
    sorry                         = wrapSend mq cols
    smellInv ms d invCoins target =
        let pair@(eiss, ecs) = uncurry (resolveMobInvCoins i ms . pure $ target) invCoins
        in if uncurry (&&) . ((()!#) *** (()!#)) $ pair
          then sorry sorrySmellExcessTargets
          else case eiss of
            [] -> let (canCoins, can'tCoinMsgs) = distillEcs ecs
                  in case can'tCoinMsgs of
                    []    -> let (coinTxt, isPlur) = mkCoinPieceTxt canCoins
                                 smellDesc         = T.concat [ "The "
                                                              , coinTxt
                                                              , " smell"
                                                              , not isPlur |?| "s"
                                                              , " of metal, with just a hint of grime." ]
                                 bs                = pure (T.concat [ serialize d
                                                                    , " smells "
                                                                    , aCoinSomeCoins canCoins
                                                                    , "." ], i `delete` desigIds d)
                                 logMsg            = prd $ "smelled " <> aCoinSomeCoins canCoins
                             in ioHelper smellDesc bs logMsg
                    (t:_) -> sorry t
            (eis:_) -> case eis of
              Left  msg        -> sorry msg
              Right [targetId] -> let (targetSing, t) = (getSing `fanUncurry` getType) (targetId, ms)
                                      ic              = t == CorpseType
                                      smellDesc       = case t of
                                        LightType | getLightIsLit targetId ms -> sorrySmellLitLight targetSing
                                        VesselType                            -> case getVesselCont targetId ms of
                                          Nothing     -> the' . (<> " is empty.") . getSing targetId $ ms
                                          Just (l, _) -> l^.liqSmellDesc
                                        _ -> getEntSmell targetId ms
                                      bs = map f $ i `delete` desigIds d
                                        where
                                          f i' = (T.concat [ serialize d
                                                           , " smells "
                                                           , aOrAn (ic ? mkCorpseAppellation i' ms targetId :? targetSing)
                                                           , " "
                                                           , parensQuote "carried"
                                                           , "." ], pure i')
                                      logMsg = T.concat [ "smelled ", aOrAn targetSing, " ", parensQuote "carried", "." ]
                                  in ioHelper smellDesc bs logMsg
              Right _          -> sorry sorrySmellExcessTargets
    -----
    smellEq ms d eqMap target =
        let (gecrs, miss, rcs) = resolveEntCoinNames i ms (pure target) (M.elems eqMap) mempty
            eis                = procGecrMisMobEq . head . zip gecrs $ miss
        in if ()!# rcs
          then sorry sorryEquipCoins
          else case eis of
            Left  msg        -> sorry msg
            Right [targetId] ->
                let (targetSing, smellDesc) = (getSing `fanUncurry` getEntSmell) (targetId, ms)
                    slotDesc = descSlotForId i ms targetId eqMap
                    bs       = pure (T.concat [ serialize d
                                              , " smells "
                                              , aOrAn targetSing
                                              , slotDesc |!| spcL slotDesc
                                              , "." ], i `delete` desigIds d)
                    logMsg   = T.concat [ "smelled ", aOrAn targetSing, " ", slotDesc, "." ]
                    res      = join (checkSlotSmellTaste targetSing <$> lookupMapValue targetId eqMap)
                in case getType targetId ms of
                  LightType | getLightIsLit targetId ms -> sorry . sorrySmellLitLight $ targetSing
                  _ -> uncurry3 ioHelper . (, bs, logMsg) . fromMaybe smellDesc $ res
            Right _ -> sorry sorrySmellExcessTargets
    -----
    smellRm ms d invCoins maybeHooks target = -- You can smell a mob or a corpse in your current room.
        let pair@(eiss, ecs) = uncurry (resolveRmInvCoins i ms . pure $ target) invCoins
        in (uncurry (&&) . ((()!#) *** (()!#)) $ pair) ? sorry sorrySmellExcessTargets :? case eiss of
          []      -> sorry $ let (canCoins, can'tCoinMsgs) = distillEcs ecs
                             in case can'tCoinMsgs of []    -> sorrySmellRmCoins . mkCoinPieceTxt $ canCoins
                                                      (x:_) -> x
          (eis:_) -> case ((()!#) *** (()!#)) (invCoins, maybeHooks) of
            (True,  False) -> smellRmHelper eis
            (False, True ) -> let helper v ms' | tuple <- procHooks i ms' v "smell" . pure $ target
                                               , (targets', (ms'', hooksToSelfs, hooksBs, hooksLogMsgs), fs) <- tuple
                                               , sorryMsgs <- targets' |!| pure sorrySmellEmptyRmWithHooks
                                               = (ms'', [ hooksLogMsgs |#| logPla "smell" i . prd . slashes
                                                        , sorryMsgs    |#| multiWrapSend mq cols
                                                        , hooksToSelfs |#| multiWrapSend mq cols
                                                        , bcastIfNotIncogNl i hooksBs
                                                        , sequence_ fs ])
                              in mkRndmVector >>= \v -> helper v |&| modifyState >=> sequence_
            (True,  True ) -> let helper v ms' | tuple <- procHooks i ms' v "smell" . pure $ target
                                               , (targets', (ms'', hooksToSelfs, hooksBs, hooksLogMsgs), fs) <- tuple
                                               = if ()# targets'
                                                   then (ms'', [ hooksLogMsgs |#| logPla "smell" i . prd . slashes
                                                               , hooksToSelfs |#| multiWrapSend mq cols
                                                               , bcastIfNotIncogNl i hooksBs
                                                               , sequence_ fs ])
                                                   else (ms', pure . smellRmHelper $ eis)
                              in mkRndmVector >>= \v -> helper v |&| modifyState >=> sequence_
            x              -> pmf "smell smellRm" x
      where
        smellRmHelper = \case
          Left  msg        -> sorry msg
          Right [targetId] -> let (targetSing, smellDesc) = (getSing `fanUncurry` getEntSmell) (targetId, ms)
                                  targetDesig = serialize . mkStdDesig targetId ms $ Don'tCap
                                  bs          = [ (T.concat [ serialize d
                                                            , " smells "
                                                            , targetDesig
                                                            , "." ], desigIds d \\ [ i, targetId ])
                                                , (serialize d <> " smells you.", pure targetId) ]
                                  logMsg      = parseInBandsSuffix i ms . prd $ "smelled " <> targetDesig
                                  smellMob    = ioHelper smellDesc bs logMsg
                                  smellCorpse = let corpseBs = map f $ i `delete` desigIds d
                                                      where
                                                        f i' = (T.concat [ serialize d
                                                                         , " smells "
                                                                         , aOrAn . mkCorpseAppellation i' ms $ targetId
                                                                         , " "
                                                                         , parensQuote "on the ground"
                                                                         , "." ], pure i')
                                                    corpseLogMsg = T.concat [ "smelled "
                                                                            , aOrAn targetSing
                                                                            , " "
                                                                            , parensQuote "on the ground"
                                                                            , "." ]
                                                in ioHelper smellDesc corpseBs corpseLogMsg
                              in case getType targetId ms of NpcType    -> smellMob
                                                             PlaType    -> smellMob
                                                             CorpseType -> smellCorpse
                                                             _          -> sorry . sorrySmellRmNoHooks $ targetSing
          Right _          -> sorry sorrySmellExcessTargets
    -----
    ioHelper = smellTasteIOHelper "smell" i mq cols
smell p = advise p ["smell"] adviceSmellExcessArgs


smellTasteIOHelper :: HasCallStack => Text
                                   -> Id
                                   -> MsgQueue
                                   -> Cols
                                   -> Text
                                   -> [Broadcast]
                                   -> Text
                                   -> MudStack ()
smellTasteIOHelper fn i mq cols msg bs logMsg = logPla fn i logMsg >> wrapSend mq cols msg >> bcastIfNotIncogNl i bs


-----


spiritDispCmdList :: HasCallStack => ActionFun
spiritDispCmdList p@(LowerNub' i as) = logPlaExecArgs "?" as i >> dispCmdList spiritCmds p
spiritDispCmdList p                  = pmf "spiritDispCmdList" p


-----


stats :: HasCallStack => ActionFun
stats (NoArgs i mq cols) = getState >>= \ms ->
    let mkStats   = dropEmpties [ top
                                , xpsHelper
                                , prd . capitalize . pp . getHand i $ ms
                                , prd $ "Known languages: " <> commas [ pp lang | lang <- sort . getKnownLangs i $ ms ]
                                , prd $ "Level " <> showTxt l
                                , prd $ commaShow expr <> " experience point" <> sOnNon1 expr
                                , T.concat [ commaShow nxt, " experience point", sOnNon1 nxt, " to next level." ]
                                , let pts = getSkillPts i ms in prd $ commaShow pts <> " unspent skill point" <> sOnNon1 pts
                                , mobRmDescHelper
                                , tempDescHelper ]
        top       = underline . onTrue (isPla i ms) (quoteWith' (spiritTxt, sexRace)) . getSing i $ ms
        spiritTxt = isSpiritId i ms |?| "The disembodied spirit of "
        sexRace   = T.concat [ ", the ", sexy, " ", r ]
        (sexy, r) = mkPrettySexRace i ms
        xpsHelper | (hps, mps, pps, fps) <- getPts i ms
                  = spaces [ f "h" hps, f "m" mps, f "p" pps, f "f" fps ]
          where
            f a pair@(both %~ commaShow -> (x, y)) = T.concat [ colorWith (mkColorTxtForXps pair) x, "/", y, a, "p" ]
        (l, expr)       = getLvlExp i ms
        nxt             = subtract expr . snd $ calcLvlExps !! l
        mobRmDescHelper = maybeEmp (prd . ("Your room description is " <>))        $ dblQuote <$> getMobRmDesc i ms
        tempDescHelper  = maybeEmp ("Your temporary character description is " <>) $ dblQuote <$> getTempDesc  i ms
    in logPlaExec "stats" i >> multiWrapSend mq cols mkStats
stats p = withoutArgs stats p


-----


stop :: HasCallStack => ActionFun
stop p@(NoArgs i mq cols) = getState >>= \ms -> case filter (view _3) . mkStopTuples p $ ms of
  []                     -> wrapSend mq cols sorryStopNotDoingAnything
  ((_, actType, _, f):_) -> stopLogHelper i (pure actType) >> f
stop p@(OneArgLower i mq cols a) = getState >>= \ms ->
    if ((||) <$> (`T.isPrefixOf` "all") <*> (== T.singleton allChar)) a
      then case filter (view _3) . mkStopTuples p $ ms of [] -> wrapSend mq cols sorryStopNotDoingAnything
                                                          xs -> ((>>) <$> stopLogHelper i . select _2 <*> mapM_ (view _4)) xs
      else case filter (views _1 (a `T.isPrefixOf`)) . mkStopTuples p $ ms of
        []                     -> wrapSend mq cols . sorryStopActName $ a
        ((_, actType, b, f):_) -> b ? (stopLogHelper i (pure actType) >> f) :? wrapSend mq cols (sorryStopNotDoing actType)
stop p = advise p ["stop"] adviceStopExcessArgs


stopLogHelper :: HasCallStack => Id -> [ActType] -> MudStack ()
stopLogHelper i [actType] = logPla "stop" i . prd $ "stopped " <> pp actType
stopLogHelper i actTypes  = logPla "stop" i . prd $ "stopped " <> T.intercalate (spaced "and") (map pp actTypes)


mkStopTuples :: HasCallStack => ActionParams -> MudState -> [(Text, ActType, Bool, MudStack ())]
mkStopTuples p@ActionParams { myId } ms = map (\(a, b, c) -> (pp a, a, uncurry b (myId, ms), uncurry c (p, ms))) xs
  where
    xs = [ (Sacrificing, isSacrificing, stopSacrificing)
         , (Drinking,    isDrinking,    stopDrinking   )
         , (Eating,      isEating,      stopEating     )
         , (Attacking,   isAttacking,   stopAttacking  ) ]


-----


taste :: HasCallStack => ActionFun
taste p@AdviseNoArgs              = advise p ["taste"] adviceTasteNoArgs
taste p@(OneArgLower i mq cols a) = getState >>= \ms ->
    let (invCoins, eqMap) = (getInvCoins `fanUncurry` getEqMap) (i, ms)
        d                 = mkStdDesig  i ms DoCap
        f                 = if uncurry (&&) . ((()#) *** (()#)) $ (invCoins, eqMap)
                              then sorry sorryTasteNothingToTaste
                              else case singleArgInvEqRm InInv a of
                                (InInv, target) | ()# invCoins -> sorry dudeYourHandsAreEmpty
                                                | otherwise    -> tasteInv ms d invCoins target
                                (InEq,  target) | ()# eqMap    -> sorry dudeYou'reNaked
                                                | otherwise    -> tasteEq  ms d eqMap    target
                                (InRm,  _     )                -> sorry sorryTasteInRm
    in checkActing p ms (Right "taste an item") [ Drinking, Eating, Sacrificing ] f
  where
    sorry                         = wrapSend mq cols
    tasteInv ms d invCoins target =
        let pair@(eiss, ecs) = uncurry (resolveMobInvCoins i ms . pure $ target) invCoins
        in (uncurry (&&) . ((()!#) *** (()!#)) $ pair) ? sorry sorryTasteExcessTargets :? case eiss of
          (eis:_) -> case eis of
            Left  msg        -> sorry msg
            Right [targetId] -> let (targetSing, t) = (getSing `fanUncurry` getType) (targetId, ms)
                                    ic              = t == CorpseType
                                    tasteDesc       = case t of
                                      LightType | getLightIsLit targetId ms -> sorryTasteLitLight targetSing
                                      VesselType                            -> case getVesselCont targetId ms of
                                        Nothing     -> the' $ targetSing <> " is empty."
                                        Just (l, _) -> l^.liqTasteDesc
                                      _ -> getObjTaste targetId ms
                                    bs = map f $ i `delete` desigIds d
                                      where
                                        f i' = (T.concat [ serialize d
                                                         , " tastes "
                                                         , aOrAn (ic ? mkCorpseAppellation i' ms targetId :? targetSing)
                                                         , " "
                                                         , parensQuote "carried"
                                                         , "." ], pure i')
                                    logMsg = T.concat [ "tasted ", aOrAn targetSing, " ", parensQuote "carried", "." ]
                                in ioHelper tasteDesc bs logMsg
            Right _          -> sorry sorryTasteExcessTargets
          _ -> let (canCoins, can'tCoinMsgs) = distillEcs ecs in case can'tCoinMsgs of
            []    -> let (coinTxt, _) = mkCoinPieceTxt canCoins
                         tasteDesc    = "You are first struck by an unmistakably metallic taste, followed soon by the \
                                        \salty essence of sweat and waxy residue left by the hands of the many people \
                                        \who handled the " <> coinTxt <> " before you."
                         bs           = pure (T.concat [ serialize d
                                                       , " tastes "
                                                       , aCoinSomeCoins canCoins
                                                       , "." ], i `delete` desigIds d)
                         logMsg       = prd $ "tasted " <> aCoinSomeCoins canCoins
                     in ioHelper tasteDesc bs logMsg
            (t:_) -> sorry t
    -----
    tasteEq ms d eqMap target | (gecrs, miss, _) <- resolveEntCoinNames i ms (pure target) (M.elems eqMap) mempty =
        case zip gecrs miss of
          []       -> sorry sorryEquipCoins
          (pair:_) -> case procGecrMisMobEq pair of
            Left  msg        -> sorry msg
            Right [targetId] -> let (targetSing, tasteDesc) = (getSing `fanUncurry` getObjTaste) (targetId, ms)
                                    slotDesc = descSlotForId i ms targetId eqMap
                                    bs       = pure (T.concat [ serialize d
                                                              , " tastes "
                                                              , aOrAn targetSing
                                                              , slotDesc |!| spcL slotDesc
                                                              , "." ], i `delete` desigIds d)
                                    logMsg   = T.concat [ "tasted ", aOrAn targetSing, " ", slotDesc, "." ]
                                    res      = join (checkSlotSmellTaste targetSing <$> lookupMapValue targetId eqMap)
                                in case getType targetId ms of
                                  LightType | getLightIsLit targetId ms -> sorry . sorryTasteLitLight $ targetSing
                                  _ -> uncurry3 ioHelper . (, bs, logMsg) . fromMaybe tasteDesc $ res
            Right _          -> sorry sorryTasteExcessTargets
    -----
    ioHelper = smellTasteIOHelper "taste" i mq cols
taste p = advise p ["taste"] adviceTasteExcessArgs


-----


tele :: HasCallStack => ActionFun
tele p@AdviseNoArgs                         = advise p ["telepathy"] adviceTeleNoArgs
tele p@AdviseOneArg                         = advise p ["telepathy"] adviceTeleNoMsg
tele   (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let (s, p) = (getSing `fanUncurry` getPla) (i, ms) in if isIncognito p
      then wrapSend mq cols . sorryIncog $ "telepathy"
      else let SingleTarget { .. } = mkSingleTarget mq cols target "The name of the person you wish to message"
               notFound            = sendFun . notFoundSuggestAsleeps target asleeps $ ms
               found targetSing    =
                   let helper targetId = case emotifyTwoWay "telepathy" i ms targetId msg of
                         Left  errorMsgs  -> multiSendFun errorMsgs
                         Right (Right bs) -> ioHelper targetId bs
                         Right (Left  ()) -> case expCmdifyTwoWay i ms targetId targetSing msg of
                           Left  errorMsg -> sendFun errorMsg
                           Right bs       -> ioHelper targetId bs
                       ioHelper targetId bs = let (b1@(toSelf, _), b2) = formatBs targetId bs in do
                           logPlaOut "telepathy" i . pure $ toSelf
                           bcastNl . consLocPrefBcast i $ [ b1, b2 ]
                           alertMsgHelper i "telepathy" toSelf
                           ts <- liftIO mkTimestamp
                           withDbExHandler_ "tele" . insertDbTblTele . TeleRec ts s targetSing $ toSelf
                       formatBs targetId [toMe, toTarget] = let f n m = bracketQuote n |<>| m
                                                            in ( toMe     & _1 %~ f s
                                                               , toTarget & _1 %~ f (mkStyled targetId) )
                       formatBs _        bs               = pmf "tele found formatBs" bs
                       mkStyled targetId = let (target'sAwakes, _) = getDblLinkedSings targetId ms
                                               styleds             = styleAbbrevs Don'tQuote target'sAwakes
                                           in head . filter ((== s) . dropANSI) $ styleds
                   in either sendFun helper . checkMutuallyTuned i ms $ targetSing
               (awakes, asleeps) = getDblLinkedSings i ms
           in findFullNameForAbbrev strippedTarget awakes |&| maybe notFound found
tele p = pmf "tele" p


-----


tempDescAction :: HasCallStack => ActionFun
tempDescAction (NoArgs i mq cols) = do
    logPla "tempDescAction" i "clearing temporary character description."
    tweak $ mobTbl.ind i.tempDesc .~ Nothing
    wrapSend mq cols "Your temporary character description has been cleared."
tempDescAction (Msg i mq cols desc@(dblQuote -> desc')) = if T.length desc > maxTempDescLen
  then wrapSend mq cols $ "A temporary character description cannot exceed " <> showTxt maxTempDescLen <> " \
                          \characters in length."
  else do
    logPla "tempDescAction" i $ "setting temporary character description set to " <> desc'
    tweak $ mobTbl.ind i.tempDesc ?~ desc
    wrapSend mq cols $ "Your temporary character description has been set to " <> desc'
tempDescAction p = pmf "tempDescAction" p


-----


time :: HasCallStack => ActionFun
time (NoArgs i mq cols) = mIf (isOutside i <$> getState)
  (logPlaOut  "time" i . pure =<< showTime mq cols)
  (logPlaExec "time" i >> wrapSend mq cols sorryTimeNotOutside)
time p = withoutArgs time p


-----


tune :: HasCallStack => ActionFun
tune (NoArgs i mq cols) = getState >>= \ms ->
    let linkPairs   = map (dupFirst (`getIdForPCSing` ms)) . getLinked i $ ms
        linkSings   = sort . map snd . filter (isDblLinked ms . (i, ) . fst) $ linkPairs
        styleds     = styleAbbrevs Don'tQuote linkSings
        linkTunings = map (linkTbl M.!) linkSings
        linkTbl     = getTeleLinkTbl i ms
        (chanNames, chanTunings)   = mkChanNamesTunings i ms
        helper title names tunings = let txts = mkConnTxts in [ title, ()!# txts ? commas txts :? none ]
          where
            mkConnTxts = [ n <> T.cons '=' (inOut t) | n <- names | t <- tunings ]
    in do logPlaExecArgs "tune" [] i
          let msgs = [ helper "Two-way telepathic links:" styleds linkTunings
                     , mMempty
                     , helper "Telepathic channels:" (styleAbbrevs Don'tQuote chanNames) chanTunings ]
          multiWrapSend mq cols . concat $ msgs
tune (Lower' i as) = helper |&| modifyState >=> \(bs, logMsgs) -> logMsgs |#| logPlaOut "tune" i >> bcastNl bs
  where
    helper ms = let s       = getSing i ms
                    linkTbl = getTeleLinkTbl i ms
                    chans   = getPCChans     i ms
                    (linkTbl', chans', msgs, logMsgs) = foldl' (helperTune s) (linkTbl, chans, [], []) as
                in ( upd ms [ teleLinkMstrTbl.ind i .~ linkTbl'
                            , chanTbl %~ flip (foldr (\c -> ind (c^.chanId) .~ c)) chans' ]
                   , (mkBcast i . T.unlines $ msgs, logMsgs) )
tune p = pmf "tune" p


-----


typo :: HasCallStack => ActionFun
typo p@AdviseNoArgs = advise p ["typo"] adviceTypoNoArgs
typo p              = bugTypoLogger p TypoLog


-----


unlink :: HasCallStack => ActionFun
unlink p@AdviseNoArgs            = advise p ["unlink"] adviceUnlinkNoArgs
unlink p@(LowerNub i mq cols as) = getState >>= \ms ->
    let (f, guessWhat) | any hasLocPref as = (stripLocPref, sorryUnlinkIgnore)
                       | otherwise         = (id,           ""               )
        as'                                = map (capitalize . T.toLower . f) as
    in checkActing p ms (Right "sever a telepathic link") (pure Sacrificing) $ do
        tingleLoc <- rndmElem [ "behind your eyes"
                              , "deep in your lower back"
                              , "in your scalp"
                              , "on the back of your neck"
                              , "somewhere between your ears" ]
        res <- helperLinkUnlink ms i mq cols
        flip maybeVoid res $ \(meLinkedToOthers, othersLinkedToMe, twoWays) ->
            let helper ms' = let (ms'', bs, logMsgs) = foldl' procArg (ms', [], []) as'
                             in (ms'', (bs, logMsgs))
                procArg a@(ms', _, _) targetSing = if
                  | targetSing `elem` twoWays ++ meLinkedToOthers ++ othersLinkedToMe -> procArgHelper
                  | otherwise -> sorry $ sorryUnlinkName targetSing |<>| hintUnlink
                  where
                    sorry msg = a & _2 <>~ mkBcast i (nlnl msg)
                    procArgHelper
                      | targetId <- getIdForPCSing targetSing ms'
                      , myPla    <- getPla i ms'
                      = if not $ hasPp i ms' 5 || isSpiritId targetId ms'
                          then sorry . sorryPp $ "sever your link with " <> targetSing
                          else let srcMsg   = T.concat [ focusingInnateMsg, "you sever your link with ", targetSing, "." ]
                                   s        = getSing i ms'
                                   targetBs | colorize <- colorWith unlinkColor
                                            , bs       <- mkBcast targetId . nlnl . colorize . unlinkMsg tingleLoc $ s
                                            = isAwake targetId ms' |?| bs
                                   ms''     = upd ms' [ teleLinkMstrTbl.ind i       .at targetSing .~ Nothing
                                                      , teleLinkMstrTbl.ind targetId.at s          .~ Nothing
                                                      , pcTbl .ind i       .linked %~ (targetSing `delete`)
                                                      , pcTbl .ind targetId.linked %~ (s          `delete`)
                                                      , mobTbl.ind i       .curPp  -~ onTrue (isSpirit myPla) (const 0) 5 ]
                               in a & _1 .~  ms''
                                    & _2 <>~ (nlnl srcMsg, pure i) : targetBs
                                    & _3 <>~ pure targetSing
            in helper |&| modifyState >=> \(bs, logMsgs) -> do
                logMsgs |#| logPla "unlink" i . slashes
                bcast . onFalse (()# guessWhat) ((guessWhat, pure i) :) $ bs
unlink p = pmf "unlink" p


-----


unready :: HasCallStack => ActionFun
unready p@AdviseNoArgs     = advise p ["unready"] adviceUnreadyNoArgs
unready p@(LowerNub' i as) = genericAction p helper "unready"
  where
    helper ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InEq as
            sorryInInv             = inInvs |!| sorryUnreadyInInv
            sorryInRm              = inRms  |!| sorryUnreadyInRm
            sorryCoins             = rcs    |!| sorryUnreadyCoins
            d                      = mkStdDesig i ms DoCap
            is                     = M.elems . getEqMap i $ ms
            (gecrs, miss, rcs)     = resolveEntCoinNames i ms inEqs is mempty
            eiss                   = zipWith (curry procGecrMisMobEq) gecrs miss
            (et, it, toSelfs, bs, logMsgs) = foldl' (helperUnready i ms d) (ms^.eqTbl, ms^.invTbl, [], [], []) eiss
        in genericCheckActing i ms (Right "unready an item") [ Drinking, Sacrificing ] $ if ()!# is
          then (ms & eqTbl .~ et & invTbl .~ it, ( dropBlanks $ [ sorryInInv, sorryInRm, sorryCoins ] ++ toSelfs
                                                 , bs
                                                 , logMsgs ))
          else genericSorry ms dudeYou'reNaked
unready p = pmf "unready" p


-----


uptime :: HasCallStack => ActionFun
uptime (NoArgs i mq cols) = sequence_ [ logPlaExec "uptime" i, wrapSend mq cols =<< uptimeHelper =<< getUptime ]
uptime p                  = withoutArgs uptime p


getUptime :: HasCallStack => MudStack Int64
getUptime = ((-) `on` sec) <$> liftIO (getTime Monotonic) <*> asks (view startTime)


uptimeHelper :: HasCallStack => Int64 -> MudStack Text
uptimeHelper up = helper <$> getSum `fmap2` getRecordUptime
  where
    helper         = maybe mkUptimeTxt (\recUp -> up > recUp ? mkNewRecTxt :? mkRecTxt recUp)
    mkUptimeTxt    = mkTxtHelper "."
    mkNewRecTxt    = mkTxtHelper $ " - " <> colorWith newRecordColor "it's a new record!"
    mkRecTxt recUp = mkTxtHelper . prd . spcL . parensQuote $ ("record uptime: " <> renderIt recUp)
    mkTxtHelper    = ("Up " <>) . (renderIt up <>)
    renderIt       = T.pack . renderSecs . fromIntegral


getRecordUptime :: HasCallStack => MudStack (Maybe (Sum Int64))
getRecordUptime = liftIO (mkMudFilePath uptimeFileFun) >>= \file ->
    let readUptime = Just . Sum . read <$> readFile file
    in mIf (liftIO . doesFileExist $ file)
           (liftIO readUptime `catch` (emptied . fileIOExHandler "getRecordUptime"))
           mMempty


-----


vulpenoidean :: HasCallStack => ActionFun
vulpenoidean = sayHelper VulpenoidLang


-----


whisper :: HasCallStack => ActionFun
whisper p@AdviseNoArgs                                      = advise p ["whisper"] adviceWhisperNoArgs
whisper p@AdviseOneArg                                      = advise p ["whisper"] adviceWhisperNoMsg
whisper p@(WithArgs i mq cols (target:(T.unwords -> rest))) = getState >>= \ms ->
    checkActing p ms (Right "whisper") allValues $ if isIncognitoId i ms
      then wrapSend mq cols . sorryIncog $ "whisper"
      else helper |&| modifyState >=> ioHelper ms
  where
    helper ms = let d        = mkStdDesig i ms DoCap
                    invCoins = first (i `delete`) . getMobRmVisibleInvCoins i $ ms
                in if ()!# invCoins
                  then case singleArgInvEqRm InRm target of
                    (InInv, _      ) -> sorry sorryWhisperInInv
                    (InEq,  _      ) -> sorry sorryWhisperInEq
                    (InRm,  target') -> case uncurry (resolveRmInvCoins i ms . pure $ target') invCoins of
                      (_,                    [Left [msg]]) -> sorry msg
                      (_,                    Right _:_   ) -> sorry sorryWhisperCoins
                      ([Left  msg       ], _             ) -> sorry msg
                      ([Right (_:_:_)   ], _             ) -> sorry sorryWhisperExcessTargets
                      ([Right [targetId]], _             ) ->
                          let targetDesig = serialize . mkStdDesig targetId ms $ Don'tCap
                          in if isNpcPla targetId ms
                            then whispering d targetId targetDesig . formatMsg $ rest
                            else sorry . sorryWhisperTargetType . getSing targetId $ ms
                      x -> pmf "whisper helper" x
                  else sorry sorryWhisperNoOneHere
      where
        sorry                                 = (ms, ) . (, [], "") . pure
        whispering d targetId targetDesig msg =
            let toSelfMsg     = T.concat [ "You whisper to ", targetDesig, ", ", msg ]
                toTargetMsg   = serialize d <> " whispers to you, " <> msg
                toTargetBcast = (nl toTargetMsg, pure targetId)
                toOthersMsg   = T.concat [ serialize d, " whispers something to ", targetDesig, "." ]
                toOthersBcast = (nl toOthersMsg, desigIds d \\ [ i, targetId ])
            in (ms, (pure toSelfMsg, [ toTargetBcast, toOthersBcast ], toSelfMsg))
    formatMsg = dblQuote . capitalizeMsg . punctuateMsg
    ioHelper ms triple@(x:xs, _, _) | (toSelfs, bs, logMsg) <- triple & _1 .~ parseInBands       i ms x : xs
                                                                      & _3 %~ parseInBandsSuffix i ms
                                    = do logMsg |#| logPlaOut "whisper" i . pure
                                         logMsg |#| alertMsgHelper i "whisper"
                                         multiWrapSend mq cols toSelfs
                                         bcastIfNotIncogNl i bs
    ioHelper _ triple               = pmf "whisper ioHelper" triple
whisper p = pmf "whisper" p


-----


who :: HasCallStack => ActionFun
who (NoArgs i mq cols) = getState >>= \ms ->
    sequence_ [ logPlaExecArgs "who" [] i, pager i mq Nothing . concatMap (wrapIndent namePadding cols) . mkWhoTxt i $ ms ]
who (LowerNub i mq cols as) = do logPlaExecArgs "who" as i
                                 dispMatches i mq cols namePadding Isn'tRegex as . mkWhoTxt i =<< getState
who p                       = pmf "who" p


-----


whoAmI :: HasCallStack => ActionFun
whoAmI (NoArgs i mq cols) = sequence_ [ logPlaExec "whoami" i, wrapSend mq cols =<< helper =<< getState ]
  where
    helper ms = return $ let s = getSing i ms in if isNpc i ms
      then let sexy = getSex i ms
           in T.concat [ "You are ", aOrAnOnLower s, sexy /= NoSex |?| spcL . parensQuote . pp $ sexy, "." ]
      else prd . T.concat $ [ "You are ", colorWith knownNameColor s, " ", parensQuote $ if isSpiritId i ms
        then "a disembodied spirit"
        else "a " <> (uncurry (|<>|) . mkPrettySexRace i $ ms) ]
whoAmI p = withoutArgs whoAmI p


-----


zoom :: HasCallStack => ActionFun
zoom (NoArgs i mq cols  ) = zoomHelper i mq cols dfltZoom
zoom (OneArg i mq cols a) = case reads . T.unpack $ a :: [(Int, String)] of
  [(x, "")] | x <= 0    -> sorry
            | otherwise -> zoomHelper i mq cols x
  _                     -> sorry
  where
    sorry = wrapSend mq cols . sorryParseZoom $ a
zoom p = advise p ["zoom"] adviceZoomExcessArgs


zoomHelper :: HasCallStack => Id -> MsgQueue -> Cols -> Int -> MudStack ()
zoomHelper i mq cols x = do sendGmcpRmInfo (Just x) i =<< getState
                            wrapSend mq cols . prd $ "Set map zoom level to " <> showTxt x
