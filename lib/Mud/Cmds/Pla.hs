{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MonadComprehensions, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, TransformListComp, TupleSections, TypeFamilies, ViewPatterns #-}

module Mud.Cmds.Pla ( getRecordUptime
                    , getUptime
                    , handleEgress
                    , look
                    , mkNonStdRmLinkCmds
                    , noOfNpcCmds
                    , noOfPlaCmds
                    , npcCmds
                    , plaCmds
                    , showMotd ) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Msgs.Advice
import Mud.Cmds.Msgs.CmdDesc
import Mud.Cmds.Msgs.Dude
import Mud.Cmds.Msgs.Hint
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.EmoteExp.EmoteExp
import Mud.Cmds.Util.EmoteExp.TwoWayEmoteExp
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Coins
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Misc.ANSI
import Mud.Misc.Database
import Mud.Misc.LocPref
import Mud.Misc.Logging hiding (logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import Mud.Misc.NameResolution
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iWelcome)
import Mud.Threads.Act
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.Misc
import Mud.Threads.Regen
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Padding
import Mud.Util.List hiding (headTail)
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Token
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), first, second)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, _4, _5, at, both, each, set, to, view, views)
import Control.Lens.Operators ((%~), (&), (+~), (-~), (.~), (<>~), (?~), (^.))
import Control.Monad ((>=>), foldM, forM, forM_, guard, mplus, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Char (isDigit, isLetter)
import Data.Either (lefts, partitionEithers)
import Data.Function (on)
import Data.Int (Int64)
import Data.IntMap.Lazy ((!))
import Data.Ix (inRange)
import Data.List ((\\), delete, foldl', intercalate, intersperse, nub, nubBy, partition, sort, sortBy, unfoldr)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>), All(..), Sum(..))
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Tuple (swap)
import GHC.Exts (sortWith)
import Prelude hiding (log, pi)
import qualified Data.IntMap.Lazy as IM (IntMap, (!), keys)
import qualified Data.Map.Lazy as M ((!), elems, filter, keys, lookup, map, singleton, size, toList)
import qualified Data.Set as S (filter, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import System.Clock (Clock(..), TimeSpec(..), getTime)
import System.Console.ANSI (ColorIntensity(..), clearScreenCode)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Time.Utils (renderSecs)


default (Int, Double)


-----


{-# ANN module ("HLint: ignore Use &&"        :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN module ("HLint: ignore Use ||"        :: String) #-}


-----


blowUp :: Text -> Text -> [Text] -> a
blowUp = U.blowUp "Mud.Cmds.Pla"


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Pla"


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


plaCmds :: [Cmd]
plaCmds = sort $ regularCmds ++ priorityAbbrevCmds ++ expCmds


regularCmds :: [Cmd]
regularCmds = map (uncurry4 mkRegularCmd) regularCmdTuples


regularCmdTuples :: [(CmdFullName, ActionFun, Bool, CmdDesc)]
regularCmdTuples =
    [ ("?",          plaDispCmdList,  True,  cmdDescDispCmdList)
    , ("about",      about,           True,  "About CurryMUD.")
    , ("admin",      admin,           True,  "Display a list of administrators, or send a message to an administrator.")
    , ("bars",       bars,            True,  cmdDescBars)
    , ("bug",        bug,             True,  "Report a bug.")
    , ("channel",    chan,            True,  "Send a message on a telepathic channel " <> plusRelatedMsg)
    , ("d",          go "d",          True,  cmdDescGoDown)
    , ("e",          go "e",          True,  cmdDescGoEast)
    , ("equipment",  equip,           True,  cmdDescEquip)
    , ("expressive", expCmdList,      True,  cmdDescExpCmdList)
    , ("n",          go "n",          True,  cmdDescGoNorth)
    , ("ne",         go "ne",         True,  cmdDescGoNortheast)
    , ("newchannel", newChan,         True,  "Create one or more new telepathic channels.")
    , ("nw",         go "nw",         True,  cmdDescGoNorthwest)
    , ("question",   question,        True,  "Ask/answer newbie questions " <> plusRelatedMsg)
    , ("qui",        quitCan'tAbbrev, True,  "")
    , ("quit",       quit,            False, "Quit playing CurryMUD.")
    , ("read",       readAction,      True,  cmdDescRead)
    , ("remove",     remove,          True,  cmdDescRemove)
    , ("s",          go "s",          True,  cmdDescGoSouth)
    , ("se",         go "se",         True,  cmdDescGoSoutheast)
    , ("set",        setAction,       True,  "View or change settings.")
    , ("sw",         go "sw",         True,  cmdDescGoSouthwest)
    , ("take",       getAction,       True,  cmdDescGet)
    , ("tune",       tune,            True,  "Display a list of your telepathic connections, or tune in/out one or \
                                             \more telepathic connections.")
    , ("typo",       typo,            True,  "Report a typo.")
    , ("u",          go "u",          True,  cmdDescGoUp)
    , ("unlink",     unlink,          True,  "Sever one or more telepathic links.")
    , ("uptime",     uptime,          True,  "Display how long CurryMUD has been running.")
    , ("w",          go "w",          True,  cmdDescGoWest)
    , ("whoami",     whoAmI,          True,  "Confirm your name, sex, and race.") ]


mkRegularCmd :: CmdFullName -> ActionFun -> Bool -> CmdDesc -> Cmd
mkRegularCmd cfn f b cd = Cmd { cmdName           = cfn
                              , cmdPriorityAbbrev = Nothing
                              , cmdFullName       = cfn
                              , cmdAction         = Action f b
                              , cmdDesc           = cd }


priorityAbbrevCmds :: [Cmd]
priorityAbbrevCmds = concatMap (uncurry5 mkPriorityAbbrevCmd) priorityAbbrevCmdTuples


priorityAbbrevCmdTuples :: [(CmdFullName, CmdPriorityAbbrevTxt, ActionFun, Bool, CmdDesc)]
priorityAbbrevCmdTuples =
    [ ("clear",      "cl",  clear,      True, cmdDescClear)
    , ("color",      "col", color,      True, "Perform a color test.")
    , ("connect",    "co",  connect,    True, "Connect one or more people to a telepathic channel.")
    , ("disconnect", "di",  disconnect, True, "Disconnect one or more people from a telepathic channel.")
    , ("drink",      "dri", drink,      True, cmdDescDrink)
    , ("drop",       "dr",  dropAction, True, cmdDescDrop)
    , ("emote",      "em",  emote,      True, cmdDescEmote)
    , ("exits",      "ex",  exits,      True, cmdDescExits)
    , ("get",        "g",   getAction,  True, cmdDescGet)
    , ("give",       "gi",  give,       True, cmdDescGive)
    , ("help",       "h",   help,       True, "Get help on one or more commands or topics.")
    , ("intro",      "in",  intro,      True, "Display a list of the people who have introduced themselves to you, or \
                                              \introduce yourself to one or more people.")
    , ("inventory",  "i",   inv,        True, cmdDescInv)
    , ("leave",      "le",  leave,      True, "Sever your connections to one or more telepathic channels.")
    , ("link",       "li",  link,       True, "Display a list of the people with whom you have established a \
                                              \telepathic link, or establish a telepathic link with one or more \
                                              \people.")
    , ("look",       "l",   look,       True, cmdDescLook)
    , ("motd",       "m",   motd,       True, "Display the message of the day.")
    , ("put",        "p",   putAction,  True, cmdDescPut)
    , ("ready",      "r",   ready,      True, cmdDescReady)
    , ("say",        "sa",  say,        True, cmdDescSay)
    , ("show",       "sh",  showAction, True, cmdDescShow)
    , ("stats",      "st",  stats,      True, cmdDescStats)
    , ("telepathy",  "t",   tele,       True, "Send a private message to a person with whom you have established a \
                                              \two-way telepathic link.")
    , ("unready",    "un",  unready,    True, cmdDescUnready)
    , ("who",        "wh",  who,        True, "Display or search a list of who is currently awake.") ]


mkPriorityAbbrevCmd :: CmdFullName -> CmdPriorityAbbrevTxt -> ActionFun -> Bool -> CmdDesc -> [Cmd]
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


noOfPlaCmds :: Int
noOfPlaCmds = length regularCmdTuples + length priorityAbbrevCmdTuples


-----


{-
NPC commands must conform to the following rules:
* Messages should not be sent to the executor of the command in the form of broadcasts. (Otherwise they will be
erroneously indented with "toNpcColor" in the case that the executor is an NPC.)
* Given the above, "toSelf" messages should be subjected to "parseDesig", as necessary, before being sent to the
executor (via "wrapSend" or a related function). Log messages may likewise need to be subjected to "parseDesig",
depending on their content.
* When an NPC executes a command, that NPC's name should be represented as a "Desig" in any broadcasts sent to others.
-}


npcCmds :: [Cmd]
npcCmds = sort $ npcRegularCmds ++ npcPriorityAbbrevCmds ++ expCmds


npcRegularCmds :: [Cmd]
npcRegularCmds = map (uncurry4 mkRegularCmd) npcRegularCmdTuples


npcRegularCmdTuples :: [(CmdFullName, ActionFun, Bool, CmdDesc)]
npcRegularCmdTuples =
    [ (".",          npcAsSelf,      False, "Execute a command as your admin PC.")
    , ("?",          npcDispCmdList, True,  cmdDescDispCmdList)
    , ("bars",       bars,           True,  cmdDescBars)
    , ("d",          go "d",         True,  cmdDescGoDown)
    , ("e",          go "e",         True,  cmdDescGoEast)
    , ("equipment",  equip,          True,  cmdDescEquip)
    , ("expressive", expCmdList,     True,  cmdDescExpCmdList)
    , ("n",          go "n",         True,  cmdDescGoNorth)
    , ("ne",         go "ne",        True,  cmdDescGoNortheast)
    , ("nw",         go "nw",        True,  cmdDescGoNorthwest)
    , ("read",       readAction,     True,  cmdDescRead)
    , ("remove",     remove,         True,  cmdDescRemove)
    , ("s",          go "s",         True,  cmdDescGoSouth)
    , ("se",         go "se",        True,  cmdDescGoSoutheast)
    , ("sw",         go "sw",        True,  cmdDescGoSouthwest)
    , ("u",          go "u",         True,  cmdDescGoUp)
    , ("w",          go "w",         True,  cmdDescGoWest) ]


npcPriorityAbbrevCmds :: [Cmd]
npcPriorityAbbrevCmds = concatMap (uncurry5 mkPriorityAbbrevCmd) npcPriorityAbbrevCmdTuples


npcPriorityAbbrevCmdTuples :: [(CmdFullName, CmdPriorityAbbrevTxt, ActionFun, Bool, CmdDesc)]
npcPriorityAbbrevCmdTuples =
    [ ("clear",     "c",   clear,      True,  cmdDescClear)
    , ("drink",     "dri", drink,      True,  cmdDescDrink)
    , ("drop",      "dr",  dropAction, True,  cmdDescDrop)
    , ("emote",     "em",  emote,      True,  cmdDescEmote)
    , ("exits",     "ex",  exits,      True,  cmdDescExits)
    , ("get",       "g",   getAction,  True,  cmdDescGet)
    , ("give",      "gi",  give,       True,  cmdDescGive)
    , ("inventory", "i",   inv,        True,  cmdDescInv)
    , ("look",      "l",   look,       True,  cmdDescLook)
    , ("put",       "p",   putAction,  True,  cmdDescPut)
    , ("ready",     "r",   ready,      True,  cmdDescReady)
    , ("say",       "sa",  say,        True,  cmdDescSay)
    , ("show",      "sh",  showAction, True,  cmdDescShow)
    , ("stats",     "st",  stats,      True,  cmdDescStats)
    , ("stop",      "sto", npcStop,    False, "Stop possessing.")
    , ("unready",   "un",  unready,    True,  cmdDescUnready)
    , ("whoami",    "wh",  whoAmI,     True,  "Confirm who " <> parensQuote "or what" <> " you are.") ]


noOfNpcCmds :: Int
noOfNpcCmds = length npcRegularCmdTuples + length npcPriorityAbbrevCmdTuples


-----


about :: ActionFun
about (NoArgs i mq cols) = do
    helper |&| try >=> eitherRet ((sendGenericErrorMsg mq cols >>) . fileIOExHandler "about")
    logPlaExec "about" i
  where
    helper = multiWrapSend mq cols =<< [ T.lines cont | cont <- liftIO . T.readFile $ aboutFile ]
about p = withoutArgs about p


-----


admin :: ActionFun
admin p@(NoArgs''     _) = adminList p
admin p@(AdviseOneArg a) = advise p ["admin"] . adviceAdminNoMsg $ a
admin (MsgWithTarget i mq cols target msg) = getState >>= helper >>= \logMsgs ->
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
                                      , (not . isAdminId i $ ms) |?| (All . not . isIncognito $ adminPla) ]
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
            ioHelper _ xs = patternMatchFail "admin helper ioHelper" [ showText xs ]
            filterRoot idSings
              | isAdminId i ms = idSings
              | otherwise      =
                  let ([((`getPla` ms) -> rootPla, _)], others) = partition ((== "Root") . snd) idSings
                  in if isLoggedIn rootPla && (not . isIncognito $ rootPla)
                    then idSings
                    else others
        in (findFullNameForAbbrev strippedTarget . filterRoot . mkAdminIdSingList $ ms) |&| maybe notFound found
admin p = patternMatchFail "admin" [ showText p ]


adminList :: ActionFun
adminList (NoArgs i mq cols) = (multiWrapSend mq cols =<< helper =<< getState) >> logPlaExecArgs "admin" [] i
  where
    helper ms =
        let p            = getPla i ms
            singSuffixes = [ (s, suffix) | (ai, s) <- mkAdminIdSingList ms
                                         , let suffix = " logged " <> mkSuffix ai
                                         , then sortWith by s ]
            mkSuffix ai = let { ap = getPla ai ms; isIncog = isIncognito ap } in if isAdmin p && isIncog
              then (inOut . isLoggedIn $ ap) <> " " <> parensQuote "incognito"
              else inOut (isLoggedIn ap && not isIncog)
            singSuffixes' = onFalse (isAdmin p) (filter f) singSuffixes
              where
                f (a, b) | a == "Root" = b == " logged in"
                         | otherwise   = otherwise
            combineds = [ padName abbrev <> suffix
                        | (_, suffix) <- singSuffixes'
                        | abbrev      <- styleAbbrevs Don'tQuote . map fst $ singSuffixes' ]
        in ()!# combineds ? return combineds :? unadulterated sorryNoAdmins
adminList p = patternMatchFail "adminList" [ showText p ]


-----


bars :: ActionFun
bars (NoArgs i mq cols) = getState >>= \ms ->
    let mkBars = map (uncurry . mkBar . calcBarLen $ cols) . mkPtPairs i $ ms
    in multiWrapSend mq cols mkBars >> logPlaExecArgs "bars" [] i
bars (LowerNub i mq cols as) = getState >>= \ms ->
    let mkBars  = case second nub . partitionEithers . foldr f [] $ as of
                    (x:xs, []     ) -> (x <> " " <> hint) : xs
                    ([],   barTxts) -> barTxts
                    (x:xs, barTxts) -> barTxts ++ [""] ++ ((x <> " " <> hint) : xs)
        f a acc = (: acc) $ case filter ((a `T.isPrefixOf`) . fst) . mkPtPairs i $ ms of
          []      -> Left . sorryParseArg $ a
          [match] -> Right . uncurry (mkBar . calcBarLen $ cols) $ match
          xs      -> patternMatchFail "bars f" [ showText xs ]
    in multiWrapSend mq cols mkBars >> logPlaExecArgs "bars" as i
  where
    hint = T.concat [ "Please specify any of the following: "
                    , commas . map dblQuote $ [ "hp", "mp", "pp" ]
                    , ", or "
                    , dblQuote "fp"
                    , "." ]
bars p = patternMatchFail "bars" [ showText p ]


mkBar :: Int -> Text -> (Int, Int) -> Text
mkBar x txt (c, m) = let ratio  = c `divide` m
                         greens = round $ fromIntegral x * ratio
                         reds   = x - greens
                     in T.concat [ T.toUpper txt
                                 , ": "
                                 , colorWith greenBarColor . T.replicate greens $ " "
                                 , colorWith redBarColor   . T.replicate reds   $ " "
                                 , " "
                                 , showText . round $ 100 * ratio
                                 , "%" ]


mkPtPairs :: Id -> MudState -> [(Text, (Int, Int))]
mkPtPairs i ms = let (hps, mps, pps, fps) = getPts i ms
                 in [ ("hp", hps), ("mp", mps), ("pp", pps), ("fp", fps) ]


-----


bug :: ActionFun
bug p@AdviseNoArgs = advise p ["bug"] adviceBugNoArgs
bug p              = bugTypoLogger p BugLog


-----


chan :: ActionFun
chan (NoArgs i mq cols) = getState >>= \ms ->
    let (chanNames, chanTunings) = mkChanNamesTunings i ms
        helper names tunings     = let txts = mkChanTxts
                                   in (()!# txts ? txts :? none) |&| ("Telepathic channels:" :)
          where
            mkChanTxts = [ padChanName n <> tunedInOut t | n <- names | t <- tunings ]
    in do
        multiWrapSend mq cols . helper (styleAbbrevs Don'tQuote chanNames) $ chanTunings
        logPlaExecArgs "chan" [] i
chan (OneArg i mq cols a@(T.toLower -> a')) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryChanName $ a
        found match =
            let (cn, c)                  = getMatchingChanWithName match cns cs
                ([(_, isTuned)], others) = partition ((== s) . fst) $ c^.chanConnTbl.to M.toList
                (linkeds, nonLinkeds)    = partition (views _1 (isLinked ms . (i, ))) . filter f . map mkTriple $ others
                f                        = views _1 (`isAwake` ms)
            in mapM (updateRndmName i . view _1) nonLinkeds >>= \rndmNames ->
                let combo       = linkeds ++ zipWith (\rndmName -> _2 .~ rndmName) rndmNames nonLinkeds
                    (ins, outs) = partition (view _3) . sortBy (compare `on` view _2) $ combo
                    styleds     = styleAbbrevs Don'tQuote . map (view _2) $ ins
                    ins'        = zipWith (\styled -> _2 .~ styled) styleds ins
                    g (_, n, isTuned') = let n' = isRndmName n ? underline n :? n in padName n' <> tunedInOut isTuned'
                    combo'             = ins' ++ outs
                in if isTuned
                  then do
                      let onlyYou           = pure "You are the only person connected."
                          msgs              = ()!# combo' ? map g combo' :? onlyYou
                          affixChanName txt = parensQuote cn <> " " <> txt
                      multiWrapSend mq cols $ "Channel " <> dblQuote cn <> ":" : msgs
                      logPla "chan" i . affixChanName . commas $ [ getSing i' ms <> " is " <> tunedInOut isTuned'
                                                                 | (i', _, isTuned') <- combo' ]
                  else wrapSend mq cols . sorryTunedOutICChan $ cn
        (cs, cns, s)           = mkChanBindings i ms
        mkTriple (s', isTuned) = (getIdForMobSing s' ms, s', isTuned)
    in findFullNameForAbbrev a' (map T.toLower cns) |&| maybe notFound found
chan (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let notFound    = wrapSend mq cols . sorryChanName $ target
        found match = let (cn, c) = getMatchingChanWithName match cns cs in if
          | views chanConnTbl (not . (M.! s)) c    -> wrapSend mq cols . sorryTunedOutICChan $ cn
          | isIncognitoId i ms -> wrapSend mq cols . sorryChanIncog $ "a telepathic"
          | otherwise          -> getChanStyleds i c ms >>= \triples -> if ()# triples
            then wrapSend mq cols . sorryChanNoOneListening . dblQuote $ cn
            else let getStyled targetId = view _3 . head . filter (views _1 (== i)) <$> getChanStyleds targetId c ms
                     format (txt, is)   = if i `elem` is
                                            then ((formatChanMsg cn s txt, pure i) :) <$> mkBsWithStyled (i `delete` is)
                                            else mkBsWithStyled is
                       where
                         mkBsWithStyled is' = mapM getStyled is' >>= \styleds ->
                             return [ (formatChanMsg cn styled txt, pure i') | i' <- is' | styled <- styleds ]
                     ioHelper (expandEmbeddedIdsToSings ms -> logMsg) bs = do
                         bcastNl =<< expandEmbeddedIds ms cc bs
                         sendToWiretappers logMsg
                         logPlaOut "chan" i . pure $ parensQuote cn <> " " <> logMsg
                         ts <- liftIO mkTimestamp
                         withDbExHandler_ "chan" . insertDbTblChan . ChanRec ts (c^.chanId) cn s $ logMsg
                     sendToWiretappers tappedMsg =
                         let cn' = colorWith wiretapColor . spaced . parensQuote $ cn
                             is  = c^.wiretappers.to (map (`getIdForMobSing` ms))
                             is' = filter (isLoggedIn . (`getPla` ms)) is
                         in bcastNl . pure $ (T.concat [ cn', " ", s, ": ", tappedMsg ], is')
                     cc   = ChanContext "chan" (Just cn) False
                     f bs = let logMsg = dropANSI . fst . head $ bs in ioHelper logMsg =<< g bs
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
                       Left  errorMsg     -> ws errorMsg
                       Right (bs, logMsg) -> ioHelper logMsg =<< g bs
        (cs, cns, s) = mkChanBindings i ms
    in findFullNameForAbbrev (T.toLower target) (map T.toLower cns) |&| maybe notFound found
chan p = patternMatchFail "chan" [ showText p ]


-----


clear :: ActionFun
clear (NoArgs' i mq) = (send mq . T.pack $ clearScreenCode) >> logPlaExec "clear" i
clear p              = withoutArgs clear p


-----


color :: ActionFun
color (NoArgs' i mq) = (send mq . nl . T.concat $ msg) >> logPlaExec "color" i
  where
    msg = [ nl . T.concat $ [ mkColorDesc fg bg, colorWith ansi . spaced $  "CurryMUD" ]
          | fgc <- colors, bgc <- colors, fgc /= bgc
          , let fg = (Dull, fgc), let bg = (Dull, bgc), let ansi = mkColorANSI fg bg ] ++ other
    mkColorDesc (mkColorName -> fg) (mkColorName -> bg) = fg <> "on " <> bg
    mkColorName                                         = padColorName . showText . snd
    other = [ nl . T.concat $ [ pad 19 "Blinking",   blink     . spaced $ "CurryMUD" ]
            , nl . T.concat $ [ pad 19 "Underlined", underline . spaced $ "CurryMUD" ] ]
color p = withoutArgs color p


-----


connect :: ActionFun
connect p@AdviseNoArgs       = advise p ["connect"] adviceConnectNoArgs
connect p@(AdviseOneArg a)   = advise p ["connect"] . adviceConnectNoChan $ a
connect (Lower i mq cols as) = getState >>= \ms -> let getIds = map (`getIdForMobSing` ms) in
    if isIncognitoId i ms
      then wrapSend mq cols . sorryIncog $ "connect"
      else connectHelper i (mkLastArgWithNubbedOthers as) |&| modifyState >=> \case
        ([Left msg], Nothing) -> bcastNl . mkBcast i $ msg
        (res,        Just ci)
          | (sorryMsgs, targetSings) <- partitionEithers res
          , sorryBs   <- [ (msg, pure i) | msg <- sorryMsgs ]
          , targetIds <- getIds targetSings
          , c         <- getChan ci ms
          , cn        <- c^.chanName
          , otherIds  <- let f = (\\ (i : targetIds)) . filter (`isAwake` ms) . getIds . M.keys . M.filter id
                         in views chanConnTbl f c
          , toTargets <- (T.concat [ getSing i ms, " has connected you to the ", dblQuote cn, " channel." ], targetIds)
          , toSelf    <- (focusingInnateMsg <>) $ case targetSings of
            [one] -> T.concat [ "you connect ", one, " to the ", dblQuote cn, " channel." ]
            _     -> T.concat [ "you connect the following people to the "
                              , dblQuote cn
                              , " channel: "
                              , commas targetSings
                              , "." ] -> do
              toOthers <- mkToOthers ms otherIds targetIds cn
              bcastNl $ toTargets : toOthers ++ (()!# targetSings |?| mkBcast i toSelf) ++ sorryBs
              connectBlink targetIds ms
              logPla "connect" i $ "connected to " <> dblQuote cn <> ": " <> commas targetSings
        xs -> patternMatchFail "connect" [ showText xs ]
  where
    mkToOthers ms otherIds targetIds cn = do
        namesForMe      <- mapM (getRelativePCName ms . (, i)) otherIds
        namesForTargets <- mapM (\otherId -> mapM (getRelativePCName ms . (otherId, )) targetIds) otherIds
        let f i' me = map g
              where
                g n = (T.concat [ me, " has connected ", n, " to the ", dblQuote cn, " channel." ], pure i')
        return . concat . zipWith3 f otherIds namesForMe $ namesForTargets
    connectBlink targetIds ms = forM_ targetIds $ \targetId ->
        rndmDo (calcProbConnectBlink targetId ms) . mkExpAction "blink" . mkActionParams targetId ms $ []
connect p = patternMatchFail "connect" [ showText p ]


connectHelper :: Id -> (Text, Args) -> MudState -> (MudState, ([Either Text Sing], Maybe Id))
connectHelper i (target, as) ms =
    let (f, guessWhat) | any hasLocPref as = (stripLocPref, sorryConnectIgnore)
                       | otherwise         = (id,           ""                )
        as'         = map (capitalize . T.toLower . f) as
        notFound    = sorry . sorryChanName $ target
        found match = let (cn, c) = getMatchingChanWithName match cns cs in if views chanConnTbl (M.! s) c
          then let procTarget pair@(ms', _) a =
                       let notFoundSing         = sorryProcTarget . notFoundSuggestAsleeps a asleepSings $ ms'
                           foundSing targetSing = case c^.chanConnTbl.at targetSing of
                             Just _  -> sorryProcTarget . sorryConnectAlready targetSing $ cn
                             Nothing ->
                                 let checkChanName targetId = if hasChanOfSameName targetId
                                       then blocked . sorryConnectChanName targetSing $ cn
                                       else checkPp
                                     checkPp | not . hasPp i ms' $ 3 = let msg = sorryPp $ "connect " <> targetSing
                                                                       in sorryProcTarget msg
                                             | otherwise = pair & _1.chanTbl.ind ci.chanConnTbl.at targetSing ?~ True
                                                                & _1.mobTbl.ind i.curPp -~ 3
                                                                & _2 <>~ (pure . Right $ targetSing)
                                 in either sorryProcTarget checkChanName . checkMutuallyTuned i ms' $ targetSing
                           sorryProcTarget msg = pair & _2 <>~ (pure . Left $ msg)
                           blocked             = sorryProcTarget . (effortsBlockedMsg <>)
                       in findFullNameForAbbrev a targetSings |&| maybe notFoundSing foundSing
                   ci                         = c^.chanId
                   dblLinkeds                 = views pcTbl (filter (isDblLinked ms . (i, )) . IM.keys) ms
                   dblLinkedsPair             = partition (`isAwake` ms) dblLinkeds
                   (targetSings, asleepSings) = dblLinkedsPair & both %~ map (`getSing` ms)
                   hasChanOfSameName targetId | targetCs  <- getPCChans targetId ms
                                              , targetCns <- map (views chanName T.toLower) targetCs
                                              = T.toLower cn `elem` targetCns
                   (ms'', res)                = foldl' procTarget (ms, []) as'
               in (ms'', (onTrue (()!# guessWhat) (Left guessWhat :) res, Just ci))
          else sorry . sorryTunedOutICChan $ cn
        (cs, cns, s) = mkChanBindings i ms
        sorry        = (ms, ) . (, Nothing) . pure . Left
    in findFullNameForAbbrev target (map T.toLower cns) |&| maybe notFound found


-----


disconnect :: ActionFun
disconnect p@AdviseNoArgs       = advise p ["disconnect"] adviceDisconnectNoArgs
disconnect p@(AdviseOneArg a)   = advise p ["disconnect"] . adviceDisconnectNoChan $ a
disconnect (Lower i mq cols as) = getState >>= \ms -> let getIds = map (`getIdForMobSing` ms) in
    if isIncognitoId i ms
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
                  targetSings |#| (const . logPla "disconnect" i . T.concat $ [ "disconnected from "
                                                                              , dblQuote cn
                                                                              , ": "
                                                                              , commas targetSings ])
            xs -> patternMatchFail "disconnect" [ showText xs ]
  where
    format n = isRndmName n ? underline n :? n
    mkToOthers ms otherIds targetIds cn = do
        namesForMe      <- mapM (getRelativePCName ms . (, i)) otherIds
        namesForTargets <- mapM (\otherId -> mapM (getRelativePCName ms . (otherId, )) targetIds) otherIds
        let f i' me = map g
              where
                g n = (T.concat [ me, " has disconnected ", n, " from the ", dblQuote cn, " channel." ], pure i')
        return . concat . zipWith3 f otherIds namesForMe $ namesForTargets
disconnect p = patternMatchFail "disconnect" [ showText p ]


disconnectHelper :: Id
                 -> (Text, Args)
                 -> IM.IntMap [(Id, Text)]
                 -> MudState
                 -> (MudState, ([Either Text (Id, Sing, Text)], Maybe Id))
disconnectHelper i (target, as) idNamesTbl ms =
    let (f, guessWhat) | any hasLocPref as = (stripLocPref, sorryDisconnectIgnore)
                       | otherwise         = (id,           ""                   )
        as'         = map (T.toLower . f) as
        notFound    = sorry . sorryChanName $ target
        found match = let (cn, c) = getMatchingChanWithName match cns cs in if views chanConnTbl (M.! s) c
          then let procTarget (pair@(ms', _), b) a = case filter ((== a) . T.toLower . snd) $ idNamesTbl IM.! ci of
                     [] -> (pair & _2 <>~ (pure . Left . hint . sorryChanTargetName (dblQuote cn) $ a), True)
                     [(targetId, targetName)]
                       | not . hasPp i ms' $ 3 ->
                           let targetName' = isRndmName targetName ? underline targetName :? targetName
                               msg         = sorryPp $ "disconnect " <> targetName'
                           in (pair & _2 <>~ (pure . Left $ msg), b)
                       | otherwise -> let targetSing = getSing targetId ms'
                                      in ( pair & _1.chanTbl.ind ci.chanConnTbl.at targetSing .~ Nothing
                                                & _1.mobTbl.ind i.curPp -~ 3
                                                & _2 <>~ (pure . Right $ (targetId, targetSing, targetName))
                                         , b )
                     xs -> patternMatchFail "disconnectHelper found" [ showText xs ]
                     where
                       hint = onFalse b ((<> hintDisconnect) . (<> " "))
                   ci               = c^.chanId
                   ((ms'', res), _) = foldl' procTarget ((ms, []), False) as'
               in (ms'', (onTrue (()!# guessWhat) (Left guessWhat :) res, Just ci))
          else sorry . sorryTunedOutICChan $ cn
        (cs, cns, s) = mkChanBindings i ms
        sorry        = (ms, ) . (, Nothing) . pure . Left
    in findFullNameForAbbrev target (map T.toLower cns) |&| maybe notFound found


-----


-- TODO: Help.
-- TODO: More testing.
drink :: ActionFun
drink p@AdviseNoArgs                    = advise p ["drink"] adviceDrinkNoArgs
drink p@(AdviseOneArg _               ) = advise p ["drink"] adviceDrinkNoVessel
drink   (Lower i mq cols [amt, target]) = (,) <$> mkRndmVector <*> liftIO getCurrentTime >>= \(v, now) ->
    helper v now |&| modifyState >=> sequence_
  where
    helper v now ms
      | amt `T.isPrefixOf` "all" || amt == T.singleton allChar = next maxBound
      | otherwise = case reads . T.unpack $ amt :: [(Int, String)] of
        [(x, "")] | x <= 0    -> sorry sorryDrinkMouthfuls
                  | otherwise -> next x
        _                     -> sorry . sorryParseMouthfuls $ amt
      where
        sorry  = (ms, ) . pure . wrapSend mq cols
        next x =
            let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv . pure $ target
                d                      = mkStdDesig i ms DoCap
                ri                     = getRmId i ms
                myInvCoins             = getInvCoins i ms
                rmInvCoins             = first (i `delete`) . getNonIncogInvCoins ri $ ms
                maybeHooks             = lookupHooks i ms "drink"
                -----
                drinkInv =
                    let (eiss, ecs)  = uncurry (resolveMobInvCoins i ms inInvs) myInvCoins
                        f [targetId] = case (uncurry getSing *** uncurry getType) . dup $ (targetId, ms) of
                          (s, VesselType) -> maybe (sorry . sorryDrinkEmpty $ s) (g s) . getVesselCont targetId $ ms
                          (s, _         ) -> sorry . sorryDrinkType $ s
                          where
                            g s (l, m) =
                                let mouths                = x `min` m `min` stomAvail
                                    (stomAvail, stomSize) = calcStomachAvailSize i ms
                                    scs = replicate mouths . StomachCont (l^.liqId.to Left) now $ False
                                    t   = T.concat [ "You drink "
                                                   , showText mouths
                                                   , " mouthful"
                                                   , theLetterS $ mouths /= 1
                                                   , " of "
                                                   , l^.liqName.to theOnLower
                                                   , " from the "
                                                   , s
                                                   , allGoneTxt
                                                   , "." ]
                                    fullDesc
                                      | x `min` m > stomAvail = "You are so full that you have to stop drinking. You \
                                                                \don't feel so good..."
                                      | otherwise = mkFullDesc (stomAvail - mouths) stomSize
                                    bs = pure ( T.concat [ serialize d, " ", txt, " from ", aOrAn s, allGoneTxt, "." ]
                                              , i `delete` desigIds d )
                                      where
                                        txt = mouths == 1 ? "takes a swig" :? "drinks"
                                    allGone    = remAmt == 0
                                    allGoneTxt = allGone |?| ", emptying it"
                                    remAmt     = m - mouths
                                    newCont    | allGone   = Nothing
                                               | otherwise = Just (l, remAmt)
                                    ms'        = ms & vesselTbl.ind targetId.vesselCont .~ newCont
                                in if | ()# Sum stomAvail -> sorry sorryFull
                                      | mouths > 4        -> (ms, pure . startAct i Drinking . drinkAct $ targetId) -- TODO
                                      | otherwise -> (ms', [ multiWrapSend mq cols . dropEmpties $ [ t, l^.drinkDesc, fullDesc ]
                                                           , drinkLogMsgHelper mouths l s |#| logPla "drink" i
                                                           , scs |#| consume i
                                                           , bcastIfNotIncogNl i bs ])
                        f _ = sorry sorryDrinkExcessTargets
                    in ()!# ecs ? sorry sorryDrinkCoins :? either sorry f (head eiss)
                -----
                drinkRm =
                    case ((()!#) *** (()!#)) (rmInvCoins, maybeHooks) of
                      (True,  False) -> sorry sorryDrinkRmNoHooks
                      (False, True ) ->
                          let (inRms', (ms', toSelfs, bs, logMsgs)) = procHooks i ms v "drink" inRms -- TODO: Make a hook and test.
                              sorryMsgs                             = inRms' |!| pure sorryDrinkEmptyRmWithHooks
                          in (ms', [ multiWrapSend mq cols $ sorryMsgs ++ toSelfs
                                   , bcastIfNotIncogNl i bs
                                   , logMsgs |#| logPlaOut "drink" i ])
                      (True,  True ) ->
                          let (inRms', (ms', toSelfs, bs, logMsgs)) = procHooks i ms v "drink" inRms
                          in if ()# inRms'
                            then (ms', [ multiWrapSend mq cols toSelfs
                                       , bcastIfNotIncogNl i bs
                                       , logMsgs |#| logPlaOut "drink" i ])
                            else sorry . sorryDrinkRmWithHooks . head $ inRms'
                      a -> patternMatchFail "drink helper next drinkRm" [ showText a ]
            in if
              | ()!# inEqs                        -> sorry sorryDrinkInEq
              | ()!# inInvs    && ()#  myInvCoins -> sorry dudeYourHandsAreEmpty
              | ()!# inInvs    && ()!# myInvCoins -> drinkInv
              | ()# rmInvCoins && ()#  maybeHooks -> sorry sorryDrinkEmptyRmNoHooks
              | otherwise                         -> drinkRm
drink p = advise p ["drink"] adviceDrinkExcessArgs


drinkLogMsgHelper :: Mouthfuls -> Liq -> Sing -> Text
drinkLogMsgHelper m l s = T.concat [ "drank "
                                   , showText m
                                   , " mouthfuls"
                                   , theLetterS $ m /= 1
                                   , " of "
                                   , l^.liqName.to aOrAnOnLower
                                   , " "
                                   , let DistinctLiqId i = l^.liqId in parensQuote . showText $ i
                                   , " from "
                                   , aOrAn s
                                   , "." ]


-----


dropAction :: ActionFun
dropAction p@AdviseNoArgs     = advise p ["drop"] adviceDropNoArgs
dropAction p@(LowerNub' i as) = genericAction p helper "drop"
  where
    helper _ ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
            sorryInEq              = inEqs |!| sorryDropInEq
            sorryInRm              = inRms |!| sorryDropInRm
            invCoins               = getInvCoins i ms
            d                      = mkStdDesig  i ms DoCap
            ri                     = getRmId     i ms
            (eiss, ecs)            = uncurry (resolveMobInvCoins i ms inInvs) invCoins
            (ms',  toSelfs,  bs,  logMsgs ) = foldl' (helperDropEitherInv      i d      i ri) (ms,  [],      [], []     ) eiss
            (ms'', toSelfs', bs', logMsgs') =         helperGetDropEitherCoins i d Drop i ri  (ms', toSelfs, bs, logMsgs) ecs
        in if ()!# invCoins
          then (ms'', (dropBlanks $ [ sorryInEq, sorryInRm ] ++ toSelfs', bs', logMsgs'))
          else (ms,   (pure dudeYourHandsAreEmpty,                        [],  []      ))
dropAction p = patternMatchFail "dropAction" [ showText p ]


-----


emote :: ActionFun
emote p@AdviseNoArgs                                                     = advise p ["emote"] adviceEmoteNoArgs
emote p@ActionParams { args } | any (`elem` yous) . map T.toLower $ args = advise p ["emote"] adviceYouEmote
emote (WithArgs i mq cols as) = getState >>= \ms ->
    let d                    = mkStdDesig i ms DoCap
        ser                  = serialize d
        d'                   = d { shouldCap = Don'tCap }
        ser'                 = serialize d'
        xformed              = xformArgs True as
        xformArgs _      []  = []
        xformArgs isHead [x] | (h, t) <- headTail x
                             , h == emoteNameChar
                             , all isPunc . T.unpack $ t
                             = pure . mkRightForNonTargets $ expandEnc isHead & each <>~ t
        xformArgs isHead (x:xs) = (: xformArgs False xs) $ if
          | x == enc               -> mkRightForNonTargets . expandEnc $ isHead
          | x == enc's             -> mkRightForNonTargets $ expandEnc isHead & each <>~ "'s"
          | enc `T.isInfixOf` x    -> Left . adviceEnc $ "emote "
          | x == etc               -> Left . adviceEtc $ "emote "
          | T.take 1 x == etc      -> isHead ? Left adviceEtcHead :? (procTarget ms . T.tail $ x)
          | etc `T.isInfixOf` x    -> Left . adviceEtc $ "emote "
          | isHead, hasEnc as      -> mkRightForNonTargets $ dup3 x  & each %~ capitalizeMsg
          | isHead, x' <- " " <> x -> mkRightForNonTargets $ dup3 x' & _1   %~ (myName True <>)
                                                                     & _2   %~ (ser         <>)
                                                                     & _3   %~ (ser         <>)
          | otherwise              -> mkRightForNonTargets . dup3 $ x
        expandEnc isHead = (isHead ? (ser, ser) :? (ser', ser')) |&| uncurry (myName isHead, , )
        myName    isHead = onTrue isHead capitalize . onTrue (isNpc i ms) theOnLower . fromJust . sDesigEntSing $ d
    in case lefts xformed of
      []      -> let (parseDesig i ms -> toSelf, toOthers, targetIds, toTargetBs) = happy ms xformed
                 in do
                     wrapSend mq cols toSelf
                     bcastIfNotIncogNl i $ (toOthers, desigIds d \\ (i : targetIds)) : toTargetBs
                     logPlaOut "emote" i . pure $ toSelf
      advices -> multiWrapSend mq cols . nub $ advices
  where
    procTarget ms word =
        case swap . (both %~ T.reverse) . T.span isPunc . T.reverse $ word of
          ("",   _) -> Left . adviceEtc $ "emote "
          ("'s", _) -> Left adviceEtcEmptyPoss
          (w,    p) ->
            let (isPoss, target) = ("'s" `T.isSuffixOf` w ? (True, T.dropEnd 2) :? (False, id)) & _2 %~ (w |&|)
                invCoins         = first (i `delete`) . getMobRmNonIncogInvCoins i $ ms
            in if ()!# invCoins
              then case singleArgInvEqRm InRm target of
                (InInv, _      ) -> sorry sorryEmoteTargetInInv
                (InEq,  _      ) -> sorry sorryEmoteTargetInEq
                (InRm,  target') -> case uncurry (resolveRmInvCoins i ms . pure $ target') invCoins of
                  (_,                    [ Left [msg] ]) -> Left msg
                  (_,                    Right  _:_    ) -> sorry sorryEmoteTargetCoins
                  ([ Left  msg        ], _             ) -> Left msg
                  ([ Right (_:_:_)    ], _             ) -> Left sorryEmoteExcessTargets
                  ([ Right [targetId] ], _             ) ->
                      let targetSing = getSing targetId ms
                      in if not . isNpcPC targetId $ ms
                        then Left . sorryEmoteTargetType $ targetSing
                        else let targetDesig = addSuffix isPoss p . serialize . mkStdDesig targetId ms $ Don'tCap
                             in Right ( targetDesig
                                      , [ mkEmoteWord isPoss p targetId, ForNonTargets targetDesig ]
                                      , targetDesig )
                  x -> patternMatchFail "emote procTarget" [ showText x ]
              else Left sorryNoOneHere
    addSuffix   isPoss p = (<> p) . onTrue isPoss (<> "'s")
    mkEmoteWord isPoss   = isPoss ? ForTargetPoss :? ForTarget
    sorry t              = Left . quoteWith' (t, sorryEmoteTargetRmOnly) $ " "
emote p = patternMatchFail "emote" [ showText p ]


-----


equip :: ActionFun
equip (NoArgs   i mq cols   ) = getState >>= \ms -> send mq . nl . mkEqDesc i cols ms i (getSing i ms) $ PCType
equip (LowerNub i mq cols as) = getState >>= \ms ->
    let em@(M.elems -> is) = getEqMap i ms in send mq $ if ()!# em
      then let (inInvs, inEqs, inRms)                = sortArgsInvEqRm InEq as
               (gecrs, miss, rcs)                    = resolveEntCoinNames i ms inEqs is mempty
               eiss                                  = zipWith (curry procGecrMisMobEq) gecrs miss
               invDesc                               = foldl' helperEitherInv "" eiss
               helperEitherInv acc (Left  msg)       = (acc <>) . wrapUnlinesNl cols $ msg
               helperEitherInv acc (Right targetIds) = nl $ acc <> mkEntDescs i cols ms targetIds
               coinsDesc                             = rcs |!| wrapUnlinesNl cols sorryEquipCoins
           in T.concat [ inInvs |!| sorryInInv, inRms |!| sorryInRm, invDesc, coinsDesc ]
      else wrapUnlinesNl cols dudeYou'reNaked
  where
    sorryInInv = wrapUnlinesNl cols . sorryEquipInvLook EquipCmd $ InvCmd
    sorryInRm  = wrapUnlinesNl cols . sorryEquipInvLook EquipCmd $ LookCmd
equip p = patternMatchFail "equip" [ showText p ]


-----


exits :: ActionFun
exits (NoArgs i mq cols) = getState >>= \ms ->
    (send mq . nl . mkExitsSummary cols . getMobRm i $ ms) >> logPlaExec "exits" i
exits p = withoutArgs exits p


-----


expCmdList :: ActionFun
expCmdList (NoArgs i mq cols) =
    (pager i mq . concatMap (wrapIndent cmdNamePadding cols) $ mkExpCmdListTxt) >> logPlaExecArgs "expressive" [] i
expCmdList p@ActionParams { myId, args } =
    dispMatches p cmdNamePadding mkExpCmdListTxt >> logPlaExecArgs "expressive" args myId


mkExpCmdListTxt :: [Text]
mkExpCmdListTxt =
    let cmdNames       = [ cmdName cmd | cmd <- plaCmds ]
        styledCmdNames = styleAbbrevs Don'tQuote cmdNames
    in concatMap mkExpCmdTxt [ (styled, head matches) | (cn, styled) <- zip cmdNames styledCmdNames
                                                      , let matches = findMatches cn
                                                      , length matches == 1 ]
  where
    findMatches cn = S.toList . S.filter (\(ExpCmd ecn _) -> ecn == cn) $ expCmdSet
    mkExpCmdTxt (styled, ExpCmd ecn ect) = case ect of
      (NoTarget  toSelf _  ) -> [ paddedName <> mkInitialTxt  ecn <> toSelf ]
      (HasTarget toSelf _ _) -> [ paddedName <> mkInitialTxt (ecn <> " hanako") <> T.replace "@" "Hanako" toSelf ]
      (Versatile toSelf _ toSelfWithTarget _ _) -> [ paddedName <> mkInitialTxt ecn <> toSelf
                                                   , T.replicate cmdNamePadding (T.singleton indentFiller) <>
                                                     mkInitialTxt (ecn <> " hanako")                       <>
                                                     T.replace "@" "Hanako" toSelfWithTarget ]
      where
        paddedName         = padCmdName styled
        mkInitialTxt input = colorWith quoteColor input <> spaced (colorWith arrowColor "->")


-----


getAction :: ActionFun
getAction p@AdviseNoArgs             = advise p ["get"] adviceGetNoArgs
getAction   (Lower     _ mq cols as) | length as >= 3, (head . tail . reverse $ as) == "from" = wrapSend mq cols hintGet
getAction p@(LowerNub' i         as) = genericAction p helper "get"
  where
    helper v ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorrys                 = dropEmpties [ inInvs |!| sorryGetInInv, inEqs |!| sorryGetInEq ]
            d                      = mkStdDesig i ms DoCap
            ri                     = getRmId i ms
            invCoins               = first (i `delete`) . getNonIncogInvCoins ri $ ms
        in case ((()!#) *** (()!#)) (invCoins, lookupHooks i ms "get") of
          (False, False) -> (ms, (pure sorryGetEmptyRmNoHooks, [], []))
          -----
          (True,  False) -> invCoinsHelper ms inRms d ri invCoins & _2._1 %~ (sorrys ++)
          -----
          (False, True ) -> let (inRms', (ms', toSelfs, bs, logMsgs)) = procHooks i ms v "get" inRms
                                sorrys'                               = sorrys ++ (inRms' |!| pure sorryGetEmptyRmWithHooks)
                            in (ms', (sorrys' ++ toSelfs, bs, logMsgs))
          -----
          (True,  True ) ->
            let (inRms', (ms', hooksToSelfs, hooksBs, hooksLogMsgs))   = procHooks i ms v "get" inRms
                (ms'', (invCoinsToSelfs, invCoinsBs, invCoinsLogMsgs)) = invCoinsHelper ms' inRms' d ri invCoins
            in (ms'', ( concat [ sorrys, hooksToSelfs, invCoinsToSelfs ]
                      , hooksBs      ++ invCoinsBs
                      , hooksLogMsgs ++ invCoinsLogMsgs ))
    invCoinsHelper ms args d ri invCoins =
        let (eiss, ecs)                     = uncurry (resolveRmInvCoins i ms args) invCoins
            (ms',  toSelfs,  bs,  logMsgs ) = foldl' (helperGetEitherInv       i d     ri)  (ms,  [],      [], []     ) eiss
            (ms'', toSelfs', bs', logMsgs') =         helperGetDropEitherCoins i d Get ri i (ms', toSelfs, bs, logMsgs) ecs
        in (ms'', (toSelfs', bs', logMsgs'))
getAction p = patternMatchFail "getAction" [ showText p ]


-----


give :: ActionFun
give p@AdviseNoArgs     = advise p ["give"] adviceGiveNoArgs
give p@(AdviseOneArg a) = advise p ["give"] . adviceGiveNoName $ a
give p@(Lower' i as   ) = genericAction p helper "give"
  where
    helper _ ms =
        let b@LastArgIsTargetBindings { targetArg } = mkLastArgIsTargetBindings i ms as
            f                                       = case singleArgInvEqRm InRm targetArg of
              (InInv, _     ) -> genericSorry ms sorryGiveToInv
              (InEq,  _     ) -> genericSorry ms sorryGiveToEq
              (InRm,  target) -> shuffleGive i ms b { targetArg = target }
        in withEmptyInvChecks ms b sorryNoOneHere f
give p = patternMatchFail "give" [ showText p ]


shuffleGive :: Id -> MudState -> LastArgIsTargetBindings -> GenericRes
shuffleGive i ms LastArgIsTargetBindings { .. } =
    let (targetGecrs, targetMiss, targetRcs) = uncurry (resolveEntCoinNames i ms . pure $ targetArg) rmInvCoins
    in if ()# targetMiss && ()!# targetRcs
      then genericSorry ms sorryGiveToCoin
      else case procGecrMisRm . head . zip targetGecrs $ targetMiss of
        Left  msg        -> genericSorry ms msg
        Right [targetId] -> if isNpcPC targetId ms
          then let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv otherArgs
                   sorryInEq              = inEqs |!| sorryGiveInEq
                   sorryInRm              = inRms |!| sorryGiveInRm
                   (gecrs, miss, rcs) = uncurry (resolveEntCoinNames i ms inInvs) srcInvCoins
                   eiss               = zipWith (curry procGecrMisMobInv) gecrs miss
                   ecs                = map procReconciledCoinsMobInv rcs
                   (ms',  toSelfs,  bs,  logMsgs ) = foldl' (helperGiveEitherInv  i srcDesig targetId)
                                                            (ms, [], [], [])
                                                            eiss
                   (ms'', toSelfs', bs', logMsgs') =        helperGiveEitherCoins i srcDesig targetId
                                                            (ms', toSelfs, bs, logMsgs)
                                                            ecs
               in (ms'', (dropBlanks $ [ sorryInEq, sorryInRm ] ++ toSelfs', bs', map (parseDesig i ms) logMsgs'))
        else genericSorry ms . sorryGiveType . getSing targetId $ ms
        Right {} -> genericSorry ms sorryGiveExcessTargets


-----


go :: Text -> ActionFun
go dir p@ActionParams { args = [] } = goDispatcher p { args = pure dir   }
go dir p@ActionParams { args      } = goDispatcher p { args = dir : args }


goDispatcher :: ActionFun
goDispatcher ActionParams { args = [] } = unit
goDispatcher p@(Lower i mq cols as)     = mapM_ (tryMove i mq cols p { args = [] }) as
goDispatcher p                          = patternMatchFail "goDispatcher" [ showText p ]


tryMove :: Id -> MsgQueue -> Cols -> ActionParams -> Text -> MudStack ()
tryMove i mq cols p dir = helper |&| modifyState >=> \case
  Left  msg          -> wrapSend mq cols msg
  Right (bs, logMsg) -> look p >> bcastIfNotIncog i bs >> logPla "tryMove" i logMsg
  where
    helper ms =
        let originId = getRmId i ms
            originRm = getRm originId ms
        in case findExit originRm dir of
          Nothing -> (ms, Left sorry)
          Just (linkTxt, destId, maybeOriginMsg, maybeDestMsg) ->
            let originDesig  = mkStdDesig i ms DoCap
                s            = fromJust . sDesigEntSing $ originDesig
                originMobIds = i `delete` desigIds originDesig
                destMobIds   = findMobIds ms $ ms^.invTbl.ind destId
                ms'          = ms & mobTbl.ind i.rmId   .~ destId
                                  & invTbl.ind originId %~ (i `delete`)
                                  & invTbl.ind destId   %~ addToInv ms (pure i)
                msgAtOrigin  = nlnl $ case maybeOriginMsg of
                                 Nothing  -> T.concat [ serialize originDesig, spaced verb, expandLinkName dir, "." ]
                                 Just msg -> T.replace "%" (serialize originDesig) msg
                msgAtDest    = let destDesig = mkSerializedNonStdDesig i ms s A DoCap in nlnl $ case maybeDestMsg of
                                 Nothing  -> T.concat [ destDesig, " arrives from ", expandOppLinkName dir, "." ]
                                 Just msg -> T.replace "%" destDesig msg
                logMsg       = T.concat [ "moved "
                                        , linkTxt
                                        , " from room "
                                        , showRm originId originRm
                                        , " to room "
                                        , showRm destId . getRm destId $ ms
                                        , "." ]
            in (ms', Right ([ (msgAtOrigin, originMobIds), (msgAtDest, destMobIds) ], logMsg))
    sorry = dir `elem` stdLinkNames ? sorryGoExit :? sorryGoParseDir dir
    verb | dir == "u"              = "goes"
         | dir == "d"              = "heads"
         | dir `elem` stdLinkNames = "leaves"
         | otherwise               = "enters"
    showRm (showText -> ri) (views rmName parensQuote -> rn) = ri <> " " <> rn


findExit :: Rm -> LinkName -> Maybe (Text, Id, Maybe Text, Maybe Text)
findExit (view rmLinks -> rls) ln =
    case [ (showLink rl, getDestId rl, getOriginMsg rl, getDestMsg rl) | rl <- rls, isValid rl ] of
      [] -> Nothing
      xs -> Just . head $ xs
  where
    isValid      StdLink    { .. } = ln == linkDirToCmdName _linkDir
    isValid      NonStdLink { .. } = ln `T.isPrefixOf` _linkName
    showLink     StdLink    { .. } = showText _linkDir
    showLink     NonStdLink { .. } = _linkName
    getDestId    StdLink    { .. } = _stdDestId
    getDestId    NonStdLink { .. } = _nonStdDestId
    getOriginMsg NonStdLink { .. } = Just _originMsg
    getOriginMsg _                 = Nothing
    getDestMsg   NonStdLink { .. } = Just _destMsg
    getDestMsg   _                 = Nothing


expandLinkName :: Text -> Text
expandLinkName "n"  = "north"
expandLinkName "ne" = "northeast"
expandLinkName "e"  = "east"
expandLinkName "se" = "southeast"
expandLinkName "s"  = "south"
expandLinkName "sw" = "southwest"
expandLinkName "w"  = "west"
expandLinkName "nw" = "northwest"
expandLinkName "u"  = "up"
expandLinkName "d"  = "down"
expandLinkName x    = patternMatchFail "expandLinkName" [x]


expandOppLinkName :: Text -> Text
expandOppLinkName "n"  = "the south"
expandOppLinkName "ne" = "the southwest"
expandOppLinkName "e"  = "the west"
expandOppLinkName "se" = "the northwest"
expandOppLinkName "s"  = "the north"
expandOppLinkName "sw" = "the northeast"
expandOppLinkName "w"  = "the east"
expandOppLinkName "nw" = "the southeast"
expandOppLinkName "u"  = "below"
expandOppLinkName "d"  = "above"
expandOppLinkName x    = patternMatchFail "expandOppLinkName" [x]


-- The following 3 functions are here because they reference "go" (which references "look")...
mkNonStdRmLinkCmds :: Rm -> [Cmd]
mkNonStdRmLinkCmds (view rmLinks -> rls) = [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) =
    Cmd { cmdName = cn, cmdPriorityAbbrev = Nothing, cmdFullName = cn, cmdAction = Action (go cn) True, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _linkDir
                                               NonStdLink { .. } -> _linkName


 -----


help :: ActionFun
help (NoArgs i mq cols) = (liftIO . T.readFile $ helpDir </> "root") |&| try >=> either handler helper
  where
    handler e          = fileIOExHandler "help" e >> wrapSend mq cols helpRootErrorMsg
    helper rootHelpTxt = (isAdminId i <$> getState) >>= \ia -> do
        (sortBy (compare `on` helpName) -> hs) <- liftIO . mkHelpData $ ia
        let zipped                 = zip (styleAbbrevs Don'tQuote [ helpName h | h <- hs ]) hs
            (cmdNames, topicNames) = partition (isCmdHelp . snd) zipped & both %~ (formatHelpNames . mkHelpNames)
            helpTxt                = T.concat [ nl rootHelpTxt
                                              , nl "Help is available on the following commands:"
                                              , nl cmdNames
                                              , nl "Help is available on the following topics:"
                                              , topicNames
                                              , ia |?| footnote ]
        (pager i mq . parseHelpTxt cols $ helpTxt) >> logPla "help" i "read root help file."
    mkHelpNames zipped    = [ padHelpTopic . (styled <>) $ isAdminHelp h |?| asterisk | (styled, h) <- zipped ]
    formatHelpNames names = let wordsPerLine = cols `div` helpTopicPadding
                            in T.unlines . map T.concat . chunksOf wordsPerLine $ names
    footnote              = nlPrefix $ asterisk <> " indicates help that is available only to administrators."
help (LowerNub i mq cols as) = (isAdminId i <$> getState) >>= liftIO . mkHelpData >>= \hs -> do
    (map (parseHelpTxt cols) -> helpTxts, dropBlanks -> hns) <- unzip <$> forM as (getHelpByName cols hs)
    pager i mq . intercalateDivider cols $ helpTxts
    hns |#| logPla "help" i . ("read help on: " <>) . commas
help p = patternMatchFail "help" [ showText p ]


mkHelpData :: Bool -> IO [Help]
mkHelpData ia = helpDirs |&| mapM getHelpDirectoryContents >=> \[ plaHelpCmdNames
                                                                , plaHelpTopicNames
                                                                , adminHelpCmdNames
                                                                , adminHelpTopicNames ] -> do
    let phcs = [ Help { helpName     = T.pack phcn
                      , helpFilePath = plaHelpCmdsDir     </> phcn
                      , isCmdHelp    = True
                      , isAdminHelp  = False } | phcn <- plaHelpCmdNames     ]
        phts = [ Help { helpName     = T.pack phtn
                      , helpFilePath = plaHelpTopicsDir   </> phtn
                      , isCmdHelp    = False
                      , isAdminHelp  = False } | phtn <- plaHelpTopicNames   ]
        ahcs = [ Help { helpName     = T.pack $ adminCmdChar : whcn
                      , helpFilePath = adminHelpCmdsDir   </> whcn
                      , isCmdHelp    = True
                      , isAdminHelp  = True }  | whcn <- adminHelpCmdNames   ]
        ahts = [ Help { helpName     = T.pack whtn
                      , helpFilePath = adminHelpTopicsDir </> whtn
                      , isCmdHelp    = False
                      , isAdminHelp  = True }  | whtn <- adminHelpTopicNames ]
    return $ phcs ++ phts ++ (guard ia >> ahcs ++ ahts)
  where
    helpDirs                     = [ plaHelpCmdsDir, plaHelpTopicsDir, adminHelpCmdsDir, adminHelpTopicsDir ]
    getHelpDirectoryContents dir = dropIrrelevantFilenames . sort <$> getDirectoryContents dir


parseHelpTxt :: Cols -> Text -> [Text]
parseHelpTxt cols = concat . wrapLines cols . map expandDividers . T.lines . parseTokens
  where
    expandDividers l | l == T.singleton dividerToken = T.replicate cols "-"
                     | otherwise                     = l


getHelpByName :: Cols -> [Help] -> HelpName -> MudStack (Text, Text)
getHelpByName cols hs name = findFullNameForAbbrev name [ (h, helpName h) | h <- hs ] |&| maybe sorry found
  where
    sorry                                      = return (sorryHelpName name, "")
    found (helpFilePath -> hf, dblQuote -> hn) = (,) <$> readHelpFile hf hn <*> return hn
    readHelpFile hf hn                         = (liftIO . T.readFile $ hf) |&| try >=> eitherRet handler
      where
        handler e = do
            fileIOExHandler "getHelpByName readHelpFile" e
            return . wrapUnlines cols . helpFileErrorMsg $ hn


-----


intro :: ActionFun
intro (NoArgs i mq cols) = getState >>= \ms -> let intros = getIntroduced i ms in if ()# intros
  then let introsTxt = "No one has introduced themselves to you yet."
       in wrapSend mq cols introsTxt >> (logPlaOut "intro" i . pure $ introsTxt)
  else let introsTxt = commas intros in do
      multiWrapSend mq cols [ "You know the following names:", introsTxt ]
      logPla "intro" i $ "known names: " <> introsTxt
intro (LowerNub i mq cols as) = getState >>= \ms -> if isIncognitoId i ms
  then wrapSend mq cols . sorryIncog $ "intro"
  else helper |&| modifyState >=> \(map fromClassifiedBcast . sort -> bs, logMsgs, intro'dIds) -> do
    bcast bs
    mapM_ (awardExp 50 (getSing i ms <> " introduced")) intro'dIds
    logMsgs |#| logPla "intro" i . slashes
  where
    helper ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorryInInv = inInvs |!| mkNTB sorryIntroInInv
            sorryInEq  = inEqs  |!| mkNTB sorryIntroInEq
            invCoins@(first (i `delete`) -> invCoins') = getMobRmNonIncogInvCoins i ms
            (eiss, ecs) = uncurry (resolveRmInvCoins i ms inRms) invCoins'
            ris         = fst invCoins
            (pt, cbs,  logMsgs, intro'dIds) = foldl' (helperIntroEitherInv ms ris) (ms^.pcTbl, [], [], []) eiss
            (    cbs', logMsgs'           ) = foldl' helperIntroEitherCoins        (cbs, logMsgs)          ecs
        in if ()!# invCoins'
          then (ms & pcTbl .~ pt, (sorryInInv ++ sorryInEq ++ cbs', logMsgs', intro'dIds))
          else (ms,               (mkNTB sorryIntroNoOneHere,       [],       []        ))
    mkNTB                                           = mkNTBcast i . nlnl
    helperIntroEitherInv _  _   a (Left msg       ) = ()# msg ? a :? (a & _2 <>~ mkNTB msg)
    helperIntroEitherInv ms ris a (Right targetIds) = foldl' tryIntro a targetIds
      where
        tryIntro a'@(pt, _, _, _) targetId = let targetSing = getSing targetId ms in case getType targetId ms of
          PCType -> let s           = getSing i ms
                        targetDesig = serialize . mkStdDesig targetId ms $ Don'tCap
                        msg         = "You introduce yourself to " <> targetDesig <> "."
                        logMsg      = "Introduced to " <> targetSing <> "."
                        srcMsg      = nlnl msg
                        is          = findMobIds ms ris
                        srcDesig    = StdDesig { sDesigEntSing = Nothing
                                               , shouldCap     = DoCap
                                               , desigEntName  = mkUnknownPCEntName i ms
                                               , desigId       = i
                                               , desigIds      = is }
                        himHerself  = mkReflexPro . getSex i $ ms
                        targetMsg   = nlnl . T.concat $ [ parseDesig targetId ms . serialize $ srcDesig
                                                        , " introduces "
                                                        , himHerself
                                                        , " to you as "
                                                        , colorWith knownNameColor s
                                                        , "." ]
                        othersMsg   = nlnl . T.concat $ [ serialize srcDesig { sDesigEntSing = Just s }
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
          _      -> let b = head . mkNTB . sorryIntroType $ targetSing
                    in a' & _2 %~ (`appendIfUnique` b)
    helperIntroEitherCoins a (Left msgs) = a & _1 <>~ (mkNTBcast i . T.concat $ [ nlnl msg | msg <- msgs ])
    helperIntroEitherCoins a Right {}    =
        let cb = head . mkNTB $ sorryIntroCoin
        in first (`appendIfUnique` cb) a
    fromClassifiedBcast (TargetBcast    b) = b
    fromClassifiedBcast (NonTargetBcast b) = b
intro p = patternMatchFail "intro" [ showText p ]


-----


inv :: ActionFun
inv (NoArgs   i mq cols   ) = getState >>= \ms@(getSing i -> s) -> send mq . nl . mkInvCoinsDesc i cols ms i $ s
inv (LowerNub i mq cols as) = getState >>= \ms ->
    let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
        invCoins               = getInvCoins i ms
        (eiss, ecs)            = uncurry (resolveMobInvCoins i ms inInvs) invCoins
        invDesc                = foldl' (helperEitherInv ms) "" eiss
        coinsDesc              = foldl' helperEitherCoins    "" ecs
    in send mq $ if ()!# invCoins
    then T.concat [ inEqs |!| sorryInEq, inRms |!| sorryInRm, invDesc, coinsDesc ]
      else wrapUnlinesNl cols dudeYourHandsAreEmpty
  where
    helperEitherInv _  acc (Left  msg ) = acc <> wrapUnlinesNl cols msg
    helperEitherInv ms acc (Right is  ) = nl $ acc <> mkEntDescs i cols ms is
    helperEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
    sorryInEq                           = wrapUnlinesNl cols . sorryEquipInvLook InvCmd $ EquipCmd
    sorryInRm                           = wrapUnlinesNl cols . sorryEquipInvLook InvCmd $ LookCmd
inv p = patternMatchFail "inv" [ showText p ]


-----


leave :: ActionFun
leave p@AdviseNoArgs                   = advise p ["leave"] adviceLeaveNoArgs
leave (WithArgs i mq cols (nub -> as)) = helper |&| modifyState >=> \(ms, chanIdNameIsDels, sorryMsgs) ->
    let s                              = getSing i ms
        (chanIds, chanNames, chanRecs) = foldl' unzipper ([], [], []) chanIdNameIsDels
        unzipper acc (ci, cn, isDel)
          | isDel     = acc & _2 <>~ pure cn
                            & _3 <>~ (pure . ChanRec "" ci cn s . asteriskQuote $ "Channel deleted.")
          | otherwise = acc & _1 <>~ pure ci
                            & _2 <>~ pure cn
        toSelfMsgs = mkLeaveMsg chanNames
        msgs       = ()# sorryMsgs ? toSelfMsgs :? sorryMsgs ++ (toSelfMsgs |!| "" : toSelfMsgs)
        f bs ci    = let c        = getChan ci ms
                         otherIds = views chanConnTbl g c
                         g        = filter (`isAwake` ms) . map (`getIdForMobSing` ms) . M.keys . M.filter id
                     in (bs ++) <$> forM otherIds (\i' -> [ ( T.concat [ "You sense that "
                                                                       , n
                                                                       , " has left the "
                                                                       , views chanName dblQuote c
                                                                       , " channel." ]
                                                            , pure i' ) | n <- getRelativePCName ms (i', i) ])
    in do
        multiWrapSend mq cols msgs
        bcastNl =<< foldM f [] chanIds
        chanNames |#| logPla "leave" i . commas
        ts <- liftIO mkTimestamp
        forM_ chanRecs $ \cr -> withDbExHandler_ "leave" . insertDbTblChan $ cr { chanTimestamp = ts }
  where
    helper ms = let (ms', chanIdNameIsDels, sorryMsgs) = foldl' f (ms, [], []) as
                in (ms', (ms', chanIdNameIsDels, sorryMsgs))
      where
        f triple a@(T.toLower -> a') =
            let notFound     = triple & _3 <>~ (pure . sorryChanName $ a)
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
                 , theLetterS isPlur
                 , " to the "
                 , isPlur ? "following channels:\n" <> commas ns :? head ns <> " channel"
                 , "." ]
leave p = patternMatchFail "leave" [ showText p ]


-----


link :: ActionFun
link (NoArgs i mq cols) = do
    ms  <- getState
    res <- helperLinkUnlink ms i mq cols
    flip maybeVoid res $ \(meLinkedToOthers, othersLinkedToMe, twoWays) ->
        let msgs             = intercalate [""] . dropEmpties $ [ twoWays       |!| twoWayMsgs
                                                                , oneWaysFromMe |!| oneWayFromMeMsgs
                                                                , oneWaysToMe   |!| oneWayToMeMsgs ]
            oneWaysFromMe    = meLinkedToOthers \\ twoWays
            oneWaysToMe      = othersLinkedToMe \\ twoWays
            twoWayMsgs       = [ "Two-way links:",                mkSingsList True  twoWays       ]
            oneWayFromMeMsgs = [ "One-way links from your mind:", mkSingsList False oneWaysFromMe ]
            oneWayToMeMsgs   = [ "One-way links to your mind:",   mkSingsList False oneWaysToMe   ]
            mkSingsList doStyle ss = let (awakes, asleeps) = sortAwakesAsleeps ss
                                     in commas $ onTrue doStyle (styleAbbrevs Don'tQuote) awakes ++ asleeps
            sortAwakesAsleeps      = foldr sorter ([], [])
            sorter linkSing acc    =
                let linkId   = head . filter ((== linkSing) . flip getSing ms) $ ms^.pcTbl.to IM.keys
                    linkPla  = getPla linkId ms
                    f lens x = acc & lens %~ (x' :)
                      where
                        x' = case view (at linkSing) . getTeleLinkTbl i $ ms of
                          Nothing  -> x
                          Just val -> val ? x :? (x <> " " <> parensQuote "tuned out")
                in (linkSing |&|) $ if and [ isLoggedIn linkPla, not . isIncognito $ linkPla ]
                  then f _1
                  else f _2
        in do
           multiWrapSend mq cols msgs
           logPla "link" i . slashes . dropBlanks $ [ twoWays       |!| "Two-way: "         <> commas twoWays
                                                    , oneWaysFromMe |!| "One-way from me: " <> commas oneWaysFromMe
                                                    , oneWaysToMe   |!| "One-way to me: "   <> commas oneWaysToMe ]
link (LowerNub i mq cols as) = getState >>= \ms -> if isIncognitoId i ms
  then wrapSend mq cols . sorryIncog $ "link"
  else helper |&| modifyState >=> \(bs, logMsgs, fs) ->
      bcast bs >> sequence_ fs >> logMsgs |#| logPla "link" i . slashes
  where
    helper ms = let (inInvs, inEqs, inRms)  = sortArgsInvEqRm InRm as
                    sorryInInv              = inInvs |!| (mkBcast i . nlnl $ sorryLinkInInv)
                    sorryInEq               = inEqs  |!| (mkBcast i . nlnl $ sorryLinkInEq )
                    invCoins                = first (i `delete`) . getMobRmNonIncogInvCoins i $ ms
                    (eiss, ecs)             = uncurry (resolveRmInvCoins i ms inRms) invCoins
                    (ms', bs,  logMsgs, fs) = foldl' helperLinkEitherInv (ms, [], [], []) eiss
                    (     bs', logMsgs'   ) = foldl' helperLinkEitherCoins (bs, logMsgs) ecs
                in if ()!# invCoins
                  then (ms', (sorryInInv ++ sorryInEq ++ bs',        logMsgs', fs))
                  else (ms,  (mkBcast i . nlnl $ sorryLinkNoOneHere, [],       []))
    helperLinkEitherInv a (Left  sorryMsg ) = ()# sorryMsg ? a :? (a & _2 <>~ (mkBcast i . nlnl $ sorryMsg))
    helperLinkEitherInv a (Right targetIds) = foldl' tryLink a targetIds
      where
        tryLink a'@(ms, _, _, _) targetId = let targetSing = getSing targetId ms in case getType targetId ms of
          PCType ->
            let (srcIntros, targetIntros) = f getIntroduced
                (srcLinks,  targetLinks ) = f getLinked
                f g                       = ((i |&|) *** (targetId |&|)) (dup $ uncurry g . (, ms))
                s                         = getSing i ms
                targetDesig               = serialize . mkStdDesig targetId ms $ Don'tCap
                srcMsg    = nlnl . T.concat $ [ focusingInnateMsg
                                              , "you establish a telepathic connection from your mind to "
                                              , targetSing
                                              , "'s mind."
                                              , twoWayMsg ]
                twoWayMsg = isTwoWay |?| " This completes the psionic circuit and you may now communicate with each \
                                         \other telepathically."
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
                msgHelper txt = a' & _2 <>~ (mkBcast i . nlnl $ txt)
            in if
              | targetSing `notElem` srcIntros    -> msgHelper . sorryLinkIntroTarget       $ targetDesig
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
            action = rndmDo (calcProbLinkFlinch targetId ms) . mkExpAction "flinch" . mkActionParams targetId ms $ []
    helperLinkEitherCoins a (Left msgs) = a & _1 <>~ (mkBcast i . T.concat $ [ nlnl msg | msg <- msgs ])
    helperLinkEitherCoins a Right {}    = let b = (nlnl sorryLinkCoin, pure i) in first (`appendIfUnique` b) a
link p = patternMatchFail "link" [ showText p ]


-----


look :: ActionFun
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
    helper v |&| modifyState >=> \(toSelf, bs, hookLogMsg, maybeTargetDesigs) -> do
        send mq toSelf
        bcastIfNotIncogNl i bs
        let mkLogMsgForDesigs targetDesigs | targetSings <- [ fromJust . sDesigEntSing $ targetDesig
                                                            | targetDesig <- targetDesigs ]
                                           = "looked at " <> commas targetSings
            logMsg = T.intercalate " / " . dropBlanks $ [ maybe "" mkLogMsgForDesigs maybeTargetDesigs, hookLogMsg ]
        logMsg |#| logPla "look" i . (<> ".")
  where
    helper v ms =
        let invCoins               = first (i `delete`) . getMobRmNonIncogInvCoins i $ ms
            (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorry                  = T.concat [ inInvs |!| sorryInInv, inEqs |!| sorryInEq ]
            sorryInInv             = wrapUnlinesNl cols . sorryEquipInvLook LookCmd $ InvCmd
            sorryInEq              = wrapUnlinesNl cols . sorryEquipInvLook LookCmd $ EquipCmd
        in case ((()!#) *** (()!#)) (invCoins, lookupHooks i ms "look") of
          (False, False) -> (ms, (wrapUnlinesNl cols sorryLookEmptyRmNoHooks, [], "", Nothing))
          -----
          (True,  False) -> let (toSelf, bs, maybeDesigs) = invCoinsHelper ms inRms invCoins
                            in (ms, (sorry <> toSelf, bs, "", maybeDesigs))
          -----
          (False, True ) -> let (inRms', (ms', toSelf, bs, logMsg)) = hooksHelper ms v inRms
                                sorry' = sorry <> (inRms' |!| wrapUnlinesNl cols sorryLookEmptyRmWithHooks)
                            in (ms', (sorry' <> toSelf, bs, logMsg, Nothing))
          -----
          (True,  True ) -> let (inRms', (ms', hooksToSelf, hooksBs, logMsg)) = hooksHelper ms v inRms
                                (invCoinsToSelf, invCoinsBs, maybeDesigs)     = invCoinsHelper ms' inRms' invCoins
                            in (ms', ( sorry <> hooksToSelf <> invCoinsToSelf
                                     , hooksBs ++ invCoinsBs
                                     , logMsg
                                     , maybeDesigs ))
    -----
    invCoinsHelper ms args invCoins =
        let (eiss, ecs)  = uncurry (resolveRmInvCoins i ms args) invCoins
            invDesc      = foldl' (helperLookEitherInv ms) "" eiss
            coinsDesc    = foldl' helperLookEitherCoins    "" ecs
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
    helperLookEitherInv _  acc (Left  msg ) = acc <> wrapUnlinesNl cols msg
    helperLookEitherInv ms acc (Right is  ) = nl $ acc <> mkEntDescs i cols ms is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
    -----
    hooksHelper ms v args = procHooks i ms v "look" args & _2._2 %~ (T.unlines . map (multiWrap cols . T.lines))
                                                         & _2._4 %~ slashes
look p = patternMatchFail "look" [ showText p ]


mkRmInvCoinsDesc :: Id -> Cols -> MudState -> Id -> Text
mkRmInvCoinsDesc i cols ms ri =
    let (ris, c)            = first (i `delete`) . getNonIncogInvCoins ri $ ms
        (pcNcbs, otherNcbs) = splitPCsOthers . mkIsPC_StyledName_Count_BothList i ms $ ris
        pcDescs             = T.unlines . concatMap (wrapIndent 2 cols . mkPCDesc   ) $ pcNcbs
        otherDescs          = T.unlines . concatMap (wrapIndent 2 cols . mkOtherDesc) $ otherNcbs
    in (pcNcbs |!| pcDescs) <> (otherNcbs |!| otherDescs) <> (c |!| mkCoinsSummary cols c)
  where
    splitPCsOthers                       = (both %~ map snd) . span fst
    mkPCDesc    (en, c, (s, _)) | c == 1 = (<> " " <> en) $ if isKnownPCSing s
                                             then colorWith knownNameColor s
                                             else colorWith unknownNameColor . aOrAn $ s
    mkPCDesc    (en, c, b     ) = colorWith unknownNameColor (showText c <> " " <> mkPlurFromBoth b) <> " " <> en
    mkOtherDesc (en, c, (s, _)) | c == 1 = aOrAnOnLower s <> " " <> en
    mkOtherDesc (en, c, b     )          = showText c <> spaced (mkPlurFromBoth b) <> en

mkIsPC_StyledName_Count_BothList :: Id -> MudState -> Inv -> [(Bool, (Text, Int, BothGramNos))]
mkIsPC_StyledName_Count_BothList i ms targetIds =
  let isPCs   =                      [ getType targetId ms == PCType   | targetId <- targetIds ]
      styleds = styleAbbrevs DoQuote [ getEffName        i ms targetId | targetId <- targetIds ]
      boths   =                      [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
      counts  = mkCountList boths
  in nub . zip isPCs . zip3 styleds counts $ boths


isKnownPCSing :: Sing -> Bool
isKnownPCSing s = case T.words s of [ "male",   _ ] -> False
                                    [ "female", _ ] -> False
                                    _               -> True


extractMobIdsFromEiss :: MudState -> [Either Text Inv] -> [Id]
extractMobIdsFromEiss ms = foldl' helper []
  where
    helper acc Left   {}  = acc
    helper acc (Right is) = acc ++ findMobIds ms is


-----


motd :: ActionFun
motd (NoArgs i mq cols) = showMotd mq cols >> logPlaExec "motd" i
motd p                  = withoutArgs motd p


showMotd :: MsgQueue -> Cols -> MudStack ()
showMotd mq cols = send mq =<< helper
  where
    helper    = liftIO readMotd |&| try >=> eitherRet handler
    readMotd  = [ frame cols . multiWrap cols . T.lines . colorizeFileTxt motdColor $ cont
                | cont <- T.readFile motdFile ]
    handler e = do
        fileIOExHandler "showMotd" e
        return . wrapUnlinesNl cols $ motdErrorMsg


-----


newChan :: ActionFun
newChan p@AdviseNoArgs                   = advise p ["newchannel"] adviceNewChanNoArgs
newChan (WithArgs i mq cols (nub -> as)) = helper |&| modifyState >=> \(unzip -> (newChanNames, chanRecs), sorryMsgs) ->
    let (sorryMsgs', otherMsgs) = (intersperse "" sorryMsgs, mkNewChanMsg newChanNames)
        msgs | ()# otherMsgs    = sorryMsgs'
             | otherwise        = otherMsgs ++ (sorryMsgs' |!| "" : sorryMsgs')
    in do
        multiWrapSend mq cols msgs
        newChanNames |#| logPla "newChan" i . commas
        ts <- liftIO mkTimestamp
        forM_ chanRecs $ \cr -> withDbExHandler_ "newChan" . insertDbTblChan $ cr { chanTimestamp = ts }
  where
    helper ms = let s                              = getSing i ms
                    (ms', newChanNames, sorryMsgs) = foldl' (f s) (ms, [], []) as
                in (ms', (newChanNames, sorryMsgs))
      where
        f s triple@(ms', _, _) a@(T.toLower -> a')
          | T.length a > maxChanNameLen =
              let msg = "a channel name may not be more than " <> showText maxChanNameLen <> " characters long"
              in sorryNewChanName a msg `sorry` triple
          | T.any isNG a = sorryNewChanName a "a channel name may only contain alphabetic letters and digits" `sorry` triple
          | a' `elem` illegalNames = sorryNewChanName a "this name is reserved or already in use" `sorry` triple
          | a' `elem` map T.toLower myChanNames
          , match <- head . filter ((== a') . T.toLower) $ myChanNames = sorryNewChanExisting match `sorry` triple
          | not . hasPp i ms' $ 3 = sorryPp ("create a new channel named " <> dblQuote a) `sorry` triple
          | otherwise = let ci = views chanTbl (head . ([0..] \\) . IM.keys) $ triple^._1
                            c  = Chan ci a (M.singleton s True) []
                            cr = ChanRec "" ci a s . asteriskQuote $ "New channel created."
                        in triple & _1.chanTbl.at ci ?~ c
                                  & _1.mobTbl.ind i.curPp -~ 3
                                  & _2 <>~ pure (a, cr)
        sorry msg    = _3 <>~ pure msg
        isNG c       = not $ isLetter c || isDigit c
        illegalNames = [ "admin", "all", "question" ] ++ pcNames
        pcNames      = map (uncapitalize . (`getSing` ms)) $ ms^.pcTbl.to IM.keys
        myChanNames  = map (view chanName) . getPCChans i $ ms
    mkNewChanMsg []     = []
    mkNewChanMsg ns@[_] = pure    . mkMsgHelper False $ ns
    mkNewChanMsg ns     = T.lines . mkMsgHelper True  $ ns
    mkMsgHelper isPlur (map dblQuote -> ns) =
        T.concat [ focusingInnateMsg
                 , "you create a "
                 , isPlur |?| "group of "
                 , "telepathic network"
                 , theLetterS isPlur
                 , " to which others may be connected. To "
                 , isPlur ? "these " :? "this "
                 , dblQuote . ("channel" <>) . theLetterS $ isPlur
                 , " you assign the "
                 , isPlur |?| "following "
                 , "name"
                 , isPlur ? "s:\n" <> commas ns :? " " <> head ns
                 , "." ]
newChan p = patternMatchFail "newChan" [ showText p ]


-----


npcAsSelf :: ActionFun
npcAsSelf p = execIfPossessed p "." npcAsSelfHelper


npcAsSelfHelper :: ActionFun
npcAsSelfHelper p@(NoArgs' i mq)     = advise p [] adviceAsSelfNoArgs >> sendDfltPrompt mq i
npcAsSelfHelper (WithArgs i mq _ as) = do
    logPlaExecArgs "." as i
    liftIO . atomically . writeTQueue mq . AsSelf . nl . T.unwords $ as
npcAsSelfHelper p = patternMatchFail "npcAsSelfHelper" [ showText p ]


-----


npcDispCmdList :: ActionFun
npcDispCmdList p@(LowerNub' i as) = dispCmdList npcCmds p >> logPlaExecArgs "?" as i
npcDispCmdList p                  = patternMatchFail "npcDispCmdList" [ showText p ]


-----


npcStop :: ActionFun
npcStop p = execIfPossessed p "stop" npcStopHelper


npcStopHelper :: ActionFun
npcStopHelper (NoArgs i mq cols) = getState >>= \ms -> let pi = fromJust . getPossessor i $ ms in do
    wrapSend mq cols $ "You stop possessing " <> aOrAnOnLower (getSing    i ms) <> "."
    sendDfltPrompt mq pi
    logPla "stop" i  $ "stopped possessing "  <> aOrAnOnLower (descSingId i ms) <> "."
    tweaks [ plaTbl.ind pi.possessing .~ Nothing, npcTbl.ind i.possessor .~ Nothing ]
npcStopHelper p = withoutArgs npcStopHelper p


-----


plaDispCmdList :: ActionFun
plaDispCmdList p@(LowerNub' i as) = dispCmdList plaCmds p >> logPlaExecArgs "?" as i
plaDispCmdList p                  = patternMatchFail "plaDispCmdList" [ showText p ]


-----


putAction :: ActionFun
putAction p@AdviseNoArgs     = advise p ["put"] advicePutNoArgs
putAction p@(AdviseOneArg a) = advise p ["put"] . advicePutNoCon $ a
putAction p@(Lower' i as)    = genericAction p helper "put"
  where
    helper v ms =
      let LastArgIsTargetBindings { .. } = mkLastArgIsTargetBindings i ms as
          shuffler target b is           = shufflePut i ms srcDesig target b otherArgs is srcInvCoins
      in case singleArgInvEqRm InInv targetArg of
        (InInv, target) -> shuffler target False srcInvCoins procGecrMisMobInv
        (InEq,  _     ) -> genericSorry ms . sorryConInEq $ Put
        (InRm,  target) ->
            let invCoinsHelper = shuffler target True rmInvCoins procGecrMisRm
                f hooks g      = case filter ((dropPrefixes target `elem`) . triggers) hooks of
                                   []      -> g
                                   matches -> hooksHelper otherArgs matches
            in case (()!# rmInvCoins, lookupHooks i ms "put") of
              (False, Nothing   ) -> genericSorry ms sorryNoConHere
              (True,  Nothing   ) -> invCoinsHelper
              (False, Just hooks) -> f hooks . genericSorry ms . sorryPutEmptyRmWithHooks $ target
              (True,  Just hooks) -> f hooks invCoinsHelper
      where
        hooksHelper args matches =
            let h@Hook { hookName }              = head matches
                (_, (ms', toSelfs, bs, logMsgs)) = getHookFun hookName ms i h v (args, (ms, [], [], []))
            in (ms', (toSelfs, bs, logMsgs))
putAction p = patternMatchFail "putAction" [ showText p ]


type CoinsWithCon = Coins
type PCInv        = Inv
type PCCoins      = Coins


shufflePut :: Id
           -> MudState
           -> Desig
           -> ConName
           -> IsConInRm
           -> Args
           -> (InvWithCon, CoinsWithCon)
           -> (PCInv, PCCoins)
           -> ((GetEntsCoinsRes, Maybe Inv) -> Either Text Inv)
           -> GenericRes
shufflePut i ms d conName icir as invCoinsWithCon@(invWithCon, _) mobInvCoins f =
    let (conGecrs, conMiss, conRcs) = uncurry (resolveEntCoinNames i ms . pure $ conName) invCoinsWithCon
    in if ()# conMiss && ()!# conRcs
      then genericSorry ms sorryPutInCoin
      else case f . head . zip conGecrs $ conMiss of
        Left  msg     -> genericSorry ms msg
        Right [conId] | (conSing, conType) <- (uncurry getSing *** uncurry getType) . dup $ (conId, ms) ->
            if conType /= ConType
              then genericSorry ms . sorryConHelper i ms conId $ conSing
              else let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
                       sorryInEq              = inEqs |!| sorryPutInEq
                       sorryInRm              = inRms |!| sorryPutInRm
                       (gecrs, miss, rcs)     = uncurry (resolveEntCoinNames i ms inInvs) mobInvCoins
                       eiss                   = zipWith (curry procGecrMisMobInv) gecrs miss
                       ecs                    = map procReconciledCoinsMobInv rcs
                       mnom                   = mkMaybeNthOfM ms icir conId conSing invWithCon
                       (ms',  toSelfs,  bs,  logMsgs ) = foldl' (helperPutEitherInv  i d mnom conId conSing)
                                                                (ms, [], [], [])
                                                                eiss
                       (ms'', toSelfs', bs', logMsgs') =        helperPutEitherCoins i d mnom conId conSing
                                                                (ms', toSelfs, bs, logMsgs)
                                                                ecs
                   in (ms'', (dropBlanks $ [ sorryInEq, sorryInRm ] ++ toSelfs', bs', logMsgs'))
        Right {} -> genericSorry ms sorryPutExcessCon


-----


question :: ActionFun
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
               styleds = styleAbbrevs Don'tQuote . map (view _2) $ tunedIns
               combo   = map f $ zipWith (\styled -> _2 .~ styled) styleds tunedIns ++ tunedOuts
                 where
                  f (i', n, ia) | ia           = (i', n <> asterisk)
                                | isRndmName n = (i', underline n  )
                                | otherwise    = (i', n            )
               mkDesc (i', n) = pad (succ namePadding) n <> (tunedInOut . isTunedQuestionId i' $ ms)
               descs          = mkDesc (i, getSing i ms <> (isAdminId i ms |?| asterisk)) : map mkDesc combo
               descs'         = "Question channel:" : descs
           in pager i mq descs' >> logPlaExecArgs "question" [] i
question (Msg i mq cols msg) = getState >>= \ms -> if
  | not . isTunedQuestionId i $ ms -> wrapSend mq cols . sorryTunedOutOOCChan $ "question"
  | isIncognitoId i ms             -> wrapSend mq cols . sorryChanIncog $ "the question"
  | otherwise                      -> getQuestionStyleds i ms >>= \triples -> if ()# triples
    then wrapSend mq cols . sorryChanNoOneListening $ "question"
    else let ioHelper (expandEmbeddedIdsToSings ms -> logMsg) bs = do
                 bcastNl =<< expandEmbeddedIds ms questionChanContext bs
                 logPlaOut "question" i . pure $ logMsg
                 ts <- liftIO mkTimestamp
                 withDbExHandler_ "question" . insertDbTblQuestion . QuestionRec ts s $ logMsg
             s    = getSing i ms
             f bs = let logMsg = dropANSI . fst . head $ bs
                    in ioHelper logMsg =<< g bs
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
question p = patternMatchFail "question" [ showText p ]


-----


quit :: ActionFun
quit (NoArgs' i mq)                        = logPlaExec "quit" i >> (liftIO . atomically . writeTQueue mq $ Quit)
quit ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols adviceQuitExcessArgs


handleEgress :: Id -> MudStack () -- TODO: Stop eating/drinking/moving.
handleEgress i = liftIO getCurrentTime >>= \now -> do
    informEgress
    helper now |&| modifyState >=> \(s, bs, logMsgs) -> do
        pauseEffects      i
        throwWaitRegen    i
        throwWaitDigester i
        closePlaLog       i
        bcast bs
        bcastAdmins $ s <> " has left CurryMUD."
        forM_ logMsgs . uncurry . logPla $ "handleEgress"
        logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", parensQuote s, " has left CurryMUD." ]
  where
    informEgress = getState >>= \ms -> let d = serialize . mkStdDesig i ms $ DoCap in
        unless (getRmId i ms == iWelcome) . bcastOthersInRm i . nlnl . egressMsg $ d
    helper now ms =
        let ri                 = getRmId i ms
            s                  = getSing i ms
            (ms', bs, logMsgs) = peepHelper ms s
            ms''               = if T.takeWhile (not . isDigit) s `elem` map showText (allValues :: [Race])
                                   then removeAdHoc i ms'
                                   else updateHostMap (possessHelper (movePC ms' ri)) s now
        in (ms'', (s, bs, logMsgs))
    peepHelper ms s =
        let (peeperIds, peepingIds) = getPeepersPeeping i ms
            bs                      = [ (nlnl    . T.concat $ [ "You are no longer peeping "
                                                              , s
                                                              , " "
                                                              , parensQuote $ s <> " has disconnected"
                                                              , "." ], pure peeperId) | peeperId <- peeperIds ]
            logMsgs                 = [ (peeperId, T.concat   [ "no longer peeping "
                                                              , s
                                                              , " "
                                                              , parensQuote $ s <> " has disconnected"
                                                              , "." ]) | peeperId <- peeperIds ]
        in (ms & plaTbl %~ stopPeeping     peepingIds
               & plaTbl %~ stopBeingPeeped peeperIds
               & plaTbl.ind i.peeping .~ []
               & plaTbl.ind i.peepers .~ [], bs, logMsgs)
      where
        stopPeeping     peepingIds pt = let f peepedId ptAcc = ptAcc & ind peepedId.peepers %~ (i `delete`)
                                        in foldr f pt peepingIds
        stopBeingPeeped peeperIds  pt = let f peeperId ptAcc = ptAcc & ind peeperId.peeping %~ (i `delete`)
                                        in foldr f pt peeperIds
    updateHostMap ms s now = flip (set $ hostTbl.at s) ms $ case getHostMap s ms of
      Nothing      -> Just . M.singleton host $ newRecord
      Just hostMap -> case hostMap^.at host of Nothing -> Just $ hostMap & at host ?~ newRecord
                                               Just r  -> Just $ hostMap & at host ?~ reviseRecord r
      where
        newRecord      = HostRecord { _noOfLogouts   = 1
                                    , _secsConnected = duration
                                    , _lastLogout    = now }
        reviseRecord r = r & noOfLogouts   +~ 1
                           & secsConnected +~ duration
                           & lastLogout    .~ now
        host           = getCurrHostName i ms
        duration       = round $ now `diffUTCTime` conTime
        conTime        = fromJust . getConnectTime i $ ms
    possessHelper ms = let f = maybe id (\npcId -> npcTbl.ind npcId.possessor .~ Nothing) . getPossessing i $ ms
                       in ms & plaTbl.ind i.possessing .~ Nothing & f
    movePC ms ri     = ms & invTbl     .ind ri         %~ (i `delete`)
                          & invTbl     .ind iLoggedOut %~ (i :)
                          & msgQueueTbl.at  i          .~ Nothing
                          & mobTbl     .ind i.rmId     .~ iLoggedOut
                          & plaTbl     .ind i.lastRmId ?~ ri


-----


quitCan'tAbbrev :: ActionFun
quitCan'tAbbrev (NoArgs _ mq cols) = wrapSend mq cols sorryQuitCan'tAbbrev
quitCan'tAbbrev p                  = withoutArgs quitCan'tAbbrev p


-----


readAction :: ActionFun
readAction p@AdviseNoArgs          = advise p ["read"] adviceReadNoArgs
readAction (LowerNub i mq cols as) = (,) <$> getState <*> mkRndmVector >>= \(ms, v) ->
    let (inInvs, inEqs, inRms) = sortArgsInvEqRm InInv as
        sorryInEq              = inEqs |!| wrapUnlinesNl cols sorryReadInEq
        sorryInRm              = inRms |!| wrapUnlinesNl cols sorryReadNoHooks
        d                      = mkStdDesig  i ms DoCap
        invCoins               = getInvCoins i ms
    in case ((()!#) *** (()!#)) (invCoins, lookupHooks i ms "read") of
      (False, False) -> let sorry = inInvs |!| wrapUnlinesNl cols dudeYourHandsAreEmpty
                        in send mq . T.concat $ [ sorry, sorryInEq, sorryInRm ]
      -----
      (True,  False) -> let sorry = sorryInEq <> sorryInRm
                        in ioHelper (invCoinsHelper ms inInvs d invCoins & _1 %~ (sorry <>))
      -----
      (False, True ) -> let sorry = (inInvs |!| wrapUnlinesNl cols dudeYourHandsAreEmpty) <> sorryInEq
                            (inRms', (_, toSelfs, bs, logMsgs)) = procHooks i ms v "read" inRms
                            sorry' = sorry <> (wrapper . map sorryReadWithHooks $ inRms')
                        in ioHelper (sorry' <> wrapper toSelfs, bs, logMsgs)
      -----
      (True,  True ) ->
        let (inRms', (_, hooksToSelfs, hooksBs, hooksLogMsgs)) = procHooks i ms v "read" inRms
            sorry = sorryInEq <> (wrapper . map sorryReadWithHooks $ inRms')
            (invCoinsToSelf, invCoinsBs, invCoinsLogMsgs) = invCoinsHelper ms inInvs d invCoins
        in ioHelper ( T.concat [ sorry, wrapper hooksToSelfs, invCoinsToSelf ]
                    , hooksBs      ++ invCoinsBs
                    , hooksLogMsgs ++ invCoinsLogMsgs )
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
    ioHelper (toSelf, bs, logMsgs) = do
        send mq toSelf
        bcastIfNotIncogNl i bs
        logMsgs |#| logPla "read" i . slashes
    wrapper = T.unlines . map (multiWrap cols . T.lines)
readAction p = patternMatchFail "readAction" [ showText p ]


readHelper :: Id -> Cols -> MudState -> Desig -> (Text, [Broadcast], [Text]) -> Inv -> (Text, [Broadcast], [Text])
readHelper i cols ms d = foldl' helper
  where
    helper acc targetId =
        let s                 = getSing targetId ms
            readIt txt header = acc & _1 <>~ (multiWrapNl cols . T.lines $ header <> txt)
                                    & _2 <>~ pure ( T.concat [ serialize d, " reads ", aOrAn s, "." ]
                                                  , i `delete` desigIds d )
                                    & _3 <>~ pure (s <> " " <> parensQuote (showText targetId))
        in case getType targetId ms of
          WritableType ->
              let (Writable msg r) = getWritable targetId ms in case msg of
                Nothing          -> acc & _1 <>~ wrapUnlinesNl cols (blankWritableMsg s)
                Just (txt, lang) -> case r of
                  Nothing -> if isKnownLang i ms lang
                    then readIt txt . T.concat $ [ "The following is written on the ", s, " in ", pp lang, ":\n" ]
                    else acc & _1 <>~ wrapUnlinesNl cols (sorryReadLang s lang)
                  Just recipSing
                    | isPC    i ms
                    , getSing i ms == recipSing || isAdminId i ms
                    , b    <- isKnownLang i ms lang
                    , txt' <- onFalse b (const . sorryReadOrigLang $ lang) txt
                    -> readIt txt' . mkMagicMsgHeader s b $ lang
                    | otherwise -> acc & _1 <>~ wrapUnlinesNl cols (sorryReadUnknownLang s)
          _ -> acc & _1 <>~ wrapUnlinesNl cols (sorryReadType s)
    mkMagicMsgHeader s b lang =
        T.concat [ "At first glance, the writing on the "
                 , s
                 , " appears to be in a language you don't recognize. Then suddenly the unfamiliar glyphs come alive, \
                   \squirming and melting into new forms. In a matter of seconds, the text transforms into "
                 , nl $ if b
                     then "the following message, written in " <> pp lang <> ":"
                     else "a message written in "              <> pp lang <> "." ]


-----


ready :: ActionFun
ready p@AdviseNoArgs     = advise p ["ready"] adviceReadyNoArgs
ready p@(LowerNub' i as) = genericAction p helper "ready"
  where
    helper _ ms =
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
        in if ()!# invCoins
          then (ms & eqTbl .~ et & invTbl .~ it, ( dropBlanks $ [ sorryInEq, sorryInRm, sorryCoins ] ++ toSelfs
                                                 , bs
                                                 , logMsgs ))
          else genericSorry ms dudeYourHandsAreEmpty
ready p = patternMatchFail "ready" [ showText p ]


helperReady :: Id
            -> MudState
            -> Desig
            -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
            -> (Either Text Inv, Maybe RightOrLeft)
            -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
helperReady _ _  _ a (Left  msg,       _   ) = a & _3 <>~ pure msg
helperReady i ms d a (Right targetIds, mrol) = foldl' (readyDispatcher i ms d mrol) a targetIds


readyDispatcher :: Id
                -> MudState
                -> Desig
                -> Maybe RightOrLeft
                -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
                -> Id
                -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyDispatcher i ms d mrol a targetId = let targetSing = getSing targetId ms in
    helper |&| maybe (sorry targetSing) (\f -> f i ms d mrol a targetId targetSing)
  where
    helper = case getType targetId ms of
      ClothType -> Just readyCloth
      ConType   -> boolToMaybe (getIsCloth targetId ms) readyCloth
      WpnType   -> Just readyWpn
      ArmType   -> Just readyArm
      _         -> Nothing
    sorry targetSing = a & _3 <>~ (pure . sorryReadyType $ targetSing)


-- Readying clothing:


readyCloth :: Id
           -> MudState
           -> Desig
           -> Maybe RightOrLeft
           -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
           -> Id
           -> Sing
           -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyCloth i ms d mrol a@(et, _, _, _, _) clothId clothSing | em <- et ! i, cloth <- getCloth clothId ms =
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


getAvailClothSlot :: Id -> MudState -> Cloth -> EqMap -> Either Text Slot
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
    sorry | cloth `elem` [ Earring .. Ring ]                   = sorryReadyClothFull      . pp $ cloth
          | cloth `elem` [ Skirt, Dress, Backpack, Cloak ]     = sorryReadyAlreadyWearing . pp $ cloth
          | ci <- em M.! clothToSlot cloth, s <- getSing ci ms = sorryReadyAlreadyWearing        s


otherSex :: Sex -> Sex
otherSex Male   = Female
otherSex Female = Male
otherSex NoSex  = Female


rEarringSlots, lEarringSlots, noseRingSlots, necklaceSlots, rBraceletSlots, lBraceletSlots :: [Slot]
rEarringSlots  = [ EarringR1S,    EarringR2S  ]
lEarringSlots  = [ EarringL1S,    EarringL2S  ]
noseRingSlots  = [ NoseRing1S,    NoseRing2S  ]
necklaceSlots  = [ Necklace1S  .. Necklace2S  ]
rBraceletSlots = [ BraceletR1S .. BraceletR3S ]
lBraceletSlots = [ BraceletL1S .. BraceletL3S ]


getDesigClothSlot :: MudState -> Sing -> Cloth -> EqMap -> RightOrLeft -> Either Text Slot
getDesigClothSlot ms clothSing cloth em rol
  | cloth `elem` [ NoseRing, Necklace ] ++ [ Shirt .. Cloak ] = sorryRol
  | isRingRol rol, cloth /= Ring                              = sorryRol
  | cloth == Ring, not . isRingRol $ rol                      = Left ringHelp
  | otherwise = case cloth of
    Earring  -> findSlotFromList rEarringSlots  lEarringSlots  |&| maybe (Left sorryEarring ) Right
    Bracelet -> findSlotFromList rBraceletSlots lBraceletSlots |&| maybe (Left sorryBracelet) Right
    Ring     -> M.lookup slotFromRol em |&| maybe (Right slotFromRol)
                                                  (Left . sorryReadyAlreadyWearingRing slotFromRol . (`getSing` ms))
    _        -> patternMatchFail "getDesigClothSlot" [ showText cloth ]
  where
    findSlotFromList rs ls = findAvailSlot em $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot getSlotFromList"  [ showText rol ]
    sorryRol         = Left . sorryReadyRol clothSing $ rol
    sorryEarring     = sorryReadyClothFullOneSide cloth . getSlotFromList rEarringSlots  $ lEarringSlots
    sorryBracelet    = sorryReadyClothFullOneSide cloth . getSlotFromList rBraceletSlots $ lBraceletSlots
    slotFromRol      = fromRol rol :: Slot


-- Readying weapons:


readyWpn :: Id
         -> MudState
         -> Desig
         -> Maybe RightOrLeft
         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
         -> Id
         -> Sing
         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyWpn i ms d mrol a@(et, _, _, _, _) wpnId wpnSing | em <- et ! i, wpn <- getWpn wpnId ms, sub <- wpn^.wpnSub =
    if not . isSlotAvail em $ BothHandsS
      then sorry sorryReadyAlreadyWieldingTwoHanded
      else case mrol |&| maybe (getAvailWpnSlot ms i em) (getDesigWpnSlot ms wpnSing em) of
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


getAvailWpnSlot :: MudState -> Id -> EqMap -> Either Text Slot
getAvailWpnSlot ms i em = let h@(otherHand -> oh) = getHand i ms in
    (findAvailSlot em . map getSlotForHand $ [ h, oh ]) |&| maybe (Left sorryReadyAlreadyWieldingTwoWpns) Right
  where
    getSlotForHand h = case h of RHand  -> RHandS
                                 LHand  -> LHandS
                                 NoHand -> RHandS


getDesigWpnSlot :: MudState -> Sing -> EqMap -> RightOrLeft -> Either Text Slot
getDesigWpnSlot ms wpnSing em rol
  | isRingRol rol = Left . sorryReadyWpnRol $ wpnSing
  | otherwise     = M.lookup desigSlot em |&| maybe (Right desigSlot) (Left . sorry)
  where
    desigSlot = case rol of R -> RHandS
                            L -> LHandS
                            _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]
    sorry i   = sorryReadyAlreadyWielding (getSing i ms) desigSlot


-- Readying armor:


readyArm :: Id
         -> MudState
         -> Desig
         -> Maybe RightOrLeft
         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
         -> Id
         -> Sing
         -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
readyArm i ms d mrol a@(et, _, _, _, _) armId armSing | em <- et ! i, sub <- getArmSub armId ms =
    case mrol |&| maybe (getAvailArmSlot ms sub em) sorry of
      Left  msg  -> a & _3 <>~ pure msg
      Right slot -> moveReadiedItem i a slot armId . mkReadyArmMsgs $ sub
  where
    sorry          = Left . sorryReadyRol armSing
    mkReadyArmMsgs = \case
      Head   -> putOnMsgs                     i d armSing
      Hands  -> putOnMsgs                     i d armSing
      Feet   -> putOnMsgs                     i d armSing
      Shield -> mkReadyMsgs "ready" "readies" i d armSing
      _      -> donMsgs                       i d armSing


getAvailArmSlot :: MudState -> ArmSub -> EqMap -> Either Text Slot
getAvailArmSlot ms (armSubToSlot -> slot) em = maybeSingleSlot em slot |&| maybe (Left sorry) Right
  where
    sorry | i <- em M.! slot, s <- getSing i ms = sorryReadyAlreadyWearing s


-----


remove :: ActionFun
remove p@AdviseNoArgs     = advise p ["remove"] adviceRemoveNoArgs
remove p@(AdviseOneArg a) = advise p ["remove"] . adviceRemoveNoCon $ a
remove p@(Lower' i as)    = genericAction p helper "remove"
  where
    helper _ ms =
      let LastArgIsTargetBindings { .. } = mkLastArgIsTargetBindings i ms as
      in case singleArgInvEqRm InInv targetArg of
        (InInv, target) -> shuffleRem i ms srcDesig target False otherArgs srcInvCoins procGecrMisMobInv
        (InEq,  _     ) -> genericSorry ms . sorryConInEq $ Rem
        (InRm,  target) -> if ()# rmInvCoins
          then genericSorry ms sorryNoConHere
          else shuffleRem i ms srcDesig target True otherArgs rmInvCoins procGecrMisRm
remove p = patternMatchFail "remove" [ showText p ]


shuffleRem :: Id
           -> MudState
           -> Desig
           -> ConName
           -> IsConInRm
           -> Args
           -> (InvWithCon, CoinsWithCon)
           -> ((GetEntsCoinsRes, Maybe Inv) -> Either Text Inv)
           -> GenericRes
shuffleRem i ms d conName icir as invCoinsWithCon@(invWithCon, _) f =
    let (conGecrs, conMiss, conRcs) = uncurry (resolveEntCoinNames i ms . pure $ conName) invCoinsWithCon
    in if ()# conMiss && ()!# conRcs
      then genericSorry ms sorryRemCoin
      else case f . head . zip conGecrs $ conMiss of
        Left  msg     -> genericSorry ms msg
        Right [conId] | (conSing, conType) <- (uncurry getSing *** uncurry getType) . dup $ (conId, ms) ->
            if conType /= ConType
              then genericSorry ms . sorryConHelper i ms conId $ conSing
              else let (as', guessWhat)   = stripLocPrefs
                       invCoinsInCon      = getInvCoins conId ms
                       (gecrs, miss, rcs) = uncurry (resolveEntCoinNames i ms as') invCoinsInCon
                       eiss               = zipWith (curry . procGecrMisCon $ conSing) gecrs miss
                       ecs                = map (procReconciledCoinsCon conSing) rcs
                       mnom               = mkMaybeNthOfM ms icir conId conSing invWithCon
                       (ms',  toSelfs,  bs,  logMsgs ) = foldl' (helperRemEitherInv  i d mnom conId conSing icir)
                                                                (ms, [], [], [])
                                                                eiss
                       (ms'', toSelfs', bs', logMsgs') =        helperRemEitherCoins i d mnom conId conSing icir
                                                                (ms', toSelfs, bs, logMsgs)
                                                                ecs
                   in if ()!# invCoinsInCon
                     then (ms'', (guessWhat ++ toSelfs', bs', logMsgs'))
                     else genericSorry ms . sorryRemEmpty $ conSing
        Right {} -> genericSorry ms sorryRemExcessCon
  where
    stripLocPrefs = onTrue (any hasLocPref as) g (as, [])
    g pair        = pair & _1 %~ map stripLocPref
                         & _2 .~ pure sorryRemIgnore


-----


say :: ActionFun
say p@AdviseNoArgs                    = advise p ["say"] adviceSayNoArgs
say p@(WithArgs i mq cols args@(a:_)) = getState >>= \ms -> if
  | isIncognitoId i ms         -> wrapSend mq cols . sorryIncog $ "say"
  | T.head a == adverbOpenChar -> case parseAdverb . T.unwords $ args of
    Left  msg                    -> adviseHelper msg
    Right (adverb, rest@(T.words -> rs@(head -> r)))
      | T.head r == sayToChar, T.length r > 1 -> if length rs > 1
        then sayTo (Just adverb) (T.tail rest) |&| modifyState >=> ioHelper ms
        else adviseHelper adviceSayToNoUtterance
      | otherwise -> ioHelper ms =<< simpleSayHelper ms (Just adverb) rest
  | T.head a == sayToChar, T.length a > 1 -> if length args > 1
    then sayTo Nothing (T.tail . T.unwords $ args) |&| modifyState >=> ioHelper ms
    else adviseHelper adviceSayToNoUtterance
  | otherwise -> ioHelper ms =<< simpleSayHelper ms Nothing (T.unwords args)
  where
    adviseHelper                = advise p ["say"]
    parseAdverb (T.tail -> msg) = case T.break (== adverbCloseChar) msg of
      (_,   "")            -> Left  adviceAdverbCloseChar
      ("",  _ )            -> Left  adviceEmptyAdverb
      (" ", _ )            -> Left  adviceEmptyAdverb
      (_,   x ) | x == acl -> Left  adviceSayAdverbNoUtterance
      (adverb, right)      -> Right (adverb, T.drop 2 right)
    sayTo maybeAdverb (T.words -> (target:rest@(r:_))) ms =
        let d              = mkStdDesig i ms DoCap
            invCoins       = first (i `delete`) . getMobRmNonIncogInvCoins i $ ms
        in if ()!# invCoins
          then case singleArgInvEqRm InRm target of
            (InInv, _      ) -> sorry sorrySayInInv
            (InEq,  _      ) -> sorry sorrySayInEq
            (InRm,  target') -> case uncurry (resolveRmInvCoins i ms . pure $ target') invCoins of
              (_,                    [ Left [msg] ]) -> sorry msg
              (_,                    Right  _:_    ) -> sorry sorrySayCoins
              ([ Left  msg        ], _             ) -> sorry msg
              ([ Right (_:_:_)    ], _             ) -> sorry sorrySayExcessTargets
              ([ Right [targetId] ], _             ) ->
                  let targetDesig = serialize . mkStdDesig targetId ms $ Don'tCap
                  in if isNpcPC targetId ms
                    then parseRearAdverb |&| either sorry (sayToHelper d targetId targetDesig)
                    else sorry . sorrySayTargetType . getSing targetId $ ms
              x -> patternMatchFail "say sayTo" [ showText x ]
          else sorry sorrySayNoOneHere
      where
        sorry           = (ms, ) . (, [], "") . pure
        parseRearAdverb = case maybeAdverb of
          Just adverb                          -> Right (adverb <> " ", "", formatMsg . T.unwords $ rest)
          Nothing | T.head r == adverbOpenChar -> case parseAdverb . T.unwords $ rest of
                      Right (adverb, rest') -> Right ("", " " <> adverb, formatMsg rest')
                      Left  msg             -> Left  msg
                  | otherwise -> Right ("", "", formatMsg . T.unwords $ rest)
        sayToHelper d targetId targetDesig (frontAdv, rearAdv, msg) =
            let toSelfMsg     = T.concat [ "You say ",            frontAdv, "to ", targetDesig, rearAdv, ", ", msg ]
                toTargetMsg   = T.concat [ serialize d, " says ", frontAdv, "to you",           rearAdv, ", ", msg ]
                toTargetBcast = (nl toTargetMsg, pure targetId)
                toOthersMsg   = T.concat [ serialize d, " says ", frontAdv, "to ", targetDesig, rearAdv, ", ", msg ]
                toOthersBcast = (nl toOthersMsg, desigIds d \\ [ i, targetId ])
                f             | isNpc i        ms = (, [])
                              | isNpc targetId ms = firstMobSay i
                              | otherwise         = (, [])
                (pt, hints)   = ms^.plaTbl.to f
            in (ms & plaTbl .~ pt, ( onTrue (()!# hints) (++ hints) . pure $ toSelfMsg
                                   , [ toTargetBcast, toOthersBcast ]
                                   , toSelfMsg ))
    sayTo maybeAdverb msg _ = patternMatchFail "say sayTo" [ showText maybeAdverb, msg ]
    formatMsg               = dblQuote . capitalizeMsg . punctuateMsg
    ioHelper ms triple@(x:xs, _, _) | f                     <- parseDesig i ms
                                    , (toSelfs, bs, logMsg) <- triple & _1 .~ f x : xs
                                                                      & _3 %~ f = do { multiWrapSend mq cols toSelfs
                                                                                     ; bcastIfNotIncogNl i bs
                                                                                     ; logMsg |#| logPlaOut "say" i . pure }
    ioHelper _  triple              = patternMatchFail "say ioHelper" [ showText triple ]
    simpleSayHelper ms (maybe "" (" " <>) -> adverb) (formatMsg -> msg) =
        return $ let d             = mkStdDesig i ms DoCap
                     toSelfMsg     = T.concat [ "You say", adverb, ", ", msg ]
                     toOthersMsg   = T.concat [ serialize d, " says", adverb, ", ", msg ]
                     toOthersBcast = (nl toOthersMsg, i `delete` desigIds d)
                 in (pure toSelfMsg, pure toOthersBcast, toSelfMsg)
say p = patternMatchFail "say" [ showText p ]


firstMobSay :: Id -> PlaTbl -> (PlaTbl, [Text])
firstMobSay i pt | pt^.ind i.to isNotFirstMobSay = (pt, pure "")
                 | otherwise = (pt & ind i %~ setPlaFlag IsNotFirstMobSay True, [ "", hintSay ])


-----


setAction :: ActionFun
setAction (NoArgs i mq cols) = getState >>= \ms ->
    let (styleAbbrevs Don'tQuote -> names, values) = unzip . mkSettingPairs i $ ms
    in multiWrapSend mq cols [ padSettingName (n <> ": ") <> v | n <- names | v <- values ] >> logPlaExecArgs "set" [] i
setAction (Lower' i as) = helper |&| modifyState >=> \(bs, logMsgs) ->
    bcastNl bs >> logMsgs |#| logPlaOut "set" i
  where
    helper ms = let (p, msgs, logMsgs) = foldl' (helperSettings i ms) (getPla i ms, [], []) as
                in (ms & plaTbl.ind i .~ p, (mkBcast i . T.unlines $ msgs, logMsgs))
setAction p = patternMatchFail "setAction" [ showText p ]


mkSettingPairs :: Id -> MudState -> [(Text, Text)]
mkSettingPairs i ms = let p = getPla i ms
                      in onTrue (isAdmin p) (adminPair p :) . pairs $ p
  where
    pairs p   = [ ("columns",  showText . getColumns   i  $ ms)
                , ("lines",    showText . getPageLines i  $ ms)
                , ("question", inOut    . isTunedQuestion $ p ) ]
    adminPair = ("admin", ) . inOut . isTunedAdmin


helperSettings :: Id -> MudState -> (Pla, [Text], [Text]) -> Text -> (Pla, [Text], [Text])
helperSettings _ _ a@(_, msgs, _) arg@(T.length . T.filter (== '=') -> noOfEqs)
  | or [ noOfEqs /= 1, T.head arg == '=', T.last arg == '=' ] =
      let msg    = sorryParseArg arg
          f      = any (adviceSettingsInvalid `T.isInfixOf`) msgs ?  (++ pure msg)
                                                                  :? (++ [ msg <> adviceSettingsInvalid ])
      in a & _2 %~ f
helperSettings i ms a (T.breakOn "=" -> (name, T.tail -> value)) =
    findFullNameForAbbrev name (map fst . mkSettingPairs i $ ms) |&| maybe notFound found
  where
    notFound    = appendMsg . sorrySetName $ name
    appendMsg m = a & _2 <>~ pure m
    found       = \case "admin"    -> alterTuning "admin" IsTunedAdmin
                        "columns"  -> procEither . alterNumeric minCols      maxCols      "columns" $ columns
                        "lines"    -> procEither . alterNumeric minPageLines maxPageLines "lines"   $ pageLines
                        "question" -> alterTuning "question" IsTunedQuestion
                        t          -> patternMatchFail "helperSettings found" [t]
      where
        procEither f = parseInt |&| either appendMsg f
        parseInt     = case (reads . T.unpack $ value :: [(Int, String)]) of [(x, "")] -> Right x
                                                                             _         -> sorryParse
        sorryParse   = Left . sorryParseSetting value $ name
    alterNumeric minVal maxVal settingName lens x
      | not . inRange (minVal, maxVal) $ x = appendMsg . sorrySetRange settingName minVal $ maxVal
      | otherwise = let msg = T.concat [ "Set ", settingName, " to ", showText x, "." ]
                    in appendMsg msg & _1.lens .~ x & _3 <>~ pure msg
    alterTuning n flag = case filter ((== value) . fst) inOutOnOffs of
      [(_, newBool)] -> let msg   = T.concat [ "Tuned ", inOut newBool, " the ", n, " channel." ]
                        in appendMsg msg & _1 %~ setPlaFlag flag newBool & _3 <>~ pure msg
      [] -> appendMsg . sorryParseInOut value $ n
      xs -> patternMatchFail "helperSettings alterTuning" [ showText xs ]


-----


showAction :: ActionFun
showAction p@AdviseNoArgs       = advise p ["show"] adviceShowNoArgs
showAction p@(AdviseOneArg a)   = advise p ["show"] . adviceShowNoName $ a
showAction (Lower i mq cols as) = getState >>= \ms -> if isIncognitoId i ms
  then wrapSend mq cols . sorryIncog $ "show"
  else let eqMap      = getEqMap    i ms
           invCoins   = getInvCoins i ms
           rmInvCoins = first (i `delete`) . getMobRmNonIncogInvCoins i $ ms
       in if
         | ()# eqMap && ()# invCoins -> wrapSend mq cols dudeYou'reScrewed
         | ()# rmInvCoins            -> wrapSend mq cols sorryNoOneHere
         | otherwise                 -> case singleArgInvEqRm InRm . last $ as of
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
                   in if theType theTarget `notElem` [ NpcType, PCType ]
                     then wrapSend mq cols . sorryShowTarget . theSing $ theTarget
                     else do
                         multiWrapSend mq cols . dropBlanks $ sorryRmMsg : [ parseDesig i ms msg
                                                                           | msg <- invToSelfs ++ eqToSelfs ]
                         bcastNl $ invBs ++ eqBs
                         let logMsg = slashes . dropBlanks $ [ invLogMsg |!| parensQuote "inv" <> " " <> invLogMsg
                                                             , eqLogMsg  |!| parensQuote "eq"  <> " " <> eqLogMsg ]
                         logMsg |#| logPla "show" i . (T.concat [ "showed to "
                                                                , theSing theTarget
                                                                , ": " ] <>)
                 Right _ -> wrapSend mq cols sorryShowExcessTargets
  where
    tryThisInstead = " Try showing something to someone in your current room."
    showInv ms d invCoins inInvs IdSingTypeDesig { .. }
      | ()!# invCoins =
          let (eiss, ecs)                         = uncurry (resolveMobInvCoins i ms inInvs) invCoins
              showInvHelper                       = foldl' helperEitherInv ([], [], []) eiss
              helperEitherInv acc (Left  msg    ) = acc & _1 <>~ pure msg
              helperEitherInv acc (Right itemIds) = acc & _1 <>~ mkToSelfMsgs itemIds
                                                        & _2 <>~ mkBs
                                                        & _3 <>~ pure mkLogMsg
                where
                  mkBs     = mkToTargetBs itemIds ++ mkToOthersBs itemIds
                  mkLogMsg = commas . map (`getSing` ms) $ itemIds
              mkToSelfMsgs itemIds = [ T.concat [ "You show the ", getSing itemId ms, " to ", theDesig, "." ]
                                     | itemId <- itemIds ]
              mkToTargetBs itemIds = [ ( T.concat [ serialize d
                                                  , " shows you "
                                                  , underline . aOrAn . getSing itemId $ ms
                                                  , nl ":"
                                                  , getEntDesc itemId ms ]
                                       , pure theId )
                                     | itemId <- itemIds ]
              mkToOthersBs itemIds = [ ( T.concat [ serialize d
                                                  , " shows "
                                                  , aOrAn . getSing itemId $ ms
                                                  , " to "
                                                  , theDesig
                                                  , "." ]
                                       , desigIds d \\ [ i, theId ] )
                                     | itemId <- itemIds ]
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
      | otherwise = (pure dudeYourHandsAreEmpty, [], "")
    showEq ms d eqMap inEqs IdSingTypeDesig { .. }
      | ()!# eqMap =
          let (gecrs, miss, rcs)                  = resolveEntCoinNames i ms inEqs (M.elems eqMap) mempty
              eiss                                = zipWith (curry procGecrMisMobEq) gecrs miss
              showEqHelper                        = foldl' helperEitherInv ([], [], []) eiss
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
                                                      , " "
                                                      , parensQuote . mkSlotDesc i ms . reverseLookup itemId $ eqMap
                                                      , nl ":"
                                                      , getEntDesc itemId ms ]
                                           , pure theId )
                                         | itemId <- itemIds ]
              mkToOthersBs     itemIds = [ ( T.concat [ serialize d
                                                      , " shows "
                                                      , aOrAn . getSing itemId $ ms
                                                      , " "
                                                      , parensQuote . mkSlotDesc i ms . reverseLookup itemId $ eqMap
                                                      , " to "
                                                      , theDesig
                                                      , "." ]
                                           , desigIds d \\ [ i, theId ] )
                                         | itemId <- itemIds ]
              -----
              showCoinsInEqHelper = rcs |!| sorryEquipCoins
          in let (toSelfMsgs, bs, logMsgs) = showEqHelper
             in (showCoinsInEqHelper : toSelfMsgs, bs, slashes logMsgs)
      | otherwise = (pure dudeYou'reNaked, [], "")
showAction p = patternMatchFail "showAction" [ showText p ]


mkSlotDesc :: Id -> MudState -> Slot -> Text
mkSlotDesc i ms s = case s of
  -- Clothing slots:
  EarringR1S  -> wornOn -- "right ear"
  EarringR2S  -> wornOn -- "right ear"
  EarringL1S  -> wornOn -- "left ear"
  EarringL2S  -> wornOn -- "left ear"
  NoseRing1S  -> wornOn -- "nose"
  NoseRing2S  -> wornOn -- "nose"
  Necklace1S  -> wornOn -- "neck"
  Necklace2S  -> wornOn -- "neck"
  Necklace3S  -> wornOn -- "neck"
  BraceletR1S -> wornOn -- "right wrist"
  BraceletR2S -> wornOn -- "right wrist"
  BraceletR3S -> wornOn -- "right wrist"
  BraceletL1S -> wornOn -- "left wrist"
  BraceletL2S -> wornOn -- "left wrist"
  BraceletL3S -> wornOn -- "left wrist"
  RingRIS     -> wornOn -- "right index finger"
  RingRMS     -> wornOn -- "right middle finger"
  RingRRS     -> wornOn -- "right ring finger"
  RingRPS     -> wornOn -- "right pinky finger"
  RingLIS     -> wornOn -- "left index finger"
  RingLMS     -> wornOn -- "left middle finger"
  RingLRS     -> wornOn -- "left ring finger"
  RingLPS     -> wornOn -- "left pinky finger"
  ShirtS      -> wornAs -- "shirt"
  SmockS      -> wornAs -- "smock"
  CoatS       -> wornAs -- "coat"
  TrousersS   -> "worn as trousers" -- "trousers"
  SkirtS      -> wornAs -- "skirt"
  DressS      -> wornAs -- "dress"
  FullBodyS   -> "worn about " <> hisHer <> " body" -- "about body"
  BackpackS   -> "worn on "    <> hisHer <> " back" -- "backpack"
  CloakS      -> wornAs -- "cloak"
  -- Armor slots:
  HeadS       -> wornOn -- "head"
  TorsoS      -> wornOn -- "torso"
  ArmsS       -> wornOn -- "arms"
  HandsS      -> wornOn -- "hands"
  LowerBodyS  -> wornOn -- "lower body"
  FeetS       -> wornOn -- "feet"
  -- Weapon/shield slots:
  RHandS      -> heldIn -- "right hand"
  LHandS      -> heldIn -- "left hand"
  BothHandsS  -> "wielding with both hands" -- "both hands"
  where
    hisHer = mkPossPro . getSex i $ ms
    wornOn = T.concat [ "worn on ", hisHer, " ", pp s ]
    wornAs = "worn as " <> (aOrAn . pp $ s)
    heldIn = "held in " <> hisHer <> pp s


-----


stats :: ActionFun
stats (NoArgs i mq cols) = getState >>= \ms ->
    let mkStats   = dropEmpties [ top
                                , pp . getHand i $ ms
                                , "level " <> showText l
                                , (commaEvery3 . showText $ x  ) <> " experience points"
                                , (commaEvery3 . showText $ nxt) <> " experience points to next level"
                                , mkFullDesc avail size ]
        top           = onTrue (isPC i ms) (<> sexRace) . getSing i $ ms
        sexRace       = T.concat [ ", the ", sexy, " ", r ]
        (sexy, r)     = (uncapitalize . showText *** uncapitalize . showText) . getSexRace i $ ms
        (l, x)        = getLvlExp i ms
        nxt           = subtract x . snd $ calcLvlExps !! l
        (avail, size) = calcStomachAvailSize i ms
    in multiWrapSend mq cols mkStats >> logPlaExec "stats" i
stats p = withoutArgs stats p


-----


tele :: ActionFun
tele p@AdviseNoArgs     = advise p ["telepathy"] adviceTeleNoArgs
tele p@(AdviseOneArg a) = advise p ["telepathy"] . adviceTeleNoMsg $ a
tele (MsgWithTarget i mq cols target msg) = getState >>= \ms ->
    let (s, p) = (getSing i ms, getPla i ms) in if isIncognito p
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
                       ioHelper targetId bs = let bs'@[(toSelf, _), _] = formatBs targetId bs in do
                           bcastNl . consLocPrefBcast i $ bs'
                           logPlaOut "tele" i . pure $ toSelf
                           ts <- liftIO mkTimestamp
                           withDbExHandler_ "tele" . insertDbTblTele . TeleRec ts s targetSing $ toSelf
                       formatBs targetId [toMe, toTarget] = let f n m = bracketQuote n <> " " <> m
                                                            in [ toMe     & _1 %~ f s
                                                               , toTarget & _1 %~ f (mkStyled targetId) ]
                       formatBs _        bs               = patternMatchFail "tele found formatBs" [ showText bs ]
                       mkStyled targetId = let (target'sAwakes, _) = getDblLinkedSings targetId ms
                                               styleds             = styleAbbrevs Don'tQuote target'sAwakes
                                           in head . filter ((== s) . dropANSI) $ styleds
                   in either sendFun helper . checkMutuallyTuned i ms $ targetSing
               (awakes, asleeps) = getDblLinkedSings i ms
           in findFullNameForAbbrev strippedTarget awakes |&| maybe notFound found
tele p = patternMatchFail "tele" [ showText p ]


getDblLinkedSings :: Id -> MudState -> ([Sing], [Sing])
getDblLinkedSings i ms = foldr helper ([], []) . getLinked i $ ms
  where
    helper s pair = let lens = isAwake (getIdForMobSing s ms) ms ? _1 :? _2
                    in pair & lens %~ (s :)


-----


tune :: ActionFun
tune (NoArgs i mq cols) = getState >>= \ms ->
    let linkPairs   = map (first (`getIdForMobSing` ms) . dup) . getLinked i $ ms
        linkSings   = sort . map snd . filter (isDblLinked ms . (i, ) . fst) $ linkPairs
        styleds     = styleAbbrevs Don'tQuote linkSings
        linkTunings = foldr (\s -> (linkTbl M.! s :)) [] linkSings
        linkTbl     = getTeleLinkTbl i ms
        (chanNames, chanTunings)   = mkChanNamesTunings i ms
        helper title names tunings = let txts = mkConnTxts
                                     in [ title, ()!# txts ? commas txts :? none ]
          where
            mkConnTxts = [ n <> "=" <> inOut t | n <- names | t <- tunings ]
    in do
        let msgs = [ helper "Two-way telepathic links:" styleds linkTunings
                   , pure ""
                   , helper "Telepathic channels:" (styleAbbrevs Don'tQuote chanNames) chanTunings ]
        multiWrapSend mq cols . concat $ msgs
        logPlaExecArgs "tune" [] i
tune (Lower' i as) = helper |&| modifyState >=> \(bs, logMsgs) ->
    bcastNl bs >> logMsgs |#| logPlaOut "tune" i
  where
    helper ms = let s       = getSing i ms
                    linkTbl = getTeleLinkTbl i ms
                    chans   = getPCChans     i ms
                    (linkTbl', chans', msgs, logMsgs) = foldl' (helperTune s) (linkTbl, chans, [], []) as
                in ( ms & teleLinkMstrTbl.ind i .~ linkTbl'
                        & chanTbl %~ flip (foldr (\c -> ind (c^.chanId) .~ c)) chans'
                   , (mkBcast i . T.unlines $ msgs, logMsgs) )
tune p = patternMatchFail "tune" [ showText p ]


helperTune :: Sing -> (TeleLinkTbl, [Chan], [Text], [Text]) -> Text -> (TeleLinkTbl, [Chan], [Text], [Text])
helperTune _ a arg@(T.length . T.filter (== '=') -> noOfEqs)
  | or [ noOfEqs /= 1, T.head arg == '=', T.last arg == '=' ] = a & _3 %~ tuneInvalidArg arg
helperTune s a@(linkTbl, chans, _, _) arg@(T.breakOn "=" -> (name, T.tail -> value)) = case lookup value inOutOnOffs of
  Nothing  -> a & _3 %~ tuneInvalidArg arg
  Just val -> let connNames = "all" : linkNames ++ chanNames
              in findFullNameForAbbrev name connNames |&| maybe notFound (found val)
  where
    linkNames   = map uncapitalize . M.keys $ linkTbl
    chanNames   = map (views chanName T.toLower) chans
    notFound    = a & _3 <>~ (pure . sorryTuneName $ name)
    found val n = if n == "all"
                    then appendMsg "all telepathic connections" & _1 %~ M.map (const val)
                                                                & _2 %~ map (chanConnTbl.at s ?~ val)
                    else foundHelper
      where
        appendMsg connName = let msg = T.concat [ "You tune ", connName, " ", inOut val, "." ]
                             in a & _3 <>~ pure msg
                                  & _4 <>~ pure msg
        foundHelper
          | n `elem` linkNames = foundLink
          | n `elem` chanNames = foundChan
          | otherwise          = blowUp "helperTune found foundHelper" "connection name not found" . pure $ n
          where
            foundLink = let n' = capitalize n in appendMsg n' & _1.at n' ?~ val
            foundChan =
                let ([match], others) = partition (views chanName ((== n) . T.toLower)) chans
                in appendMsg (views chanName dblQuote match) & _2 .~ (match & chanConnTbl.at s ?~ val) : others


tuneInvalidArg :: Text -> [Text] -> [Text]
tuneInvalidArg arg msgs = let msg = sorryParseArg arg in
    msgs |&| (any (adviceTuneInvalid `T.isInfixOf`) msgs ? (++ pure msg) :? (++ [ msg <> adviceTuneInvalid ]))


-----


typo :: ActionFun
typo p@AdviseNoArgs = advise p ["typo"] adviceTypoNoArgs
typo p              = bugTypoLogger p TypoLog


-----


unlink :: ActionFun
unlink p@AdviseNoArgs          = advise p ["unlink"] adviceUnlinkNoArgs
unlink (LowerNub i mq cols as) =
    let (f, guessWhat) | any hasLocPref as = (stripLocPref, sorryUnlinkIgnore)
                       | otherwise         = (id,           ""               )
        as'      = map (capitalize . T.toLower . f) as
    in do
        tingleLoc <- rndmElem [ "behind your eyes"
                              , "deep in your lower back"
                              , "in your scalp"
                              , "on the back of your neck"
                              , "somewhere between your ears" ]
        ms        <- getState
        res       <- helperLinkUnlink ms i mq cols
        flip maybeVoid res $ \(meLinkedToOthers, othersLinkedToMe, twoWays) ->
            let helper ms' = let (ms'', bs, logMsgs) = foldl' procArg (ms', [], []) as'
                             in (ms'', (bs, logMsgs))
                procArg a@(ms', _, _) targetSing = if
                  | targetSing `elem` twoWays ++ meLinkedToOthers ++ othersLinkedToMe -> procArgHelper
                  | otherwise -> sorry $ sorryUnlinkName targetSing <> " " <> hintUnlink
                  where
                    sorry msg     = a & _2 <>~ (mkBcast i . nlnl $ msg)
                    procArgHelper
                      | not . hasPp i ms' $ 3 = sorry . sorryPp $ "sever your link with " <> targetSing
                      | targetId <- getIdForMobSing targetSing ms'
                      , s        <- getSing i ms
                      , srcMsg   <- T.concat [ focusingInnateMsg, "you sever your link with ", targetSing, "." ]
                      , targetBs <- let bs       = mkBcast targetId . nlnl . colorize . unlinkMsg tingleLoc $ s
                                        colorize = colorWith unlinkColor
                                    in (isLoggedIn . getPla targetId $ ms') |?| bs
                      , ms''     <- ms' & teleLinkMstrTbl.ind i       .at targetSing .~ Nothing
                                        & teleLinkMstrTbl.ind targetId.at s          .~ Nothing
                                        & pcTbl .ind i       .linked %~ (targetSing `delete`)
                                        & pcTbl .ind targetId.linked %~ (s          `delete`)
                                        & mobTbl.ind i       .curPp  -~ 3
                      = a & _1 .~  ms''
                          & _2 <>~ (nlnl srcMsg, pure i) : targetBs
                          & _3 <>~ pure targetSing
            in helper |&| modifyState >=> \(bs, logMsgs) -> do
                bcast . onTrue (()!# guessWhat) ((guessWhat, pure i) :) $ bs
                logMsgs |#| logPla "unlink" i . slashes
unlink p = patternMatchFail "unlink" [ showText p ]


-----


unready :: ActionFun
unready p@AdviseNoArgs     = advise p ["unready"] adviceUnreadyNoArgs
unready p@(LowerNub' i as) = genericAction p helper "unready"
  where
    helper _ ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InEq as
            sorryInInv             = inInvs |!| sorryUnreadyInInv
            sorryInRm              = inRms  |!| sorryUnreadyInRm
            sorryCoins             = rcs    |!| sorryUnreadyCoins
            d                      = mkStdDesig i ms DoCap
            is                     = M.elems . getEqMap i $ ms
            (gecrs, miss, rcs)     = resolveEntCoinNames i ms inEqs is mempty
            eiss                   = zipWith (curry procGecrMisMobEq) gecrs miss
            (et, it, toSelfs, bs, logMsgs) = foldl' (helperUnready i ms d) (ms^.eqTbl, ms^.invTbl, [], [], []) eiss
        in if ()!# is
          then (ms & eqTbl .~ et & invTbl .~ it, ( dropBlanks $ [ sorryInInv, sorryInRm, sorryCoins ] ++ toSelfs
                                                 , bs
                                                 , logMsgs ))
          else genericSorry ms dudeYou'reNaked
unready p = patternMatchFail "unready" [ showText p ]


helperUnready :: Id
              -> MudState
              -> Desig
              -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
              -> Either Text Inv
              -> (EqTbl, InvTbl, [Text], [Broadcast], [Text])
helperUnready i ms d a = \case
  Left  msg       -> a & _3 <>~ pure msg
  Right targetIds -> let (bs, msgs) = mkUnreadyDescs i ms d targetIds
                     in a & _1.ind i %~ M.filter (`notElem` targetIds)
                          & _2.ind i %~ addToInv ms targetIds
                          & _3 <>~ msgs
                          & _4 <>~ bs
                          & _5 <>~ msgs


mkUnreadyDescs :: Id
               -> MudState
               -> Desig
               -> Inv
               -> ([Broadcast], [Text])
mkUnreadyDescs i ms d targetIds = unzip [ helper icb | icb <- mkIdCountBothList i ms targetIds ]
  where
    helper (targetId, count, b@(targetSing, _)) = if count == 1
      then let toSelfMsg   = T.concat [ "You ", mkVerb targetId SndPer, " the ", targetSing, "." ]
               toOthersMsg = T.concat [ serialize d, spaced . mkVerb targetId $ ThrPer, aOrAn targetSing,  "." ]
           in ((toOthersMsg, otherPCIds), toSelfMsg)
      else let toSelfMsg   = T.concat [ "You "
                                      , mkVerb targetId SndPer
                                      , spaced . showText $ count
                                      , mkPlurFromBoth b
                                      , "." ]
               toOthersMsg = T.concat [ serialize d
                                      , spaced . mkVerb targetId $ ThrPer
                                      , showText count
                                      , " "
                                      , mkPlurFromBoth b
                                      , "." ]
           in ((toOthersMsg, otherPCIds), toSelfMsg)
    mkVerb targetId person = case getType targetId ms of
      ClothType -> case getCloth targetId ms of
        Earring  -> mkVerbRemove  person
        NoseRing -> mkVerbRemove  person
        Necklace -> mkVerbTakeOff person
        Bracelet -> mkVerbTakeOff person
        Ring     -> mkVerbTakeOff person
        Backpack -> mkVerbTakeOff person
        _        -> mkVerbDoff    person
      ConType -> mkVerbTakeOff person
      WpnType | person == SndPer -> "stop wielding"
              | otherwise        -> "stops wielding"
      ArmType -> case getArmSub targetId ms of
        Head   -> mkVerbTakeOff person
        Hands  -> mkVerbTakeOff person
        Feet   -> mkVerbTakeOff person
        Shield -> mkVerbUnready person
        _      -> mkVerbDoff    person
      t -> patternMatchFail "mkUnreadyDescs mkVerb" [ showText t ]
    mkVerbRemove  = \case SndPer -> "remove"
                          ThrPer -> "removes"
    mkVerbTakeOff = \case SndPer -> "take off"
                          ThrPer -> "takes off"
    mkVerbDoff    = \case SndPer -> "doff"
                          ThrPer -> "doffs"
    mkVerbUnready = \case SndPer -> "unready"
                          ThrPer -> "unreadies"
    otherPCIds    = i `delete` desigIds d


mkIdCountBothList :: Id -> MudState -> Inv -> [(Id, Int, BothGramNos)]
mkIdCountBothList i ms targetIds =
    let boths@(mkCountList -> counts) = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
    in nubBy ((==) `on` dropFst) . zip3 targetIds counts $ boths


-----


uptime :: ActionFun
uptime (NoArgs i mq cols) = do
    wrapSend mq cols =<< uptimeHelper =<< getUptime
    logPlaExec "uptime" i
uptime p = withoutArgs uptime p


getUptime :: MudStack Int64
getUptime = ((-) `on` sec) <$> (liftIO . getTime $ Monotonic) <*> asks (view startTime)


uptimeHelper :: Int64 -> MudStack Text
uptimeHelper up = helper <$> (fmap . fmap) getSum getRecordUptime
  where
    helper         = maybe mkUptimeTxt (\recUp -> up > recUp ? mkNewRecTxt :? mkRecTxt recUp)
    mkUptimeTxt    = mkTxtHelper "."
    mkNewRecTxt    = mkTxtHelper $ " - " <> colorWith newRecordColor "it's a new record!"
    mkRecTxt recUp = mkTxtHelper $ " " <> parensQuote ("record uptime: " <> renderIt recUp) <> "."
    mkTxtHelper    = ("Up " <>) . (renderIt up <>)
    renderIt       = T.pack . renderSecs . fromIntegral


getRecordUptime :: MudStack (Maybe (Sum Int64))
getRecordUptime = mIf (liftIO . doesFileExist $ uptimeFile)
                      (liftIO readUptime `catch` (emptied . fileIOExHandler "getRecordUptime"))
                      (return Nothing)
  where
    readUptime = Just . Sum . read <$> readFile uptimeFile


-----


who :: ActionFun
who (NoArgs i mq cols) = getState >>= \ms ->
    (pager i mq . concatMap (wrapIndent namePadding cols) . mkWhoTxt i $ ms) >> logPlaExecArgs "who" [] i
who p@ActionParams { myId, args } = getState >>= \ms ->
    (dispMatches p namePadding . mkWhoTxt myId $ ms) >> logPlaExecArgs "who" args myId


mkWhoTxt :: Id -> MudState -> [Text]
mkWhoTxt i ms = let txts = mkCharList i ms
                in (++ [ mkFooter i ms ]) $ txts |!| mkWhoHeader ++ txts


mkCharList :: Id -> MudState -> [Text]
mkCharList i ms =
    let plaIds                = i `delete` getLoggedInPlaIds ms
        (linkeds,  others   ) = partition (isLinked    ms . (i, )) plaIds
        (twoWays,  oneWays  ) = partition (isDblLinked ms . (i, )) linkeds
        (tunedIns, tunedOuts) = partition (isTunedIn   ms . (i, )) twoWays
        -----
        tunedIns'         = mkSingSexRaceLvls tunedIns
        mkSingSexRaceLvls = sortBy (compare `on` view _1) . map helper
        helper plaId      = let (s, r, l) = mkPrettifiedSexRaceLvl plaId ms in (getSing plaId ms, s, r, l)
        styleds           = styleAbbrevs Don'tQuote . map (view _1) $ tunedIns'
        -----
        tunedOuts' = mkSingSexRaceLvls (tunedOuts ++ oneWays)
        -----
        others' = sortBy raceLvlSex . map (`mkPrettifiedSexRaceLvl` ms) $ others
          where
            raceLvlSex (s, r, l) (s', r', l') = (r `compare` r') <> (l `compare` l') <> (s `compare` s')
        -----
        descTunedIns = zipWith (curry descThem) styleds tunedIns'
          where
            descThem (styled, (_, s, r, l)) = T.concat [ padName styled, padSex s, padRace r, l ]
        descTunedOuts = map descThem tunedOuts'
          where
            descThem (s, s', r, l) = T.concat [ padName s, padSex s', padRace r, l ]
        descOthers = map descThem others'
          where
            descThem (s, r, l) = T.concat [ padName "?", padSex  s, padRace r, l ]
    in concat [ descTunedIns, descTunedOuts, descOthers ]


isTunedIn :: MudState -> (Id, Id) -> Bool
isTunedIn ms (i, i') | s <- getSing i' ms = fromMaybe False (view (at s) . getTeleLinkTbl i $ ms)


mkFooter :: Id -> MudState -> Text
mkFooter i ms = let plaIds@(length -> x) = getLoggedInPlaIds ms
                    y                    = length . filter (== True) $ maruBatsus
                in T.concat [ showText x
                            , " "
                            , pluralize ("person", "people") x
                            , " awake"
                            , plaIds == pure i |?| ": you"
                            , y /= 0 |?| (" " <> (parensQuote . T.concat $ [ "excluding "
                                                                          , showText y
                                                                          , " administrator"
                                                                          , pluralize ("", "s") y ]))
                            , "." ]
  where
    maruBatsus = map (uncurry (&&) . (isLoggedIn *** not . isIncognito) . dup . (`getPla` ms)) ais
    ais        = getLoggedInAdminIds ms


-----


whoAmI :: ActionFun
whoAmI (NoArgs i mq cols) = (wrapSend mq cols =<< helper =<< getState) >> logPlaExec "whoami" i
  where
    helper ms = return $ let s = getSing i ms in if isNpc i ms
      then let sexy = getSex i ms
           in T.concat [ "You are "
                       , aOrAnOnLower s
                       , sexy /= NoSex |?| (" " <> parensQuote (pp sexy))
                       , "." ]
      else let (sexy, r) = (uncapitalize . showText *** uncapitalize . showText) . getSexRace i $ ms
           in T.concat [ "You are "
                       , colorWith knownNameColor s
                       , " "
                       , parensQuote . T.concat $ [ "a ", sexy, " ", r ]
                       , "." ]
whoAmI p = withoutArgs whoAmI p
