{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MonadComprehensions, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, TransformListComp, ViewPatterns #-}

module Mud.Cmds.Pla ( getRecordUptime
                    , getUptime
                    , handleEgress
                    , look
                    , mkCmdListWithNonStdRmLinks
                    , plaCmds
                    , showMotd ) where

import Mud.ANSI
import Mud.Cmds.ExpCmds
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.ActionParams.Util
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.Data.State.Util.STM
import Mud.Logging hiding (logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import Mud.NameResolution
import Mud.TheWorld.Ids
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.FilePaths
import Mud.TopLvlDefs.Misc
import Mud.Util.ANSI
import Mud.Util.List (appendIfUnique, mkCountList)
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Token
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, at, both, over, to)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.~), (<>~), (?~), (.~), (^.))
import Control.Monad (forM, forM_, guard, mplus, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete, foldl', intercalate, intersperse, nub, nubBy, partition, sort, sortBy, unfoldr)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), mempty)
import GHC.Exts (sortWith)
import Prelude hiding (pi)
import System.Clock (Clock(..), TimeSpec(..), getTime)
import System.Console.ANSI (clearScreenCode)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Time.Utils (renderSecs)
import qualified Data.IntMap.Lazy as IM (IntMap, keys)
import qualified Data.Map.Lazy as M (elems, filter, null)
import qualified Data.Set as S (filter, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Cmds.Pla"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Pla"


-----


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Cmds.Pla"


logPla :: T.Text -> Id -> T.Text -> MudStack ()
logPla = L.logPla "Mud.Cmds.Pla"


logPlaExec :: CmdName -> Id -> MudStack ()
logPlaExec = L.logPlaExec "Mud.Cmds.Pla"


logPlaExecArgs :: CmdName -> Args -> Id -> MudStack ()
logPlaExecArgs = L.logPlaExecArgs "Mud.Cmds.Pla"


logPlaOut :: T.Text -> Id -> [T.Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Cmds.Pla"


-- ==================================================


plaCmds :: [Cmd]
plaCmds = sort $ regularCmds ++ priorityAbbrevCmds ++ expCmds


regularCmds :: [Cmd]
regularCmds = map (uncurry3 mkRegularCmd)
    [ ("?", plaDispCmdList,           "Display or search this command list.")
    , ("about",      about,           "About CurryMUD.")
    , ("admin",      admin,           "Send a message to an administrator.")
    , ("d",          go "d",          "Go down.")
    , ("e",          go "e",          "Go east.")
    , ("equip",      equip,           "Display your readied equipment, or examine one or more items in your readied \
                                      \equipment.")
    , ("expressive", expCmdList,      "Display or search a list of available expressive commands and their results.")
    , ("i",          inv,             "Display your inventory, or examine one or more items in your inventory.")
    , ("l",          look,            "Display a description of your current location, or examine one or more items in \
                                      \your current location.")
    , ("n",          go "n",          "Go north.")
    , ("ne",         go "ne",         "Go northeast.")
    , ("nw",         go "nw",         "Go northwest.")
    , ("qui",        quitCan'tAbbrev, "")
    , ("quit",       quit,            "Quit playing CurryMUD.")
    , ("remove",     remove,          "Remove one or more items from a container.")
    , ("s",          go "s",          "Go south.")
    , ("se",         go "se",         "Go southeast.")
    , ("set",        setAction,       "View or change settings.")
    , ("sw",         go "sw",         "Go southwest.")
    , ("take",       getAction,       "Pick up one or more items.")
    , ("typo",       typo,            "Report a typo.")
    , ("u",          go "u",          "Go up.")
    , ("uptime",     uptime,          "Display how long CurryMUD has been running.")
    , ("w",          go "w",          "Go west.")
    , ("what",       what,            "Disambiguate one or more abbreviations or prefixed names.")
    , ("whoadmin",   whoAdmin,        "Display a list of the administrators who are currently logged in.")
    , ("whoami",     whoAmI,          "Confirm your name, sex, and race.") ]


mkRegularCmd :: CmdFullName -> Action -> CmdDesc -> Cmd
mkRegularCmd cfn act cd = Cmd { cmdName           = cfn
                              , cmdPriorityAbbrev = Nothing
                              , cmdFullName       = cfn
                              , action            = act
                              , cmdDesc           = cd }


priorityAbbrevCmds :: [Cmd]
priorityAbbrevCmds = concatMap (uncurry4 mkPriorityAbbrevCmd)
    [ ("bug",     "b",  bug,        "Report a bug.")
    , ("clear",   "c",  clear,      "Clear the screen.")
    , ("drop",    "dr", dropAction, "Drop one or more items.")
    , ("emote",   "em", emote,      "Freely describe an action.")
    , ("exits",   "ex", exits,      "Display obvious exits.")
    , ("get",     "g",  getAction,  "Pick up one or more items.")
    , ("help",    "h",  help,       "Get help on one or more commands or topics.")
    , ("intro",   "in", intro,      "Introduce yourself.")
    , ("motd",    "m",  motd,       "Display the message of the day.")
    , ("put",     "p",  putAction,  "Put one or more items into a container.")
    , ("ready",   "r",  ready,      "Ready one or more items.")
    , ("say",     "sa", say,        "Say something out loud.")
    , ("unready", "un", unready,    "Unready one or more items.") ]


mkPriorityAbbrevCmd :: CmdFullName -> CmdPriorityAbbrevTxt -> Action -> CmdDesc -> [Cmd]
mkPriorityAbbrevCmd cfn cpat act cd = unfoldr helper (T.init cfn) ++ [ Cmd { cmdName           = cfn
                                                                           , cmdPriorityAbbrev = Just cpat
                                                                           , cmdFullName       = cfn
                                                                           , action            = act
                                                                           , cmdDesc           = cd } ]
  where
    helper ""                      = Nothing
    helper abbrev | abbrev == cpat = Just (mkExplicitAbbrevCmd, "")
                  | otherwise      = Just (mkExplicitAbbrevCmd, T.init abbrev)
      where
        mkExplicitAbbrevCmd = Cmd { cmdName           = abbrev
                                  , cmdPriorityAbbrev = Nothing
                                  , cmdFullName       = cfn
                                  , action            = act
                                  , cmdDesc           = "" }


-----


about :: Action
about (NoArgs i mq cols) = do
    logPlaExec "about" i
    try helper >>= eitherRet (\e -> fileIOExHandler "about" e >> sendGenericErrorMsg mq cols)
  where
    helper = multiWrapSend mq cols =<< [ T.lines cont | cont <- liftIO . T.readFile $ aboutFile ]
about p = withoutArgs about p


-----


admin :: Action
admin p@AdviseNoArgs = advise p ["admin"] advice
  where
    advice = T.concat [ "Please specify the name of an administrator followed by a message, as in "
                      , quoteColor
                      , dblQuote "admin jason are you available? I need your assistance"
                      , dfltColor
                      , "." ]
admin p@(AdviseOneArg a) = advise p ["admin"] advice
  where
    advice = T.concat [ "Please also provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ "admin " <> a <> " are you available? I need your assistance"
                      , dfltColor
                      , "." ]
admin (MsgWithTarget i mq cols target msg) = do
    et        <- getEntTbl
    (mqt, pt) <- getMqtPt
    let aiss = mkAdminIdsSingsList i et pt
        s    = (et ! i)^.sing
        notFound    | target `T.isInfixOf` s = wrapSend mq cols   "You can't send a message to yourself."
                    | otherwise              = wrapSend mq cols $ "No administrator by the name of " <>
                                                                  dblQuote target                    <>
                                                                  " is currently logged in."
        found match | (ai, target') <- head . filter ((== match) . snd) $ aiss
                    , amq           <- mqt ! ai
                    , aCols         <- (pt ! ai)^.columns = do
                       logNotice "admin"    . T.concat $ [ s, " sent message to ",   target', ": ", dblQuote msg ]
                       logPla    "admin" i  . T.concat $ [     "sent message to "  , target', ": ", dblQuote msg ]
                       logPla    "admin" ai . T.concat $ [ "received message from ", s,       ": ", dblQuote msg ]
                       wrapSend mq  cols    . T.concat $ [ "You send ",              target', ": ", dblQuote msg ]
                       wrapSend amq aCols   . T.concat $ [ bracketQuote s, " ", adminMsgColor, msg, dfltColor    ]
    maybe notFound found . findFullNameForAbbrev target . map snd $ aiss
admin p = patternMatchFail "admin" [ showText p ]


mkAdminIdsSingsList :: Id -> IM.IntMap Ent -> IM.IntMap Pla -> [(Id, Sing)]
mkAdminIdsSingsList i et pt = [ (pi, s) | pi <- IM.keys pt
                                        , getPlaFlag IsAdmin (pt ! pi)
                                        , pi /= i
                                        , let s = (et ! pi)^.sing
                                        , then sortWith by s ]


-----


bug :: Action
bug p@AdviseNoArgs = advise p ["bug"] advice
  where
    advice = T.concat [ "Please describe the bug you've found, as in "
                      , quoteColor
                      , dblQuote "bug I've fallen and I can't get up!"
                      , dfltColor
                      , "." ]
bug p = bugTypoLogger p BugLog


-----


clear :: Action
clear (NoArgs' i mq) = logPlaExec "clear" i >> (send mq . T.pack $ clearScreenCode)
clear p              = withoutArgs clear p


-----


dropAction :: Action
dropAction p@AdviseNoArgs = advise p ["drop"] advice
  where
    advice = T.concat [ "Please specify one or more items to drop, as in "
                      , quoteColor
                      , dblQuote "drop sword"
                      , dfltColor
                      , "." ]
dropAction (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "drop" i $ logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, ri, is, c) = mkDropReadyBindings i ws
        in (is, c) |*|
          ( let (eiss, ecs)           = resolvePCInvCoins i ws as is c
                (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Drop i ri) (ws,  [], []     ) eiss
                (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Drop i ri) (ws', bs, logMsgs) ecs
            in putTMVar t ws'' >> return (bs', logMsgs')
          ,    putTMVar t ws   >> return (mkBroadcast i dudeYourHandsAreEmpty, []) )
dropAction p = patternMatchFail "dropAction" [ showText p ]


-----


emote :: Action
emote p@AdviseNoArgs = advise p ["emote"] advice
  where
    advice = T.concat [ "Please provide a description of an action, as in "
                      , quoteColor
                      , dblQuote "emote laughs with relief as tears roll down her face"
                      , dfltColor
                      , "." ]
emote p@(ActionParams { plaId, args })
  | any (`elem` args) [ enc, enc <> "'s" ] = readWSTMVar >>= \ws ->
      let (d, s, _, _, _) = mkCapStdDesig plaId ws
          toSelfMsg       = bracketQuote . T.replace enc s . formatMsgArgs $ args
          toSelfBrdcst    = (nlnl toSelfMsg, [plaId])
          toOthersMsg | c == emoteNameChar = T.concat [ serialize d, T.tail h, " ", T.unwords . tail $ args ]
                      | otherwise          = capitalizeMsg . T.unwords $ args
          toOthersMsg'    = T.replace enc (serialize d { isCap = False }) . punctuateMsg $ toOthersMsg
          toOthersBrdcst  = (nlnl toOthersMsg', plaId `delete` pcIds d)
      in logPlaOut "emote" plaId [toSelfMsg] >> bcast [ toSelfBrdcst, toOthersBrdcst ]
  | any (enc `T.isInfixOf`) args = advise p ["emote"] advice
  | otherwise = readWSTMVar >>= \ws ->
    let (d, s, _, _, _) = mkCapStdDesig plaId ws
        msg             = punctuateMsg . T.unwords $ args
        toSelfMsg       = bracketQuote $ s <> " " <> msg
        toSelfBrdcst    = (nlnl toSelfMsg, [plaId])
        toOthersMsg     = serialize d <> " " <> msg
        toOthersBrdcst  = (nlnl toOthersMsg, plaId `delete` pcIds d)
    in logPlaOut "emote" plaId [toSelfMsg] >> bcast [ toSelfBrdcst, toOthersBrdcst ]
  where
    h@(T.head -> c) = head args
    enc             = T.singleton emoteNameChar
    advice          = T.concat [ dblQuote enc
                               , " must either be used alone, or with a "
                               , dblQuote "'s"
                               , " suffix (to create a possessive noun), as in "
                               , quoteColor
                               , dblQuote $ "emote shielding her eyes from the sun, " <> enc <> " looks out across the \
                                            \plains"
                               , dfltColor
                               , ", or "
                               , quoteColor
                               , dblQuote $ "emote " <> enc <> "'s leg twitches involuntarily as she laughs with gusto"
                               , dfltColor
                               , "." ]


-----


equip :: Action
equip (NoArgs i mq cols) = send mq . nl =<< [ mkEqDesc i cols ws i e PCType | (ws, e) <- getEnt' i ]
equip (LowerNub i mq cols as) = do
    (ws, em@(M.elems -> is)) <- getEq' i
    send mq $ if not . M.null $ em
      then let (gecrs, miss, rcs)           = resolveEntCoinNames i ws as is mempty
               eiss                         = zipWith (curry procGecrMisPCEq) gecrs miss
               invDesc                      = foldl' (helperEitherInv ws) "" eiss
               coinsDesc | not . null $ rcs = wrapUnlinesNl cols "You don't have any coins among your readied \
                                                                 \equipment."
                         | otherwise        = ""
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYou'reNaked
  where
    helperEitherInv _  acc (Left  msg) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ws acc (Right is ) = nl $ acc <> mkEntDescs i cols ws is
equip p = patternMatchFail "equip" [ showText p ]


-----


exits :: Action
exits (NoArgs i mq cols) = logPlaExec "exits" i >> (send mq . nl =<< [ mkExitsSummary cols r | r <- getPCRm i ])
exits p                  = withoutArgs exits p


-----


expCmdList :: Action
expCmdList (NoArgs i mq cols) = pager i mq . concatMap (wrapIndent (succ maxCmdLen) cols) $ mkExpCmdListTxt
expCmdList p                  = dispMatches p (succ maxCmdLen) mkExpCmdListTxt


mkExpCmdListTxt :: [T.Text]
mkExpCmdListTxt =
    let cmdNames       = [ cmdName cmd | cmd <- plaCmds ]
        styledCmdNames = styleAbbrevs Don'tBracket cmdNames
    in concatMap mkExpCmdTxt [ (styled, head matches) | (cn, styled) <- zip cmdNames styledCmdNames
                                                      , let matches = findMatches cn
                                                      , length matches == 1 ]
  where
    findMatches cn = S.toList . S.filter (\(ExpCmd ecn _) -> ecn == cn) $ expCmdSet
    mkExpCmdTxt (styled, ExpCmd ecn act) = case act of
      (NoTarget  toSelf _   ) -> [ paddedName <> mkInitialTxt ecn <> toSelf ]
      (HasTarget toSelf _ _ ) -> [ paddedName <> mkInitialTxt (ecn <> " hanako") <> T.replace "@" "Hanako" toSelf ]
      (Versatile toSelf _ toSelfWithTarget _ _) -> [ paddedName <> mkInitialTxt ecn <> toSelf
                                                   , T.replicate (succ maxCmdLen) (T.singleton indentFiller) <>
                                                     mkInitialTxt (ecn <> " hanako")                         <>
                                                     T.replace "@" "Hanako" toSelfWithTarget ]
      where
        paddedName         = pad (succ maxCmdLen) styled
        mkInitialTxt input = T.concat [ quoteColor
                                      , dblQuote input
                                      , dfltColor
                                      , " "
                                      , arrowColor
                                      , "->"
                                      , dfltColor
                                      , " " ]


-----


getAction :: Action
getAction p@AdviseNoArgs = advise p ["get"] advice
  where
    advice = T.concat [ "Please specify one or more items to pick up, as in "
                      , quoteColor
                      , dblQuote "get sword"
                      , dfltColor
                      , "." ]
getAction (Lower _ mq cols as) | length as >= 3, (head . tail .reverse $ as) == "from" =
    wrapSend mq cols . T.concat $ [ hintANSI
                                  , "Hint:"
                                  , noHintANSI
                                  , " it appears that you want to remove an object from a container. In that case, \
                                    \please use the "
                                  , dblQuote "remove"
                                  , " command. For example, to remove a ring from your sack, type "
                                  , quoteColor
                                  , dblQuote "remove ring sack"
                                  , dfltColor
                                  , "." ]
getAction (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "get" i $ logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, ri, _, ris, rc) = mkGetLookBindings i ws
        in (ris, rc) |*|
          ( let (eiss, ecs)           = resolveRmInvCoins i ws as ris rc
                (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Get ri i) (ws,  [], []     ) eiss
                (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Get ri i) (ws', bs, logMsgs) ecs
            in putTMVar t ws'' >> return (bs', logMsgs')
          ,    putTMVar t ws   >> return (mkBroadcast i "You don't see anything here to pick up.", []) )
getAction p = patternMatchFail "getAction" [ showText p ]


-----


go :: T.Text -> Action
go dir p@(ActionParams { args = [], .. }) = goDispatcher p { args = [dir] }
go dir p@(ActionParams { args,      .. }) = goDispatcher p { args = dir : args }


goDispatcher :: Action
goDispatcher (ActionParams { args = [], .. }) = return ()
goDispatcher (Lower i mq cols as)             = mapM_ (tryMove i mq cols) as
goDispatcher p                                = patternMatchFail "goDispatcher" [ showText p ]


tryMove :: Id -> MsgQueue -> Cols -> T.Text -> MudStack ()
tryMove i mq cols dir = helper >>= \case
  Left  msg          -> wrapSend mq cols msg
  Right (logMsg, bs) -> bcast bs >> logPla "tryMove" i logMsg >> look ActionParams { plaId       = i
                                                                                   , plaMsgQueue = mq
                                                                                   , plaCols     = cols
                                                                                   , args        = [] }
  where
    helper = onWS $ \(t, ws) ->
        let (originD, s, p, originRi, (i `delete`) -> originIs) = mkCapStdDesig i ws
            originRm                                            = (ws^.rmTbl) ! originRi
        in case findExit originRm dir of
          Nothing -> putTMVar t ws >> (return . Left $ sorry)
          Just (linkTxt, destRi, mom, mdm)
            | p'          <- p & rmId .~ destRi
            , destRm      <- (ws^.rmTbl)  ! destRi
            , destIs      <- (ws^.invTbl) ! destRi
            , destIs'     <- sortInv ws $ destIs ++ [i]
            , originPis   <- i `delete` pcIds originD
            , destPis     <- findPCIds ws destIs
            , msgAtOrigin <- nlnl $ case mom of
                               Nothing -> T.concat [ serialize originD, " ", verb, " ", expandLinkName dir, "." ]
                               Just f  -> f . serialize $ originD
            , msgAtDest   <- let destD = mkSerializedNonStdDesig i ws s A
                             in nlnl $ case mdm of
                               Nothing -> T.concat [ destD, " arrives from ", expandOppLinkName dir, "." ]
                               Just f  -> f destD
            , logMsg      <- T.concat [ "moved "
                                      , linkTxt
                                      , " from room "
                                      , showRm originRi originRm
                                      , " to room "
                                      , showRm destRi   destRm
                                      , "." ]
            -> do
                putTMVar t (ws & pcTbl.at  i        ?~ p'
                               & invTbl.at originRi ?~ originIs
                               & invTbl.at destRi   ?~ destIs')
                return . Right $ (logMsg, [ (msgAtOrigin, originPis), (msgAtDest, destPis) ])
    sorry | dir `elem` stdLinkNames = "You can't go that way."
          | otherwise               = dblQuote dir <> " is not a valid exit."
    verb
      | dir == "u"              = "goes"
      | dir == "d"              = "heads"
      | dir `elem` stdLinkNames = "leaves"
      | otherwise               = "enters"
    showRm (showText -> ri) (views rmName parensQuote -> rn) = ri <> " " <> rn


findExit :: Rm -> LinkName -> Maybe (T.Text, Id, Maybe (T.Text -> T.Text), Maybe (T.Text -> T.Text))
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


expandLinkName :: T.Text -> T.Text
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


expandOppLinkName :: T.Text -> T.Text
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


-----


help :: Action
help (NoArgs i mq cols) = (try . liftIO . T.readFile $ helpDir ++ "root") >>= either handler helper
  where
    handler e = do
        fileIOExHandler "help" e
        wrapSend mq cols "Unfortunately, the root help file could not be retrieved."
    helper rootHelpTxt = mkHelpData i >>= \(sortBy (compare `on` helpName) -> hs) ->
        let styleds                = zip (styleAbbrevs Don'tBracket [ helpName h | h <- hs ]) hs
            (cmdNames, topicNames) = over both (formatHelpNames . mkHelpNames) . partition (isCmdHelp . snd) $ styleds
            helpTxt                = T.concat [ nl rootHelpTxt
                                              , nl "Help is available on the following commands:"
                                              , nl cmdNames
                                              , nl "Help is available on the following topics:"
                                              , topicNames
                                              , footnote hs ]
        in logPla "help" i "read root help file." >> (pager i mq . parseHelpTxt cols $ helpTxt)
    mkHelpNames styleds   = [ pad padding . (styled <>) $ isAdminHelp h |?| asterisk | (styled, h) <- styleds ]
    padding               = maxHelpTopicLen + 2
    asterisk              = asteriskColor <> "*" <> dfltColor
    formatHelpNames names = let wordsPerLine = cols `div` padding
                            in T.unlines . map T.concat . chunksOf wordsPerLine $ names
    footnote hs           = any isAdminHelp hs |?| nlPrefix $ asterisk <> " indicates help that is available only to \
                                                                          \administrators."
help (LowerNub i mq cols as) = mkHelpData i >>= \hs -> do
    (map (parseHelpTxt cols) -> helpTxts, dropBlanks -> hns) <- unzip <$> forM as (getHelpByName cols hs)
    unless (null hns) . logPla "help" i . ("read help on: " <>) . T.intercalate ", " $ hns
    pager i mq . intercalate [ "", mkDividerTxt cols, "" ] $ helpTxts
help p = patternMatchFail "help" [ showText p ]


mkHelpData :: Id -> MudStack [Help]
mkHelpData i = getPlaIsAdmin i >>= \ia -> do
    [ plaHelpCmdNames, plaHelpTopicNames, adminHelpCmdNames, adminHelpTopicNames ] <- mapM getHelpDirectoryContents helpDirs
    let phcs = [ Help (T.pack                  phcn) (plaHelpCmdsDir     ++ phcn) True  False | phcn <- plaHelpCmdNames     ]
        phts = [ Help (T.pack                  phtn) (plaHelpTopicsDir   ++ phtn) False False | phtn <- plaHelpTopicNames   ]
        ahcs = [ Help (T.pack $ adminCmdChar : whcn) (adminHelpCmdsDir   ++ whcn) True  True  | whcn <- adminHelpCmdNames   ]
        ahts = [ Help (T.pack                  whtn) (adminHelpTopicsDir ++ whtn) False True  | whtn <- adminHelpTopicNames ]
    return $ phcs ++ phts ++ (guard ia >> ahcs ++ ahts)
  where
    helpDirs                     = [ plaHelpCmdsDir, plaHelpTopicsDir, adminHelpCmdsDir, adminHelpTopicsDir ]
    getHelpDirectoryContents dir = delete ".DS_Store" . drop 2 . sort <$> (liftIO . getDirectoryContents $ dir)


parseHelpTxt :: Cols -> T.Text -> [T.Text]
parseHelpTxt cols = concat . wrapLines cols . T.lines . parseTokens


getHelpByName :: Cols -> [Help] -> HelpName -> MudStack (T.Text, T.Text)
getHelpByName cols hs name =
    maybe sorry found . findFullNameForAbbrev name $ [ helpName h | h <- hs ]
  where
    sorry           = return ("No help is available on " <> dblQuote name <> ".", "")
    found hn        = let h = head . filter ((== hn) . helpName) $ hs
                      in (,) <$> readHelpFile (hn, helpFilePath h) <*> (return . dblQuote $ hn)
    readHelpFile (hn, hf) = (try . liftIO . T.readFile $ hf) >>= eitherRet handler
      where
        handler e = do
            fileIOExHandler "getHelpByName readHelpFile" e
            return . wrapUnlines cols $ "Unfortunately, the " <> dblQuote hn <> " help file could not be retrieved."


-----


intro :: Action
intro (NoArgs i mq cols) = getPCIntroduced i >>= \intros ->
    if null intros
      then let introsTxt = "No one has introduced themselves to you yet." in do
          wrapSend mq cols introsTxt
          logPlaOut "intro" i [introsTxt]
      else let introsTxt = T.intercalate ", " intros in do
          multiWrapSend mq cols [ "You know the following names:", introsTxt ]
          logPlaOut "intro" i [introsTxt]
intro (LowerNub' i as) = helper >>= \(cbs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "intro" i $ logMsgs
    bcast . map fromClassifiedBroadcast . sort $ cbs
  where
    helper = onWS $ \(t, ws) ->
        let (view sing -> s)         = (ws^.entTbl)   ! i
            (view rmId -> ri)        = (ws^.pcTbl)    ! i
            is@((i `delete`) -> is') = (ws^.invTbl)   ! ri
            c                        = (ws^.coinsTbl) ! ri
        in (is', c) |*|
          ( let (eiss, ecs)           = resolveRmInvCoins i ws as is' c
                (ws', cbs,  logMsgs ) = foldl' (helperIntroEitherInv s is) (ws, [],  []     ) eiss
                (     cbs', logMsgs') = foldl' helperIntroEitherCoins      (    cbs, logMsgs) ecs
            in putTMVar t ws' >> return (cbs', logMsgs')
          ,    putTMVar t ws  >> return ( mkNTBroadcast i . nlnl $ "You don't see anyone here to introduce yourself to."
                                        , [] ) )
    helperIntroEitherInv _ _   a (Left msg) | T.null msg = a
                                            | otherwise  = a & _2 <>~ (mkNTBroadcast i . nlnl $ msg)
    helperIntroEitherInv s ris a (Right is) = foldl' tryIntro a is
      where
        tryIntro a'@(ws, _, _) targetId | targetType                <- (ws^.typeTbl) ! targetId
                                        , (view sing -> targetSing) <- (ws^.entTbl)  ! targetId = case targetType of
          PCType | targetPC@(view introduced -> intros)  <- (ws^.pcTbl)  ! targetId
                 , pis                                   <- findPCIds ws ris
                 , targetDesig                           <- serialize . mkStdDesig targetId ws targetSing False $ ris
                 , (views sex mkReflexPro -> himHerself) <- (ws^.mobTbl) ! i
                 -> if s `elem` intros
                   then let msg = nlnl $ "You've already introduced yourself to " <> targetDesig <> "."
                        in a' & _2 <>~ mkNTBroadcast i msg
                   else let p         = targetPC & introduced .~ sort (s : intros)
                            ws'       = ws & pcTbl.at targetId ?~ p
                            msg       = "You introduce yourself to " <> targetDesig <> "."
                            logMsg    = parsePCDesig i ws msg
                            srcMsg    = nlnl msg
                            srcDesig  = StdDesig { stdPCEntSing = Nothing
                                                 , isCap        = True
                                                 , pcEntName    = mkUnknownPCEntName i ws
                                                 , pcId         = i
                                                 , pcIds        = pis }
                            targetMsg = nlnl . T.concat $ [ serialize srcDesig
                                                          , " introduces "
                                                          , himHerself
                                                          , " to you as "
                                                          , knownNameColor
                                                          , s
                                                          , dfltColor
                                                          , "." ]
                            othersMsg = nlnl . T.concat $ [ serialize srcDesig { stdPCEntSing = Just s }
                                                          , " introduces "
                                                          , himHerself
                                                          , " to "
                                                          , targetDesig
                                                          , "." ]
                            cbs = [ NonTargetBroadcast (srcMsg,    [i])
                                  , TargetBroadcast    (targetMsg, [targetId])
                                  , NonTargetBroadcast (othersMsg, pis \\ [ i, targetId ]) ]
                        in a' & _1 .~ ws' & _2 <>~ cbs & _3 <>~ [logMsg]
          _      | msg <- "You can't introduce yourself to " <> aOrAnOnLower targetSing <> "."
                 , b   <- NonTargetBroadcast (nlnl msg, [i]) -> over _2 (`appendIfUnique` b) a'
    helperIntroEitherCoins a (Left  msgs) =
        a & _1 <>~ concat [ mkNTBroadcast i . nlnl $ msg | msg <- msgs ]
    helperIntroEitherCoins a (Right _   ) =
        over _1 (`appendIfUnique` NonTargetBroadcast (nlnl "You can't introduce yourself to a coin.", [i])) a
    fromClassifiedBroadcast (TargetBroadcast    b) = b
    fromClassifiedBroadcast (NonTargetBroadcast b) = b
intro p = patternMatchFail "intro" [ showText p ]


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (NoArgs i mq cols) = send mq . nl =<< [ mkInvCoinsDesc i cols ws i e | (ws, e) <- getEnt' i ]
inv (LowerNub i mq cols as) = getInvCoins' i >>= \(ws, (is, c)) ->
    send mq $ (is, c) |*| ( let (eiss, ecs) = resolvePCInvCoins i ws as is c
                                invDesc     = foldl' (helperEitherInv ws) "" eiss
                                coinsDesc   = foldl' helperEitherCoins    "" ecs
                            in invDesc <> coinsDesc
                          , wrapUnlinesNl cols dudeYourHandsAreEmpty )
  where
    helperEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
inv p = patternMatchFail "inv" [ showText p ]


-----


look :: Action
look (NoArgs i mq cols) = send mq . nl . uncurry (<>) =<<
    [ ( multiWrap cols [ T.concat [ underlineANSI, " ", r^.rmName, " ", noUnderlineANSI ], r^.rmDesc ]
      , mkExitsSummary cols r <> mkRmInvCoinsDesc i cols ws ri )
    | (ws, (ri, r)) <- getPCRmIdRm' i ]
look (LowerNub i mq cols as) = helper >>= firstLook i cols >>= \case
  (Left  msg, _           ) -> send mq msg
  (Right msg, Nothing     ) -> send mq msg
  (Right msg, Just (d, ds)) ->
      let pis = i `delete` pcIds d
          d'  = serialize d
          f targetDesig acc | targetId <- pcId targetDesig =
              (nlnl $ d' <> " looks at you.", [targetId]) :
              (nlnl . T.concat $ [ d', " looks at ", serialize targetDesig, "." ], targetId `delete` pis) :
              acc
      in do
          forM_ [ fromJust . stdPCEntSing $ targetDesig | targetDesig <- ds ] $ \es ->
              logPla "look" i ("looked at " <> es <> ".")
          send mq msg
          bcast . foldr f [] $ ds
  where
    helper = onWS $ \(t, ws) ->
        let (d, _, ris, ris', rc) = mkGetLookBindings i ws
        in (ris', rc) |*|
          ( let (eiss, ecs) = resolveRmInvCoins i ws as ris' rc
                invDesc     = foldl' (helperLookEitherInv ws) "" eiss
                coinsDesc   = foldl' helperLookEitherCoins    "" ecs
                ds          = [ mkStdDesig pi ws s False ris | pi <- extractPCIdsFromEiss ws eiss
                              , let (view sing -> s) = (ws^.entTbl) ! pi ]
            in putTMVar t ws >> return (Right $ invDesc <> coinsDesc, Just (d, ds))
          ,    putTMVar t ws >> return ( Left . wrapUnlinesNl cols $ "You don't see anything here to look at."
                                       , Nothing ) )
    helperLookEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperLookEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
look p = patternMatchFail "look" [ showText p ]


firstLook :: Id
          -> Cols
          -> (Either T.Text T.Text, Maybe (PCDesig, [PCDesig]))
          -> MudStack (Either T.Text T.Text, Maybe (PCDesig, [PCDesig]))
firstLook i cols a = getPlaFlag IsNotFirstLook <$> getPla i >>= \infl -> if infl
  then return a
  else let msg = T.concat [ hintANSI
                          , "Hint:"
                          , noHintANSI
                          , " use the "
                          , dblQuote "l"
                          , " command to examine one or more items in your current location. To examine items in \
                            \your inventory, use the "
                          , dblQuote "i"
                          , " command "
                          , parensQuote $ "for example: " <> quoteColor <> dblQuote "i bread" <> dfltColor
                          , ". To examine items in your readied equipment, use the "
                          , dblQuote "equip"
                          , " command "
                          , parensQuote $ "for example: " <> quoteColor <> dblQuote "equip sword" <> dfltColor
                          , ". "
                          , quoteColor
                          , dblQuote "i"
                          , dfltColor
                          , " and "
                          , quoteColor
                          , dblQuote "equip"
                          , dfltColor
                          , " alone will list the items in your inventory and readied equipment, respectively." ]
       in do
           void . modifyPlaFlag i IsNotFirstLook $ True
           return . over _1 (appendToEither . wrapUnlinesNl cols $ msg) $ a
  where
    appendToEither msg (Left sorryMsg) = Left $ sorryMsg <> msg
    appendToEither msg right           = (<> msg) <$> right


mkRmInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> T.Text
mkRmInvCoinsDesc i cols ws ri | ((i `delete`) -> ris) <- (ws^.invTbl) ! ri
                              , (pcNcbs, otherNcbs)   <- splitPCsOthers . mkIsPC_StyledName_Count_BothList i ws $ ris
                              , pcDescs    <- T.unlines . concatMap (wrapIndent 2 cols . mkPCDesc   ) $ pcNcbs
                              , otherDescs <- T.unlines . concatMap (wrapIndent 2 cols . mkOtherDesc) $ otherNcbs
                              , c          <- (ws^.coinsTbl) ! ri
                              = (pcNcbs |!| pcDescs) <> (otherNcbs |!| otherDescs) <> (c |!| mkCoinsSummary cols c)
  where
    splitPCsOthers                       = over both (map snd) . span fst
    mkPCDesc    (en, c, (s, _)) | c == 1 = (<> " " <> en) $ if isKnownPCSing s
                                             then knownNameColor   <> s       <> dfltColor
                                             else unknownNameColor <> aOrAn s <> dfltColor
    mkPCDesc    (en, c, b     )          = T.concat [ unknownNameColor
                                                    , showText c
                                                    , " "
                                                    , mkPlurFromBoth b
                                                    , dfltColor
                                                    , " "
                                                    , en ]
    mkOtherDesc (en, c, (s, _)) | c == 1 = aOrAnOnLower s <> " " <> en
    mkOtherDesc (en, c, b     )          = T.concat [ showText c, " ", mkPlurFromBoth b, " ", en ]


mkIsPC_StyledName_Count_BothList :: Id -> WorldState -> Inv -> [(Bool, (T.Text, Int, BothGramNos))]
mkIsPC_StyledName_Count_BothList i ws is | ips   <-                        [ (ws^.typeTbl) ! i' == PCType | i' <- is ]
                                         , ens   <- styleAbbrevs DoBracket [ getEffName        i ws i'    | i' <- is ]
                                         , ebgns <-                        [ getEffBothGramNos i ws i'    | i' <- is ]
                                         , cs    <- mkCountList ebgns = nub . zip ips . zip3 ens cs $ ebgns


isKnownPCSing :: Sing -> Bool
isKnownPCSing (T.words -> ss) = case ss of [ "male",   _ ] -> False
                                           [ "female", _ ] -> False
                                           _               -> True


extractPCIdsFromEiss :: WorldState -> [Either T.Text Inv] -> [Id]
extractPCIdsFromEiss ws = foldl' helper []
  where
    helper acc (Left  _ )  = acc
    helper acc (Right is)  = acc ++ findPCIds ws is


-----


motd :: Action
motd (NoArgs i mq cols) = logPlaExec "motd" i >> showMotd mq cols
motd p                  = withoutArgs motd p


showMotd :: MsgQueue -> Cols -> MudStack ()
showMotd mq cols = send mq =<< helper
  where
    helper    = (try . liftIO $ readMotd) >>= eitherRet handler
    readMotd  = [ frame cols . multiWrap cols . T.lines . colorizeFileTxt motdColor $ cont
                | cont <- T.readFile motdFile ]
    handler e = do
        fileIOExHandler "showMotd" e
        return . wrapUnlinesNl cols $ "Unfortunately, the message of the day could not be retrieved."


-----


plaDispCmdList :: Action
plaDispCmdList p@(LowerNub' i as) = logPlaExecArgs "?" as i >> dispCmdList plaCmds p
plaDispCmdList p                  = patternMatchFail "plaDispCmdList" [ showText p ]


-----


putAction :: Action
putAction p@AdviseNoArgs = advise p ["put"] advice
  where
    advice = T.concat [ "Please specify one or more items you want to put followed by where you want to put them, as \
                        \in "
                      , quoteColor
                      , dblQuote "put doll sack"
                      , dfltColor
                      , "." ]
putAction p@(AdviseOneArg a) = advise p ["put"] advice
  where
    advice = T.concat [ "Please also specify where you want to put it, as in "
                      , quoteColor
                      , dblQuote $ "put " <> a <> " sack"
                      , dfltColor
                      , "." ]
putAction (Lower' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "put" i $ logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let (d, ris, rc, pis, pc, cn, argsWithoutCon) = mkPutRemBindings i ws as
      in (pis, pc) |*|
        ( if T.head cn == rmChar && cn /= T.singleton rmChar
          then if not . null $ ris
            then shufflePut i (t, ws) d (T.tail cn) True argsWithoutCon ris rc pis pc procGecrMisRm
            else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
          else shufflePut i (t, ws) d cn False argsWithoutCon pis pc pis pc procGecrMisPCInv
        , putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, []) )
putAction p = patternMatchFail "putAction" [ showText p ]


type CoinsWithCon = Coins
type PCInv        = Inv
type PCCoins      = Coins


shufflePut :: Id
           -> (TMVar WorldState, WorldState)
           -> PCDesig
           -> ConName
           -> IsConInRm
           -> Args
           -> InvWithCon
           -> CoinsWithCon
           -> PCInv
           -> PCCoins
           -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv)
           -> STM ([Broadcast], [T.Text])
shufflePut i (t, ws) d cn icir as is c pis pc f | (conGecrs, conMiss, conRcs) <- resolveEntCoinNames i ws [cn] is c =
    if null conMiss && (not . null $ conRcs)
      then putTMVar t ws >> return (mkBroadcast i "You can't put something inside a coin.", [])
      else case f . head . zip conGecrs $ conMiss of
        Left  (mkBroadcast i -> bc) -> putTMVar t ws >> return (bc, [])
        Right [ci] | e <- (ws^.entTbl) ! ci, typ <- (ws^.typeTbl) ! ci -> if typ /= ConType
          then putTMVar t ws >> return (mkBroadcast i $ theOnLowerCap (e^.sing) <> " isn't a container.", [])
          else let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as pis pc
                   eiss                  = zipWith (curry procGecrMisPCInv) gecrs miss
                   ecs                   = map procReconciledCoinsPCInv rcs
                   mnom                  = mkMaybeNthOfM icir ws ci e is
                   (ws',  bs,  logMsgs ) = foldl' (helperPutRemEitherInv   i d Put mnom i ci e) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Put mnom i ci e) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
        Right _ -> putTMVar t ws   >> return (mkBroadcast i "You can only put things into one container at a time.", [])


-----


quit :: Action
quit (NoArgs' i mq)                        = (liftIO . atomically . writeTQueue mq $ Quit) >> logPlaExec "quit" i
quit ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols msg
  where
    msg = "Type " <> dblQuote "quit" <> " with no arguments to quit the game."


handleEgress :: Id -> MudStack ()
handleEgress i = getPCRmId i >>= \ri -> do
    unless (ri == iWelcome) . notifyEgress $ i
    (wsTMVar, mqtTMVar, ptTMVar) <- (,,) <$> getWSTMVar        <*> getNWSRec msgQueueTblTMVar <*> getNWSRec plaTblTMVar
    let takeTMVars =                (,,) <$> takeTMVar wsTMVar <*> takeTMVar mqtTMVar         <*> takeTMVar ptTMVar
    (s, bs, logMsgs, pt)         <- liftIO . atomically $ takeTMVars >>= \(ws, mqt, pt) ->
        let (view rmId -> ri')    = (ws^.pcTbl)  ! i
            ((i `delete`) -> ris) = (ws^.invTbl) ! ri'
            (view sing -> s)      = (ws^.entTbl) ! i
            (pt', bs, logMsgs)    = peepHelper pt s
            ws'                   = ws  & typeTbl.at  i   .~ Nothing
                                        & entTbl.at   i   .~ Nothing
                                        & invTbl.at   i   .~ Nothing
                                        & coinsTbl.at i   .~ Nothing
                                        & eqTbl.at    i   .~ Nothing
                                        & mobTbl.at   i   .~ Nothing
                                        & pcTbl.at    i   .~ Nothing
                                        & invTbl.at   ri' ?~ ris
            mqt'                  = mqt & at i .~ Nothing
            pt''                  = pt' & at i .~ Nothing
        in do
            sequence_ [ putTMVar wsTMVar ws', putTMVar mqtTMVar mqt', putTMVar ptTMVar pt'' ]
            return (s, bs, logMsgs, pt'')
    forM_ logMsgs $ uncurry (logPla "handleEgress")
    logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", parensQuote s, " has left the game." ]
    closePlaLog i
    bcastNl bs
    bcastAdmins pt $ s <> " has left the game."
  where
    peepHelper pt@((! i) -> p) s =
        let pt'       = stopPeeping
            peeperIds = p^.peepers
            pt''      = stopBeingPeeped pt' peeperIds
            bs        = [ (T.concat [ "You are no longer peeping "
                                    , s
                                    , " "
                                    , parensQuote $ s <> " has disconnected"
                                    , "." ], [peeperId]) | peeperId <- peeperIds ]
            logMsgs   = [ (peeperId, T.concat [ "no longer peeping "
                                              , s
                                              , " "
                                              , parensQuote $ s <> " has disconnected"
                                              , "." ]) | peeperId <- peeperIds ]
        in (pt'', bs, logMsgs)
      where
        stopPeeping                   =
            let helper peepedId ptAcc = let thePeeped = ptAcc ! peepedId
                                        in ptAcc & at peepedId ?~ over peepers (i `delete`) thePeeped
            in foldr helper pt $ p^.peeping
        stopBeingPeeped pt' peeperIds =
            let helper peeperId ptAcc = let thePeeper = ptAcc ! peeperId
                                        in ptAcc & at peeperId ?~ over peeping (i `delete`) thePeeper
            in foldr helper pt' peeperIds


notifyEgress :: Id -> MudStack ()
notifyEgress i = bcast =<< [ [(nlnl $ serialize d <> " has left the game.", pis)] | ws <- readWSTMVar
                           , let (d, _, _, _, _) = mkCapStdDesig i ws, let pis = i `delete` pcIds d ]


-----


quitCan'tAbbrev :: Action
quitCan'tAbbrev (NoArgs _ mq cols) | msg <- T.concat [ "The "
                                                     , dblQuote "quit"
                                                     , " command may not be abbreviated. Type "
                                                     , dblQuote "quit"
                                                     , " with no arguments to quit the game." ] = wrapSend mq cols msg
quitCan'tAbbrev p = withoutArgs quitCan'tAbbrev p


-----


ready :: Action
ready p@AdviseNoArgs = advise p ["ready"] advice
  where
    advice = T.concat [ "Please specify one or more items to ready, as in "
                      , quoteColor
                      , dblQuote "ready sword"
                      , dfltColor
                      , "." ]
ready (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "ready" i $ logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, _, is, c) = mkDropReadyBindings i ws
        in (is, c) |*|
          ( let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols i ws as is mempty
                eiss                      = zipWith (curry procGecrMisReady) gecrs miss
                bs                        = rcs |!| mkBroadcast i "You can't ready coins."
                (ws', bs', logMsgs)       = foldl' (helperReady i d) (ws, bs, []) . zip eiss $ mrols
            in putTMVar t ws' >> return (bs', logMsgs)
          ,    putTMVar t ws  >> return (mkBroadcast i dudeYourHandsAreEmpty, []) )
ready p = patternMatchFail "ready" [ showText p ]


helperReady :: Id
            -> PCDesig
            -> (WorldState, [Broadcast], [T.Text])
            -> (Either T.Text Inv, Maybe RightOrLeft)
            -> (WorldState, [Broadcast], [T.Text])
helperReady i d a (eis, mrol) = case eis of
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is                   -> foldl' (readyDispatcher i d mrol) a is


readyDispatcher :: Id
                -> PCDesig
                -> Maybe RightOrLeft
                -> (WorldState, [Broadcast], [T.Text])
                -> Id
                -> (WorldState, [Broadcast], [T.Text])
readyDispatcher i d mrol a@(ws, _, _) ei@(((ws^.entTbl) !) -> e) = maybe sorry (\f -> f i d mrol a ei e) mf
  where
    mf = case (ws^.typeTbl) ! ei of
      ClothType -> Just readyCloth
      ConType   -> toMaybe (view isCloth $ (ws^.conTbl) ! ei) readyCloth
      WpnType   -> Just readyWpn
      ArmType   -> Just readyArm
      _         -> Nothing
    sorry | b <- mkBroadcast i $ "You can't ready " <> aOrAn (e^.sing) <> "." = a & _2 <>~ b


-- Readying clothing:


readyCloth :: Id
           -> PCDesig
           -> Maybe RightOrLeft
           -> (WorldState, [Broadcast], [T.Text])
           -> Id
           -> Ent
           -> (WorldState, [Broadcast], [T.Text])
readyCloth i d mrol a@(ws, _, _) ei e@(view sing -> s) =
    let em = (ws^.eqTbl)    ! i
        c  = (ws^.clothTbl) ! ei
    in case maybe (getAvailClothSlot ws i c em) (getDesigClothSlot ws e c em) mrol of
      Left  (mkBroadcast i -> b) -> a & _2 <>~ b
      Right slot                 -> moveReadiedItem i a em slot ei . mkReadyClothMsgs slot $ c
  where
    mkReadyClothMsgs (pp -> slot) = \case
      Earring  -> wearMsgs
      NoseRing -> putOnMsgs i d s
      Necklace -> putOnMsgs i d s
      Bracelet -> wearMsgs
      Ring     -> slideMsgs
      Backpack -> putOnMsgs i d s
      _        -> donMsgs   i d s
      where
        wearMsgs   = (  T.concat [ "You wear the ",  s, " on your ", slot, "." ]
                     , (T.concat [ serialize d, " wears ",  aOrAn s, " on ", p, " ", slot, "." ], otherPCIds) )
        slideMsgs  = (  T.concat [ "You slide the ", s, " on your ", slot, "." ]
                     , (T.concat [ serialize d, " slides ", aOrAn s, " on ", p, " ", slot, "." ], otherPCIds) )
        p          = views sex mkPossPro $ (ws^.mobTbl) ! i
        otherPCIds = i `delete` pcIds d


getAvailClothSlot :: WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot ws i c em | m <- (ws^.mobTbl) ! i, s <- m^.sex, h <- m^.hand = procMaybe $ case c of
  Earring  -> getEarringSlotForSex s `mplus` (getEarringSlotForSex . otherSex $ s)
  NoseRing -> findAvailSlot em noseRingSlots
  Necklace -> findAvailSlot em necklaceSlots
  Bracelet -> getBraceletSlotForHand h `mplus` (getBraceletSlotForHand . otherHand $ h)
  Ring     -> getRingSlot s h
  _        -> maybeSingleSlot em . clothToSlot $ c
  where
    procMaybe                = maybe (Left . sorryFullClothSlots ws c $ em) Right
    getEarringSlotForSex s   = findAvailSlot em $ case s of
      Male   -> lEarringSlots
      Female -> rEarringSlots
      _      -> patternMatchFail "getAvailClothSlot getEarringSlotForSex"   [ showText s ]
    getBraceletSlotForHand h = findAvailSlot em $ case h of
      RHand  -> lBraceletSlots
      LHand  -> rBraceletSlots
      _      -> patternMatchFail "getAvailClothSlot getBraceletSlotForHand" [ showText h ]
    getRingSlot s h          = findAvailSlot em $ case s of
      Male    -> case h of
        RHand -> [ RingLRS, RingLIS, RingRRS, RingRIS, RingLMS, RingRMS, RingLPS, RingRPS ]
        LHand -> [ RingRRS, RingRIS, RingLRS, RingLIS, RingRMS, RingLMS, RingRPS, RingLPS ]
        _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
      Female  -> case h of
        RHand -> [ RingLRS, RingLIS, RingRRS, RingRIS, RingLPS, RingRPS, RingLMS, RingRMS ]
        LHand -> [ RingRRS, RingRIS, RingLRS, RingLIS, RingRPS, RingLPS, RingRMS, RingLMS ]
        _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
      _       -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText s ]


otherSex :: Sex -> Sex
otherSex Male   = Female
otherSex Female = Male
otherSex NoSex  = NoSex


rEarringSlots, lEarringSlots, noseRingSlots, necklaceSlots, rBraceletSlots, lBraceletSlots :: [Slot]
rEarringSlots  = [ EarringR1S, EarringR2S ]
lEarringSlots  = [ EarringL1S, EarringL2S ]
noseRingSlots  = [ NoseRing1S, NoseRing2S ]
necklaceSlots  = [ Necklace1S .. Necklace2S ]
rBraceletSlots = [ BraceletR1S .. BraceletR3S ]
lBraceletSlots = [ BraceletL1S .. BraceletL3S ]


sorryFullClothSlots :: WorldState -> Cloth -> EqMap -> T.Text
sorryFullClothSlots ws c@(pp -> c') em
  | c `elem` [ Earring .. Ring ]               = "You can't wear any more " <> c'       <> "s."
  | c `elem` [ Skirt, Dress, Backpack, Cloak ] = "You're already wearing "  <> aOrAn c' <> "."
  | otherwise = let i = em^.at (clothToSlot c).to fromJust
                    e = (ws^.entTbl) ! i
                in "You're already wearing " <> aOrAn (e^.sing) <> "."


getDesigClothSlot :: WorldState -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot ws (view sing -> s) c em rol
  | c `elem` [ NoseRing, Necklace ] ++ [ Shirt .. Cloak ] = Left sorryCan'tWearThere
  | isRingRol rol, c /= Ring                              = Left sorryCan'tWearThere
  | c == Ring, not . isRingRol $ rol                      = Left ringHelp
  | otherwise                                             = case c of
    Earring  -> maybe (Left  sorryEarring)  Right (findSlotFromList rEarringSlots  lEarringSlots)
    Bracelet -> maybe (Left  sorryBracelet) Right (findSlotFromList rBraceletSlots lBraceletSlots)
    Ring     -> maybe (Right slotFromRol)
                      (\i -> let e = (ws^.entTbl) ! i in Left . sorryRing slotFromRol $ e)
                      (em^.at slotFromRol)
    _        -> patternMatchFail "getDesigClothSlot" [ showText c ]
  where
    sorryCan'tWearThere    = T.concat [ "You can't wear ", aOrAn s, " on your ", pp rol, "." ]
    findSlotFromList rs ls = findAvailSlot em $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot getSlotFromList"  [ showText rol ]
    sorryEarring  = sorryFullClothSlotsOneSide c . getSlotFromList rEarringSlots  $ lEarringSlots
    sorryBracelet = sorryFullClothSlotsOneSide c . getSlotFromList rBraceletSlots $ lBraceletSlots
    slotFromRol   = fromRol rol :: Slot
    sorryRing (pp -> slot) (view sing -> ringS) = T.concat [ "You're already wearing "
                                                           , aOrAn ringS
                                                           , " on your "
                                                           , slot
                                                           , "." ]


sorryFullClothSlotsOneSide :: Cloth -> Slot -> T.Text
sorryFullClothSlotsOneSide (pp -> c) (pp -> s) = T.concat [ "You can't wear any more "
                                                          , c
                                                          , "s on your "
                                                          , s
                                                          , "." ]


-- Readying weapons:


readyWpn :: Id
         -> PCDesig
         -> Maybe RightOrLeft
         -> (WorldState, [Broadcast], [T.Text])
         -> Id
         -> Ent
         -> (WorldState, [Broadcast], [T.Text])
readyWpn i d mrol a@(ws, _, _) ei e@(view sing -> s) | em  <- (ws^.eqTbl)  ! i
                                                     , w   <- (ws^.wpnTbl) ! ei
                                                     , sub <- w^.wpnSub = if not . isSlotAvail em $ BothHandsS
  then let b = mkBroadcast i "You're already wielding a two-handed weapon." in a & _2 <>~ b
  else case maybe (getAvailWpnSlot ws i em) (getDesigWpnSlot ws e em) mrol of
    Left  (mkBroadcast i -> b) -> a & _2 <>~ b
    Right slot  -> case sub of
      OneHanded -> let readyMsgs = (   T.concat [ "You wield the ", s, " with your ", pp slot, "." ]
                                   , ( T.concat [ serialize d, " wields ", aOrAn s, " with ", p, " ", pp slot, "." ]
                                     , otherPCIds ) )
                   in moveReadiedItem i a em slot ei readyMsgs
      TwoHanded
        | all (isSlotAvail em) [ RHandS, LHandS ] ->
            let readyMsgs = ( "You wield the " <> s <> " with both hands."
                            , ( T.concat [ serialize d, " wields ", aOrAn s, " with both hands." ], otherPCIds ) )
            in moveReadiedItem i a em BothHandsS ei readyMsgs
        | otherwise -> let b = mkBroadcast i $ "Both hands are required to wield the " <> s <> "."
                       in a & _2 <>~ b
  where
    p          = views sex mkPossPro $ (ws^.mobTbl) ! i
    otherPCIds = i `delete` pcIds d


getAvailWpnSlot :: WorldState -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot ws i em | (view hand -> h@(otherHand -> oh)) <- (ws^.mobTbl) ! i =
    maybe (Left "You're already wielding two weapons.")
          Right
          (findAvailSlot em . map getSlotForHand $ [ h, oh ])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> patternMatchFail "getAvailWpnSlot getSlotForHand" [ showText h ]


getDesigWpnSlot :: WorldState -> Ent -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigWpnSlot ws (views sing aOrAn -> s) em rol
  | isRingRol rol = Left $ "You can't wield " <> s <> " with your finger!"
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e = (ws^.entTbl) ! i in Left . sorry $ e)
                          (em^.at desigSlot)
  where
    sorry (views sing aOrAn -> wpnS) = T.concat [ "You're already wielding "
                                                , wpnS
                                                , " with your "
                                                , pp desigSlot
                                                , "." ]
    desigSlot = case rol of R -> RHandS
                            L -> LHandS
                            _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-- Readying armor:


readyArm :: Id
         -> PCDesig
         -> Maybe RightOrLeft
         -> (WorldState, [Broadcast], [T.Text])
         -> Id
         -> Ent
         -> (WorldState, [Broadcast], [T.Text])
readyArm i d mrol a@(ws, _, _) ei (view sing -> s) =
    let em                   = (ws^.eqTbl)  ! i
        (view armSub -> sub) = (ws^.armTbl) ! ei
    in case maybe (getAvailArmSlot ws sub em) sorryCan'tWearThere mrol of
      Left  (mkBroadcast i -> b) -> a & _2 <>~ b
      Right slot                 -> moveReadiedItem i a em slot ei . mkReadyArmMsgs $ sub
  where
    sorryCan'tWearThere rol = Left . T.concat $ [ "You can't wear ", aOrAn s, " on your ", pp rol, "." ]
    mkReadyArmMsgs = \case
      Head   -> putOnMsgs                     i d s
      Hands  -> putOnMsgs                     i d s
      Feet   -> putOnMsgs                     i d s
      Shield -> mkReadyMsgs "ready" "readies" i d s
      _      -> donMsgs                       i d s


getAvailArmSlot :: WorldState -> ArmSub -> EqMap -> Either T.Text Slot
getAvailArmSlot ws sub em = procMaybe . maybeSingleSlot em . armSubToSlot $ sub
  where
    procMaybe        = maybe (Left sorryFullArmSlot) Right
    sorryFullArmSlot = let i = em^.at (armSubToSlot sub).to fromJust
                           e = (ws^.entTbl) ! i
                       in "You're already wearing " <> aOrAn (e^.sing) <> "."


-----


remove :: Action
remove p@AdviseNoArgs = advise p ["remove"] advice
  where
    advice = T.concat [ "Please specify one or more items to remove followed by the container you want to remove \
                        \them from, as in "
                      , quoteColor
                      , dblQuote "remove doll sack"
                      , dfltColor
                      , "." ]
remove p@(AdviseOneArg a) = advise p ["remove"] advice
  where
    advice = T.concat [ "Please also specify the container you want to remove it from, as in "
                      , quoteColor
                      , dblQuote $ "remove " <> a <> " sack"
                      , dfltColor
                      , "." ]
remove (Lower' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "remove" i $ logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let (d, ris, rc, pis, pc, cn, argsWithoutCon) = mkPutRemBindings i ws as
      in if T.head cn == rmChar && cn /= T.singleton rmChar
        then if not . null $ ris
          then shuffleRem i (t, ws) d (T.tail cn) True argsWithoutCon ris rc procGecrMisRm
          else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
        else shuffleRem i (t, ws) d cn False argsWithoutCon pis pc procGecrMisPCInv
remove p = patternMatchFail "remove" [ showText p ]


shuffleRem :: Id
           -> (TMVar WorldState, WorldState)
           -> PCDesig
           -> ConName
           -> IsConInRm
           -> Args
           -> InvWithCon
           -> CoinsWithCon
           -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv)
           -> STM ([Broadcast], [T.Text])
shuffleRem i (t, ws) d cn icir as is c f
  | (conGecrs, conMiss, conRcs) <- resolveEntCoinNames i ws [cn] is c = if null conMiss && (not . null $ conRcs)
    then putTMVar t ws >> return (mkBroadcast i "You can't remove something from a coin.", [])
    else case f . head . zip conGecrs $ conMiss of
      Left  msg -> putTMVar t ws >> return (mkBroadcast i msg, [])
      Right [ci] | e@(view sing -> s) <- (ws^.entTbl) ! ci, typ <- (ws^.typeTbl) ! ci ->
        if typ /= ConType
          then putTMVar t ws >> return (mkBroadcast i $ theOnLowerCap s <> " isn't a container.", [])
          else let cis                   = (ws^.invTbl)   ! ci
                   cc                    = (ws^.coinsTbl) ! ci
                   (gecrs, miss, rcs)    = resolveEntCoinNames i ws as cis cc
                   eiss                  = [ curry (procGecrMisCon s) gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map (procReconciledCoinsCon s) rcs
                   mnom                  = mkMaybeNthOfM icir ws ci e is
                   (ws',  bs,  logMsgs)  = foldl' (helperPutRemEitherInv   i d Rem mnom ci i e) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Rem mnom ci i e) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
      Right _ -> do
          putTMVar t ws
          return (mkBroadcast i "You can only remove things from one container at a time.", [])


-----


say :: Action
say p@AdviseNoArgs = advise p ["say"] advice
  where
    advice = T.concat [ "Please specify what you'd like to say, as in "
                      , quoteColor
                      , dblQuote "say nice to meet you, too"
                      , dfltColor
                      , "." ]
say p@(WithArgs i mq cols args@(a:_))
  | T.head a == adverbOpenChar = case parseAdverb . T.unwords $ args of
    Left  sorryMsg -> sorry sorryMsg
    Right (adverb, rest@(T.words -> rs@(head -> r)))
      | T.head r == sayToChar, T.length r > 1 -> if length rs > 1
        then sayTo (Just adverb) . T.tail $ rest
        else sorry adviceEmptySayTo
      | otherwise -> simpleSayHelper (Just adverb) rest
  | T.head a == sayToChar, T.length a > 1 = if length args > 1
    then sayTo Nothing . T.tail . T.unwords $ args
    else sorry adviceEmptySayTo
  | otherwise = simpleSayHelper Nothing . T.unwords $ args
  where
    parseAdverb (T.tail -> msg) = case T.break (== adverbCloseChar) msg of
      (_,   "")            -> Left adviceCloseChar
      ("",  _ )            -> Left adviceEmptyAdverb
      (" ", _ )            -> Left adviceEmptyAdverb
      (_,   x ) | x == acc -> Left adviceEmptySay
      (adverb, right)      -> Right (adverb, T.drop 2 right)
    aoc               = T.singleton adverbOpenChar
    acc               = T.singleton adverbCloseChar
    adviceCloseChar   = "An adverbial phrase must be terminated with a " <> dblQuote acc <> example
    example           = T.concat [ ", as in "
                                 , quoteColor
                                 , dblQuote $ "say " <> quoteWith' (aoc, acc) "enthusiastically" <> " nice to meet \
                                              \you, too"
                                 , dfltColor
                                 , "." ]
    adviceEmptyAdverb = T.concat [ "Please provide an adverbial phrase between "
                                 , dblQuote aoc
                                 , " and "
                                 , dblQuote acc
                                 , example ]
    adviceEmptySay    = "Please also specify what you'd like to say" <> example
    adviceEmptySayTo  = T.concat [ "Please also specify what you'd like to say, as in "
                                 , quoteColor
                                 , dblQuote $ "say " <> T.singleton sayToChar <> "taro nice to meet you, too"
                                 , dfltColor
                                 , "." ]
    sorry             = advise p ["say"]
    sayTo ma (T.words -> (target:rest@(r:_))) = readWSTMVar >>= \ws ->
        let (d, _, _, ri, ris@((i `delete`) -> ris')) = mkCapStdDesig i ws
            c                                         = (ws^.coinsTbl) ! ri
        in (ris', c) |*|
          ( case resolveRmInvCoins i ws [target] ris' c of
              (_,                    [ Left  [sorryMsg] ]) -> wrapSend mq cols sorryMsg
              (_,                    Right _:_           ) -> wrapSend mq cols "You're talking to coins now?"
              ([ Left sorryMsg    ], _                   ) -> wrapSend mq cols sorryMsg
              ([ Right (_:_:_)    ], _                   ) -> wrapSend mq cols "Sorry, but you can only say something \
                                                                               \to one person at a time."
              ([ Right [targetId] ], _                   ) ->
                let targetType                = (ws^.typeTbl) ! targetId
                    (view sing -> targetSing) = (ws^.entTbl)  ! targetId
                in case targetType of
                  PCType  | targetDesig <- serialize . mkStdDesig targetId ws targetSing False $ ris
                          -> either sorry (sayToHelper ws d targetId targetDesig) parseRearAdverb
                  MobType -> either sorry (sayToMobHelper d targetSing)           parseRearAdverb
                  _       -> wrapSend mq cols $ "You can't talk to " <> aOrAn targetSing <> "."
              x -> patternMatchFail "say sayTo" [ showText x ]
          , wrapSend mq cols "You don't see anyone here to talk to." )
      where
        parseRearAdverb = case ma of
          Just adv -> Right (adv <> " ", "", formatMsg . T.unwords $ rest)
          Nothing  | T.head r == adverbOpenChar -> case parseAdverb . T.unwords $ rest of
                       Right (adverb, rest') -> Right ("", " " <> adverb, formatMsg rest')
                       Left sorryMsg         -> Left sorryMsg
                   | otherwise -> Right ("", "", formatMsg . T.unwords $ rest)
        sayToHelper ws d targetId targetDesig (frontAdv, rearAdv, msg) =
            let toSelfMsg      = T.concat [ "You say ",            frontAdv, "to ", targetDesig, rearAdv, ", ", msg ]
                toSelfBrdcst   = (nlnl toSelfMsg, [i])
                toTargetMsg    = T.concat [ serialize d, " says ", frontAdv, "to you",           rearAdv, ", ", msg ]
                toTargetBrdcst = (nlnl toTargetMsg, [targetId])
                toOthersMsg    = T.concat [ serialize d, " says ", frontAdv, "to ", targetDesig, rearAdv, ", ", msg ]
                toOthersBrdcst = (nlnl toOthersMsg, pcIds d \\ [ i, targetId ])
            in do
                logPlaOut "say" i [ parsePCDesig i ws toSelfMsg ]
                bcast [ toSelfBrdcst, toTargetBrdcst, toOthersBrdcst ]
        sayToMobHelper d targetSing (frontAdv, rearAdv, msg) =
            let toSelfMsg      = T.concat [ "You say ", frontAdv, "to ", theOnLower targetSing, rearAdv, ", ", msg ]
                toOthersMsg    = T.concat [ serialize d
                                          , " says "
                                          , frontAdv
                                          , "to "
                                          , theOnLower targetSing
                                          , rearAdv
                                          , ", "
                                          , msg ]
                toOthersBrdcst = (nlnl toOthersMsg, i `delete` pcIds d)
            in do
                logPlaOut "say" i [ toSelfMsg ]
                bcast =<< [ [ (nlnl toSelfMsg <> fms, [i]), toOthersBrdcst ] | fms <- firstMobSay i ]
    sayTo ma msg            = patternMatchFail "say sayTo" [ showText ma, msg ]
    formatMsg               = dblQuote . capitalizeMsg . punctuateMsg
    simpleSayHelper ma rest = readWSTMVar >>= \ws ->
        let adverb          = case ma of Nothing  -> ""
                                         Just adv -> " " <> adv
            (d, _, _, _, _) = mkCapStdDesig i ws
            msg             = formatMsg rest
            toSelfMsg       = T.concat [ "You say", adverb, ", ", msg ]
            toSelfBrdcst    = (nlnl toSelfMsg, [i])
            toOthersMsg     = T.concat [ serialize d, " says", adverb, ", ", msg ]
            toOthersBrdcst  = (nlnl toOthersMsg, i `delete` pcIds d)
        in logPlaOut "say" i [toSelfMsg] >> bcast [ toSelfBrdcst, toOthersBrdcst ]
say p = patternMatchFail "say" [ showText p ]


firstMobSay :: Id -> MudStack T.Text
firstMobSay i = mIf (getPlaFlag IsNotFirstMobSay <$> getPla i)
                    (return "")
                    (let msg = nlnl . T.concat $ [ hintANSI
                                                 , "Hint:"
                                                 , noHintANSI
                                                 , " to communicate with non-player characters, use the "
                                                 , dblQuote "ask"
                                                 , " command. For example, to ask a city guard about crime, type "
                                                 , quoteColor
                                                 , dblQuote "ask guard crime"
                                                 , dfltColor
                                                 , "." ]
                     in (void . modifyPlaFlag i IsNotFirstMobSay $ True) >> return msg)


-----


setAction :: Action
setAction (NoArgs i mq cols) = do
    logPlaExecArgs "set" [] i
    multiWrapSend mq cols =<< [ [ pad 9 (n <> ": ") <> v | n <- names | v <- values ] | p <- getPla i
                              , let values = map showText [ cols, p^.pageLines ]
                              , let names  = styleAbbrevs Don'tBracket settingNames ]
setAction (LowerNub i mq cols as) = helper >>= \(msgs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "set" i $ logMsgs
    multiWrapSend mq cols msgs
  where
    helper = onNWS plaTblTMVar $ \(ptTMVar, pt) ->
        let p                   = pt ! i
            (p', msgs, logMsgs) = foldl' helperSettings (p, [], []) as
            pt'                 = pt & at i ?~ p'
        in putTMVar ptTMVar pt' >> return (msgs, logMsgs)
setAction p = patternMatchFail "setAction" [ showText p ]


settingNames :: [T.Text]
settingNames = [ "columns", "lines" ]


helperSettings :: (Pla, [T.Text], [T.Text]) -> T.Text -> (Pla, [T.Text], [T.Text])
helperSettings a@(_, msgs, _) arg@(T.length . T.filter (== '=') -> noOfEqs)
  | noOfEqs /= 1 || T.head arg == '=' || T.last arg == '='
  , msg    <- dblQuote arg <> " is not a valid argument."
  , advice <- T.concat [ " Please specify the setting you want to change, followed immediately by "
                       , dblQuote "="
                       , ", followed immediately by the new value you want to assign, as in "
                       , quoteColor
                       , dblQuote "set columns=80"
                       , dfltColor
                       , "." ]
  , f      <- any (advice `T.isInfixOf`) msgs ? (++ [msg]) :? (++ [ msg <> advice ]) = over _2 f a
helperSettings a@(p, _, _) (T.breakOn "=" -> (n, T.tail -> v)) =
    maybe notFound found . findFullNameForAbbrev n $ settingNames
  where
    notFound    = appendMsg $ dblQuote n <> " is not a valid setting name."
    appendMsg m = a & _2 <>~ [m]
    found       = \case "columns" -> procEither changeColumns
                        "lines"   -> procEither changePageLines
                        t         -> patternMatchFail "helperSettings found" [t]
      where
        procEither f = either appendMsg f parseInt
        parseInt     = case (reads . T.unpack $ v :: [(Int, String)]) of
          []        -> sorryParse
          [(x, "")] -> Right x
          _         -> sorryParse
        sorryParse   = Left . T.concat $ [ dblQuote v, " is not a valid value for the ", dblQuote n, " setting." ]
    changeColumns   = changeSetting minCols      maxCols      "columns" columns
    changePageLines = changeSetting minPageLines maxPageLines "lines"   pageLines
    changeSetting minVal@(showText -> minValTxt) maxVal@(showText -> maxValTxt) settingName lens x@(showText -> xTxt)
      | x < minVal || x > maxVal = appendMsg . T.concat $ [ capitalize settingName
                                                          , " must be between "
                                                          , minValTxt
                                                          , " and "
                                                          , maxValTxt
                                                          , "." ]
      | p'  <- p & lens .~ x, msg <- T.concat [ "Set ", settingName, " to ", xTxt, "." ]
      = appendMsg msg & _1 .~ p' & _3 <>~ [msg]


-----


typo :: Action
typo p@AdviseNoArgs = advise p ["typo"] advice
  where
    advice = T.concat [ "Please describe the typo you've found, as in "
                      , quoteColor
                      , dblQuote "typo 'accross from the fireplace' should be 'across from the fireplace'"
                      , dfltColor
                      , "." ]
typo p = bugTypoLogger p TypoLog


-----


unready :: Action
unready p@AdviseNoArgs = advise p ["unready"] advice
  where
    advice = T.concat [ "Please specify one or more items to unready, as in "
                      , quoteColor
                      , dblQuote "unready sword"
                      , dfltColor
                      , "." ]
unready (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) . logPlaOut "unready" i $ logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, _, _, _, _) = mkCapStdDesig i ws
            em              = (ws^.eqTbl) ! i
            is              = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs)  = resolveEntCoinNames i ws as is mempty
                   eiss                = zipWith (curry procGecrMisPCEq) gecrs miss
                   bs                  = rcs |!| mkBroadcast i "You can't unready coins."
                   (ws', bs', logMsgs) = foldl' (helperUnready i d em) (ws, bs, []) eiss
               in putTMVar t ws' >> return (bs', logMsgs)
          else    putTMVar t ws  >> return (mkBroadcast i dudeYou'reNaked, [])
unready p = patternMatchFail "unready" [ showText p ]


helperUnready :: Id
              -> PCDesig
              -> EqMap
              -> (WorldState, [Broadcast], [T.Text])
              -> Either T.Text Inv
              -> (WorldState, [Broadcast], [T.Text])
helperUnready i d em a@(ws, _, _) = \case
  Left  (mkBroadcast i -> b) -> a & _2 <>~ b
  Right is | pis        <- (ws^.invTbl) ! i
           , ws'        <- ws & eqTbl.at  i ?~ M.filter (`notElem` is) em
                              & invTbl.at i ?~ (sortInv ws . (pis ++) $ is)
           , (bs, msgs) <- mkUnreadyDescs i ws' d is
           -> a & _1 .~ ws' & _2 <>~ bs & _3 <>~ msgs


mkUnreadyDescs :: Id -> WorldState -> PCDesig -> Inv -> ([Broadcast], [T.Text])
mkUnreadyDescs i ws d is = over _1 concat . unzip $ [ helper icb | icb <- mkIdCountBothList i ws is ]
  where
    helper (ei, c, b@(s, _)) = if c == 1
      then let msg = T.concat [ "You ", mkVerb ei SndPer, " the ", s, "." ] in
          ( [ (msg, [i])
            , (T.concat [ serialize d, " ", mkVerb ei ThrPer, " ", aOrAn s, "." ], otherPCIds) ]
          , msg )
      else let msg = T.concat [ "You ", mkVerb ei SndPer, " ", showText c, " ", mkPlurFromBoth b, "." ] in
          ( [ (msg, [i])
            , ( T.concat [ serialize d, " ", mkVerb ei ThrPer, " ", showText c, " ", mkPlurFromBoth b, "." ]
              , otherPCIds ) ]
          , msg )
    mkVerb ei p =  case (ws^.typeTbl)  ! ei of
      ClothType -> case (ws^.clothTbl) ! ei of
        Earring  -> mkVerbRemove  p
        NoseRing -> mkVerbRemove  p
        Necklace -> mkVerbTakeOff p
        Bracelet -> mkVerbTakeOff p
        Ring     -> mkVerbTakeOff p
        Backpack -> mkVerbTakeOff p
        _        -> mkVerbDoff    p
      ConType -> mkVerbTakeOff p
      WpnType | p == SndPer -> "stop wielding"
              | otherwise   -> "stops wielding"
      ArmType -> case view armSub $ (ws^.armTbl) ! ei of
        Head   -> mkVerbTakeOff p
        Hands  -> mkVerbTakeOff p
        Feet   -> mkVerbTakeOff p
        Shield -> mkVerbUnready p
        _      -> mkVerbDoff    p
      t       -> patternMatchFail "mkUnreadyDescs mkVerb" [ showText t ]
    mkVerbRemove  = \case SndPer -> "remove"
                          ThrPer -> "removes"
    mkVerbTakeOff = \case SndPer -> "take off"
                          ThrPer -> "takes off"
    mkVerbDoff    = \case SndPer -> "doff"
                          ThrPer -> "doffs"
    mkVerbUnready = \case SndPer -> "unready"
                          ThrPer -> "unreadies"
    otherPCIds    = i `delete` pcIds d


mkIdCountBothList :: Id -> WorldState -> Inv -> [(Id, Int, BothGramNos)]
mkIdCountBothList i ws is | ebgns <- [ getEffBothGramNos i ws i' | i' <- is ], cs <- mkCountList ebgns =
    nubBy equalCountsAndBoths . zip3 is cs $ ebgns
  where
    equalCountsAndBoths (_, c, b) (_, c', b') = c == c' && b == b'


-----


uptime :: Action
uptime (NoArgs i mq cols) = do
    logPlaExec "uptime" i
    wrapSend mq cols =<< uptimeHelper =<< getUptime
uptime p = withoutArgs uptime p


uptimeHelper :: Int -> MudStack T.Text
uptimeHelper ut = helper <$> getRecordUptime
  where
    helper                     = \case Nothing  -> mkUptimeTxt
                                       Just rut -> ut > rut ? mkNewRecTxt :? mkRecTxt rut
    mkUptimeTxt                = mkTxtHelper "."
    mkNewRecTxt                = mkTxtHelper . T.concat $ [ " - "
                                                          , newRecordColor
                                                          , "it's a new record!"
                                                          , dfltColor ]
    mkRecTxt (renderIt -> rut) = mkTxtHelper $ " (record uptime: " <> rut <> ")."
    mkTxtHelper                = ("Up " <>) . (renderIt ut <>)
    renderIt                   = T.pack . renderSecs . toInteger


getRecordUptime :: MudStack (Maybe Int)
getRecordUptime = mIf (liftIO . doesFileExist $ uptimeFile)
                      (liftIO readUptime `catch` (\e -> fileIOExHandler "getRecordUptime" e >> return Nothing))
                      (return Nothing)
  where
    readUptime = Just . read <$> readFile uptimeFile


getUptime :: MudStack Int
getUptime = (-) <$> sec `fmap` (liftIO . getTime $ Monotonic) <*> sec `fmap` getNWSRec startTime


-----


what :: Action
what p@AdviseNoArgs = advise p ["what"] advice
  where
    advice = T.concat [ "Please specify one or more abbreviations to disambiguate, as in "
                      , quoteColor
                      , dblQuote "what up"
                      , dfltColor
                      , "." ]
what (LowerNub i mq cols as) =
    logPlaExecArgs "what" as i >> (send mq =<< [ T.concat . map (helper ws r) $ as | (ws, r) <- getPCRm' i ])
  where
    helper ws r n = nl . T.concat $ whatCmd cols r n : [ whatInv i cols ws it n | it <- [ PCInv, PCEq, RmInv ] ]
what p = patternMatchFail "what" [ showText p ]


whatCmd :: Cols -> Rm -> T.Text -> T.Text
whatCmd cols (mkCmdListWithNonStdRmLinks -> cmds) (T.toLower -> n@(whatQuote -> n')) =
    wrapUnlines cols . maybe notFound found . findFullNameForAbbrev n $ [ cmdName cmd | cmd <- cmds ]
  where
    notFound = n' <> " doesn't refer to any commands."
    found cn = let cfn = dblQuote . cmdFullName . head . filter ((== cn) . cmdName) $ cmds
               in T.concat [ n', " may refer to the ", cfn, " command." ]


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks (view rmLinks -> rls) = sort $ plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) =
    Cmd { cmdName = cn, cmdPriorityAbbrev = Nothing, cmdFullName = cn, action = go cn, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> T.Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _linkDir
                                               NonStdLink { .. } -> _linkName


whatQuote :: T.Text -> T.Text
whatQuote = (<> dfltColor) . (quoteColor <>) . dblQuote


whatInv :: Id -> Cols -> WorldState -> InvType -> T.Text -> T.Text
whatInv i cols ws it n | (is, gecrs, rcs) <- resolveName = if not . null $ gecrs
  then whatInvEnts i cols ws it n (head gecrs) is
  else T.concat . map (whatInvCoins cols it n) $ rcs
  where
    resolveName | (is, c) <- getLocInvCoins, (gecrs, _, rcs) <- resolveEntCoinNames i ws [n] is c = (is, gecrs, rcs)
    getLocInvCoins    = case it of PCInv -> (((ws^.invTbl) !) *** ((ws^.coinsTbl) !)) . dup $ i
                                   PCEq  -> (M.elems $ (ws^.eqTbl) ! i, mempty)
                                   RmInv -> (((ws^.invTbl) !) *** ((ws^.coinsTbl) !)) . dup $ ri
    (view rmId -> ri) = (ws^.pcTbl) ! i


whatInvEnts :: Id -> Cols -> WorldState -> InvType -> T.Text -> GetEntsCoinsRes -> Inv -> T.Text
whatInvEnts i cols ws it@(getLocTxtForInvType -> locTxt) (whatQuote -> r) gecr is = wrapUnlines cols $ case gecr of
  Mult { entsRes = (Just es), .. }
    | nameSearchedFor == acp -> T.concat [ whatQuote acp
                                         , " may refer to everything "
                                         , locTxt
                                         , supplement
                                         , "." ]
    | e@(view sing -> s) <- head es, len <- length es -> if len > 1
      then let ebgns@(head -> h)         = take len [ getEffBothGramNos i ws i' | e' <- es, let i' = e'^.entId ]
               target | all (== h) ebgns = mkPlurFromBoth h
                      | otherwise        = (<> "s") . bracketQuote . getEffName i ws $ e^.entId
           in T.concat [ r
                       , " may refer to the "
                       , showText len
                       , " "
                       , target
                       , " "
                       , locTxt
                       , "." ]
      else let ens = [ getEffName i ws i' | i' <- is ]
           in T.concat [ r
                       , " may refer to the "
                       , T.pack . checkFirst e $ ens
                       , s
                       , " "
                       , locTxt
                       , "." ]
  Indexed { entRes = (Right e@(view sing -> s)), .. } -> T.concat [ r
                                                                  , " may refer to the "
                                                                  , mkOrdinal index
                                                                  , " "
                                                                  , bracketQuote . getEffName i ws $ e^.entId
                                                                  , " "
                                                                  , parensQuote s
                                                                  , " "
                                                                  , locTxt
                                                                  , "." ]
  _                                                   -> T.concat [ r
                                                                  , " doesn't refer to anything "
                                                                  , locTxt
                                                                  , "." ]
  where
    acp                                     = T.singleton allChar
    supplement | it `elem` [ PCInv, RmInv ] = " " <> parensQuote "including any coins"
               | otherwise                  = ""
    checkFirst e ens | en <- getEffName i ws $ e^.entId, matches <- filter (== en) ens =
        guard (length matches > 1) >> "first "


getLocTxtForInvType :: InvType -> T.Text
getLocTxtForInvType = \case PCInv -> "in your inventory"
                            PCEq  -> "in your readied equipment"
                            RmInv -> "in this room"


whatInvCoins :: Cols -> InvType -> T.Text -> ReconciledCoins -> T.Text
whatInvCoins cols it@(getLocTxtForInvType -> locTxt) (whatQuote -> r) rc
  | it == PCEq = ""
  | otherwise  = wrapUnlines cols $ case rc of
    Left  Empty                                 -> T.concat [ r
                                                            , " doesn't refer to any coins "
                                                            , locTxt
                                                            , " "
                                                            , supplementNone "coins"
                                                            , "." ]
    Left  (NoneOf (mkTxtForCoins -> cn))        -> T.concat [ r
                                                            , " doesn't refer to any "
                                                            , cn
                                                            , " "
                                                            , locTxt
                                                            , " "
                                                            , supplementNone cn
                                                            , "." ]
    Left  (SomeOf (mkTxtForCoins -> cn))        -> T.concat [ r
                                                            , " doesn't refer to any "
                                                            , cn
                                                            , " "
                                                            , locTxt
                                                            , " "
                                                            , supplementNotEnough cn
                                                            , "." ]
    Right (SomeOf (mkTxtForCoinsWithAmt -> cn)) -> T.concat [ r
                                                            , " may refer to "
                                                            , cn
                                                            , " "
                                                            , locTxt
                                                            , "." ]
    _                                           -> patternMatchFail "whatInvCoins" [ showText rc ]
  where
    supplementNone cn      = case it of PCInv -> parensQuote $ "you don't have any "       <> cn
                                        RmInv -> parensQuote $ "there aren't any "         <> cn <> " here"
                                        PCEq  -> oops "supplementNone"
    supplementNotEnough cn = case it of PCInv -> parensQuote $ "you don't have that many " <> cn
                                        RmInv -> parensQuote $ "there aren't that many "   <> cn <> " here"
                                        PCEq  -> oops "supplementNotEnough"
    oops fn                = blowUp ("whatInvCoins " <> fn) "called for InvType of PCEq" []
    mkTxtForCoins c@(Coins (cop, sil, gol))
      | cop /= 0  = "copper pieces"
      | sil /= 0  = "silver pieces"
      | gol /= 0  = "gold pieces"
      | otherwise = blowUp "whatInvCoins mkTxtForCoins" "attempted to make text for empty coins" [ showText c ]
    mkTxtForCoinsWithAmt c@(Coins (cop, sil, gol))
      | cop == 1  = "1 copper piece"
      | cop /= 0  = showText cop <> " copper pieces"
      | sil == 1  = "1 silver piece"
      | sil /= 0  = showText sil <> " silver pieces"
      | gol == 1  = "1 gold piece"
      | gol /= 0  = showText gol <> " gold pieces"
      | otherwise = blowUp "whatInvCoins mkTxtForCoinsWithAmt" "attempted to make text for empty coins" [ showText c ]


-----


whoAdmin :: Action
whoAdmin (NoArgs i mq cols) = do
    logPlaExec "whoadmin" i
    multiWrapSend mq cols =<< mkAdminListTxt i <$> readWSTMVar <*> readTMVarInNWS plaTblTMVar
whoAdmin p = withoutArgs whoAdmin p


mkAdminListTxt :: Id -> WorldState -> IM.IntMap Pla -> [T.Text]
mkAdminListTxt i ws pt =
    let ais                         = [ pi | pi <- IM.keys pt, getPlaFlag IsAdmin (pt ! pi) ]
        (ais', self) | i `elem` ais = (i `delete` ais, selfColor <> view sing ((ws^.entTbl) ! i) <> dfltColor)
                     | otherwise    = (ais, "")
        aas                         = styleAbbrevs Don'tBracket [ s | ai <- ais'
                                                                    , let s = view sing $ (ws^.entTbl) ! ai
                                                                    , then sortWith by s ]
        aas'                        = dropBlanks $ self : aas
        footer                      = [ numOfAdmins ais <> " logged in." ]
    in null aas' ? footer :? T.intercalate ", " aas' : footer
  where
    numOfAdmins (length -> noa) | noa == 1  = "1 administrator"
                                | otherwise = showText noa <> " administrators"


-----


whoAmI :: Action
whoAmI (NoArgs i mq cols) = do
    logPlaExec "whoami" i
    ((pp *** pp) . getSexRace i -> (sexy, r), s) <- getEntSing' i
    wrapSend mq cols . T.concat $ [ "You are ", knownNameColor, s, dfltColor, " (a ", sexy, " ", r, ")." ]
whoAmI p = withoutArgs whoAmI p
