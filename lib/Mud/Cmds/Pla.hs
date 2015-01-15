{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Pla ( getRecordUptime
                    , getUptime
                    , handleEgress
                    , look
                    , mkCmdListWithNonStdRmLinks
                    , plaCmds
                    , showMotd ) where

import Mud.ANSI
import Mud.Cmds.Util.Abbrev
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
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
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
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
import Control.Lens (_1, _2, _3, at, both, over)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (?~), (.~), (^.))
import Control.Lens.Setter (set)
import Control.Monad (forM_, guard, mplus, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.IntMap.Lazy ((!))
import Data.List ((\\), delete, foldl', intercalate, intersperse, nub, nubBy, partition, sort, sortBy)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), mempty)
import Prelude hiding (pi)
import System.Clock (Clock(..), TimeSpec(..), getTime)
import System.Console.ANSI (clearScreenCode)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Time.Utils (renderSecs)
import qualified Data.IntMap.Lazy as IM (IntMap, keys)
import qualified Data.Map.Lazy as M (elems, filter, null)
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
plaCmds =
    [ Cmd { cmdName = "?", action = plaDispCmdList, cmdDesc = "Display or search this command list." }
    , Cmd { cmdName = "about", action = about, cmdDesc = "About CurryMUD." }
    , Cmd { cmdName = "admin", action = admin, cmdDesc = "Send a message to an administrator." }
    , Cmd { cmdName = "clear", action = clear, cmdDesc = "Clear the screen." }
    , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
    , Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop one or more items." }
    , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
    , Cmd { cmdName = "equip", action = equip, cmdDesc = "Display your readied equipment, or examine one or more items \
                                                         \in your readied equipment." }
    , Cmd { cmdName = "exits", action = exits, cmdDesc = "Display obvious exits." }
    , Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick up one or more items." }
    , Cmd { cmdName = "help", action = help, cmdDesc = "Get help on one or more commands or topics." }
    , Cmd { cmdName = "i", action = inv, cmdDesc = "Display your inventory, or examine one or more items in your \
                                                   \inventory." }
    , Cmd { cmdName = "intro", action = intro, cmdDesc = "Introduce yourself." }
    , Cmd { cmdName = "look", action = look, cmdDesc = "Display a description of your current location, or examine one \
                                                       \or more items in your current location." }
    , Cmd { cmdName = "motd", action = motd, cmdDesc = "Display the message of the day." }
    , Cmd { cmdName = "n", action = go "n", cmdDesc = "Go north." }
    , Cmd { cmdName = "ne", action = go "ne", cmdDesc = "Go northeast." }
    , Cmd { cmdName = "nw", action = go "nw", cmdDesc = "Go northwest." }
    , Cmd { cmdName = "put", action = putAction, cmdDesc = "Put one or more items into a container." }
    , Cmd { cmdName = "quit", action = quit, cmdDesc = "Quit playing CurryMUD." }
    , Cmd { cmdName = "ready", action = ready, cmdDesc = "Ready one or more items." }
    , Cmd { cmdName = "remove", action = remove, cmdDesc = "Remove one or more items from a container." }
    , Cmd { cmdName = "s", action = go "s", cmdDesc = "Go south." }
    , Cmd { cmdName = "se", action = go "se", cmdDesc = "Go southeast." }
    , Cmd { cmdName = "sw", action = go "sw", cmdDesc = "Go southwest." }
    , Cmd { cmdName = "u", action = go "u", cmdDesc = "Go up." }
    , Cmd { cmdName = "unready", action = unready, cmdDesc = "Unready one or more items." }
    , Cmd { cmdName = "uptime", action = uptime, cmdDesc = "Display how long CurryMUD has been running." }
    , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." }
    , Cmd { cmdName = "what", action = what, cmdDesc = "Disambiguate one or more abbreviations." }
    , Cmd { cmdName = "whoadmin", action = whoAdmin, cmdDesc = "Display a list of the administrators who are currently \
                                                               \logged in." }
    , Cmd { cmdName = "whoami", action = whoAmI, cmdDesc = "Confirm your name, sex, and race." } ]


-----


about :: Action
about (NoArgs i mq cols) = do
    logPlaExec "about" i
    try helper >>= eitherRet (\e -> fileIOExHandler "about" e >> sendGenericErrorMsg mq cols)
  where
    helper = multiWrapSend mq cols . T.lines =<< (liftIO . T.readFile $ aboutFile)
about p = withoutArgs about p


-----


admin :: Action
admin p@AdviseNoArgs                         = advise p ["admin"] advice
  where
    advice = T.concat [ "Please specify the name of an administrator, followed by a message, as in "
                      , quoteColor
                      , dblQuote "admin jason are you available? I need your assistance"
                      , dfltColor
                      , "." ]
admin p@(AdviseOneArg a)                     = advise p ["admin"] advice
  where
    advice = T.concat [ "Please also provide a message to send, as in "
                      , quoteColor
                      , dblQuote $ "admin " <> a <> " are you available? I need your assistance"
                      , dfltColor
                      , "." ]
admin   (MsgWithTarget i mq cols target msg) = do
    ws        <- readWSTMVar
    (mqt, pt) <- getMqtPt
    let aiss = mkAdminIdsSingsList i ws pt
    let s    = view sing $ (ws^.entTbl) ! i
    let notFound    | target `T.isInfixOf` s = wrapSend mq cols   "You can't send a message to yourself."
                    | otherwise              = wrapSend mq cols $ "No administrator by the name of " <>
                                                                  dblQuote target                    <>
                                                                  " is currently logged in."
    let found match | (i', target') <- head . filter ((== match) . snd) $ aiss
                    , mq'           <- mqt ! i'
                    , cols'         <- (pt ! i')^.columns = do
                       logNotice "admin"    . T.concat $ [ s, " sent message to ",   target', ": ", dblQuote msg ]
                       logPla    "admin" i  . T.concat $ [     "sent message to "  , target', ": ", dblQuote msg ]
                       logPla    "admin" i' . T.concat $ [ "received message from ", s,       ": ", dblQuote msg ]
                       wrapSend mq  cols    . T.concat $ [ "You send ",              target', ": ", dblQuote msg ]
                       wrapSend mq' cols'   . T.concat $ [ bracketQuote s, " ", adminMsgColor, msg, dfltColor    ]
    maybe notFound found . findFullNameForAbbrev target . map snd $ aiss
admin p = patternMatchFail "admin" [ showText p ]


mkAdminIdsSingsList :: Id -> WorldState -> IM.IntMap Pla -> [(Id, Sing)]
mkAdminIdsSingsList i ws pt = sortBy (compare `on` snd) [ (pi, view sing $ (ws^.entTbl) ! pi) | pi <- IM.keys pt
                                                                                              , (pt ! pi)^.isAdmin
                                                                                              , pi /= i ]


-----


clear :: Action
clear (NoArgs' i mq) = logPlaExec "clear" i >> (send mq . T.pack $ clearScreenCode)
clear p              = withoutArgs clear p


-----


dropAction :: Action
dropAction p@AdviseNoArgs     = advise p ["drop"] advice
  where
    advice = T.concat [ "Please specify one or more items to drop, as in "
                      , quoteColor
                      , dblQuote "drop sword"
                      , dfltColor
                      , "." ]
dropAction   (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "drop" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, ri, is, c) = mkDropReadyBindings i ws
        in if (not . null $ is) || (c /= mempty)
          then let (eiss, ecs)           = resolvePCInvCoins i ws as is c
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Drop i ri) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Drop i ri) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
dropAction p = patternMatchFail "dropAction" [ showText p ]


-----


equip :: Action
equip (NoArgs i mq cols) = getEnt' i >>= \(ws, e) ->
    send mq . nl . mkEqDesc i cols ws i e $ PCType
equip (LowerNub i mq cols as) = do
    (ws, em@(M.elems -> is)) <- getEq' i
    send mq $ if not . M.null $ em
      then let (gecrs, miss, rcs)           = resolveEntCoinNames i ws as is mempty
               eiss                         = [ curry procGecrMisPCEq gecr mis | gecr <- gecrs | mis <- miss ]
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
exits (NoArgs i mq cols) = getPCRm i >>= \r ->
    logPlaExec "exits" i >> (send mq . nl . mkExitsSummary cols $ r)
exits p = withoutArgs exits p


-----


getAction :: Action
getAction p@AdviseNoArgs     = advise p ["get"] advice
  where
    advice = T.concat [ "Please specify one or more items to pick up, as in "
                      , quoteColor
                      , dblQuote "get sword"
                      , dfltColor
                      , "." ]
getAction   (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "get" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, ri, _, ris, rc) = mkGetLookBindings i ws
        in if (not . null $ ris) || (rc /= mempty)
          then let (eiss, ecs)           = resolveRmInvCoins i ws as ris rc
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Get ri i) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Get ri i) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i "You don't see anything here to pick up.", [])
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
        let (d, s, p, originRi, (i `delete`) -> originIs) = mkCapStdDesig i ws
            originRm                                      = (ws^.rmTbl) ! originRi
        in case findExit originRm dir of
          Nothing -> putTMVar t ws >> (return . Left $ sorry)
          Just (linkTxt, destRi, mom, mdm)
            | p'          <- p & rmId .~ destRi
            , destRm      <- (ws^.rmTbl)  ! destRi
            , destIs      <- (ws^.invTbl) ! destRi
            , destIs'     <- sortInv ws $ destIs ++ [i]
            , originPis   <- i `delete` pcIds d
            , destPis     <- findPCIds ws destIs
            , msgAtOrigin <- nlnl $ case mom of
                               Nothing -> T.concat [ serialize d, " ", verb, " ", expandLinkName dir, "." ]
                               Just f  -> f . serialize $ d
            , msgAtDest   <- let d' = mkSerializedNonStdDesig i ws s A
                             in nlnl $ case mdm of
                               Nothing -> T.concat [ d', " arrives from ", expandOppLinkName dir, "." ]
                               Just f  -> f d'
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
    mkHelpNames styleds   = [ pad padding $ styled <> if isAdminHelp h then asterisk else "" | (styled, h) <- styleds ]
    padding               = maxHelpTopicLen + 2
    asterisk              = asteriskColor <> "*" <> dfltColor
    formatHelpNames names = let wordsPerLine = cols `div` padding
                            in T.unlines . map T.concat . chunksOf wordsPerLine $ names
    footnote hs           = if any isAdminHelp hs
      then nl' $ asterisk <> " indicates help that is available only to administrators."
      else ""
help (LowerNub i mq cols as) = mkHelpData i >>= \hs -> do
    (map (parseHelpTxt cols) -> helpTxts, dropBlanks -> hns) <- unzip <$> mapM (getHelpByName cols hs) as
    unless (null hns) (logPla "help" i $ "read help on: " <> T.intercalate ", " hns)
    pager i mq . intercalate [ "", mkDividerTxt cols, "" ] $ helpTxts
help p = patternMatchFail "help" [ showText p ]


mkHelpData :: Id -> MudStack [Help]
mkHelpData i = getPlaIsAdmin i >>= \ia -> do
    [ plaHelpCmdNames, plaHelpTopicNames, adminHelpCmdNames, adminHelpTopicNames ] <- mapM getHelpDirectoryContents helpDirs
    let plaHelpCmds     = [ Help (T.pack                  phcn) (plaHelpCmdsDir   ++ phcn) True  False | phcn <- plaHelpCmdNames     ]
    let plaHelpTopics   = [ Help (T.pack                  phtn) (plaHelpTopicsDir ++ phtn) False False | phtn <- plaHelpTopicNames   ]
    let adminHelpCmds   = [ Help (T.pack $ adminCmdChar : whcn) (adminHelpCmdsDir ++ whcn) True  True  | whcn <- adminHelpCmdNames   ]
    let adminHelpTopics = [ Help (T.pack                whtn) (adminHelpTopicsDir ++ whtn) False True  | whtn <- adminHelpTopicNames ]
    return $ plaHelpCmds ++ plaHelpTopics ++ if ia then adminHelpCmds ++ adminHelpTopics else []
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
    found hn        | h <- head . filter ((== hn) . helpName) $ hs =
                        (,) <$> readHelpFile (hn, helpFilePath h) <*> (return . dblQuote $ hn)
    readHelpFile (hn, hf) = (try . liftIO . T.readFile $ hf) >>= eitherRet handler
      where
        handler e = do
            fileIOExHandler "getHelpByName readHelpFile" e
            return . wrapUnlines cols $ "Unfortunately, the " <> dblQuote hn <> " help file could not be retrieved."


-----


intro :: Action
intro (NoArgs i mq cols) = do
    intros <- getPCIntroduced i
    if null intros
      then let introsTxt = "No one has introduced themselves to you yet." in do
          wrapSend mq cols introsTxt
          logPlaOut "intro" i [introsTxt]
      else let introsTxt = T.intercalate ", " intros in do
          multiWrapSend mq cols [ "You know the following names:", introsTxt ]
          logPlaOut "intro" i [introsTxt]
intro (LowerNub' i as) = helper >>= \(cbs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "intro" i logMsgs
    bcast . map fromClassifiedBroadcast . sort $ cbs
  where
    helper = onWS $ \(t, ws) ->
        let (view sing -> s)         = (ws^.entTbl)   ! i
            (view rmId -> ri)        = (ws^.pcTbl)    ! i
            is@((i `delete`) -> is') = (ws^.invTbl)   ! ri
            c                        = (ws^.coinsTbl) ! ri
        in if (not . null $ is') || (c /= mempty)
          then let (eiss, ecs)           = resolveRmInvCoins i ws as is' c
                   (ws', cbs,  logMsgs ) = foldl' (helperIntroEitherInv s is) (ws, [],  []     ) eiss
                   (     cbs', logMsgs') = foldl' helperIntroEitherCoins      (    cbs, logMsgs) ecs
               in putTMVar t ws' >> return (cbs', logMsgs')
          else do
              putTMVar t ws
              return (mkNTBroadcast i . nlnl $ "You don't see anyone here to introduce yourself to.", [])
    helperIntroEitherInv _ _   a (Left msg) | T.null msg = a
                                            | otherwise  = over _2 (++ (mkNTBroadcast i . nlnl $ msg)) a
    helperIntroEitherInv s ris a (Right is) = foldl' tryIntro a is
      where
        tryIntro a'@(ws, _, _) targetId | targetType                <- (ws^.typeTbl) ! targetId
                                        , (view sing -> targetSing) <- (ws^.entTbl)  ! targetId = case targetType of
          PCType | targetPC@(view introduced -> intros)  <- (ws^.pcTbl)  ! targetId
                 , pis                                   <- findPCIds ws ris
                 , targetDesig                           <- serialize . mkStdDesig targetId ws targetSing False $ ris
                 , (views sex mkReflexive -> himHerself) <- (ws^.mobTbl) ! i
                 -> if s `elem` intros
                   then let msg = nlnl $ "You've already introduced yourself to " <> targetDesig <> "."
                        in over _2 (++ mkNTBroadcast i msg) a'
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
                        in set _1 ws' . over _2 (++ cbs) . over _3 (++ [logMsg]) $ a'
          _      | b <- NonTargetBroadcast (nlnl $ "You can't introduce yourself to a " <> targetSing <> ".", [i])
                 -> over _2 (`appendIfUnique` b) a'
    helperIntroEitherCoins a (Left  msgs) =
        over _1 (++ concat [ mkNTBroadcast i . nlnl $ msg | msg <- msgs ]) a
    helperIntroEitherCoins a (Right _   ) =
        over _1 (`appendIfUnique` NonTargetBroadcast (nlnl "You can't introduce yourself to a coin.", [i])) a
    fromClassifiedBroadcast (TargetBroadcast    b) = b
    fromClassifiedBroadcast (NonTargetBroadcast b) = b
intro p = patternMatchFail "intro" [ showText p ]


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (NoArgs i mq cols) = getEnt' i >>= \(ws, e) ->
    send mq . nl . mkInvCoinsDesc i cols ws i $ e
inv (LowerNub i mq cols as) = getInvCoins' i >>= \(ws, (is, c)) ->
    send mq $ if (not . null $ is) || (c /= mempty)
      then let (eiss, ecs) = resolvePCInvCoins i ws as is c
               invDesc     = foldl' (helperEitherInv ws) "" eiss
               coinsDesc   = foldl' helperEitherCoins    "" ecs
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYourHandsAreEmpty
  where
    helperEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
inv p = patternMatchFail "inv" [ showText p ]


-----


look :: Action
look (NoArgs i mq cols) = getPCRmIdRm' i >>= \(ws, (ri, r)) ->
    let primary = multiWrap cols [ T.concat [ underline, " ", r^.rmName, " ", noUnderline ], r^.rmDesc ]
        suppl   = mkExitsSummary cols r <>  mkRmInvCoinsDesc i cols ws ri
    in send mq . nl $ primary <> suppl
look (LowerNub i mq cols as) = helper >>= \case
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
          bcast . foldr f [] $ ds
          send mq msg
          forM_ [ fromJust . stdPCEntSing $ targetDesig | targetDesig <- ds ] $ \es ->
              logPla "look" i ("looked at " <> es <> ".")
  where
    helper = onWS $ \(t, ws) ->
        let (d, _, ris, ris', rc) = mkGetLookBindings i ws
        in if (not . null $ ris') || (rc /= mempty)
          then let (eiss, ecs) = resolveRmInvCoins i ws as ris' rc
                   invDesc     = foldl' (helperLookEitherInv ws) "" eiss
                   coinsDesc   = foldl' helperLookEitherCoins    "" ecs
                   ds          = [ let (view sing -> s) = (ws^.entTbl) ! pi
                                   in mkStdDesig pi ws s False ris | pi <- extractPCIdsFromEiss ws eiss ]
               in putTMVar t ws >> return (Right $ invDesc <> coinsDesc, Just (d, ds))
          else    putTMVar t ws >> return ( Left . wrapUnlinesNl cols $ "You don't see anything here to look at."
                                          , Nothing )
    helperLookEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperLookEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
look p = patternMatchFail "look" [ showText p ]


mkRmInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> T.Text
mkRmInvCoinsDesc i cols ws ri | ((i `delete`) -> ris) <- (ws^.invTbl) ! ri
                              , (pcNcbs, otherNcbs)   <- splitPCsOthers . mkIsPC_StyledNameCountBothList i ws $ ris
                              , pcDescs    <- T.unlines . concatMap (wrapIndent 2 cols . mkPCDesc   ) $ pcNcbs
                              , otherDescs <- T.unlines . concatMap (wrapIndent 2 cols . mkOtherDesc) $ otherNcbs
                              , c          <- (ws^.coinsTbl) ! ri
                              = (if not . null $ pcNcbs    then pcDescs               else "") <>
                                (if not . null $ otherNcbs then otherDescs            else "") <>
                                (if c /= mempty            then mkCoinsSummary cols c else "")
  where
    splitPCsOthers                       = over both (map snd) . span fst
    mkPCDesc    (en, c, (s, _)) | c == 1 = (<> en) . (<> " ") $ if isKnownPCSing s
                                             then knownNameColor   <> s       <> dfltColor
                                             else unknownNameColor <> aOrAn s <> dfltColor
    mkPCDesc    (en, c, b     )          = T.concat [ unknownNameColor
                                                    , showText c
                                                    , " "
                                                    , mkPlurFromBoth b
                                                    , dfltColor
                                                    , " "
                                                    , en ]
    mkOtherDesc (en, c, (s, _)) | c == 1 = aOrAn s <> " " <> en
    mkOtherDesc (en, c, b     )          = T.concat [ showText c, " ", mkPlurFromBoth b, " ", en ]


mkIsPC_StyledNameCountBothList :: Id -> WorldState -> Inv -> [(Bool, (T.Text, Int, BothGramNos))]
mkIsPC_StyledNameCountBothList i ws is | ips   <-                        [ (ws^.typeTbl) ! i' == PCType | i' <- is ]
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
    readMotd  = return . frame cols . multiWrap cols . T.lines . colorizeFileTxt motdColor =<< T.readFile motdFile
    handler e = do
        fileIOExHandler "showMotd" e
        return . wrapUnlinesNl cols $ "Unfortunately, the message of the day could not be retrieved."


-----


plaDispCmdList :: Action
plaDispCmdList p@(LowerNub' i as) = logPlaExecArgs "?" as i >> dispCmdList plaCmds p
plaDispCmdList p                  = patternMatchFail "plaDispCmdList" [ showText p ]


-----


putAction :: Action
putAction p@AdviseNoArgs     = advise p ["put"] advice
  where
    advice = T.concat [ "Please specify one or more items you want to put, followed by where you want to put them, as \
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
putAction   (Lower' i as)    = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "put" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let (d, ris, rc, pis, pc, cn, argsWithoutCon) = mkPutRemBindings i ws as
      in if (not . null $ pis) || (pc /= mempty)
        then if T.head cn == rmChar && cn /= T.singleton rmChar
          then if not . null $ ris
            then shufflePut i (t, ws) d (T.tail cn) True argsWithoutCon ris rc pis pc procGecrMisRm
            else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
          else shufflePut i (t, ws) d cn False argsWithoutCon pis pc pis pc procGecrMisPCInv
        else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
putAction p = patternMatchFail "putAction" [ showText p ]


type CoinsWithCon = Coins
type PCInv        = Inv
type PCCoins      = Coins


shufflePut :: Id                                                  ->
              (TMVar WorldState, WorldState)                      ->
              PCDesig                                             ->
              ConName                                             ->
              IsConInRm                                           ->
              Args                                                ->
              InvWithCon                                          ->
              CoinsWithCon                                        ->
              PCInv                                               ->
              PCCoins                                             ->
              ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) ->
              STM ([Broadcast], [T.Text])
shufflePut i (t, ws) d cn icir as is c pis pc f | (gecrs, miss, rcs) <- resolveEntCoinNames i ws [cn] is c =
    if null miss && (not . null $ rcs)
      then putTMVar t ws >> return (mkBroadcast i "You can't put something inside a coin.", [])
      else case f . head . zip gecrs $ miss of
        Left  (mkBroadcast i -> bc)                                   -> putTMVar t ws >> return (bc, [])
        Right [ci] | e <- (ws^.entTbl) ! ci, t' <- (ws^.typeTbl) ! ci -> if t' /= ConType
          then putTMVar t ws >> return (mkBroadcast i $ "The " <> e^.sing <> " isn't a container.", [])
          else let (gecrs', miss', rcs') = resolveEntCoinNames i ws as pis pc
                   eiss                  = [ curry procGecrMisPCInv gecr mis | gecr <- gecrs' | mis <- miss' ]
                   ecs                   = map procReconciledCoinsPCInv rcs'
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


handleEgress :: Id -> MudStack () -- TODO: Notify admins.
handleEgress i = getPCRmId i >>= \ri -> do
    unless (ri == iWelcome) $ notifyEgress i
    wsTMVar  <- getWSTMVar
    mqtTMVar <- getNWSRec msgQueueTblTMVar
    ptTMVar  <- getNWSRec plaTblTMVar
    (parensQuote -> n, bs, logMsgs) <- liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        mqt <- takeTMVar mqtTMVar
        pt  <- takeTMVar ptTMVar
        -----
        let (view rmId -> ri')    = (ws^.pcTbl)  ! i
        let ((i `delete`) -> ris) = (ws^.invTbl) ! ri'
        let (view sing -> s)      = (ws^.entTbl) ! i
        let (pt', bs, logMsgs)    = peepHelper pt s
        -----
        let ws'                   = ws  & typeTbl.at  i   .~ Nothing
                                        & entTbl.at   i   .~ Nothing
                                        & invTbl.at   i   .~ Nothing
                                        & coinsTbl.at i   .~ Nothing
                                        & eqTbl.at    i   .~ Nothing
                                        & mobTbl.at   i   .~ Nothing
                                        & pcTbl.at    i   .~ Nothing
                                        & invTbl.at   ri' ?~ ris
        let mqt'                  = mqt & at i .~ Nothing
        let pt''                  = pt' & at i .~ Nothing
        -----
        putTMVar wsTMVar  ws'
        putTMVar mqtTMVar mqt'
        putTMVar ptTMVar  pt''
        return (s, bs, logMsgs)
    bcastNl bs
    forM_ logMsgs $ uncurry (logPla "handleEgress")
    logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", n, " has left the game." ]
    closePlaLog i
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
notifyEgress i = readWSTMVar >>= \ws ->
    let (d, _, _, _, _) = mkCapStdDesig i ws
        pis             = i `delete` pcIds d
    in bcast [(nlnl $ serialize d <> " has left the game.", pis)]


-----


ready :: Action
ready p@AdviseNoArgs     = advise p ["ready"] advice
  where
    advice = T.concat [ "Please specify one or more items to ready, as in "
                      , quoteColor
                      , dblQuote "ready sword"
                      , dfltColor
                      , "." ]
ready   (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "ready" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, _, is, c) = mkDropReadyBindings i ws
        in if (not . null $ is) || (c /= mempty)
          then let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols i ws as is mempty
                   eiss                      = [ curry procGecrMisReady gecr mis | gecr <- gecrs | mis <- miss ]
                   bs                        = if null rcs then [] else mkBroadcast i "You can't ready coins."
                   (ws', bs', logMsgs)       = foldl' (helperReady i d) (ws, bs, []) . zip eiss $ mrols
               in putTMVar t ws' >> return (bs', logMsgs)
          else    putTMVar t ws  >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
ready p = patternMatchFail "ready" [ showText p ]


helperReady :: Id                                     ->
               PCDesig                                ->
               (WorldState, [Broadcast], [T.Text])    ->
               (Either T.Text Inv, Maybe RightOrLeft) ->
               (WorldState, [Broadcast], [T.Text])
helperReady i d a (eis, mrol) = case eis of
  Left  (mkBroadcast i -> b) -> over _2 (++ b) a
  Right is                   -> foldl' (readyDispatcher i d mrol) a is


readyDispatcher :: Id                                  ->
                   PCDesig                             ->
                   Maybe RightOrLeft                   ->
                   (WorldState, [Broadcast], [T.Text]) ->
                   Id                                  ->
                   (WorldState, [Broadcast], [T.Text])
readyDispatcher i d mrol a@(ws, _, _) ei
  | e <- (ws^.entTbl)  ! ei
  , t <- (ws^.typeTbl) ! ei = case t of
    ClothType -> readyCloth i d mrol a ei e
    WpnType   -> readyWpn   i d mrol a ei e
    _         | b <- mkBroadcast i $ "You can't ready " <> aOrAn (e^.sing) <> "." -> over _2 (++ b) a


-- Readying clothing:


readyCloth :: Id                                  ->
              PCDesig                             ->
              Maybe RightOrLeft                   ->
              (WorldState, [Broadcast], [T.Text]) ->
              Id                                  ->
              Ent                                 ->
              (WorldState, [Broadcast], [T.Text])
readyCloth i d mrol a@(ws, _, _) ei e@(view sing -> s) =
    let em = (ws^.eqTbl)    ! i
        c  = (ws^.clothTbl) ! ei
    in case maybe (getAvailClothSlot ws i c em) (getDesigClothSlot ws e c em) mrol of
      Left  (mkBroadcast i -> b) -> over _2 (++ b) a
      Right slot                 -> moveReadiedItem i a em slot ei . mkReadyMsgs slot $ c
  where
    mkReadyMsgs (pp -> slot) = \case
        NoseC     -> putOnMsgs
        NeckC     -> putOnMsgs
        FingerC   -> (  T.concat [ "You slide the ", s, " on your ", slot, "." ]
                     , (T.concat [ serialize d, " slides ", aOrAn s, " on ", p, " ", slot, "." ], otherPCIds) )
        UpBodyC   -> donMsgs
        LowBodyC  -> donMsgs
        FullBodyC -> donMsgs
        BackC     -> putOnMsgs
        FeetC     -> putOnMsgs
        _         -> wearMsgs
      where
        putOnMsgs  = ( "You put on the " <> s <> "."
                     , (T.concat [ serialize d, " puts on ", aOrAn s, "." ], otherPCIds) )
        p          = views sex mkPossPronoun $ (ws^.mobTbl) ! i
        donMsgs    = ( "You don the " <> s <> "."
                     , (T.concat [ serialize d, " dons ", aOrAn s, "." ], otherPCIds) )
        otherPCIds = i `delete` pcIds d
        wearMsgs   = (  T.concat [ "You wear the ", s, " on your ", slot, "." ]
                     , (T.concat [ serialize d, " wears ", aOrAn s, " on ", p, " ", slot, "." ], otherPCIds) )


getAvailClothSlot :: WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot ws i c em | m <- (ws^.mobTbl) ! i, s <- m^.sex, h <- m^.hand = procMaybe $ case c of
  EarC    -> getEarSlotForSex s `mplus` (getEarSlotForSex . otherSex $ s)
  NoseC   -> findAvailSlot em noseSlots
  NeckC   -> findAvailSlot em neckSlots
  WristC  -> getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
  FingerC -> getRingSlot s h
  _       -> undefined -- TODO
  where
    procMaybe             = maybe (Left . sorryFullClothSlots $ c) Right
    getEarSlotForSex s    = findAvailSlot em $ case s of
      Male   -> lEarSlots
      Female -> rEarSlots
      _      -> patternMatchFail "getAvailClothSlot getEarSlotForSex"    [ showText s ]
    getWristSlotForHand h = findAvailSlot em $ case h of
      RHand  -> lWristSlots
      LHand  -> rWristSlots
      _      -> patternMatchFail "getAvailClothSlot getWristSlotForHand" [ showText h ]
    getRingSlot s h       = findAvailSlot em $ case s of
      Male    -> case h of
        RHand -> [ LRingFS, LIndexFS, RRingFS, RIndexFS, LMidFS, RMidFS, LPinkyFS, RPinkyFS ]
        LHand -> [ RRingFS, RIndexFS, LRingFS, LIndexFS, RMidFS, LMidFS, RPinkyFS, LPinkyFS ]
        _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
      Female  -> case h of
        RHand -> [ LRingFS, LIndexFS, RRingFS, RIndexFS, LPinkyFS, RPinkyFS, LMidFS, RMidFS ]
        LHand -> [ RRingFS, RIndexFS, LRingFS, LIndexFS, RPinkyFS, LPinkyFS, RMidFS, LMidFS ]
        _     -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText h ]
      _       -> patternMatchFail "getAvailClothSlot getRingSlot" [ showText s ]


otherSex :: Sex -> Sex
otherSex Male   = Female
otherSex Female = Male
otherSex NoSex  = NoSex


rEarSlots, lEarSlots, noseSlots, neckSlots, rWristSlots, lWristSlots :: [Slot]
rEarSlots   = [ REar1S, REar2S ]
lEarSlots   = [ LEar1S, LEar2S ]
noseSlots   = [ Nose1S, Nose2S ]
neckSlots   = [ Neck1S   .. Neck3S   ]
rWristSlots = [ RWrist1S .. RWrist3S ]
lWristSlots = [ LWrist1S .. LWrist3S ]


sorryFullClothSlots :: Cloth -> T.Text
sorryFullClothSlots c = "You can't wear any more " <> whatWhere c
  where
    whatWhere = \case
      EarC      -> aoy <> "ears."
      NoseC     -> "rings on your nose."
      NeckC     -> aoy <> "neck."
      WristC    -> aoy <> "wrists."
      FingerC   -> aoy <> "fingers."
      UpBodyC   -> coy <> "torso."
      LowBodyC  -> coy <> "legs."
      FullBodyC -> "clothing about your body."
      BackC     -> "on your back."
      FeetC     -> "footwear on your feet."
    aoy = "accessories on your "
    coy = "clothing on your "


getDesigClothSlot :: WorldState -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot ws (view sing -> s) c em rol
  | c `elem` [ NoseC, NeckC, UpBodyC, LowBodyC, FullBodyC, BackC, FeetC ] = Left sorryCantWearThere
  | isRingRol rol, c /= FingerC                                           = Left sorryCantWearThere
  | c == FingerC, not . isRingRol $ rol                                   = Left ringHelp
  | otherwise                                                             = case c of
    EarC    -> maybe (Left sorryFullEar)   Right (findSlotFromList rEarSlots   lEarSlots)
    WristC  -> maybe (Left sorryFullWrist) Right (findSlotFromList rWristSlots lWristSlots)
    FingerC -> maybe (Right slotFromRol)
                     (\i -> let e = (ws^.entTbl) ! i in Left . sorry slotFromRol $ e)
                     (em^.at slotFromRol)
    _       -> undefined -- TODO
  where
    sorryCantWearThere     = T.concat [ "You can't wear a ", s, " on your ", pp rol, "." ]
    findSlotFromList rs ls = findAvailSlot em $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot getSlotFromList"  [ showText rol ]
    sorryFullEar   = sorryFullClothSlotsOneSide . getSlotFromList rEarSlots   $ lEarSlots
    sorryFullWrist = sorryFullClothSlotsOneSide . getSlotFromList rWristSlots $ lWristSlots
    slotFromRol    = fromRol rol :: Slot
    sorry (pp -> slot) (view sing -> s') = T.concat [ "You're already wearing a "
                                                    , s'
                                                    , " on your "
                                                    , slot
                                                    , "." ]


sorryFullClothSlotsOneSide :: Slot -> T.Text
sorryFullClothSlotsOneSide (pp -> s) = "You can't wear any more on your " <> s <> "."


-- Readying weapons:


readyWpn :: Id                                  ->
            PCDesig                             ->
            Maybe RightOrLeft                   ->
            (WorldState, [Broadcast], [T.Text]) ->
            Id                                  ->
            Ent                                 ->
            (WorldState, [Broadcast], [T.Text])
readyWpn i d mrol a@(ws, _, _) ei e@(view sing -> s) | em  <- (ws^.eqTbl)  ! i
                                                     , w   <- (ws^.wpnTbl) ! ei
                                                     , sub <- w^.wpnSub = if not . isSlotAvail em $ BothHandsS
  then let b = mkBroadcast i "You're already wielding a two-handed weapon." in over _2 (++ b) a
  else case maybe (getAvailWpnSlot ws i em) (getDesigWpnSlot ws e em) mrol of
    Left  (mkBroadcast i -> b) -> over _2 (++ b) a
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
                       in over _2 (++ b) a
  where
    p          = views sex mkPossPronoun $ (ws^.mobTbl) ! i
    otherPCIds = i `delete` pcIds d


getAvailWpnSlot :: WorldState -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot ws i em | (view hand -> h@(otherHand -> h')) <- (ws^.mobTbl) ! i =
    maybe (Left "You're already wielding two weapons.")
          Right
          (findAvailSlot em . map getSlotForHand $ [ h, h' ])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> patternMatchFail "getAvailWpnSlot getSlotForHand" [ showText h ]


getDesigWpnSlot :: WorldState -> Ent -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigWpnSlot ws (view sing -> s) em rol
  | isRingRol rol = Left $ "You can't wield a " <> s <> " with your finger!"
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e = (ws^.entTbl) ! i in Left . sorry $ e)
                          (em^.at desigSlot)
  where
    sorry (view sing -> s') = T.concat [ "You're already wielding a "
                                       , s'
                                       , " with your "
                                       , pp desigSlot
                                       , "." ]
    desigSlot               = case rol of R -> RHandS
                                          L -> LHandS
                                          _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-----


remove :: Action
remove p@AdviseNoArgs     = advise p ["remove"] advice
  where
    advice = T.concat [ "Please specify one or more items to remove, followed by the container you want to remove \
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
remove   (Lower' i as)    = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "remove" i logMsgs
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


shuffleRem :: Id                                                  ->
              (TMVar WorldState, WorldState)                      ->
              PCDesig                                             ->
              ConName                                             ->
              IsConInRm                                           ->
              Args                                                ->
              InvWithCon                                          ->
              CoinsWithCon                                        ->
              ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv) ->
              STM ([Broadcast], [T.Text])
shuffleRem i (t, ws) d cn icir as is c f
  | (gecrs, miss, rcs) <- resolveEntCoinNames i ws [cn] is c = if null miss && (not . null $ rcs)
    then putTMVar t ws >> return (mkBroadcast i "You can't remove something from a coin.", [])
    else case f . head . zip gecrs $ miss of
      Left  msg -> putTMVar t ws >> return (mkBroadcast i msg, [])
      Right [ci] | e@(view sing -> s) <- (ws^.entTbl) ! ci, t' <- (ws^.typeTbl) ! ci ->
        if t' /= ConType
          then putTMVar t ws >> return (mkBroadcast i $ "The " <> s <> " isn't a container.", [])
          else let cis                   = (ws^.invTbl)   ! ci
                   cc                    = (ws^.coinsTbl) ! ci
                   (gecrs', miss', rcs') = resolveEntCoinNames i ws as cis cc
                   eiss                  = [ curry (procGecrMisCon s) gecr mis | gecr <- gecrs' | mis <- miss' ]
                   ecs                   = map (procReconciledCoinsCon s) rcs'
                   mnom                  = mkMaybeNthOfM icir ws ci e is
                   (ws',  bs,  logMsgs)  = foldl' (helperPutRemEitherInv   i d Rem mnom ci i e) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Rem mnom ci i e) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
      Right _ -> do
          putTMVar t ws
          return (mkBroadcast i "You can only remove things from one container at a time.", [])


-----


unready :: Action
unready p@AdviseNoArgs     = advise p ["unready"] advice
  where
    advice = T.concat [ "Please specify one or more items to unready, as in "
                      , quoteColor
                      , dblQuote "unready sword"
                      , dfltColor
                      , "." ]
unready   (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "unready" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (d, _, _, _, _) = mkCapStdDesig i ws
            em              = (ws^.eqTbl) ! i
            is              = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs)  = resolveEntCoinNames i ws as is mempty
                   eiss                = [ curry procGecrMisPCEq gecr mis | gecr <- gecrs | mis <- miss ]
                   bs                  = if null rcs then [] else mkBroadcast i "You can't unready coins."
                   (ws', bs', logMsgs) = foldl' (helperUnready i d em) (ws, bs, []) eiss
               in putTMVar t ws' >> return (bs', logMsgs)
          else    putTMVar t ws  >> return (mkBroadcast i dudeYou'reNaked, [])
unready p = patternMatchFail "unready" [ showText p ]


helperUnready :: Id                                  ->
                 PCDesig                             ->
                 EqMap                               ->
                 (WorldState, [Broadcast], [T.Text]) ->
                 Either T.Text Inv                   ->
                 (WorldState, [Broadcast], [T.Text])
helperUnready i d em a@(ws, _, _) = \case
  Left  (mkBroadcast i -> b) -> over _2 (++ b) a
  Right is | pis        <- (ws^.invTbl) ! i
           , ws'        <- ws & eqTbl.at  i ?~ M.filter (`notElem` is) em
                              & invTbl.at i ?~ (sortInv ws . (pis ++) $ is)
           , (bs, msgs) <- mkUnreadyDescs i ws' d is
           -> set _1 ws' . over _2 (++ bs) . over _3 (++ msgs) $ a


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
        EarC                -> mkVerbRemove p
        NoseC               -> mkVerbRemove p
        UpBodyC             -> mkVerbDoff   p
        LowBodyC            -> mkVerbDoff   p
        FullBodyC           -> mkVerbDoff   p
        _     | p == SndPer -> "take off"
              | otherwise   -> "takes off"
      WpnType | p == SndPer -> "stop wielding"
              | otherwise   -> "stops wielding"
      _                     -> undefined -- TODO
    mkVerbRemove = \case SndPer -> "remove"
                         ThrPer -> "removes"
    mkVerbDoff   = \case SndPer -> "doff"
                         ThrPer -> "doffs"
    otherPCIds   = i `delete` pcIds d


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
                                       Just rut -> if ut > rut then mkNewRecTxt else mkRecTxt rut
    mkUptimeTxt                = mkTxtHelper "."
    mkNewRecTxt                = mkTxtHelper . T.concat $ [ " - "
                                                          , newRecordColor
                                                          , "it's a new record!"
                                                          , dfltColor ]
    mkRecTxt (renderIt -> rut) = mkTxtHelper $ " (record uptime: " <> rut <> ")."
    mkTxtHelper                = ("Up " <>) . (renderIt ut <>)
    renderIt                   = T.pack . renderSecs . toInteger


getRecordUptime :: MudStack (Maybe Int)
getRecordUptime = (liftIO . doesFileExist $ uptimeFile) >>= \case
  True  -> liftIO readUptime `catch` (\e -> fileIOExHandler "getRecordUptime" e >> return Nothing)
  False -> return Nothing
  where
    readUptime = Just . read <$> readFile uptimeFile


getUptime :: MudStack Int
getUptime = (-) <$> (sec <$> (liftIO . getTime $ Monotonic)) <*> (sec <$> getNWSRec startTime)


-----


-- TODO: Disambiguate player names?
what :: Action
what p@AdviseNoArgs            = advise p ["what"] advice
  where
    advice = T.concat [ "Please specify one or more abbreviations to disambiguate, as in "
                      , quoteColor
                      , dblQuote "what up"
                      , dfltColor
                      , "." ]
what   (LowerNub i mq cols as) = getPCRm' i >>= \(ws, r) ->
    logPlaExecArgs "what" as i >> (send mq . T.concat . map (helper ws r) $ as)
  where
    helper ws r n = nl . T.concat $ whatCmd cols r n : [ whatInv i cols ws it n | it <- [ PCInv, PCEq, RmInv ] ]
what p = patternMatchFail "what" [ showText p ]


whatCmd :: Cols -> Rm -> T.Text -> T.Text
whatCmd cols (mkCmdListWithNonStdRmLinks -> cmds) (T.toLower -> n@(whatQuote -> n')) =
    wrapUnlines cols . maybe notFound found . findFullNameForAbbrev n . filter isPlaCmd $ [ cmdName cmd | cmd <- cmds ]
  where
    isPlaCmd               = (`notElem` [ adminCmdChar, debugCmdChar ]) . T.head
    notFound               = n' <> " doesn't refer to any commands."
    found (dblQuote -> cn) = T.concat [ n', " may refer to the ", cn, " command." ]


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks (view rmLinks -> rls) =
    sortBy (compare `on` cmdName) $ plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) = Cmd { cmdName = cn, action = go cn, cmdDesc = "" }


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
      then let ebgns@(head -> h)         = take len [ getEffBothGramNos i ws i' | (view entId -> i') <- es ]
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
    (mkAdminListTxt i <$> readWSTMVar <*> readTMVarInNWS plaTblTMVar) >>= multiWrapSend mq cols
whoAdmin p = withoutArgs whoAdmin p


mkAdminListTxt :: Id -> WorldState -> IM.IntMap Pla -> [T.Text]
mkAdminListTxt i ws pt =
    let ais                         = [ pi | pi <- IM.keys pt, (pt ! pi)^.isAdmin ]
        (ais', self) | i `elem` ais = (i `delete` ais, selfColor <> view sing ((ws^.entTbl) ! i) <> dfltColor)
                     | otherwise    = (ais, "")
        aas                         = styleAbbrevs Don'tBracket . sort $ [ view sing $ (ws^.entTbl) ! ai | ai <- ais' ]
        aas'                        = dropBlanks $ self : aas
        footer                      = [ numOfAdmins ais <> " logged in." ]
    in if null aas' then footer else T.intercalate ", " aas' : footer
  where
    numOfAdmins (length -> noa) | noa == 1  = "1 administrator"
                                | otherwise = showText noa <> " administrators"


-----


whoAmI :: Action
whoAmI (NoArgs i mq cols) = do
    logPlaExec "whoami" i
    ((pp *** pp) . getSexRace i -> (s', r), s) <- getEntSing' i
    wrapSend mq cols . T.concat $ [ "You are ", knownNameColor, s, dfltColor, " (a ", s', " ", r, ")." ]
whoAmI p = withoutArgs whoAmI p
