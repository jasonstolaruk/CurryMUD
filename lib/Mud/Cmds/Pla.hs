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
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
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
import Control.Arrow ((***), first)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, _4, at, both, over, to)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (.~), (<>~), (?~), (.~), (^.))
import Control.Monad ((>=>), forM, forM_, guard, mplus, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit -- TODO: (($$), (=$))
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
import System.FilePath ((</>))
import System.Time.Utils (renderSecs)
import qualified Data.Conduit.Binary as CB -- TODO: (sourceFile)
import qualified Data.Conduit.Text as CT -- TODO
import qualified Data.IntMap.Lazy as IM (keys)
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
about (NoArgs i mq cols) = do -- TODO: Conduit.
    logPlaExec "about" i
    -- helper |$| try >=> eitherRet (\e -> fileIOExHandler "about" e >> sendGenericErrorMsg mq cols)
    runResourceT $ source $$ conduit =$ sink
  where
    -- helper = multiWrapSend mq cols =<< [ T.lines cont | cont <- liftIO . T.readFile $ aboutFile ]
    source  = CB.sourceFile aboutFile
    conduit = CT.decodeUtf8
    sink :: Sink T.Text (ResourceT (ReaderT MudData IO)) ()
    sink    = awaitForever $ lift . lift . multiWrapSend mq cols . T.lines
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
admin (MsgWithTarget i mq cols target msg) = ask >>= liftIO . atomically . helperSTM >>= \(et, mqt, pt) ->
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
    in maybe notFound found . findFullNameForAbbrev target . map snd $ aiss
  where
    helperSTM md = (,,) <$> readTVar (md^.entTblTVar)
                        <*> readTVar (md^.msgQueueTblTVar)
                        <*> readTVar (md^.plaTblTVar)
admin p = patternMatchFail "admin" [ showText p ]


mkAdminIdsSingsList :: Id -> EntTbl -> PlaTbl -> [(Id, Sing)]
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
dropAction (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "drop" i $ logMsgs
  where
    helperSTM md = (,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                             <*> readTVar (md^.entTblTVar)
                             <*> readTVar (md^.invTblTVar)
                             <*> readTVar (md^.mobTblTVar)
                             <*> readTVar (md^.msgQueueTblTVar)
                             <*> readTVar (md^.pcTblTVar)
                             <*> readTVar (md^.plaTblTVar)
                             <*> readTVar (md^.typeTblTVar) >>= \(ct, et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, ri, is, c) = mkDropReadyBindings i ct et it mt pcTbl tt
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (is, c)
          then let (eiss, ecs)          = resolvePCInvCoins i et mt pcTbl as is c
                   (it', bs,  logMsgs ) = foldl' (helperGetDropEitherInv i et mt pcTbl tt d Drop i ri) (it, [], []) eiss
                   (ct', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Drop i ri) (ct, bs, logMsgs) ecs
               in do
                   writeTVar (md^.coinsTblTVar) ct'
                   writeTVar (md^.invTblTVar)   it'
                   bcastNlSTM mt mqt pcTbl plaTbl bs'
                   return logMsgs'
          else wrapSendSTM mq cols "You don't see anything here to pick up." >> return []
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
  | any (`elem` args) [ enc, enc <> "'s" ] =
      ask >>= liftIO . atomically . helperSTM >>= \(et, it, mt, mqt, pcTbl, plaTbl, tt) ->
          let (d, s, _, _, _) = mkCapStdDesig plaId et it mt pcTbl tt
              toSelfMsg       = bracketQuote . T.replace enc s . formatMsgArgs $ args
              toSelfBrdcst    = (nlnl toSelfMsg, [plaId])
              toOthersMsg | c == emoteNameChar = T.concat [ serialize d, T.tail h, " ", T.unwords . tail $ args ]
                          | otherwise          = capitalizeMsg . T.unwords $ args
              toOthersMsg'    = T.replace enc (serialize d { isCap = False }) . punctuateMsg $ toOthersMsg
              toOthersBrdcst  = (nlnl toOthersMsg', plaId `delete` pcIds d)
          in logPlaOut "emote" plaId [toSelfMsg] >> bcast mt mqt pcTbl plaTbl [ toSelfBrdcst, toOthersBrdcst ]
  | any (enc `T.isInfixOf`) args = advise p ["emote"] advice
  | otherwise = ask >>= liftIO . atomically . helperSTM >>= \(et, it, mt, mqt, pcTbl, plaTbl, tt) ->
    let (d, s, _, _, _) = mkCapStdDesig plaId et it mt pcTbl tt
        msg             = punctuateMsg . T.unwords $ args
        toSelfMsg       = bracketQuote $ s <> " " <> msg
        toSelfBrdcst    = (nlnl toSelfMsg, [plaId])
        toOthersMsg     = serialize d <> " " <> msg
        toOthersBrdcst  = (nlnl toOthersMsg, plaId `delete` pcIds d)
    in logPlaOut "emote" plaId [toSelfMsg] >> bcast mt mqt pcTbl plaTbl [ toSelfBrdcst, toOthersBrdcst ]
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.msgQueueTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.plaTblTVar)
                            <*> readTVar (md^.typeTblTVar)
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
equip (NoArgs i mq cols) = ask >>= liftIO . atomically . helperSTM >>= \(entTbl, eqTbl, mt, pt) ->
    send mq . nl . mkEqDesc i cols entTbl eqTbl mt pt i (entTbl ! i) $ PCType
  where
    helperSTM md = (,,,) <$> readTVar (md^.entTblTVar)
                         <*> readTVar (md^.eqTblTVar)
                         <*> readTVar (md^.mobTblTVar)
                         <*> readTVar (md^.pcTblTVar)
equip (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \(ct, entTbl, eqTbl, it, mt, pt, tt) ->
    let em@(M.elems -> is) = eqTbl ! i
    in send mq $ if not . M.null $ em
      then let (gecrs, miss, rcs)              = resolveEntCoinNames i entTbl mt pt as is mempty
               eiss                            = zipWith (curry procGecrMisPCEq) gecrs miss
               helperEitherInv acc (Left  msg) = (acc <>) . wrapUnlinesNl cols $ msg
               helperEitherInv acc (Right is') = nl $ acc <> mkEntDescs i cols ct entTbl eqTbl it mt pt tt is'
               invDesc                         = foldl' helperEitherInv "" eiss
               coinsDesc | not . null $ rcs    = wrapUnlinesNl cols "You don't have any coins among your readied \
                                                                    \equipment."
                         | otherwise           = ""
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYou'reNaked
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.coinsTblTVar)
                            <*> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.eqTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.typeTblTVar)
equip p = patternMatchFail "equip" [ showText p ]


-----


exits :: Action
exits (NoArgs i mq cols) = ask >>= liftIO . atomically . helperSTM >>= \(rt, pt) ->
    let ri = (pt ! i)^.rmId
        r  = rt ! ri
    in logPlaExec "exits" i >> (send mq . nl . mkExitsSummary cols $ r)
  where
    helperSTM md = (,) <$> readTVar (md^.rmTblTVar)
                       <*> readTVar (md^.pcTblTVar)
exits p = withoutArgs exits p


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
getAction (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "get" i $ logMsgs
  where
    helperSTM md = (,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                             <*> readTVar (md^.entTblTVar)
                             <*> readTVar (md^.invTblTVar)
                             <*> readTVar (md^.mobTblTVar)
                             <*> readTVar (md^.msgQueueTblTVar)
                             <*> readTVar (md^.pcTblTVar)
                             <*> readTVar (md^.plaTblTVar)
                             <*> readTVar (md^.typeTblTVar) >>= \(ct, et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, ri, _, ris, rc) = mkGetLookBindings i ct et it mt pcTbl tt
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (ris, rc)
          then let (eiss, ecs)          = resolveRmInvCoins i et mt pcTbl as ris rc
                   (it', bs,  logMsgs ) = foldl' (helperGetDropEitherInv i et mt pcTbl tt d Get ri i) (it, [], []) eiss
                   (ct', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Get ri i) (ct, bs, logMsgs) ecs
               in do
                   writeTVar (md^.coinsTblTVar) ct'
                   writeTVar (md^.invTblTVar)   it'
                   bcastNlSTM mt mqt pcTbl plaTbl bs'
                   return logMsgs'
          else wrapSendSTM mq cols "You don't see anything here to pick up." >> return []
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
tryMove i mq cols dir = ask >>= liftIO . atomically . helperSTM >>= \case
  Left  msg    -> wrapSend mq cols msg
  Right logMsg -> do
      logPla "tryMove" i logMsg
      look ActionParams { plaId = i, plaMsgQueue = mq, plaCols = cols, args = [] }
  where
    helperSTM md = (,,,,,,,) <$> readTVar (md^.entTblTVar)
                             <*> readTVar (md^.invTblTVar)
                             <*> readTVar (md^.mobTblTVar)
                             <*> readTVar (md^.msgQueueTblTVar)
                             <*> readTVar (md^.pcTblTVar)
                             <*> readTVar (md^.plaTblTVar)
                             <*> readTVar (md^.rmTblTVar)
                             <*> readTVar (md^.typeTblTVar) >>= \(et, it, mt, mqt, pcTbl, plaTbl, rt, tt) ->
        let (originD, s, p, originRi, (i `delete`) -> originIs) = mkCapStdDesig i et it mt pcTbl tt
            originRm                                            = rt ! originRi
        in case findExit originRm dir of
          Nothing                          -> return . Left $ sorry
          Just (linkTxt, destRi, mom, mdm) ->
              let p'          = p & rmId .~ destRi
                  pcTbl'      = pcTbl & at i ?~ p'
                  destRm      = rt ! destRi
                  destIs      = it ! destRi
                  destIs'     = sortInv et tt $ destIs ++ [i]
                  originPis   = i `delete` pcIds originD
                  destPis     = findPCIds tt destIs
                  msgAtOrigin = nlnl $ case mom of
                                  Nothing -> T.concat [ serialize originD, " ", verb, " ", expandLinkName dir, "." ]
                                  Just f  -> f . serialize $ originD
                  msgAtDest   = let destD = mkSerializedNonStdDesig i mt pcTbl' s A
                                in nlnl $ case mdm of
                                  Nothing -> T.concat [ destD, " arrives from ", expandOppLinkName dir, "." ]
                                  Just f  -> f destD
                  logMsg      = T.concat [ "moved "
                                         , linkTxt
                                         , " from room "
                                         , showRm originRi originRm
                                         , " to room "
                                         , showRm destRi   destRm
                                         , "." ]
              in do
                  writeTVar (md^.pcTblTVar) pcTbl'
                  writeTVar (md^.invTblTVar) $ it & at originRi ?~ originIs & at destRi ?~ destIs'
                  bcastSTM mt mqt pcTbl' plaTbl [ (msgAtOrigin, originPis), (msgAtDest, destPis) ]
                  return . Right $ logMsg
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
help (NoArgs i mq cols) = (liftIO . T.readFile $ helpDir </> "root") |$| try >=> either handler helper
  where
    handler e = do
        fileIOExHandler "help" e
        wrapSend mq cols "Unfortunately, the root help file could not be retrieved."
    helper rootHelpTxt = ask >>= liftIO . mkHelpData i >>= \(sortBy (compare `on` helpName) -> hs) ->
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
help (LowerNub i mq cols as) = ask >>= liftIO . mkHelpData i >>= \hs -> do
    (map (parseHelpTxt cols) -> helpTxts, dropBlanks -> hns) <- unzip <$> forM as (getHelpByName cols hs)
    unless (null hns) . logPla "help" i . ("read help on: " <>) . T.intercalate ", " $ hns
    pager i mq . intercalate [ "", mkDividerTxt cols, "" ] $ helpTxts
help p = patternMatchFail "help" [ showText p ]


mkHelpData :: Id -> MudData -> IO [Help]
mkHelpData i md = helpDirs |$| mapM getHelpDirectoryContents >=> \[ plaHelpCmdNames
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
    ia <- getPlaFlag IsAdmin . (! i) <$> readTVarIO (md^.plaTblTVar)
    return $ phcs ++ phts ++ (guard ia >> ahcs ++ ahts)
  where
    helpDirs                     = [ plaHelpCmdsDir, plaHelpTopicsDir, adminHelpCmdsDir, adminHelpTopicsDir ]
    getHelpDirectoryContents dir = delete ".DS_Store" . drop 2 . sort <$> getDirectoryContents dir


parseHelpTxt :: Cols -> T.Text -> [T.Text]
parseHelpTxt cols = concat . wrapLines cols . T.lines . parseTokens


getHelpByName :: Cols -> [Help] -> HelpName -> MudStack (T.Text, T.Text)
getHelpByName cols hs name =
    maybe sorry found . findFullNameForAbbrev name $ [ helpName h | h <- hs ]
  where
    sorry           = return ("No help is available on " <> dblQuote name <> ".", "")
    found hn        = let h = head . filter ((== hn) . helpName) $ hs
                      in (,) <$> readHelpFile (hn, helpFilePath h) <*> (return . dblQuote $ hn)
    readHelpFile (hn, hf) = (liftIO . T.readFile $ hf) |$| try >=> eitherRet handler
      where
        handler e = do
            fileIOExHandler "getHelpByName readHelpFile" e
            return . wrapUnlines cols $ "Unfortunately, the " <> dblQuote hn <> " help file could not be retrieved."


-----


intro :: Action
intro (NoArgs i mq cols) = ask >>= \md -> do
    intros <- view introduced . (! i) <$> (liftIO . readTVarIO $ md^.pcTblTVar)
    if null intros
      then let introsTxt = "No one has introduced themselves to you yet." in do
          wrapSend mq cols introsTxt
          logPlaOut "intro" i [introsTxt]
      else let introsTxt = T.intercalate ", " intros in do
          multiWrapSend mq cols [ "You know the following names:", introsTxt ]
          logPlaOut "intro" i [introsTxt]
intro (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "intro" i $ logMsgs
  where
    helperSTM md = (,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                             <*> readTVar (md^.entTblTVar)
                             <*> readTVar (md^.invTblTVar)
                             <*> readTVar (md^.mobTblTVar)
                             <*> readTVar (md^.msgQueueTblTVar)
                             <*> readTVar (md^.pcTblTVar)
                             <*> readTVar (md^.plaTblTVar)
                             <*> readTVar (md^.typeTblTVar) >>= \(ct, et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let ri                       = (pcTbl ! i)^.rmId
            is@((i `delete`) -> is') = it ! ri
            c                        = ct ! ri
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (is', c)
          then let (eiss, ecs)           = resolveRmInvCoins i et mt pcTbl as is' c
                   (pcTbl', cbs,  logMsgs ) = foldl' (helperIntroEitherInv et mt tt is) (pcTbl, [],  []     ) eiss
                   (        cbs', logMsgs') = foldl' helperIntroEitherCoins             (       cbs, logMsgs) ecs
               in do
                 writeTVar (md^.pcTblTVar) pcTbl'
                 bcastSTM mt mqt pcTbl' plaTbl . map fromClassifiedBroadcast . sort $ cbs'
                 return logMsgs'
          else (wrapSendSTM mq cols . nlnl $ "You don't see anyone here to introduce yourself to.") >> return []
    helperIntroEitherInv _  _  _  _   a (Left msg) | T.null msg = a
                                                   | otherwise  = a & _2 <>~ (mkNTBroadcast i . nlnl $ msg)
    helperIntroEitherInv et mt tt ris a (Right is) = foldl' tryIntro a is
      where
        tryIntro a'@(pcTbl, _, _) targetId | targetType                <- tt ! targetId
                                           , (view sing -> targetSing) <- et ! targetId = case targetType of
          PCType | s                                    <- (et ! i)^.sing
                 , targetPC@(view introduced -> intros) <- pcTbl ! targetId
                 , pis                                  <- findPCIds tt ris
                 , targetDesig                          <- serialize . mkStdDesig targetId mt pcTbl tt targetSing False $ ris
                 , himHerself                           <- mkReflexPro $ (mt ! i)^.sex
                 -> if s `elem` intros
                   then let msg = nlnl $ "You've already introduced yourself to " <> targetDesig <> "."
                        in a' & _2 <>~ mkNTBroadcast i msg
                   else let p         = targetPC & introduced .~ sort (s : intros)
                            pcTbl'    = pcTbl & at targetId ?~ p
                            msg       = "You introduce yourself to " <> targetDesig <> "."
                            logMsg    = parsePCDesig i mt pcTbl' msg
                            srcMsg    = nlnl msg
                            srcDesig  = StdDesig { stdPCEntSing = Nothing
                                                 , isCap        = True
                                                 , pcEntName    = mkUnknownPCEntName i mt pcTbl'
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
                        in a' & _1 .~ pcTbl' & _2 <>~ cbs & _3 <>~ [logMsg]
          _      | msg <- "You can't introduce yourself to " <> aOrAnOnLower targetSing <> "."
                 , b   <- NonTargetBroadcast (nlnl msg, [i]) -> over _2 (`appendIfUnique` b) a'
    helperIntroEitherCoins a (Left  msgs) =
        a & _1 <>~ concat [ mkNTBroadcast i . nlnl $ msg | msg <- msgs ]
    helperIntroEitherCoins a (Right _   ) =
        first (`appendIfUnique` NonTargetBroadcast (nlnl "You can't introduce yourself to a coin.", [i])) a
    fromClassifiedBroadcast (TargetBroadcast    b) = b
    fromClassifiedBroadcast (NonTargetBroadcast b) = b
intro p = patternMatchFail "intro" [ showText p ]


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (NoArgs i mq cols) = ask >>= liftIO . atomically . helperSTM >>= \(ct, et, it, mt, pt) ->
    send mq . nl . mkInvCoinsDesc i cols ct et it mt pt i $ et ! i
  where
    helperSTM md = (,,,,) <$> readTVar (md^.coinsTblTVar)
                          <*> readTVar (md^.entTblTVar)
                          <*> readTVar (md^.invTblTVar)
                          <*> readTVar (md^.mobTblTVar)
                          <*> readTVar (md^.pcTblTVar)
inv (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \(ct, entTbl, eqTbl, it, mt, pt, tt) ->
    let c  = ct ! i
        is = it ! i
    in send mq $ if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (is, c)
      then let (eiss, ecs) = resolvePCInvCoins i entTbl mt pt as is c
               invDesc     = foldl' (helperEitherInv ct entTbl eqTbl it mt pt tt) "" eiss
               coinsDesc   = foldl' helperEitherCoins                             "" ecs
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYourHandsAreEmpty
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.coinsTblTVar)
                            <*> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.eqTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.typeTblTVar)
    helperEitherInv _  _      _     _  _  _  _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ct entTbl eqTbl it mt pt tt acc (Right is  ) = nl $ acc <> mkEntDescs i cols ct entTbl eqTbl it mt pt tt is
    helperEitherCoins acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperEitherCoins acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
inv p = patternMatchFail "inv" [ showText p ]


-----


look :: Action
look (NoArgs i mq cols) = ask >>= liftIO . atomically . helperSTM >>= \(ct, et, it, mt, pt, rt, tt) ->
    let ri = (pt ! i)^.rmId
        r  = rt ! ri
    in send mq . nl . T.concat $ multiWrap cols [ T.concat [ underlineANSI, " ", r^.rmName, " ", noUnderlineANSI ], r^.rmDesc ] : [ mkExitsSummary cols r, mkRmInvCoinsDesc i cols ct et it mt pt tt ri ]
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.coinsTblTVar)
                            <*> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.rmTblTVar)
                            <*> readTVar (md^.typeTblTVar)
look (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= maybeVoid helper
  where
    -- TODO: Shouldn't we do all our logging inside a single transaction?
    helper ds = forM_ [ fromJust . stdPCEntSing $ targetDesig | targetDesig <- ds ] $ \es ->
        logPla "look" i $ "looked at " <> es <> "."
    helperSTM md = (,,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                              <*> readTVar (md^.entTblTVar)
                              <*> readTVar (md^.eqTblTVar)
                              <*> readTVar (md^.invTblTVar)
                              <*> readTVar (md^.mobTblTVar)
                              <*> readTVar (md^.msgQueueTblTVar)
                              <*> readTVar (md^.pcTblTVar)
                              <*> readTVar (md^.plaTblTVar)
                              <*> readTVar (md^.typeTblTVar) >>= \(ct, entTbl, eqTbl, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, _, ris, ris', rc) = mkGetLookBindings i ct entTbl it mt pcTbl tt
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (ris', rc)
          then let (eiss, ecs)     = resolveRmInvCoins i entTbl mt pcTbl as ris' rc
                   invDesc         = foldl' (helperLookEitherInv ct entTbl eqTbl it mt pcTbl tt) "" eiss
                   coinsDesc       = foldl' helperLookEitherCoins                                "" ecs
                   msg             = invDesc <> coinsDesc
                   (plaTbl', msg') = firstLook i cols (plaTbl, msg)
                   ds              = [ mkStdDesig pi mt pcTbl tt s False ris | pi <- extractPCIdsFromEiss tt eiss
                                     , let s = (entTbl ! pi)^.sing ]
                   pis             = i `delete` pcIds d
                   d'              = serialize d
                   f targetDesig acc | targetId <- pcId targetDesig =
                       (nlnl $ d' <> " looks at you.", [targetId]) :
                       (nlnl . T.concat $ [ d', " looks at ", serialize targetDesig, "." ], targetId `delete` pis) :
                       acc
               in do
                   writeTVar (md^.plaTblTVar) plaTbl'
                   bcastSTM mt mqt pcTbl plaTbl' . foldr f [] $ ds
                   sendSTM mq msg'
                   return . Just $ ds
          else let msg             = wrapUnlinesNl cols "You don't see anything here to look at."
                   (plaTbl', msg') = firstLook i cols (plaTbl, msg)
               in writeTVar (md^.plaTblTVar) plaTbl' >> sendSTM mq msg' >> return Nothing
    helperLookEitherInv _  _      _     _  _  _     _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperLookEitherInv ct entTbl eqTbl it mt pcTbl tt acc (Right is  ) = nl $ acc <> mkEntDescs i cols ct entTbl eqTbl it mt pcTbl tt is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . multiWrapNl cols . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
look p = patternMatchFail "look" [ showText p ]


mkRmInvCoinsDesc :: Id -> Cols -> CoinsTbl -> EntTbl -> InvTbl -> MobTbl -> PCTbl -> TypeTbl -> Id -> T.Text
mkRmInvCoinsDesc i cols ct et it mt pt tt ri =
    let ((i `delete`) -> ris) = it ! ri
        (pcNcbs, otherNcbs)   = splitPCsOthers . mkIsPC_StyledName_Count_BothList i et mt pt tt $ ris
        pcDescs               = T.unlines . concatMap (wrapIndent 2 cols . mkPCDesc   ) $ pcNcbs
        otherDescs            = T.unlines . concatMap (wrapIndent 2 cols . mkOtherDesc) $ otherNcbs
        c                     = ct ! ri
    in (pcNcbs |!| pcDescs) <> (otherNcbs |!| otherDescs) <> (c |!| mkCoinsSummary cols c)
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


mkIsPC_StyledName_Count_BothList :: Id
                                 -> EntTbl
                                 -> MobTbl
                                 -> PCTbl
                                 -> TypeTbl
                                 -> Inv
                                 -> [(Bool, (T.Text, Int, BothGramNos))]
mkIsPC_StyledName_Count_BothList i et mt pt tt is =
  let ips   =                        [ tt ! i' == PCType               | i' <- is ]
      ens   = styleAbbrevs DoBracket [ getEffName        i et mt pt i' | i' <- is ]
      ebgns =                        [ getEffBothGramNos i et mt pt i' | i' <- is ]
      cs    = mkCountList ebgns
  in nub . zip ips . zip3 ens cs $ ebgns


firstLook :: Id
          -> Cols
          -> (PlaTbl, T.Text)
          -> (PlaTbl, T.Text)
firstLook i cols a@(pt, _) = let p = pt ! i in if getPlaFlag IsNotFirstLook p
  then a
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
           p'  = setPlaFlag IsNotFirstLook True p
           pt' = pt & at i ?~ p'
       in a & _1 .~ pt' & _2 <>~ wrapUnlinesNl cols msg


isKnownPCSing :: Sing -> Bool
isKnownPCSing (T.words -> ss) = case ss of [ "male",   _ ] -> False
                                           [ "female", _ ] -> False
                                           _               -> True


extractPCIdsFromEiss :: TypeTbl -> [Either T.Text Inv] -> [Id]
extractPCIdsFromEiss tt = foldl' helper []
  where
    helper acc (Left  _ )  = acc
    helper acc (Right is)  = acc ++ findPCIds tt is


-----


motd :: Action
motd (NoArgs i mq cols) = logPlaExec "motd" i >> showMotd mq cols
motd p                  = withoutArgs motd p


showMotd :: MsgQueue -> Cols -> MudStack ()
showMotd mq cols = send mq =<< helper
  where
    helper    = liftIO readMotd |$| try >=> eitherRet handler
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
putAction (Lower i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "put" i $ logMsgs
  where
    helperSTM md = (,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                             <*> readTVar (md^.entTblTVar)
                             <*> readTVar (md^.invTblTVar)
                             <*> readTVar (md^.mobTblTVar)
                             <*> readTVar (md^.msgQueueTblTVar)
                             <*> readTVar (md^.pcTblTVar)
                             <*> readTVar (md^.plaTblTVar)
                             <*> readTVar (md^.typeTblTVar) >>= \(ct, et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, ris, rc, pis, pc, cn, argsWithoutCon) = mkPutRemBindings i ct et it mt pcTbl tt as
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (pis, pc)
          then if T.head cn == rmChar && cn /= T.singleton rmChar
            then if not . null $ ris
              then shufflePutSTM i mq cols md ct et it mt mqt pcTbl plaTbl tt d (T.tail cn) True argsWithoutCon ris rc pis pc procGecrMisRm
              else wrapSendSTM mq cols "You don't see any containers here." >> return []
            else shufflePutSTM i mq cols md ct et it mt mqt pcTbl plaTbl tt d cn False argsWithoutCon pis pc pis pc procGecrMisPCInv
          else wrapSendSTM mq cols dudeYourHandsAreEmpty >> return []
putAction p = patternMatchFail "putAction" [ showText p ]


type CoinsWithCon = Coins
type PCInv        = Inv
type PCCoins      = Coins


shufflePutSTM :: Id
              -> MsgQueue
              -> Cols
              -> MudData
              -> CoinsTbl
              -> EntTbl
              -> InvTbl
              -> MobTbl
              -> MsgQueueTbl
              -> PCTbl
              -> PlaTbl
              -> TypeTbl
              -> PCDesig
              -> ConName
              -> IsConInRm
              -> Args
              -> InvWithCon
              -> CoinsWithCon
              -> PCInv
              -> PCCoins
              -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv)
              -> STM [T.Text]
shufflePutSTM i mq cols md ct et it mt mqt pcTbl plaTbl tt d cn icir as is c pis pc f =
    let (conGecrs, conMiss, conRcs) = resolveEntCoinNames i et mt pcTbl [cn] is c
    in if null conMiss && (not . null $ conRcs)
      then wrapSendSTM mq cols "You can't put something inside a coin." >> return []
      else case f . head . zip conGecrs $ conMiss of
        Left  (mkBroadcast i -> bc) -> bcastNlSTM mt mqt pcTbl plaTbl bc >> return []
        Right [ci] | e <- et ! ci, typ <- tt ! ci -> if typ /= ConType
          then wrapSendSTM mq cols (theOnLowerCap (e^.sing) <> " isn't a container.") >> return []
          else let (gecrs, miss, rcs)   = resolveEntCoinNames i et mt pcTbl as pis pc
                   eiss                 = zipWith (curry procGecrMisPCInv) gecrs miss
                   ecs                  = map procReconciledCoinsPCInv rcs
                   mnom                 = mkMaybeNthOfM icir et ci e is
                   (it', bs,  logMsgs ) = foldl' (helperPutRemEitherInv i et mt pcTbl tt d Put mnom i ci e)
                                                 (it, [], [])
                                                 eiss
                   (ct', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Put mnom i ci e) (ct, bs, logMsgs) ecs
               in do
                   writeTVar (md^.coinsTblTVar) ct'
                   writeTVar (md^.invTblTVar)   it'
                   bcastNlSTM mt mqt pcTbl plaTbl bs'
                   return logMsgs'
        Right _ -> sendSTM mq "You can only put things into one container at a time." >> return []


-----


quit :: Action
quit (NoArgs' i mq)                        = (liftIO . atomically . writeTQueue mq $ Quit) >> logPlaExec "quit" i
quit ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols msg
  where
    msg = "Type " <> dblQuote "quit" <> " with no arguments to quit the game."


handleEgress :: Id -> MudStack ()
handleEgress i = ask >>= liftIO . atomically . helperSTM >>= \(s, logMsgs) -> do
    forM_ logMsgs $ uncurry (logPla "handleEgress")
    logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", parensQuote s, " has left the game." ]
    closePlaLog i
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.msgQueueTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.plaTblTVar)
                            <*> readTVar (md^.typeTblTVar) >>= \(et, it, mt, mqt, pcTbl, plaTbl, tt) -> do
        let ri = (pcTbl ! i)^.rmId
        unless (ri == iWelcome) $ let (d, _, _, _, _) = mkCapStdDesig i et it mt pcTbl tt
                                      pis             = i `delete` pcIds d
                                  in bcastSTM mt mqt pcTbl plaTbl [(nlnl $ serialize d <> " has left the game.", pis)]
        let ris                    = i `delete` (it ! ri)
            s                      = (et ! i)^.sing
            (plaTbl', bs, logMsgs) = peepHelper plaTbl s
        bcastNlSTM mt mqt pcTbl plaTbl' bs
        bcastAdminsSTM mt mqt pcTbl plaTbl' $ s <> " has left the game."
        modifyTVar (md^.coinsTblTVar)    $ at i .~ Nothing
        modifyTVar (md^.entTblTVar)      $ at i .~ Nothing
        modifyTVar (md^.eqTblTVar)       $ at i .~ Nothing
        modifyTVar (md^.msgQueueTblTVar) $ at i .~ Nothing
        modifyTVar (md^.pcTblTVar)       $ at i .~ Nothing
        modifyTVar (md^.typeTblTVar)     $ at i .~ Nothing
        writeTVar (md^.invTblTVar) $ it      & at i .~ Nothing & at ri ?~ ris
        writeTVar (md^.plaTblTVar) $ plaTbl' & at i .~ Nothing
        return (s, logMsgs)
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
ready (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "ready" i $ logMsgs
  where
    helperSTM md = (,,,,,,,,,,,,) <$> readTVar (md^.armTblTVar) -- TODO: We don't really need to get all these tables here...?
                                  <*> readTVar (md^.clothTblTVar)
                                  <*> readTVar (md^.coinsTblTVar)
                                  <*> readTVar (md^.conTblTVar)
                                  <*> readTVar (md^.entTblTVar)
                                  <*> readTVar (md^.eqTblTVar)
                                  <*> readTVar (md^.invTblTVar)
                                  <*> readTVar (md^.mobTblTVar)
                                  <*> readTVar (md^.msgQueueTblTVar)
                                  <*> readTVar (md^.pcTblTVar)
                                  <*> readTVar (md^.plaTblTVar)
                                  <*> readTVar (md^.typeTblTVar)
                                  <*> readTVar (md^.wpnTblTVar) >>= \(armTbl, clothTbl, coinsTbl, conTbl, entTbl, eqTbl, it, mt, mqt, pcTbl, plaTbl, tt, wt) ->
        let (d, _, is, c) = mkDropReadyBindings i coinsTbl entTbl it mt pcTbl tt
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (is, c)
          then let (gecrs, mrols, miss, rcs)   = resolveEntCoinNamesWithRols i entTbl mt pcTbl as is mempty
                   eiss                        = zipWith (curry procGecrMisReady) gecrs miss
                   bs                          = rcs |!| mkBroadcast i "You can't ready coins."
                   (eqTbl', it', bs', logMsgs) = foldl' (helperReady i armTbl clothTbl conTbl entTbl mt tt wt d)
                                                        (eqTbl, it, bs, []) . zip eiss $ mrols
               in do
                   writeTVar (md^.eqTblTVar)  eqTbl'
                   writeTVar (md^.invTblTVar) it'
                   bcastNlSTM mt mqt pcTbl plaTbl bs'
                   return logMsgs
          else wrapSendSTM mq cols dudeYourHandsAreEmpty >> return []
ready p = patternMatchFail "ready" [ showText p ]


helperReady :: Id
            -> ArmTbl
            -> ClothTbl
            -> ConTbl
            -> EntTbl
            -> MobTbl
            -> TypeTbl
            -> WpnTbl
            -> PCDesig
            -> (EqTbl, InvTbl, [Broadcast], [T.Text])
            -> (Either T.Text Inv, Maybe RightOrLeft)
            -> (EqTbl, InvTbl, [Broadcast], [T.Text])
helperReady i armTbl clothTbl conTbl entTbl mt tt wt d a (eis, mrol) = case eis of
  Left  (mkBroadcast i -> b) -> a & _3 <>~ b
  Right is                   -> foldl' (readyDispatcher i armTbl clothTbl conTbl entTbl mt tt wt d mrol) a is


readyDispatcher :: Id
                -> ArmTbl
                -> ClothTbl
                -> ConTbl
                -> EntTbl
                -> MobTbl
                -> TypeTbl
                -> WpnTbl
                -> PCDesig
                -> Maybe RightOrLeft
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
                -> Id
                -> (EqTbl, InvTbl, [Broadcast], [T.Text])
readyDispatcher i armTbl clothTbl conTbl entTbl mt tt wt d mrol a ei =
    let e = entTbl ! ei in maybe (sorry e) (\f -> f d mrol a ei e) mf
  where
    mf = case tt ! ei of
      ClothType -> Just $ readyCloth i clothTbl entTbl mt
      ConType   -> toMaybe ((conTbl ! ei)^.isCloth) $ readyCloth i clothTbl entTbl mt
      WpnType   -> Just $ readyWpn i entTbl mt wt
      ArmType   -> Just $ readyArm i armTbl entTbl
      _         -> Nothing
    sorry e | b <- mkBroadcast i $ "You can't ready " <> aOrAn (e^.sing) <> "." = a & _3 <>~ b


-- Readying clothing:


readyCloth :: Id
           -> ClothTbl
           -> EntTbl
           -> MobTbl
           -> PCDesig
           -> Maybe RightOrLeft
           -> (EqTbl, InvTbl, [Broadcast], [T.Text])
           -> Id
           -> Ent
           -> (EqTbl, InvTbl, [Broadcast], [T.Text])
readyCloth i ct entTbl mt d mrol a@(eqTbl, _, _, _) ei e@(view sing -> s) =
    let em = eqTbl ! i
        c  = ct    ! ei
    in case maybe (getAvailClothSlot entTbl mt i c em) (getDesigClothSlot entTbl e c em) mrol of
      Left  (mkBroadcast i -> b) -> a & _3 <>~ b
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
        p          = mkPossPro $ (mt ! i)^.sex
        otherPCIds = i `delete` pcIds d


getAvailClothSlot :: EntTbl -> MobTbl -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot et mt i c em | m <- mt ! i, s <- m^.sex, h <- m^.hand = procMaybe $ case c of
  Earring  -> getEarringSlotForSex s `mplus` (getEarringSlotForSex . otherSex $ s)
  NoseRing -> findAvailSlot em noseRingSlots
  Necklace -> findAvailSlot em necklaceSlots
  Bracelet -> getBraceletSlotForHand h `mplus` (getBraceletSlotForHand . otherHand $ h)
  Ring     -> getRingSlot s h
  _        -> maybeSingleSlot em . clothToSlot $ c
  where
    procMaybe                = maybe (Left . sorryFullClothSlots et c $ em) Right
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


sorryFullClothSlots :: EntTbl -> Cloth -> EqMap -> T.Text
sorryFullClothSlots et c@(pp -> c') em
  | c `elem` [ Earring .. Ring ]               = "You can't wear any more " <> c'       <> "s."
  | c `elem` [ Skirt, Dress, Backpack, Cloak ] = "You're already wearing "  <> aOrAn c' <> "."
  | otherwise = let i = em^.at (clothToSlot c).to fromJust
                    e = et ! i
                in "You're already wearing " <> aOrAn (e^.sing) <> "."


getDesigClothSlot :: EntTbl -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot et (view sing -> s) c em rol
  | c `elem` [ NoseRing, Necklace ] ++ [ Shirt .. Cloak ] = Left sorryCan'tWearThere
  | isRingRol rol, c /= Ring                              = Left sorryCan'tWearThere
  | c == Ring, not . isRingRol $ rol                      = Left ringHelp
  | otherwise                                             = case c of
    Earring  -> maybe (Left  sorryEarring)  Right (findSlotFromList rEarringSlots  lEarringSlots)
    Bracelet -> maybe (Left  sorryBracelet) Right (findSlotFromList rBraceletSlots lBraceletSlots)
    Ring     -> maybe (Right slotFromRol)
                      (\i -> let e = et ! i in Left . sorryRing slotFromRol $ e)
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
         -> EntTbl
         -> MobTbl
         -> WpnTbl
         -> PCDesig
         -> Maybe RightOrLeft
         -> (EqTbl, InvTbl, [Broadcast], [T.Text])
         -> Id
         -> Ent
         -> (EqTbl, InvTbl, [Broadcast], [T.Text])
readyWpn i entTbl mt wt d mrol a@(eqTbl, _, _, _) ei e@(view sing -> s) =
    let em  = eqTbl ! i
        w   = wt    ! ei
        sub = w^.wpnSub
    in if not . isSlotAvail em $ BothHandsS
      then let b = mkBroadcast i "You're already wielding a two-handed weapon." in a & _3 <>~ b
      else case maybe (getAvailWpnSlot mt i em) (getDesigWpnSlot entTbl e em) mrol of
        Left  (mkBroadcast i -> b) -> a & _3 <>~ b
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
                           in a & _3 <>~ b
  where
    p          = mkPossPro $ (mt ! i)^.sex
    otherPCIds = i `delete` pcIds d


getAvailWpnSlot :: MobTbl -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot mt i em | (view hand -> h@(otherHand -> oh)) <- mt ! i =
    maybe (Left "You're already wielding two weapons.")
          Right
          (findAvailSlot em . map getSlotForHand $ [ h, oh ])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> patternMatchFail "getAvailWpnSlot getSlotForHand" [ showText h ]


getDesigWpnSlot :: EntTbl -> Ent -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigWpnSlot et (views sing aOrAn -> s) em rol
  | isRingRol rol = Left $ "You can't wield " <> s <> " with your finger!"
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e = et ! i in Left . sorry $ e)
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
         -> ArmTbl
         -> EntTbl
         -> PCDesig
         -> Maybe RightOrLeft
         -> (EqTbl, InvTbl, [Broadcast], [T.Text])
         -> Id
         -> Ent
         -> (EqTbl, InvTbl, [Broadcast], [T.Text])
readyArm i armTbl entTbl d mrol a@(eqTbl, _, _, _) ei (view sing -> s) =
    let em                   = eqTbl  ! i
        (view armSub -> sub) = armTbl ! ei
    in case maybe (getAvailArmSlot entTbl sub em) sorryCan'tWearThere mrol of
      Left  (mkBroadcast i -> b) -> a & _3 <>~ b
      Right slot                 -> moveReadiedItem i a em slot ei . mkReadyArmMsgs $ sub
  where
    sorryCan'tWearThere rol = Left . T.concat $ [ "You can't wear ", aOrAn s, " on your ", pp rol, "." ]
    mkReadyArmMsgs = \case
      Head   -> putOnMsgs                     i d s
      Hands  -> putOnMsgs                     i d s
      Feet   -> putOnMsgs                     i d s
      Shield -> mkReadyMsgs "ready" "readies" i d s
      _      -> donMsgs                       i d s


getAvailArmSlot :: EntTbl -> ArmSub -> EqMap -> Either T.Text Slot
getAvailArmSlot et sub em = procMaybe . maybeSingleSlot em . armSubToSlot $ sub
  where
    procMaybe        = maybe (Left sorryFullArmSlot) Right
    sorryFullArmSlot = let i = em^.at (armSubToSlot sub).to fromJust
                           e = et ! i
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
remove (Lower i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "remove" i $ logMsgs
  where
    helperSTM md = (,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                             <*> readTVar (md^.entTblTVar)
                             <*> readTVar (md^.invTblTVar)
                             <*> readTVar (md^.mobTblTVar)
                             <*> readTVar (md^.msgQueueTblTVar)
                             <*> readTVar (md^.pcTblTVar)
                             <*> readTVar (md^.plaTblTVar)
                             <*> readTVar (md^.typeTblTVar) >>= \(ct, et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, ris, rc, pis, pc, cn, argsWithoutCon) = mkPutRemBindings i ct et it mt pcTbl tt as
        in if T.head cn == rmChar && cn /= T.singleton rmChar
          then if not . null $ ris
            then shuffleRemSTM i mq cols md ct et it mt mqt pcTbl plaTbl tt d (T.tail cn) True argsWithoutCon ris rc procGecrMisRm
            else wrapSendSTM mq cols "You don't see any containers here." >> return []
          else shuffleRemSTM i mq cols md ct et it mt mqt pcTbl plaTbl tt d cn False argsWithoutCon pis pc procGecrMisPCInv
remove p = patternMatchFail "remove" [ showText p ]


shuffleRemSTM :: Id
              -> MsgQueue
              -> Cols
              -> MudData
              -> CoinsTbl
              -> EntTbl
              -> InvTbl
              -> MobTbl
              -> MsgQueueTbl
              -> PCTbl
              -> PlaTbl
              -> TypeTbl
              -> PCDesig
              -> ConName
              -> IsConInRm
              -> Args
              -> InvWithCon
              -> CoinsWithCon
              -> ((GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv)
              -> STM [T.Text]
shuffleRemSTM i mq cols md ct et it mt mqt pcTbl plaTbl tt d cn icir as is c f =
    let (conGecrs, conMiss, conRcs) = resolveEntCoinNames i et mt pcTbl [cn] is c
    in if null conMiss && (not . null $ conRcs)
      then wrapSendSTM mq cols "You can't remove something from a coin." >> return []
      else case f . head . zip conGecrs $ conMiss of
        Left  msg -> wrapSendSTM mq cols msg >> return []
        Right [ci] | e@(view sing -> s) <- et ! ci, typ <- tt ! ci ->
          if typ /= ConType
            then wrapSendSTM mq cols (theOnLowerCap s <> " isn't a container.") >> return []
            else let cis                  = it ! ci
                     cc                   = ct ! ci
                     (gecrs, miss, rcs)   = resolveEntCoinNames i et mt pcTbl as cis cc
                     eiss                 = [ curry (procGecrMisCon s) gecr mis | gecr <- gecrs | mis <- miss ]
                     ecs                  = map (procReconciledCoinsCon s) rcs
                     mnom                 = mkMaybeNthOfM icir et ci e is
                     (it',  bs,  logMsgs) = foldl' (helperPutRemEitherInv i et mt pcTbl tt d Rem mnom ci i e)
                                                   (it, [], [])
                                                   eiss
                     (ct', bs', logMsgs') = foldl' (helperPutRemEitherCoins i d Rem mnom ci i e) (ct, bs, logMsgs) ecs
                 in do
                     writeTVar (md^.coinsTblTVar) ct'
                     writeTVar (md^.invTblTVar)   it'
                     bcastNlSTM mt mqt pcTbl plaTbl bs'
                     return logMsgs'
        Right _ -> wrapSendSTM mq cols "You can only remove things from one container at a time." >> return []


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
        then ask >>= \md -> (liftIO . atomically . sayToSTM md (Just adverb) . T.tail $ rest) >>= maybeVoid (logPlaOut "say" i)
        else sorry adviceEmptySayTo
      | otherwise -> ask >>= \md -> (liftIO . atomically . simpleSayHelperSTM md (Just adverb) $ rest) >>= logPlaOut "say" i
  | T.head a == sayToChar, T.length a > 1 = if length args > 1
    then ask >>= \md -> (liftIO . atomically . sayToSTM md Nothing . T.tail . T.unwords $ args) >>= maybeVoid (logPlaOut "say" i)
    else sorry adviceEmptySayTo
  | otherwise = ask >>= \md -> (liftIO . atomically . simpleSayHelperSTM md Nothing . T.unwords $ args) >>= logPlaOut "say" i
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
    sorrySTM msg      = adviseSTM p ["say"] msg >> return Nothing
    sayToSTM md ma (T.words -> (target:rest@(r:_))) = (,,,,,,,) <$> readTVar (md^.coinsTblTVar)
                                                                <*> readTVar (md^.entTblTVar)
                                                                <*> readTVar (md^.invTblTVar)
                                                                <*> readTVar (md^.mobTblTVar)
                                                                <*> readTVar (md^.msgQueueTblTVar)
                                                                <*> readTVar (md^.pcTblTVar)
                                                                <*> readTVar (md^.plaTblTVar)
                                                                <*> readTVar (md^.typeTblTVar) >>= \(ct, et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, _, _, ri, ris@((i `delete`) -> ris')) = mkCapStdDesig i et it mt pcTbl tt
            c                                         = ct ! ri
        in if uncurry (||) . ((/= mempty) *** (/= mempty)) $ (ris', c)
          then case resolveRmInvCoins i et mt pcTbl [target] ris' c of
            (_,                    [ Left  [sorryMsg] ]) -> wrapSendSTM mq cols sorryMsg >> return Nothing
            (_,                    Right _:_           ) -> wrapSendSTM mq cols "You're talking to coins now?" >> return Nothing
            ([ Left sorryMsg    ], _                   ) -> wrapSendSTM mq cols sorryMsg >> return Nothing
            ([ Right (_:_:_)    ], _                   ) -> wrapSendSTM mq cols "Sorry, but you can only say something \
                                                                                \to one person at a time." >> return Nothing
            ([ Right [targetId] ], _                   ) ->
              let targetType = tt ! targetId
                  targetSing = (et ! targetId)^.sing
              in case targetType of
                PCType  | targetDesig <- serialize . mkStdDesig targetId mt pcTbl tt targetSing False $ ris
                        -> either sorrySTM (sayToHelper    mt mqt pcTbl plaTbl d targetId targetDesig) parseRearAdverb
                MobType -> either sorrySTM (sayToMobHelper mt mqt pcTbl plaTbl d targetSing)           parseRearAdverb
                _       -> wrapSendSTM mq cols ("You can't talk to " <> aOrAn targetSing <> ".") >> return Nothing
            x -> patternMatchFail "say sayTo" [ showText x ]
          else wrapSendSTM mq cols "You don't see anyone here to talk to." >> return Nothing
      where
        parseRearAdverb = case ma of
          Just adv -> Right (adv <> " ", "", formatMsg . T.unwords $ rest)
          Nothing  | T.head r == adverbOpenChar -> case parseAdverb . T.unwords $ rest of
                       Right (adverb, rest') -> Right ("", " " <> adverb, formatMsg rest')
                       Left sorryMsg         -> Left sorryMsg
                   | otherwise -> Right ("", "", formatMsg . T.unwords $ rest)
        sayToHelper mt mqt pcTbl plaTbl d targetId targetDesig (frontAdv, rearAdv, msg) =
            let toSelfMsg      = T.concat [ "You say ",            frontAdv, "to ", targetDesig, rearAdv, ", ", msg ]
                toSelfBrdcst   = (nlnl toSelfMsg, [i])
                toTargetMsg    = T.concat [ serialize d, " says ", frontAdv, "to you",           rearAdv, ", ", msg ]
                toTargetBrdcst = (nlnl toTargetMsg, [targetId])
                toOthersMsg    = T.concat [ serialize d, " says ", frontAdv, "to ", targetDesig, rearAdv, ", ", msg ]
                toOthersBrdcst = (nlnl toOthersMsg, pcIds d \\ [ i, targetId ])
            in do
                bcastSTM mt mqt pcTbl plaTbl [ toSelfBrdcst, toTargetBrdcst, toOthersBrdcst ]
                return . Just $ [ parsePCDesig i mt pcTbl toSelfMsg ]
        sayToMobHelper mt mqt pcTbl plaTbl d targetSing (frontAdv, rearAdv, msg) =
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
                (plaTbl', fms) = firstMobSay i plaTbl
            in do
                writeTVar (md^.plaTblTVar) plaTbl'
                bcastSTM mt mqt pcTbl plaTbl [ (nlnl toSelfMsg <> fms, [i]), toOthersBrdcst ]
                return . Just $ [ toSelfMsg ]
    sayToSTM _ ma msg             = patternMatchFail "say sayToSTM" [ showText ma, msg ] -- TODO: Elided md...
    formatMsg                     = dblQuote . capitalizeMsg . punctuateMsg
    simpleSayHelperSTM md ma rest = (,,,,,,) <$> readTVar (md^.entTblTVar)
                                             <*> readTVar (md^.invTblTVar)
                                             <*> readTVar (md^.mobTblTVar)
                                             <*> readTVar (md^.msgQueueTblTVar)
                                             <*> readTVar (md^.pcTblTVar)
                                             <*> readTVar (md^.plaTblTVar)
                                             <*> readTVar (md^.typeTblTVar) >>= \(et, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let adverb          = case ma of Nothing  -> ""
                                         Just adv -> " " <> adv
            (d, _, _, _, _) = mkCapStdDesig i et it mt pcTbl tt
            msg             = formatMsg rest
            toSelfMsg       = T.concat [ "You say", adverb, ", ", msg ]
            toSelfBrdcst    = (nlnl toSelfMsg, [i])
            toOthersMsg     = T.concat [ serialize d, " says", adverb, ", ", msg ]
            toOthersBrdcst  = (nlnl toOthersMsg, i `delete` pcIds d)
        in do
            bcastSTM mt mqt pcTbl plaTbl [ toSelfBrdcst, toOthersBrdcst ]
            return [toSelfMsg]
say p = patternMatchFail "say" [ showText p ]


firstMobSay :: Id -> PlaTbl -> (PlaTbl, T.Text)
firstMobSay i pt = let p = pt ! i in if getPlaFlag IsNotFirstMobSay p
  then (pt, "")
  else let msg = nlnl . T.concat $ [ hintANSI
                                   , "Hint:"
                                   , noHintANSI
                                   , " to communicate with non-player characters, use the "
                                   , dblQuote "ask"
                                   , " command. For example, to ask a city guard about crime, type "
                                   , quoteColor
                                   , dblQuote "ask guard crime"
                                   , dfltColor
                                   , "." ]
           p' = setPlaFlag IsNotFirstMobSay True p
       in (pt & at i ?~ p', msg)


-----


setAction :: Action
setAction (NoArgs i mq cols) = ask >>= \md -> do
    logPlaExecArgs "set" [] i
    multiWrapSend mq cols =<< [ [ pad 9 (n <> ": ") <> v | n <- names | v <- values ]
                              | p <- (! i) <$> (liftIO . readTVarIO $ md^.plaTblTVar)
                              , let values = map showText [ cols, p^.pageLines ]
                              , let names  = styleAbbrevs Don'tBracket settingNames ]
setAction (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "set" i $ logMsgs
  where
    helperSTM md = readTVar (md^.plaTblTVar) >>= \pt ->
        let (p', msgs, logMsgs) = foldl' helperSettings (pt ! i, [], []) as
        in do
            modifyTVar (md^.plaTblTVar) $ at i ?~ p'
            multiWrapSendSTM mq cols msgs
            return logMsgs
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
unready (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >>= \logMsgs ->
    unless (null logMsgs) . logPlaOut "unready" i $ logMsgs
  where
    helperSTM md = (,,,,,,,,,) <$> readTVar (md^.armTblTVar)
                               <*> readTVar (md^.clothTblTVar)
                               <*> readTVar (md^.entTblTVar)
                               <*> readTVar (md^.eqTblTVar)
                               <*> readTVar (md^.invTblTVar)
                               <*> readTVar (md^.mobTblTVar)
                               <*> readTVar (md^.msgQueueTblTVar)
                               <*> readTVar (md^.pcTblTVar)
                               <*> readTVar (md^.plaTblTVar)
                               <*> readTVar (md^.typeTblTVar) >>= \(armTbl, ct, entTbl, eqTbl, it, mt, mqt, pcTbl, plaTbl, tt) ->
        let (d, _, _, _, _) = mkCapStdDesig i entTbl it mt pcTbl tt
            em              = eqTbl ! i
            is              = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs)          = resolveEntCoinNames i entTbl mt pcTbl as is mempty
                   eiss                        = zipWith (curry procGecrMisPCEq) gecrs miss
                   bs                          = rcs |!| mkBroadcast i "You can't unready coins."
                   (eqTbl', it', bs', logMsgs) = foldl' (helperUnready i armTbl ct entTbl mt pcTbl tt d em) (eqTbl, it, bs, []) eiss
               in do
                   writeTVar (md^.eqTblTVar) eqTbl'
                   writeTVar (md^.invTblTVar) it'
                   bcastNlSTM mt mqt pcTbl plaTbl bs'
                   return logMsgs
          else wrapSendSTM mq cols dudeYou'reNaked >> return []
unready p = patternMatchFail "unready" [ showText p ]


helperUnready :: Id
              -> ArmTbl
              -> ClothTbl
              -> EntTbl
              -> MobTbl
              -> PCTbl
              -> TypeTbl
              -> PCDesig
              -> EqMap
              -> (EqTbl, InvTbl, [Broadcast], [T.Text])
              -> Either T.Text Inv
              -> (EqTbl, InvTbl, [Broadcast], [T.Text])
helperUnready i armTbl ct entTbl mt pt tt d em a@(eqTbl, it, _, _) = \case
  Left  (mkBroadcast i -> b) -> a & _3 <>~ b
  Right is | pis        <- it ! i
           , eqTbl'     <- eqTbl & at i ?~ M.filter (`notElem` is) em
           , it'        <- it    & at i ?~ sortInv entTbl tt (pis ++ is)
           , (bs, msgs) <- mkUnreadyDescs i armTbl ct entTbl mt pt tt d is
           -> a & _1 .~ eqTbl' & _2 .~ it' & _3 <>~ bs & _4 <>~ msgs


mkUnreadyDescs :: Id
               -> ArmTbl
               -> ClothTbl
               -> EntTbl
               -> MobTbl
               -> PCTbl
               -> TypeTbl
               -> PCDesig
               -> Inv
               -> ([Broadcast], [T.Text])
mkUnreadyDescs i armTbl ct et mt pt tt d is =
    first concat . unzip $ [ helper icb | icb <- mkIdCountBothList i et mt pt is ]
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
    mkVerb ei p =  case tt ! ei of
      ClothType -> case ct ! ei of
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
      ArmType -> case view armSub $ armTbl ! ei of
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


mkIdCountBothList :: Id -> EntTbl -> MobTbl -> PCTbl -> Inv -> [(Id, Int, BothGramNos)]
mkIdCountBothList i et mt pt is | ebgns <- [ getEffBothGramNos i et mt pt i' | i' <- is ], cs <- mkCountList ebgns =
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
getUptime = (-) <$> sec `fmap` (liftIO . getTime $ Monotonic) <*> sec `fmap` (view startTime <$> ask) -- TODO: sec?


-----


what :: Action
what p@AdviseNoArgs = advise p ["what"] advice
  where
    advice = T.concat [ "Please specify one or more abbreviations to disambiguate, as in "
                      , quoteColor
                      , dblQuote "what up"
                      , dfltColor
                      , "." ]
what (LowerNub i mq cols as) = ask >>= liftIO . atomically . helperSTM >> logPlaExecArgs "what" as i
  where
    helperSTM md = (,,,,,,) <$> readTVar (md^.coinsTblTVar)
                            <*> readTVar (md^.entTblTVar)
                            <*> readTVar (md^.eqTblTVar)
                            <*> readTVar (md^.invTblTVar)
                            <*> readTVar (md^.mobTblTVar)
                            <*> readTVar (md^.pcTblTVar)
                            <*> readTVar (md^.rmTblTVar) >>= \(ct, entTbl, eqTbl, it, mt, pt, rt) ->
        let ri       = (pt ! i)^.rmId
            r        = rt ! ri
            helper n = nl . T.concat $ whatCmd cols r n : [ whatInv i cols ct entTbl eqTbl it mt pt invType n | invType <- [ PCInv, PCEq, RmInv ] ]
        in sendSTM mq . T.concat . map helper $ as
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


whatInv :: Id -> Cols -> CoinsTbl -> EntTbl -> EqTbl -> InvTbl -> MobTbl -> PCTbl -> InvType -> T.Text -> T.Text
whatInv i cols ct entTbl eqTbl it mt pt invType n | (is, gecrs, rcs) <- resolveName = if not . null $ gecrs
  then whatInvEnts i cols entTbl mt pt invType n (head gecrs) is
  else T.concat . map (whatInvCoins cols invType n) $ rcs
  where
    resolveName | (is, c) <- getLocInvCoins, (gecrs, _, rcs) <- resolveEntCoinNames i entTbl mt pt [n] is c = (is, gecrs, rcs)
    getLocInvCoins = case invType of PCInv -> ((it !) *** (ct !)) . dup $ i
                                     PCEq  -> (M.elems $ eqTbl ! i, mempty)
                                     RmInv -> ((it !) *** (ct !)) . dup $ ri
    ri             = (pt ! i)^.rmId


whatInvEnts :: Id
            -> Cols
            -> EntTbl
            -> MobTbl
            -> PCTbl
            -> InvType
            -> T.Text
            -> GetEntsCoinsRes
            -> Inv
            -> T.Text
whatInvEnts i cols et mt pt invType@(getLocTxtForInvType -> locTxt) (whatQuote -> r) gecr is =
    wrapUnlines cols $ case gecr of
      Mult { entsRes = (Just es), .. }
        | nameSearchedFor == acp -> T.concat [ whatQuote acp
                                             , " may refer to everything "
                                             , locTxt
                                             , supplement
                                             , "." ]
        | e@(view sing -> s) <- head es, len <- length es -> if len > 1
          then let ebgns@(head -> h)         = take len [ getEffBothGramNos i et mt pt i'
                                                        | e' <- es, let i' = e'^.entId ]
                   target | all (== h) ebgns = mkPlurFromBoth h
                          | otherwise        = (<> "s") . bracketQuote . getEffName i et mt pt $ e^.entId
               in T.concat [ r
                           , " may refer to the "
                           , showText len
                           , " "
                           , target
                           , " "
                           , locTxt
                           , "." ]
          else let ens = [ getEffName i et mt pt i' | i' <- is ]
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
                                                                      , bracketQuote . getEffName i et mt pt $ e^.entId
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
    supplement | invType `elem` [ PCInv, RmInv ] = " " <> parensQuote "including any coins"
               | otherwise                       = ""
    checkFirst e ens | en <- getEffName i et mt pt $ e^.entId, matches <- filter (== en) ens =
        guard (length matches > 1) >> "first "


getLocTxtForInvType :: InvType -> T.Text
getLocTxtForInvType = \case PCInv -> "in your inventory"
                            PCEq  -> "in your readied equipment"
                            RmInv -> "in this room"


whatInvCoins :: Cols -> InvType -> T.Text -> ReconciledCoins -> T.Text
whatInvCoins cols invType@(getLocTxtForInvType -> locTxt) (whatQuote -> r) rc
  | invType == PCEq = ""
  | otherwise       = wrapUnlines cols $ case rc of
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
    supplementNone cn      = case invType of PCInv -> parensQuote $ "you don't have any "       <> cn
                                             RmInv -> parensQuote $ "there aren't any "         <> cn <> " here"
                                             PCEq  -> oops "supplementNone"
    supplementNotEnough cn = case invType of PCInv -> parensQuote $ "you don't have that many " <> cn
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
whoAdmin (NoArgs i mq cols) = ask >>= liftIO . atomically . helperSTM >> logPlaExec "whoadmin" i
  where
    helperSTM md = (,) <$> readTVar (md^.entTblTVar) <*> readTVar (md^.plaTblTVar) >>= \(et, pt) ->
        let ais                         = [ pi | pi <- IM.keys pt, getPlaFlag IsAdmin (pt ! pi) ]
            (ais', self) | i `elem` ais = (i `delete` ais, selfColor <> view sing (et ! i) <> dfltColor)
                         | otherwise    = (ais, "")
            aas                         = styleAbbrevs Don'tBracket [ s | ai <- ais'
                                                                        , let s = (et ! ai)^.sing, then sortWith by s ]
            aas'                        = dropBlanks $ self : aas
            footer                      = [ numOfAdmins ais <> " logged in." ]
        in multiWrapSendSTM mq cols (null aas' ? footer :? T.intercalate ", " aas' : footer)
      where
        numOfAdmins (length -> noa) | noa == 1  = "1 administrator"
                                    | otherwise = showText noa <> " administrators"
whoAdmin p = withoutArgs whoAdmin p


-----


whoAmI :: Action
whoAmI (NoArgs i mq cols) = ask >>= liftIO . atomically . helperSTM >> logPlaExec "whoami" i
  where
    helperSTM md = (,,) <$> readTVar (md^.entTblTVar)
                        <*> readTVar (md^.mobTblTVar)
                        <*> readTVar (md^.pcTblTVar) >>= \(et, mt, pt) ->
        let s         = (et ! i)^.sing
            (sexy, r) = (pp *** pp) ((mt ! i)^.sex, (pt ! i)^.race)
        in wrapSendSTM mq cols . T.concat $ [ "You are ", knownNameColor, s, dfltColor, " (a ", sexy, " ", r, ")." ]
whoAmI p = withoutArgs whoAmI p
