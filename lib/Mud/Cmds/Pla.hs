{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, ParallelListComp, PatternSynonyms, RecordWildCards, TupleSections, ViewPatterns #-}

module Mud.Cmds.Pla ( getRecordUptime
                    , getUptime
                    , handleEgress
                    , look
                    , mkCmdListWithNonStdRmLinks
                    , mkSerializedNonStdDesig
                    , plaCmds
                    , readFileExHandler
                    , showMotd ) where

import Mud.Cmds.Util
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util
import Mud.Logging hiding (logIOEx, logIOExRethrow, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import Mud.NameResolution
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Logging as L (logIOEx, logIOExRethrow, logNotice, logPla, logPlaExec, logPlaExecArgs, logPlaOut)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***), first)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (IOException)
import Control.Exception.Lifted (catch, try)
import Control.Lens (_1, _2, _3, at, both, folded, over, to)
import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((&), (?~), (.~), (^.), (^..))
import Control.Lens.Setter (set)
import Control.Monad (forM_, guard, mplus, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IntMap.Lazy ((!))
import Data.List (delete, elemIndex, find, foldl', intercalate, intersperse, nub, nubBy, sort, sortBy)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Monoid ((<>), mempty)
import Data.Text.Strict.Lens (packed)
import Data.Time (diffUTCTime, getCurrentTime)
import Prelude hiding (pi)
import System.Console.ANSI (clearScreenCode)
import System.Directory (doesFileExist, getDirectoryContents)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Time.Utils (renderSecs)
import qualified Data.Map.Lazy as M (elems, filter, null, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.Cmds.Pla"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Pla"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Pla"


logIOExRethrow :: T.Text -> IOException -> MudStack ()
logIOExRethrow = L.logIOExRethrow "Mud.Cmds.Pla"


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
    [ Cmd { cmdName = "?", action = plaDispCmdList, cmdDesc = "Display this command list." }
    , Cmd { cmdName = "about", action = about, cmdDesc = "About this MUD." }
    , Cmd { cmdName = "clear", action = clear, cmdDesc = "Clear the screen." }
    , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
    , Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop items on the ground." }
    , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
    , Cmd { cmdName = "equip", action = equip, cmdDesc = "Display readied equipment." }
    , Cmd { cmdName = "exits", action = exits, cmdDesc = "Display obvious exits." }
    , Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick items up off the ground." }
    , Cmd { cmdName = "help", action = help, cmdDesc = "Get help on topics or commands." }
    , Cmd { cmdName = "i", action = inv, cmdDesc = "Inventory." }
    , Cmd { cmdName = "intro", action = intro, cmdDesc = "Introduce yourself." }
    , Cmd { cmdName = "look", action = look, cmdDesc = "Look." }
    , Cmd { cmdName = "motd", action = motd, cmdDesc = "Display the message of the day." }
    , Cmd { cmdName = "n", action = go "n", cmdDesc = "Go north." }
    , Cmd { cmdName = "ne", action = go "ne", cmdDesc = "Go northeast." }
    , Cmd { cmdName = "nw", action = go "nw", cmdDesc = "Go northwest." }
    , Cmd { cmdName = "put", action = putAction, cmdDesc = "Put items in a container." }
    , Cmd { cmdName = "quit", action = quit, cmdDesc = "Quit." }
    , Cmd { cmdName = "ready", action = ready, cmdDesc = "Ready items." }
    , Cmd { cmdName = "remove", action = remove, cmdDesc = "Remove items from a container." }
    , Cmd { cmdName = "s", action = go "s", cmdDesc = "Go south." }
    , Cmd { cmdName = "se", action = go "se", cmdDesc = "Go southeast." }
    , Cmd { cmdName = "sw", action = go "sw", cmdDesc = "Go southwest." }
    , Cmd { cmdName = "u", action = go "u", cmdDesc = "Go up." }
    , Cmd { cmdName = "unready", action = unready, cmdDesc = "Unready items." }
    , Cmd { cmdName = "uptime", action = uptime, cmdDesc = "Display how long CurryMUD has been running." }
    , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." }
    , Cmd { cmdName = "what", action = what, cmdDesc = "Disambiguate abbreviations." } ]


-----


plaDispCmdList :: Action
plaDispCmdList p@(LowerNub' i as) = logPlaExecArgs "?" as i >> dispCmdList plaCmds p
plaDispCmdList p                  = patternMatchFail "plaDispCmdList" [ showText p ]


-----


about :: Action
about (NoArgs i mq cols) = do
    logPlaExec "about" i
    try helper >>= eitherRet (\e -> readFileExHandler "about" e >> sendGenericErrorMsg mq cols)
  where
    helper = multiWrapSend mq cols . T.lines =<< (liftIO . T.readFile $ aboutFile)
about p = withoutArgs about p


readFileExHandler :: T.Text -> IOException -> MudStack ()
readFileExHandler fn e
  | isDoesNotExistError e = logIOEx        fn e
  | isPermissionError   e = logIOEx        fn e
  | otherwise             = logIOExRethrow fn e


-----


clear :: Action
clear (NoArgs' i mq) = logPlaExec "clear" i >> (send mq . T.pack $ clearScreenCode)
clear p              = withoutArgs clear p


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
        let (view sing -> s) = (ws^.entTbl) ! i
            p                = (ws^.pcTbl)  ! i
            ri               = p^.rmId
            r                = (ws^.rmTbl)  ! ri
            originIs         = (ws^.invTbl) ! ri
        in case findExit r dir of
          Nothing -> putTMVar t ws >> (return . Left $ sorry)
          Just (linkTxt, ri', mom, mdm)
            | p'          <- p & rmId .~ ri'
            , r'          <- (ws^.rmTbl)  ! ri'
            , originIs'   <- i `delete` originIs
            , destIs      <- (ws^.invTbl) ! ri'
            , destIs'     <- sortInv ws $ destIs ++ [i]
            , originPis   <- findPCIds ws originIs'
            , destPis     <- findPCIds ws destIs
            , msgAtOrigin <- let d = serialize . mkStdDesig i ws s True $ originIs
                             in nlnl $ case mom of
                               Nothing -> T.concat [ d, " ", verb, " ", expandLinkName dir, "." ]
                               Just f  -> f d
            , msgAtDest   <- let d = mkSerializedNonStdDesig i ws s A
                             in nlnl $ case mdm of
                               Nothing -> T.concat [ d, " arrives from ", expandOppLinkName dir, "." ]
                               Just f  -> f d
            , logMsg      <- T.concat [ "moved "
                                      , linkTxt
                                      , " from room "
                                      , showRm ri r
                                      , " to room "
                                      , showRm ri' r'
                                      , "." ]
            -> do
                putTMVar t (ws & pcTbl.at  i   ?~ p'
                               & invTbl.at ri  ?~ originIs'
                               & invTbl.at ri' ?~ destIs')
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


linkDirToCmdName :: LinkDir -> CmdName
linkDirToCmdName North     = "n"
linkDirToCmdName Northeast = "ne"
linkDirToCmdName East      = "e"
linkDirToCmdName Southeast = "se"
linkDirToCmdName South     = "s"
linkDirToCmdName Southwest = "sw"
linkDirToCmdName West      = "w"
linkDirToCmdName Northwest = "nw"
linkDirToCmdName Up        = "u"
linkDirToCmdName Down      = "d"


mkStdDesig :: Id -> WorldState -> Sing -> Bool -> Inv -> PCDesig
mkStdDesig i ws s ic ris = StdDesig { stdPCEntSing = Just s
                                    , isCap        = ic
                                    , pcEntName    = mkUnknownPCEntName i ws
                                    , pcId         = i
                                    , pcIds        = findPCIds ws ris }


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


mkSerializedNonStdDesig :: Id -> WorldState -> Sing -> AOrThe -> T.Text
mkSerializedNonStdDesig i ws s (capitalize . pp -> aot) | (pp *** pp -> (s', r)) <- getSexRace i ws =
    serialize NonStdDesig { nonStdPCEntSing = s
                          , nonStdDesc      = T.concat [ aot, " ", s', " ", r ] }


-----


dropAction :: Action
dropAction p@AdviseNoArgs     = advise p ["drop"] $ "Please specify one or more things to drop, as \
                                                    \in " <> dblQuote "drop sword" <> "."
dropAction   (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "drop" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (view sing -> s)  = (ws^.entTbl)   ! i
            (view rmId -> ri) = (ws^.pcTbl)    ! i
            (pis, ris)        = over both ((ws^.invTbl) !) (i, ri)
            pc                = (ws^.coinsTbl) ! i
            d                 = mkStdDesig i ws s True ris
        in if (not . null $ pis) || (pc /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as pis pc
                   eiss                  = [ curry procGecrMisPCInv gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map procReconciledCoinsPCInv rcs
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Drop i ri) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Drop i ri) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
dropAction p = patternMatchFail "dropAction" [ showText p ]


-----


equip :: Action
equip (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let e = (ws^.entTbl) ! i
    in send mq . nl . mkEqDesc i cols ws i e $ PCType
equip (LowerNub i mq cols as) = readWSTMVar >>= \ws ->
    let em@(M.elems -> is) = (ws^.eqTbl) ! i
    in send mq $ if not . M.null $ em
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


mkEqDesc :: Id -> Cols -> WorldState -> Id -> Ent -> Type -> T.Text
mkEqDesc i cols ws ei (view sing -> s) t | descs <- map mkDesc . mkSlotNameIdList . M.toList $ (ws^.eqTbl) ! ei =
    case descs of [] -> none
                  _  -> (header <>) . T.unlines . concatMap (wordWrapIndent 15 cols) $ descs
  where
    mkSlotNameIdList = map (first pp)
    mkDesc (T.breakOn " finger" -> (sn, _), i')
      | e' <- (ws^.entTbl) ! i'
      , en <- if ei == i then (" " <>) . bracketQuote . fromJust $ e'^.entName else ""
      = parensPad 15 sn <> e'^.sing <> en
    none = wrapUnlines cols $ if
      | ei == i      -> dudeYou'reNaked
      | t  == PCType -> parsePCDesig i ws $ d <> " doesn't have anything readied."
      | otherwise    -> "The " <> s <> " doesn't have anything readied."
    header = wrapUnlines cols $ if
      | ei == i      -> "You have readied the following equipment:"
      | t  == PCType -> parsePCDesig i ws $ d <> " has readied the following equipment:"
      | otherwise    -> "The " <> s <> " has readied the following equipment:"
    d = mkSerializedNonStdDesig ei ws s The


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


mkEntDescs :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntDescs i cols ws is = T.intercalate "\n" . map (mkEntDesc i cols ws) $ [ (i', (ws^.entTbl) ! i') | i' <- is ]


mkEntDesc :: Id -> Cols -> WorldState -> (Id, Ent) -> T.Text
mkEntDesc i cols ws (i'@(((ws^.typeTbl) !) -> t), e@(views entDesc (wrapUnlines cols) -> ed)) =
    case t of ConType ->                 (ed <>) . mkInvCoinsDesc i cols ws i' $ e
              MobType ->                 (ed <>) . mkEqDesc       i cols ws i'   e $ t
              PCType  -> (pcHeader <>) . (ed <>) . mkEqDesc       i cols ws i'   e $ t
              _       -> ed
  where
    pcHeader = wrapUnlines cols . mkPCDescHeader i' $ ws


mkInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> Ent -> T.Text
mkInvCoinsDesc i cols ws i' (view sing -> s) | is <- (ws^.invTbl)   ! i'
                                             , c  <- (ws^.coinsTbl) ! i' = case (not . null $ is, c /= mempty) of
  (False, False) -> wrapUnlines cols $ if i' == i then dudeYourHandsAreEmpty else "The " <> s <> " is empty."
  (True,  False) -> header <> mkEntsInInvDesc i cols ws is
  (False, True ) -> header <>                                 mkCoinsSummary cols c
  (True,  True ) -> header <> mkEntsInInvDesc i cols ws is <> mkCoinsSummary cols c
  where
    header | i' == i   = nl "You are carrying:"
           | otherwise = wrapUnlines cols $ "The " <> s <> " contains:"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."


mkEntsInInvDesc :: Id -> Cols -> WorldState -> Inv -> T.Text
mkEntsInInvDesc i cols ws = T.unlines . concatMap (wordWrapIndent ind cols . helper) . mkNameCountBothList i ws
  where
    helper (bracketPad ind -> en, c, (s, _)) | c == 1 = en <> "1 " <> s
    helper (bracketPad ind -> en, c, b     )          = T.concat [ en, showText c, " ", mkPlurFromBoth b ]
    ind = 11


mkCoinsSummary :: Cols -> Coins -> T.Text
mkCoinsSummary cols c = helper [ mkNameAmt cn c' | cn <- coinNames | c' <- mkListFromCoins c ]
  where
    mkNameAmt (bracketQuote -> cn) a = if a == 0 then "" else showText a <> " " <> cn
    helper                           = T.unlines . wordWrapIndent 2 cols . T.intercalate ", " . filter (not . T.null)


mkCoinsDesc :: Cols -> Coins -> T.Text
mkCoinsDesc cols (Coins (cop, sil, gol)) =
    T.unlines . intercalate [""] . map (wordWrap cols) . filter (not . T.null) $ [ copDesc, silDesc, golDesc ]
  where -- TODO: Come up with good descriptions.
    copDesc = if cop /= 0 then "The copper piece is round and shiny." else ""
    silDesc = if sil /= 0 then "The silver piece is round and shiny." else ""
    golDesc = if gol /= 0 then "The gold piece is round and shiny."   else ""


mkPCDescHeader :: Id -> WorldState -> T.Text
mkPCDescHeader i ws | (pp *** pp -> (s, r)) <- getSexRace i ws = T.concat [ "You see a ", s, " ", r, "." ]


-----


exits :: Action
exits (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let (view rmId -> ri) = (ws^.pcTbl) ! i
        r                 = (ws^.rmTbl) ! ri
    in logPlaExec "exits" i >> (send mq . nl . mkExitsSummary cols $ r)
exits p = withoutArgs exits p


mkExitsSummary :: Cols -> Rm -> T.Text
mkExitsSummary cols (view rmLinks -> rls)
  | stdNames    <- [ rl^.linkDir.to linkDirToCmdName | rl <- rls, not . isNonStdLink $ rl ]
  , customNames <- [ rl^.linkName                    | rl <- rls,       isNonStdLink   rl ]
  = T.unlines . wordWrapIndent 2 cols . ("Obvious exits: " <>) . summarize stdNames $ customNames
  where
    summarize []  []  = "None!"
    summarize std cus = T.intercalate ", " . (std ++) $ cus


isNonStdLink :: RmLink -> Bool
isNonStdLink (NonStdLink {}) = True
isNonStdLink _               = False


-----


getAction :: Action
getAction p@AdviseNoArgs     = advise p ["get"] $ "Please specify one or more items to pick up, as \
                                                  \in " <> dblQuote "get sword" <> "."
getAction   (LowerNub' i as) = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "get" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
        let (view sing -> s)  = (ws^.entTbl)   ! i
            (view rmId -> ri) = (ws^.pcTbl)    ! i
            ris               = (ws^.invTbl)   ! ri
            ris'              = i `delete` ris
            rc                = (ws^.coinsTbl) ! ri
            d                 = mkStdDesig i ws s True ris
        in if (not . null $ ris') || (rc /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as ris' rc
                   eiss                  = [ curry procGecrMisRm gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map procReconciledCoinsRm rcs
                   (ws',  bs,  logMsgs ) = foldl' (helperGetDropEitherInv   i d Get ri i) (ws,  [], []     ) eiss
                   (ws'', bs', logMsgs') = foldl' (helperGetDropEitherCoins i d Get ri i) (ws', bs, logMsgs) ecs
               in putTMVar t ws'' >> return (bs', logMsgs')
          else putTMVar t ws >> return (mkBroadcast i "You don't see anything here to pick up.", [])
getAction p = patternMatchFail "getAction" [ showText p ]


type FromId = Id
type ToId   = Id


helperGetDropEitherInv :: Id                                  ->
                          PCDesig                             ->
                          GetOrDrop                           ->
                          FromId                              ->
                          ToId                                ->
                          (WorldState, [Broadcast], [T.Text]) ->
                          Either T.Text Inv                   ->
                          (WorldState, [Broadcast], [T.Text])
helperGetDropEitherInv i d god fi ti a@(ws, _, _) = \case
  Left  (mkBroadcast i -> b) -> over _2 (++ b) a
  Right is | (fis, tis)      <- over both ((ws^.invTbl) !) (fi, ti)
           , ws'             <- ws & invTbl.at fi ?~ deleteFirstOfEach is fis
                                   & invTbl.at ti ?~ sortInv ws (tis ++ is)
           , (bs', logMsgs') <- mkGetDropInvDesc i ws' d god is
           -> set _1 ws' . over _2 (++ bs') . over _3 (++ logMsgs') $ a


mkGetDropInvDesc :: Id -> WorldState -> PCDesig -> GetOrDrop -> Inv -> ([Broadcast], [T.Text])
mkGetDropInvDesc i ws d god (mkNameCountBothList i ws -> ncbs) | bs <- concatMap helper ncbs = (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _))
      | c == 1 = [ (T.concat [ "You ",           mkGodVerb god SndPer, " the ", s, "." ], [i])
                 , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " a ",   s, "." ], otherPCIds) ]
    helper (_, c, b) =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, rest ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, rest ], otherPCIds) ]
      where
        rest = T.concat [ " ", showText c, " ", mkPlurFromBoth b, "." ]
    otherPCIds = i `delete` pcIds d


mkNameCountBothList :: Id -> WorldState -> Inv -> [(T.Text, Int, BothGramNos)]
mkNameCountBothList i ws is = let ens   = [ getEffName        i ws i' | i' <- is ]
                                  ebgns = [ getEffBothGramNos i ws i' | i' <- is ]
                                  cs    = mkCountList ebgns
                              in nub . zip3 ens cs $ ebgns


extractLogMsgs :: Id -> [Broadcast] -> [T.Text]
extractLogMsgs i bs = [ fst b | b <- bs, snd b == [i] ]


mkGodVerb :: GetOrDrop -> Verb -> T.Text
mkGodVerb Get  SndPer = "pick up"
mkGodVerb Get  ThrPer = "picks up"
mkGodVerb Drop SndPer = "drop"
mkGodVerb Drop ThrPer = "drops"


helperGetDropEitherCoins :: Id                                  ->
                            PCDesig                             ->
                            GetOrDrop                           ->
                            FromId                              ->
                            ToId                                ->
                            (WorldState, [Broadcast], [T.Text]) ->
                            Either [T.Text] Coins               ->
                            (WorldState, [Broadcast], [T.Text])
helperGetDropEitherCoins i d god fi ti a@(ws, _, _) = \case
  Left  msgs -> over _2 (++ [ (msg, [i]) | msg <- msgs ]) a
  Right c | (fc, tc)      <- over both ((ws^.coinsTbl) !) (fi, ti)
          , ws'           <- ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                & coinsTbl.at ti ?~ tc <> c
          , (bs, logMsgs) <- mkGetDropCoinsDesc i d god c
          -> set _1 ws' . over _2 (++ bs) . over _3 (++ logMsgs) $ a


mkGetDropCoinsDesc :: Id -> PCDesig -> GetOrDrop -> Coins -> ([Broadcast], [T.Text])
mkGetDropCoinsDesc i d god (Coins (cop, sil, gol)) | bs <- concat . catMaybes $ [ c, s, g ] = (bs, extractLogMsgs i bs)
  where
    c = if cop /= 0 then Just . helper cop $ "copper piece" else Nothing
    s = if sil /= 0 then Just . helper sil $ "silver piece" else Nothing
    g = if gol /= 0 then Just . helper gol $ "gold piece"   else Nothing
    helper a cn | a == 1 =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " a ", cn, "." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " a ", cn, "." ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You ",           mkGodVerb god SndPer, " ", showText a, " ", cn, "s." ], [i])
        , (T.concat [ serialize d, " ", mkGodVerb god ThrPer, " ", showText a, " ", cn, "s." ], otherPCIds) ]
    otherPCIds = i `delete` pcIds d


-----


help :: Action
help (NoArgs i mq cols) = do
    try helper >>= eitherRet (\e -> readFileExHandler "help" e >> sendGenericErrorMsg mq cols)
    logPla "help" i "read the root help file."
  where
    helper   = send mq . nl . T.unlines . concat . wordWrapLines cols . T.lines =<< readRoot
    readRoot = liftIO . T.readFile . (helpDir ++) $ "root"
help (LowerNub i mq cols as) =
    send mq . nl . T.unlines . intercalate [ "", mkDividerTxt cols, "" ] =<< getTopics
  where
    getTopics = mapM (\a -> concat . wordWrapLines cols . T.lines <$> getHelpTopicByName i cols a) as
help p = patternMatchFail "help" [ showText p ]


getHelpTopicByName :: Id -> Cols -> HelpTopic -> MudStack T.Text
getHelpTopicByName i cols r = (liftIO . getDirectoryContents $ helpDir) >>= \(getTopics -> topics) ->
    maybe sorry
          (\t -> logPla "getHelpTopicByName" i ("read help on " <> dblQuote t <> ".") >> getHelpTopic t)
          (findFullNameForAbbrev r topics)
  where
    getTopics       = (^..folded.packed) . drop 2 . sort . delete "root"
    sorry           = return $ "No help is available on " <> dblQuote r <> "."
    helper          = liftIO . T.readFile . (helpDir ++) . T.unpack
    getHelpTopic t  = (try . helper $ t) >>= eitherRet handler
      where
        handler e = do
            readFileExHandler "getHelpTopicByName" e
            return . wrapUnlines cols $ "Unfortunately, the " <> dblQuote t <> " help file could not be retrieved."


-----


inv :: Action -- TODO: Give some indication of encumbrance.
inv (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    send mq . nl . mkInvCoinsDesc i cols ws i $ (ws^.entTbl) ! i
inv (LowerNub i mq cols as) = readWSTMVar >>= \ws ->
    let is = (ws^.invTbl)   ! i
        c  = (ws^.coinsTbl) ! i
    in send mq $ if (not . null $ is) || (c /= mempty)
      then let (gecrs, miss, rcs) = resolveEntCoinNames i ws as is c
               eiss               = [ curry procGecrMisPCInv gecr mis | gecr <- gecrs | mis <- miss ]
               ecs                = map procReconciledCoinsPCInv rcs
               invDesc            = foldl' (helperEitherInv ws) "" eiss
               coinsDesc          = foldl' helperEitherCoins    "" ecs
           in invDesc <> coinsDesc
      else wrapUnlinesNl cols dudeYourHandsAreEmpty
  where
    helperEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperEitherCoins  acc (Left  msgs) = (acc <>) . nl . multiWrap cols . intersperse "" $ msgs
    helperEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
inv p = patternMatchFail "inv" [ showText p ]


-----


intro :: Action
intro (NoArgs i mq cols) = do
    (view pcTbl -> view introduced . (! i) -> intros) <- readWSTMVar
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
        let (view sing -> s)  = (ws^.entTbl)   ! i
            (view rmId -> ri) = (ws^.pcTbl)    ! i
            is                = (ws^.invTbl)   ! ri
            is'               = i `delete` is
            c                 = (ws^.coinsTbl) ! ri
        in if (not . null $ is') || (c /= mempty)
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as is' c
                   eiss                  = [ curry procGecrMisRm gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                   = map procReconciledCoinsRm rcs
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
          PCType | targetPC@(view introduced -> intros)  <- (ws^.pcTbl) ! targetId
                 , pis                                   <- findPCIds ws ris
                 , targetDesig                           <- serialize . mkStdDesig targetId ws targetSing False $ ris
                 , (views sex mkReflexive -> himHerself) <- (ws^.mobTbl) ! i
                 -> if s `elem` intros
                   then let msg = nlnl $ "You've already introduced yourself to " <> targetDesig <> "."
                        in over _2 (++ mkNTBroadcast i msg) a'
                   else let p         = targetPC & introduced .~ sort (s : intros)
                            ws'       = ws & pcTbl.at targetId ?~ p
                            srcMsg    = nlnl $ "You introduce yourself to " <> targetDesig <> "."
                            srcDesig  = StdDesig { stdPCEntSing = Nothing
                                                 , isCap        = True
                                                 , pcEntName    = mkUnknownPCEntName i ws
                                                 , pcId         = i
                                                 , pcIds        = pis }
                            targetMsg = nlnl . T.concat $ [ serialize srcDesig
                                                          , " introduces "
                                                          , himHerself
                                                          , " to you as "
                                                          , s
                                                          , "." ]
                            othersMsg = nlnl . T.concat $ [ serialize srcDesig { stdPCEntSing = Just s }
                                                          , " introduces "
                                                          , himHerself
                                                          , " to "
                                                          , targetDesig
                                                          , "." ]
                            cbs = [ NonTargetBroadcast (srcMsg,    [i])
                                  , TargetBroadcast    (targetMsg, [targetId])
                                  , NonTargetBroadcast (othersMsg, deleteFirstOfEach [ i, targetId ] pis) ]
                        in set _1 ws' . over _2 (++ cbs) . over _3 (++ [srcMsg]) $ a'
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


look :: Action
look (NoArgs i mq cols) = readWSTMVar >>= \ws ->
    let (view rmId -> ri) = (ws^.pcTbl) ! i
        r                 = (ws^.rmTbl) ! ri
        primary           = multiWrap cols [ r^.rmName, r^.rmDesc ]
        suppl             = mkExitsSummary cols r <>  mkRmInvCoinsDesc i cols ws ri
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
        let (view sing -> s ) = (ws^.entTbl)   ! i
            (view rmId -> ri) = (ws^.pcTbl)    ! i
            ris               = (ws^.invTbl)   ! ri
            ris'              = i `delete` ris
            c                 = (ws^.coinsTbl) ! ri
            d                 = mkStdDesig i ws s True ris
        in if (not . null $ ris') || (c /= mempty)
          then let (gecrs, miss, rcs) = resolveEntCoinNames i ws as ris' c
                   eiss               = [ curry procGecrMisRm gecr mis | gecr <- gecrs | mis <- miss ]
                   ecs                = map procReconciledCoinsRm rcs
                   invDesc            = foldl' (helperLookEitherInv ws) "" eiss
                   coinsDesc          = foldl' helperLookEitherCoins    "" ecs
                   ds                 = [ let (view sing -> s') = (ws^.entTbl) ! pi
                                          in mkStdDesig pi ws s' False ris | pi <- extractPCIdsFromEiss ws eiss ]
               in putTMVar t ws >> return (Right $ invDesc <> coinsDesc, Just (d, ds))
          else    putTMVar t ws >> return ( Left . wrapUnlinesNl cols $ "You don't see anything here to look at."
                                          , Nothing )
    helperLookEitherInv _  acc (Left  msg ) = (acc <>) . wrapUnlinesNl cols $ msg
    helperLookEitherInv ws acc (Right is  ) = nl $ acc <> mkEntDescs i cols ws is
    helperLookEitherCoins  acc (Left  msgs) = (acc <>) . nl . multiWrap cols . intersperse "" $ msgs
    helperLookEitherCoins  acc (Right c   ) = nl $ acc <> mkCoinsDesc cols c
look p = patternMatchFail "look" [ showText p ]


-- TODO: Consider implementing a color scheme for lists like these such that the least significant characters of each name are highlighted or bolded somehow.
mkRmInvCoinsDesc :: Id -> Cols -> WorldState -> Id -> T.Text
mkRmInvCoinsDesc i cols ws ri =
    let (splitRmInv ws -> ((i `delete`) -> pis, ois)) = (ws^.invTbl) ! ri
        pcDescs    = T.unlines . concatMap (wordWrapIndent 2 cols . mkPCDesc   ) . mkNameCountBothList i ws $ pis
        otherDescs = T.unlines . concatMap (wordWrapIndent 2 cols . mkOtherDesc) . mkNameCountBothList i ws $ ois
        c          = (ws^.coinsTbl) ! ri
    in (if not . null $ pis then pcDescs               else "") <>
       (if not . null $ ois then otherDescs            else "") <>
       (if c /= mempty      then mkCoinsSummary cols c else "")
  where
    mkPCDesc    (bracketQuote -> en, c, (s, _)) | c == 1 = (<> en) . (<> " ") $ if isKnownPCSing s then s else aOrAn s
    mkPCDesc    a                                        = mkOtherDesc a
    mkOtherDesc (bracketQuote -> en, c, (s, _)) | c == 1 = aOrAn s <> " " <> en
    mkOtherDesc (bracketQuote -> en, c, b     )          = T.concat [ showText c, " ", mkPlurFromBoth b, " ", en ]


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
    readMotd  = return . frame cols . multiWrap cols . T.lines =<< T.readFile motdFile
    handler e = do
        readFileExHandler "getMotdTxt" e
        return . wrapUnlinesNl cols $ "Unfortunately, the message of the day could not be retrieved."


-----


putAction :: Action
putAction p@AdviseNoArgs     = advise p ["put"] $ "Please specify one or more things you want to put, followed by \
                                                  \where you want to put them, as in " <> dblQuote "put doll \
                                                  \sack" <> "."
putAction p@(AdviseOneArg a) = advise p ["put"] $ "Please also specify where you want to put it, as \
                                                  \in " <> dblQuote ("put " <> a <> " sack") <> "."
putAction   (Lower' i as)    = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "put" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let (view sing -> s)         = (ws^.entTbl)   ! i
          (view rmId -> ri)        = (ws^.pcTbl)    ! i
          (pis, ris)               = over both ((ws^.invTbl)   !) (i, ri)
          ris'                     = i `delete` ris
          (pc, rc)                 = over both ((ws^.coinsTbl) !) (i, ri)
          cn                       = last as
          (init -> argsWithoutCon) = case as of [_, _] -> as
                                                _      -> (++ [cn]) . nub . init $ as
          d                        = mkStdDesig i ws s True ris
      in if (not . null $ pis) || (pc /= mempty)
        then if T.head cn == rmChar && cn /= T.singleton rmChar
          then if not . null $ ris'
            then shufflePut i (t, ws) d (T.tail cn) True argsWithoutCon ris' rc pis pc procGecrMisRm
            else putTMVar t ws >> return (mkBroadcast i "You don't see any containers here.", [])
          else shufflePut i (t, ws) d cn False argsWithoutCon pis pc pis pc procGecrMisPCInv
        else putTMVar t ws >> return (mkBroadcast i dudeYourHandsAreEmpty, [])
putAction p = patternMatchFail "putAction" [ showText p ]


type IsConInRm    = Bool
type InvWithCon   = Inv
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


type NthOfM = (Int, Int)


mkMaybeNthOfM :: IsConInRm -> WorldState -> Id -> Ent -> InvWithCon -> Maybe NthOfM
mkMaybeNthOfM False _  _ _                _  = Nothing
mkMaybeNthOfM True  ws i (view sing -> s) is = Just . (succ . fromJust . elemIndex i *** length) . dup $ matches
  where
    matches = filter (\i' -> let (view sing -> s') = (ws^.entTbl) ! i' in s' == s) is


type ToEnt = Ent


helperPutRemEitherInv :: Id                                  ->
                         PCDesig                             ->
                         PutOrRem                            ->
                         Maybe NthOfM                        ->
                         FromId                              ->
                         ToId                                ->
                         ToEnt                               ->
                         (WorldState, [Broadcast], [T.Text]) ->
                         Either T.Text Inv                   ->
                         (WorldState, [Broadcast], [T.Text])
helperPutRemEitherInv i d por mnom fi ti te a@(ws, bs, _) = \case
  Left  (mkBroadcast i -> b) -> over _2 (++ b) a
  Right is | (is', bs')      <- if ti `elem` is
                                  then (filter (/= ti) is, bs ++ [sorry])
                                  else (is, bs)
           , (fis, tis)      <- over both ((ws^.invTbl) !) (fi, ti)
           , ws'             <- ws & invTbl.at fi ?~ deleteFirstOfEach is' fis
                                   & invTbl.at ti ?~ (sortInv ws . (tis ++) $ is')
           , (bs'', logMsgs) <- mkPutRemInvDesc i ws' d por mnom is' te
           -> set _1 ws' . set _2 (bs' ++ bs'') . over _3 (++ logMsgs) $ a
  where
    sorry = ("You can't put the " <> te^.sing <> " inside itself.", [i])


mkPutRemInvDesc :: Id -> WorldState -> PCDesig -> PutOrRem -> Maybe NthOfM -> Inv -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemInvDesc i ws d por mnom is (view sing -> ts) | bs <- concatMap helper . mkNameCountBothList i ws $ is
                                                     = (bs, extractLogMsgs i bs)
  where
    helper (_, c, (s, _)) | c == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , mkArticle
                    , s
                    , " "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " a "
                    , s
                    , " "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
      where
        mkArticle | por == Put = " the "
                  | otherwise  = " a "
    helper (_, c, b) =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , showText c
                    , " "
                    , mkPlurFromBoth b
                    , " "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
    rest       = T.concat [ " ", ts, onTheGround mnom, "."  ]
    otherPCIds = i `delete` pcIds d


mkPorVerb :: PutOrRem -> Verb -> T.Text
mkPorVerb Put SndPer = "put"
mkPorVerb Put ThrPer = "puts"
mkPorVerb Rem SndPer = "remove"
mkPorVerb Rem ThrPer = "removes"


mkPorPrep :: PutOrRem -> Verb -> Maybe NthOfM -> T.Text
mkPorPrep Put SndPer Nothing       = "in the"
mkPorPrep Put SndPer (Just (n, m)) = "in the"   <> descNthOfM n m
mkPorPrep Rem SndPer Nothing       = "from the"
mkPorPrep Rem SndPer (Just (n, m)) = "from the" <> descNthOfM n m
mkPorPrep Put ThrPer Nothing       = "in a"
mkPorPrep Put ThrPer (Just (n, m)) = "in the"   <> descNthOfM n m
mkPorPrep Rem ThrPer Nothing       = "from a"
mkPorPrep Rem ThrPer (Just (n, m)) = "from the" <> descNthOfM n m


descNthOfM :: Int -> Int -> T.Text
descNthOfM 1 1 = ""
descNthOfM n _ = " " <> mkOrdinal n


onTheGround :: Maybe NthOfM -> T.Text
onTheGround Nothing = ""
onTheGround _       = " on the ground"


helperPutRemEitherCoins :: Id                                  ->
                           PCDesig                             ->
                           PutOrRem                            ->
                           Maybe NthOfM                        ->
                           FromId                              ->
                           ToId                                ->
                           ToEnt                               ->
                           (WorldState, [Broadcast], [T.Text]) ->
                           Either [T.Text] Coins               ->
                           (WorldState, [Broadcast], [T.Text])
helperPutRemEitherCoins i d por mnom fi ti te a@(ws, _, _) = \case
  Left  msgs -> over _2 (++ [ (msg, [i]) | msg <- msgs ]) a
  Right c | (fc, tc)      <- over both ((ws^.coinsTbl) !) (fi, ti)
          , ws'           <- ws & coinsTbl.at fi ?~ fc <> negateCoins c
                                & coinsTbl.at ti ?~ tc <> c
          , (bs, logMsgs) <- mkPutRemCoinsDescs i d por mnom c te
          -> set _1 ws' . over _2 (++ bs) . over _3 (++ logMsgs) $ a


mkPutRemCoinsDescs :: Id -> PCDesig -> PutOrRem -> Maybe NthOfM -> Coins -> ToEnt -> ([Broadcast], [T.Text])
mkPutRemCoinsDescs i d por mnom (Coins (cop, sil, gol)) (view sing -> ts) | bs <- concat . catMaybes $ [ c, s, g ]
                                                                          = (bs, extractLogMsgs i bs)
  where
    c = if cop /= 0 then Just . helper cop $ "copper piece" else Nothing
    s = if sil /= 0 then Just . helper sil $ "silver piece" else Nothing
    g = if gol /= 0 then Just . helper gol $ "gold piece"   else Nothing
    helper a cn | a == 1 =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " a "
                    , cn
                    , " "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " a "
                    , cn
                    , " "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
    helper a cn =
        [ (T.concat [ "You "
                    , mkPorVerb por SndPer
                    , " "
                    , showText a
                    , " "
                    , cn
                    , "s "
                    , mkPorPrep por SndPer mnom
                    , rest ], [i])
        , (T.concat [ serialize d
                    , " "
                    , mkPorVerb por ThrPer
                    , " "
                    , showText a
                    , " "
                    , cn
                    , "s "
                    , mkPorPrep por ThrPer mnom
                    , rest ], otherPCIds) ]
    rest       = T.concat [ " ", ts, onTheGround mnom, "." ]
    otherPCIds = i `delete` pcIds d


-----


quit :: Action
quit (NoArgs' i mq)                        = (liftIO . atomically . writeTQueue mq $ Quit) >> logPlaExec "quit" i
quit ActionParams { plaMsgQueue, plaCols } = wrapSend plaMsgQueue plaCols msg
  where
    msg = "Type " <> dblQuote "quit" <> " with no arguments to quit the game."


handleEgress :: Id -> MudStack ()
handleEgress i = do
    notifyEgress i
    wsTMVar  <- getWSTMVar
    mqtTMVar <- getNWSRec msgQueueTblTMVar
    ptTMVar  <- getNWSRec plaTblTMVar
    (parensQuote -> n) <- liftIO . atomically $ do
        ws  <- takeTMVar wsTMVar
        mqt <- takeTMVar mqtTMVar
        pt  <- takeTMVar ptTMVar
        -----
        let (view sing     -> s)   = (ws^.entTbl) ! i
        let (view rmId     -> ri)  = (ws^.pcTbl)  ! i
        let ((i `delete`)  -> ris) = (ws^.invTbl) ! ri
        let ws'                    = ws  & typeTbl.at  i  .~ Nothing
                                         & entTbl.at   i  .~ Nothing
                                         & invTbl.at   i  .~ Nothing
                                         & coinsTbl.at i  .~ Nothing
                                         & eqTbl.at    i  .~ Nothing
                                         & mobTbl.at   i  .~ Nothing
                                         & pcTbl.at    i  .~ Nothing
                                         & invTbl.at   ri ?~ ris
        let mqt'                   = mqt & at i .~ Nothing
        let pt'                    = pt  & at i .~ Nothing
        -----
        putTMVar wsTMVar  ws'
        putTMVar mqtTMVar mqt'
        putTMVar ptTMVar  pt'
        return s
    logNotice "handleEgress" . T.concat $ [ "player ", showText i, " ", n, " has left the game." ]
    closePlaLog i


notifyEgress :: Id -> MudStack ()
notifyEgress i = readWSTMVar >>= \ws ->
    let (view sing    -> s)   = (ws^.entTbl) ! i
        (view rmId    -> ri)  = (ws^.pcTbl)  ! i
        ris                   = (ws^.invTbl) ! ri
        ((i `delete`) -> pis) = findPCIds ws ris
        d                     = serialize . mkStdDesig i ws s True $ ris
    in bcast [(nlnl $ d <> " has left the game.", pis)]


-----


ready :: Action
ready p@AdviseNoArgs            = advise p ["ready"] $ "Please specify one or more things to ready, as \
                                                       \in " <> dblQuote "ready sword" <> "."
ready   (LowerNub i mq cols as) = helper >>= \(msg, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "ready" i logMsgs
    send mq . nl $ msg
  where
    helper = onWS $ \(t, ws) ->
        let is = (ws^.invTbl)   ! i
            c  = (ws^.coinsTbl) ! i
        in if (not . null $ is) || (c /= mempty)
          then let (gecrs, mrols, miss, rcs) = resolveEntCoinNamesWithRols i ws as is mempty
                   eiss                      = [ curry procGecrMisReady gecr mis | gecr <- gecrs | mis <- miss ]
                   msg                       = if null rcs then "" else nl "You can't ready coins."
                   (ws', msg', logMsgs)      = foldl' (helperReady i cols) (ws, msg, []) . zip eiss $ mrols
               in putTMVar t ws' >> return (msg', logMsgs)
          else putTMVar t ws >> return (wrapUnlines cols dudeYourHandsAreEmpty, [])
ready p = patternMatchFail "ready" [ showText p ]


helperReady :: Id                                     ->
               Cols                                   ->
               (WorldState, T.Text, [T.Text])         ->
               (Either T.Text Inv, Maybe RightOrLeft) ->
               (WorldState, T.Text, [T.Text])
helperReady i cols a (eis, mrol) = case eis of
  Left  (wrapUnlines cols -> msg') -> over _2 (<> msg') a
  Right is                         -> foldl' (readyDispatcher i cols mrol) a is


readyDispatcher :: Id                             ->
                   Cols                           ->
                   Maybe RightOrLeft              ->
                   (WorldState, T.Text, [T.Text]) ->
                   Id                             ->
                   (WorldState, T.Text, [T.Text])
readyDispatcher i cols mrol a@(ws, _, _) ei =
    let e = (ws^.entTbl)  ! ei
        t = (ws^.typeTbl) ! ei
    in case t of
      ClothType -> readyCloth i cols mrol a ei e
      WpnType   -> readyWpn   i cols mrol a ei e
      _         -> over _2 (<> (wrapUnlines cols $ "You can't ready a " <> e^.sing <> ".")) a


-- Helpers for the entity type-specific ready functions:


moveReadiedItem :: Id                             ->
                   Cols                           ->
                   (WorldState, T.Text, [T.Text]) ->
                   EqMap                          ->
                   Slot                           ->
                   Id                             ->
                   T.Text                         ->
                   (WorldState, T.Text, [T.Text])
moveReadiedItem i cols a@(ws, _, _) em s ei msg
  | is  <- (ws^.invTbl) ! i
  , ws' <- ws & invTbl.at i ?~ filter (/= ei) is
              & eqTbl.at  i ?~ (em & at s ?~ ei)
  = set _1 ws' . over _2 (<> wrapUnlines cols msg) . over _3 (++ [msg]) $ a


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = em^.at s.to isNothing


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


otherHand :: Hand -> Hand
otherHand RHand  = LHand
otherHand LHand  = RHand
otherHand NoHand = NoHand


isRingRol :: RightOrLeft -> Bool
isRingRol = \case R -> False
                  L -> False
                  _ -> True


-- Readying clothing:


readyCloth :: Id                             ->
              Cols                           ->
              Maybe RightOrLeft              ->
              (WorldState, T.Text, [T.Text]) ->
              Id                             ->
              Ent                            ->
              (WorldState, T.Text, [T.Text])
readyCloth i cols mrol a@(ws, _, _) ei e@(view sing -> s) =
    let em = (ws^.eqTbl)    ! i
        c  = (ws^.clothTbl) ! ei
    in case maybe (getAvailClothSlot cols ws i c em) (getDesigClothSlot cols ws e c em) mrol of
      Left  msg  -> over _2 (<> msg) a
      Right slot -> moveReadiedItem i cols a em slot ei . mkReadyMsg slot $ c
  where
    mkReadyMsg (pp -> slot) = \case NoseC   -> putOnMsg
                                    NeckC   -> putOnMsg
                                    FingerC -> T.concat [ "You slide the ", s, " on your ", slot, "." ]
                                    _       -> wearMsg
      where
        putOnMsg = "You put on the " <> s <> "."
        wearMsg  = T.concat [ "You wear the ", s, " on your ", slot, "." ]


getAvailClothSlot :: Cols -> WorldState -> Id -> Cloth -> EqMap -> Either T.Text Slot
getAvailClothSlot cols ws i c em | m <- (ws^.mobTbl) ! i, s <- m^.sex, h <- m^.hand = procMaybe $ case c of
  EarC    -> getEarSlotForSex s `mplus` (getEarSlotForSex . otherSex $ s)
  NoseC   -> findAvailSlot em noseSlots
  NeckC   -> findAvailSlot em neckSlots
  WristC  -> getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
  FingerC -> getRingSlot s h
  _       -> undefined -- TODO
  where
    procMaybe             = maybe (Left . wrapUnlines cols . sorryFullClothSlots $ c) Right
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


getDesigClothSlot :: Cols -> WorldState -> Ent -> Cloth -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigClothSlot cols ws (view sing -> s) c em rol
  | c `elem` [ NoseC, NeckC, UpBodyC, LowBodyC, FullBodyC, BackC, FeetC ] = Left sorryCantWearThere
  | isRingRol rol, c /= FingerC                                           = Left sorryCantWearThere
  | c == FingerC, not . isRingRol $ rol                                   = Left . wrapUnlines cols $ ringHelp
  | otherwise                                                             = case c of
    EarC    -> maybe (Left sorryFullEar)   Right (findSlotFromList rEarSlots   lEarSlots)
    WristC  -> maybe (Left sorryFullWrist) Right (findSlotFromList rWristSlots lWristSlots)
    FingerC -> maybe (Right slotFromRol)
                     (\i -> let e = (ws^.entTbl) ! i in Left . sorry slotFromRol $ e)
                     (em^.at slotFromRol)
    _       -> undefined -- TODO
  where
    sorryCantWearThere     = wrapUnlines cols . T.concat $ [ "You can't wear a ", s, " on your ", pp rol, "." ]
    findSlotFromList rs ls = findAvailSlot em $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot findSlotFromList" [ showText rol ]
    getSlotFromList  rs ls = head $ case rol of
      R -> rs
      L -> ls
      _ -> patternMatchFail "getDesigClothSlot getSlotFromList"  [ showText rol ]
    sorryFullEar   = wrapUnlines cols . sorryFullClothSlotsOneSide . getSlotFromList rEarSlots   $ lEarSlots
    sorryFullWrist = wrapUnlines cols . sorryFullClothSlotsOneSide . getSlotFromList rWristSlots $ lWristSlots
    slotFromRol    = fromRol rol :: Slot
    sorry (pp -> slot) (view sing -> s') = wrapUnlines cols . T.concat $ [ "You're already wearing a "
                                                                         , s'
                                                                         , " on your "
                                                                         , slot
                                                                         , "." ]


sorryFullClothSlotsOneSide :: Slot -> T.Text
sorryFullClothSlotsOneSide (pp -> s) = "You can't wear any more on your " <> s <> "."


-- Readying weapons:


readyWpn :: Id                             ->
            Cols                           ->
            Maybe RightOrLeft              ->
            (WorldState, T.Text, [T.Text]) ->
            Id                             ->
            Ent                            ->
            (WorldState, T.Text, [T.Text])
readyWpn i cols mrol a@(ws, _, _) ei e@(view sing -> s) | em  <- (ws^.eqTbl)  ! i
                                                        , w   <- (ws^.wpnTbl) ! ei
                                                        , sub <- w^.wpnSub = if not . isSlotAvail em $ BothHandsS
  then over _2 (<> wrapUnlines cols "You're already wielding a two-handed weapon.") a
  else case maybe (getAvailWpnSlot cols ws i em) (getDesigWpnSlot cols ws e em) mrol of
    Left  msg   -> over _2 (<> msg) a
    Right slot  -> case sub of
      OneHanded -> moveReadiedItem i cols a em slot ei . T.concat $ [ "You wield the "
                                                                    , s
                                                                    , " with your "
                                                                    , pp slot
                                                                    , "." ]
      TwoHanded
        | all (isSlotAvail em) [ RHandS, LHandS ] ->
            moveReadiedItem i cols a em BothHandsS ei $ "You wield the " <> s <> " with both hands."
        | otherwise -> over _2 (<> (wrapUnlines cols $ "Both hands are required to wield the " <> s <> ".")) a


getAvailWpnSlot :: Cols -> WorldState -> Id -> EqMap -> Either T.Text Slot
getAvailWpnSlot cols ws i em | (view hand -> h) <- (ws^.mobTbl) ! i =
    maybe (Left . wrapUnlines cols $ "You're already wielding two weapons.")
          Right
          (findAvailSlot em . map getSlotForHand $ [ h, otherHand h ])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> patternMatchFail "getAvailWpnSlot getSlotForHand" [ showText h ]


getDesigWpnSlot :: Cols -> WorldState -> Ent -> EqMap -> RightOrLeft -> Either T.Text Slot
getDesigWpnSlot cols ws (view sing -> s) em rol
  | isRingRol rol = Left sorryNotRing
  | otherwise     = maybe (Right desigSlot)
                          (\i -> let e = (ws^.entTbl) ! i in Left . sorry $ e)
                          (em^.at desigSlot)
  where
    sorryNotRing            = wrapUnlines cols $ "You can't wield a " <> s <> " with your finger!"
    sorry (view sing -> s') = wrapUnlines cols . T.concat $ [ "You're already wielding a "
                                                            , s'
                                                            , " with your "
                                                            , pp desigSlot
                                                            , "." ]
    desigSlot               = case rol of R -> RHandS
                                          L -> LHandS
                                          _ -> patternMatchFail "getDesigWpnSlot desigSlot" [ showText rol ]


-----


remove :: Action
remove p@AdviseNoArgs     = advise p ["remove"] $ "Please specify one or more things to remove, followed by the \
                                                  \container you want to remove them from, as in " <> dblQuote "remove \
                                                  \doll sack" <> "."
remove p@(AdviseOneArg a) = advise p ["remove"] $ "Please also specify the container you want to remove it from, as \
                                                  \in " <> dblQuote ("remove " <> a <> " sack") <> "."
remove   (Lower' i as)    = helper >>= \(bs, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "remove" i logMsgs
    bcastNl bs
  where
    helper = onWS $ \(t, ws) ->
      let (view sing -> s)         = (ws^.entTbl) ! i
          (view rmId -> ri)        = (ws^.pcTbl)  ! i
          (pis, ris)               = over both ((ws^.invTbl)   !) (i, ri)
          (pc, rc)                 = over both ((ws^.coinsTbl) !) (i, ri)
          ris'                     = i `delete` ris
          cn                       = last as
          (init -> argsWithoutCon) = case as of [_, _] -> as
                                                _      -> (++ [cn]) . nub . init $ as
          d                        = mkStdDesig i ws s True ris
      in if T.head cn == rmChar && cn /= T.singleton rmChar
        then if not . null $ ris'
          then shuffleRem i (t, ws) d (T.tail cn) True argsWithoutCon ris' rc procGecrMisRm
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
unready p@AdviseNoArgs            = advise p ["unready"] $ "Please specify one or more things to unready, as \
                                                           \in " <> dblQuote "unready sword" <> "."
unready   (LowerNub i mq cols as) = helper >>= \(msg, logMsgs) -> do
    unless (null logMsgs) $ logPlaOut "unready" i logMsgs
    send mq . nl $ msg
  where
    helper = onWS $ \(t, ws) ->
        let em = (ws^.eqTbl) ! i
            is = M.elems em
        in if not . null $ is
          then let (gecrs, miss, rcs)    = resolveEntCoinNames i ws as is mempty
                   eiss                  = [ curry procGecrMisPCEq gecr mis | gecr <- gecrs | mis <- miss ]
                   msg                   = if null rcs then "" else nl "You can't unready coins."
                   (ws', msg', logMsgs)  = foldl' (helperUnready i cols em) (ws, msg, []) eiss
               in putTMVar t ws' >> return (msg', logMsgs)
          else putTMVar t ws >> return (wrapUnlines cols dudeYou'reNaked, [])
unready p = patternMatchFail "unready" [ showText p ]


helperUnready :: Id                             ->
                 Cols                           ->
                 EqMap                          ->
                 (WorldState, T.Text, [T.Text]) ->
                 Either T.Text Inv              ->
                 (WorldState, T.Text, [T.Text])
helperUnready i cols em a@(ws, _, _) = \case
  Left  msg -> over _2 (<> wrapUnlines cols msg) a
  Right is | pis  <- (ws^.invTbl) ! i
           , ws'  <- ws & eqTbl.at  i ?~ M.filter (`notElem` is) em
                        & invTbl.at i ?~ (sortInv ws . (pis ++) $ is)
           , msgs <- mkUnreadyDescs i ws' is
           -> set _1 ws' . over _2 (<> (T.concat . map (wrapUnlines cols) $ msgs)) . over _3 (++ msgs) $ a


mkUnreadyDescs :: Id -> WorldState -> Inv -> [T.Text]
mkUnreadyDescs i ws is = [ helper icb | icb <- mkIdCountBothList i ws is ]
  where
    helper (verb -> v, c, b@(s, _)) = T.concat $ if c == 1
      then [ "You ", v, " the ", s, "." ]
      else [ "You ", v, " ", showText c, " ", mkPlurFromBoth b, "." ]
    verb (((ws^.typeTbl) !) -> t) = case t of
      ClothType -> unwearGenericVerb -- TODO
      WpnType   -> "stop wielding"
      _         -> undefined -- TODO
    unwearGenericVerb = "take off"


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


uptimeHelper :: Integer -> MudStack T.Text
uptimeHelper ut = helper <$> getRecordUptime
  where
    helper = \case Nothing  -> mkUptimeTxt
                   Just rut -> case ut `compare` rut of GT -> mkNewRecTxt
                                                        _  -> mkRecTxt rut
    mkUptimeTxt                = mkTxtHelper "."
    mkNewRecTxt                = mkTxtHelper " - it's a new record!"
    mkRecTxt (renderIt -> rut) = mkTxtHelper $ " (record uptime: " <> rut <> ")."
    mkTxtHelper                = ("Up " <>) . (renderIt ut <>)
    renderIt                   = T.pack . renderSecs


getRecordUptime :: MudStack (Maybe Integer)
getRecordUptime = (liftIO . doesFileExist $ uptimeFile) >>= \case
  True  -> liftIO readUptime `catch` (\e -> readFileExHandler "getRecordUptime" e >> return Nothing)
  False -> return Nothing
  where
    readUptime = Just . read <$> readFile uptimeFile


getUptime :: MudStack Integer
getUptime = round <$> diff
  where
    diff = diffUTCTime <$> liftIO getCurrentTime <*> getNWSRec startTime


-----


-- TODO: Disambiguate player names.
what :: Action
what p@AdviseNoArgs            = advise p ["what"] $ "Please specify one or more abbreviations to disambiguate, as \
                                                     \in " <> dblQuote "what up" <> "."
what   (LowerNub i mq cols as) = readWSTMVar >>= \ws ->
    let (view rmId -> ri) = (ws^.pcTbl) ! i
        r                 = (ws^.rmTbl) ! ri
    in logPlaExecArgs "what" as i >> (send mq . T.concat . map (helper ws r) $ as)
  where
    helper ws r n = nl . T.concat $ whatCmd cols r n : [ whatInv i cols ws it n | it <- [ PCInv, PCEq, RmInv ] ]
what p = patternMatchFail "what" [ showText p ]


whatCmd :: Cols -> Rm -> T.Text -> T.Text
whatCmd cols (mkCmdListWithNonStdRmLinks -> cmds) (T.toLower -> n@(dblQuote -> n')) =
    wrapUnlines cols . maybe notFound found . findFullNameForAbbrev n . filter isPlaCmd $ [ cmdName cmd | cmd <- cmds ]
  where
    isPlaCmd               = (`notElem` [ wizCmdChar, debugCmdChar ]) . T.head
    notFound               = n' <> " doesn't refer to any commands."
    found (dblQuote -> cn) = T.concat [ n', " may refer to the ", cn, " command." ]


mkCmdListWithNonStdRmLinks :: Rm -> [Cmd]
mkCmdListWithNonStdRmLinks (view rmLinks -> rls) =
    sortBy sorter $ plaCmds ++ [ mkCmdForRmLink rl | rl <- rls, isNonStdLink rl ]
  where
    sorter c = uncurry compare . over both cmdName . (c,)


mkCmdForRmLink :: RmLink -> Cmd
mkCmdForRmLink (T.toLower . mkCmdNameForRmLink -> cn) = Cmd { cmdName = cn, action = go cn, cmdDesc = "" }


mkCmdNameForRmLink :: RmLink -> T.Text
mkCmdNameForRmLink rl = T.toLower $ case rl of StdLink    { .. } -> linkDirToCmdName _linkDir
                                               NonStdLink { .. } -> _linkName


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
whatInvEnts i cols ws it@(getLocTxtForInvType -> locTxt) (dblQuote -> r) gecr is = wrapUnlines cols $ case gecr of
  Mult { entsRes = (Just es), .. }
    | nameSearchedFor == acp -> T.concat [ dblQuote acp
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
whatInvCoins cols it@(getLocTxtForInvType -> locTxt) (dblQuote -> r) rc
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
                                                            , " may refer to the "
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
