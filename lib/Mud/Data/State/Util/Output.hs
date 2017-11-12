{-# OPTIONS_GHC -fno-warn-type-defaults -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module Mud.Data.State.Util.Output ( anglePrompt
                                  , bcast
                                  , bcastAdmins
                                  , bcastAdminsExcept
                                  , bcastIfNotIncog
                                  , bcastIfNotIncogNl
                                  , bcastNl
                                  , bcastOtherAdmins
                                  , bcastOthersInRm
                                  , bcastToOutsideMobs
                                  , blankLine
                                  , dbError
                                  , frame
                                  , massMsg
                                  , massSend
                                  , mkBcast
                                  , mkDfltPrompt
                                  , mkNTBcast
                                  , multiSend
                                  , multiWrapSend
                                  , multiWrapSend1Nl
                                  , ok
                                  , parseDesig
                                  , parseExpandDesig
                                  , retainedMsg
                                  , send
                                  , sendCmdNotFound
                                  , sendDfltPrompt
                                  , sendGmcpRmInfo
                                  , sendGmcpVitals
                                  , sendMsgBoot
                                  , sendPrompt
                                  , sendSilentBoot
                                  , wrapSend
                                  , wrapSend1Nl
                                  , wrapSendPrompt
                                  , writeMsg ) where

import           Mud.Cmds.Msgs.Hint
import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Msgs.Sorry
import           Mud.Data.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.GMCP
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Misc.ANSI
import           Mud.Misc.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Telnet.Chars
import           Mud.Util.List (nubSort)
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text
import           Mud.Util.Wrapping

import           Control.Arrow ((***), first)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Lens (each, to, views)
import           Control.Lens.Operators ((.~), (&), (%~), (^.), (<>~))
import           Control.Monad (forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.List ((\\), delete, elemIndex)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import           Prelude hiding (pi)
import qualified Data.IntMap.Strict as IM (elems, keys, toList)
import qualified Data.Text as T


pmf :: PatternMatchFail
pmf = U.pmf "Mud.Data.State.Util.Output"


-- ============================================================


anglePrompt :: HasCallStack => MsgQueue -> MudStack ()
anglePrompt = flip sendPrompt ">"


-----


bcast :: HasCallStack => [Broadcast] -> MudStack ()
bcast [] = unit
bcast bs = liftIO . atomically . forM_ bs . sendBcast =<< getState
  where
    sendBcast ms (msg, is) = mapM_ helper is
      where
        helper targetId = case getType targetId ms of
          PlaType -> writeIt FromServer targetId
          NpcType -> maybeVoid (writeIt ToNpc) . getPossessor targetId $ ms
          t       -> pmf "bcast sendBcast helper" t
        writeIt f i = let (mq, cols) = getMsgQueueColumns i ms
                      in writeTQueue mq . f . T.unlines . concatMap (wrap cols) . T.lines . parseDesig i ms $ msg


-----


bcastAdmins :: HasCallStack => Text -> MudStack ()
bcastAdmins = bcastAdminsHelper id


bcastAdminsHelper :: HasCallStack => (Inv -> Inv) -> Text -> MudStack ()
bcastAdminsHelper f msg =
    bcastNl . pure . (colorWith adminBcastColor msg, ) =<< f . getLoggedInAdminIds <$> getState


-----


bcastAdminsExcept :: HasCallStack => Inv -> Text -> MudStack ()
bcastAdminsExcept = bcastAdminsHelper . flip (\\)


-----


bcastIfNotIncog :: HasCallStack => Id -> [Broadcast] -> MudStack ()
bcastIfNotIncog i bs = getState >>= \ms -> onTrue (isPla i ms) (unless (isIncognito . getPla i $ ms)) . bcast $ bs


-----


bcastIfNotIncogNl :: HasCallStack => Id -> [Broadcast] -> MudStack ()
bcastIfNotIncogNl i = bcastIfNotIncog i . appendNlToBs


appendNlToBs :: HasCallStack => [Broadcast] -> [Broadcast]
appendNlToBs bs = bs ++ pure (nlTxt, nubSort . concatMap snd $ bs)


-----


bcastNl :: HasCallStack => [Broadcast] -> MudStack ()
bcastNl = bcast . appendNlToBs


-----


bcastOtherAdmins :: HasCallStack => Id -> Text -> MudStack ()
bcastOtherAdmins = bcastAdminsHelper . delete


-----


bcastOthersInRm :: HasCallStack => Id -> Text -> MudStack ()
bcastOthersInRm i msg = getState >>= \ms ->
    let helper = bcast . pure $ (msg, findMobIds ms . delete i . getMobRmInv i $ ms)
    in isPla i ms ? unless (isIncognito . getPla i $ ms) helper :? helper


-----


bcastToOutsideMobs :: HasCallStack => Text -> MudStack ()
bcastToOutsideMobs msg = getState >>= \ms -> let is  = views mobTbl IM.keys ms
                                                 f i = views rmEnv (== OutsideEnv) . getMobRm i $ ms
                                             in bcastNl . pure $ (msg, filter f is)


-----


blankLine :: HasCallStack => MsgQueue -> MudStack ()
blankLine = flip writeMsg BlankLine


-----


dbError :: HasCallStack => MsgQueue -> Cols -> MudStack ()
dbError mq cols = wrapSend mq cols dbErrorMsg >> sendSilentBoot mq


-----


massMsg :: HasCallStack => ThreadMsg -> MudStack ()
massMsg tm = liftIO . atomically . helper =<< getState
  where
    helper (views msgQueueTbl IM.elems -> mqs) = forM_ mqs . flip writeTQueue $ tm


-----


massSend :: HasCallStack => Text -> MudStack ()
massSend msg = liftIO . atomically . helper =<< getState
  where
    helper ms@(views msgQueueTbl IM.toList -> kvs) = forM_ kvs $ \(i, mq) ->
        let cols = getColumns i ms
        in writeTQueue mq . FromServer . frame cols . wrapUnlines cols $ msg


-----


mkBcast :: Id -> Text -> [Broadcast]
mkBcast i = pure . (, pure i)


-----


mkNTBcast :: Id -> Text -> [ClassifiedBcast]
mkNTBcast i = pure . NonTargetBcast . (, pure i)


-----


multiSend :: HasCallStack => MsgQueue -> [Text] -> MudStack ()
multiSend mq = send mq . T.unlines


-----


multiWrapSend :: HasCallStack => MsgQueue -> Cols -> [Text] -> MudStack ()
multiWrapSend = multiWrapSendHepler multiWrapNl


multiWrapSend1Nl :: HasCallStack => MsgQueue -> Cols -> [Text] -> MudStack ()
multiWrapSend1Nl = multiWrapSendHepler multiWrap


multiWrapSendHepler :: HasCallStack => (Cols -> [Text] -> Text) -> MsgQueue -> Cols -> [Text] -> MudStack ()
multiWrapSendHepler f mq cols = send mq . f cols


-----


ok :: HasCallStack => MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"


-----


parseDesig :: HasCallStack => Id -> MudState -> Text -> Text
parseDesig = parseDesigHelper (const id)


parseExpandDesig :: HasCallStack => Id -> MudState -> Text -> Text -- Tack on a suffix consisting of the ent sing in parenthesis (for example, "the 1st female nymph(Zappy)"). Used in logging.
parseExpandDesig = parseDesigHelper (\es -> (<> parensQuote es))


parseDesigHelper :: HasCallStack => (Sing -> Text -> Text) -> Id -> MudState -> Text -> Text
parseDesigHelper suffixer i ms = loop
  where
    loop txt = helper pairs
      where
        helper ((delim, expander):xs) | delim `T.isInfixOf` txt = let (left, d, rest) = extractDesig delim txt
                                                                  in left <> expander i ms d <> loop rest
                                      | otherwise               = helper xs
        helper []                                               = txt
    pairs = [ (stdDesigDelimiter,    expandStdDesig suffixer)
            , (nonStdDesigDelimiter, expandNonStdDesig      )
            , (corpseDesigDelimiter, expandCorpseDesig      ) ] |&| map (first T.singleton)
    extractDesig delim (T.breakOn delim -> (left, T.breakOn delim . T.tail -> (desigTxt, T.tail -> rest))) =
        (left, deserialize . quoteWith delim $ desigTxt :: Desig, rest)


expandStdDesig :: HasCallStack => (Sing -> Text -> Text) -> Id -> MudState -> Desig -> Text
expandStdDesig f i ms d@StdDesig { .. }
  | not . isMobRmLit i $ ms = mkCapsFun desigCap "someone"
  | otherwise = let intros = getIntroduced i ms
                in case desigEntSing of Just es -> if es `elem` intros
                                                     then es
                                                     else expandEntName i ms d intros |&| (isPla desigId ms ? f es :? id)
                                        Nothing -> expandEntName i ms d intros
expandStdDesig _ _ _ d = pmf "expandStdDesig" d


expandEntName :: HasCallStack => Id -> MudState -> Desig -> [Sing] -> Text
expandEntName i ms StdDesig { .. } intros | f      <- mkCapsFun desigCap
                                          , (h, t) <- headTail desigEntName
                                          , s      <- getSing desigId ms = if isPla desigId ms
                                            then f . T.concat $ [ the xth, expandSex h, " ", t ]
                                            else onFalse (isCapital s) (f . the) s
  where
    xth = let idsInRm = filter ((`notElem` intros) . (`getSing` ms)) $ i `delete` desigIds
              matches = foldr (\pi -> onTrue (mkUnknownPCEntName pi ms == desigEntName) (pi :)) [] idsInRm
          in length matches > 1 |?| maybeEmp (spcR . mkOrdinal . succ) (elemIndex desigId matches)
    expandSex 'm' = "male"
    expandSex 'f' = "female"
    expandSex x   = pmf "expandEntName expandSex" x
expandEntName _ _ d _ = pmf "expandEntName" d


expandNonStdDesig :: HasCallStack => Id -> MudState -> Desig -> Text
expandNonStdDesig i ms NonStdDesig { .. }
  | isMobRmLit i ms = dEntSing `elem` getIntroduced i ms ? dEntSing :? dDesc
  | otherwise       = mkCapsFun dCap "someone"
expandNonStdDesig _ _ d = pmf "expandNonStdDesig" d


expandCorpseDesig :: HasCallStack => Id -> MudState -> Desig -> Text
expandCorpseDesig i ms (CorpseDesig ci) = mkCorpseAppellation i ms ci
expandCorpseDesig _ _  d                = pmf "expandCorpseDesig" d


-----


retainedMsg :: HasCallStack => Id -> MudState -> Text -> MudStack ()
retainedMsg targetId ms msg@(T.uncons -> Just (x, xs))
  | isNpc targetId ms                 = bcastNl . mkBcast targetId $ stripMarker
  | isLoggedIn . getPla targetId $ ms = let (targetMq, targetCols) = getMsgQueueColumns targetId ms
                                        in wrapSend targetMq targetCols stripMarker
  | otherwise                         = tweak $ plaTbl.ind targetId.retainedMsgs <>~ pure msg
  where
    stripMarker | x == fromPersonMarker = xs
                | otherwise             = msg
retainedMsg _ _ _ = unit


-----


send :: HasCallStack => MsgQueue -> Text -> MudStack ()
send mq = writeMsg mq . FromServer


-----


sendCmdNotFound :: HasCallStack => Id -> MsgQueue -> Cols -> MudStack ()
sendCmdNotFound i mq cols = isSpiritId i <$> getState >>= \case
  True -> modifyStateSeq $ \ms ->
      let helperA pt   = ms & plaTbl .~ pt
          helperB []   = pure f
          helperB msgs = pure . multiWrapSend mq cols $ sorryCmdNotFound : msgs
      in (helperA *** helperB) . views plaTbl (firstSpiritCmdNotFound i) $ ms
  False -> f
  where
    f = send mq . nlnl $ sorryCmdNotFound


firstSpiritCmdNotFound :: HasCallStack => Id -> PlaTbl -> (PlaTbl, [Text])
firstSpiritCmdNotFound i pt
  | pt^.ind i.to isNotFirstSpiritCmdNotFound = (pt, [])
  | otherwise                                = ( pt & ind i %~ setPlaFlag IsNotFirstSpiritCmdNotFound True
                                               , [ "", hintSpiritCmdNotFound ] )


-----


sendDfltPrompt :: HasCallStack => MsgQueue -> Id -> MudStack ()
sendDfltPrompt mq i = ((>>) <$> sendPrompt mq . mkDfltPrompt i <*> sendGmcpVitals i) =<< getState


mkDfltPrompt :: HasCallStack => Id -> MudState -> Text
mkDfltPrompt i ms = let (hps,  mps,  pps,  fps ) = getPts i ms
                        (isHp, isMp, isPp, isFp) | isPla i ms = ( isShowingHp
                                                                , isShowingMp
                                                                , isShowingPp
                                                                , isShowingFp ) & each %~ (getPla i ms |&|)
                                                 | otherwise  = dup4 True
                        marker = colorWith indentColor " "
                        txt    = spaces . dropBlanks $ [ isHp |?| f "h" hps
                                                       , isMp |?| f "m" mps
                                                       , isPp |?| f "p" pps
                                                       , isFp |?| f "f" fps ]
                    in marker |<>| txt <> ">"
  where
    indentColor     = isNpc i ms ? toNpcColor :? promptIndentColor
    f a pair@(x, _) = commaShow x <> colorWith (mkColorTxtForXps pair) a


-----


sendGmcpRmInfo :: HasCallStack => Maybe Int -> Id -> MudState -> MudStack ()
sendGmcpRmInfo maybeZoom = gmcpHelper (gmcpRmInfo maybeZoom)


gmcpHelper :: HasCallStack => (Id -> MudState -> Text) -> Id -> MudState -> MudStack ()
gmcpHelper f i ms
  | isNpc i ms = maybeVoid helper . getPossessor i $ ms
  | otherwise  = helper i
  where
    helper i' | isGmcpId i' ms = send (getMsgQueue i' ms) . quoteWith' (telnetGmcpLeft, telnetGmcpRight) . f i $ ms
              | otherwise      = unit


sendGmcpVitals :: HasCallStack => Id -> MudState -> MudStack ()
sendGmcpVitals = gmcpHelper gmcpVitals


-----


sendMsgBoot :: HasCallStack => MsgQueue -> Maybe Text -> MudStack ()
sendMsgBoot mq = writeMsg mq . MsgBoot . fromMaybe dfltBootMsg


sendSilentBoot :: HasCallStack => MsgQueue -> MudStack ()
sendSilentBoot mq = writeMsg mq SilentBoot


-----


sendPrompt :: HasCallStack => MsgQueue -> Text -> MudStack ()
sendPrompt mq = writeMsg mq . Prompt


-----


wrapSend :: HasCallStack => MsgQueue -> Cols -> Text -> MudStack ()
wrapSend = wrapSendHepler wrapUnlinesNl


wrapSend1Nl :: HasCallStack => MsgQueue -> Cols -> Text -> MudStack ()
wrapSend1Nl = wrapSendHepler wrapUnlines


wrapSendHepler :: HasCallStack => (Cols -> Text -> Text) -> MsgQueue -> Cols -> Text -> MudStack ()
wrapSendHepler f mq cols = send mq . f cols


-----


wrapSendPrompt :: HasCallStack => MsgQueue -> Cols -> Text -> MudStack ()
wrapSendPrompt mq cols = sendPrompt mq . wrapUnlinesInit cols


-----


writeMsg :: HasCallStack => MsgQueue -> ThreadMsg -> MudStack ()
writeMsg mq = liftIO . atomically . writeTQueue mq
