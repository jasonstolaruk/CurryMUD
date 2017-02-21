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

import Mud.Cmds.Msgs.Hint
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.GMCP
import Mud.Data.State.Util.Misc
import Mud.Misc.ANSI
import Mud.Misc.Misc
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Telnet.Chars
import Mud.Util.List (nubSort)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (each, to, views)
import Control.Lens.Operators ((.~), (&), (%~), (^.), (<>~))
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), delete, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Prelude hiding (pi)
import qualified Data.IntMap.Strict as IM (elems, toList)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Output"


-- ============================================================


anglePrompt :: HasCallStack => MsgQueue -> MudStack ()
anglePrompt = flip sendPrompt ">"


-----


bcast :: HasCallStack => [Broadcast] -> MudStack ()
bcast [] = unit
bcast bs = getState >>= \ms -> liftIO . atomically . forM_ bs . sendBcast $ ms
  where
    sendBcast ms (msg, is) = mapM_ helper is
      where
        helper targetId = case getType targetId ms of
          PCType  -> writeIt FromServer targetId
          NpcType -> maybeVoid (writeIt ToNpc) . getPossessor targetId $ ms
          t       -> patternMatchFail "bcast sendBcast helper" . showText $ t
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
bcastIfNotIncog i bs = getState >>= \ms -> onTrue (isPC i ms) (unless (isIncognito . getPla i $ ms)) . bcast $ bs


-----


bcastIfNotIncogNl :: HasCallStack => Id -> [Broadcast] -> MudStack ()
bcastIfNotIncogNl i = bcastIfNotIncog i . appendNlBs


appendNlBs :: HasCallStack => [Broadcast] -> [Broadcast]
appendNlBs bs = bs ++ pure (theNl, nubSort . concatMap snd $ bs)


-----


bcastNl :: HasCallStack => [Broadcast] -> MudStack ()
bcastNl = bcast . appendNlBs


-----


bcastOtherAdmins :: HasCallStack => Id -> Text -> MudStack ()
bcastOtherAdmins = bcastAdminsHelper . delete


-----


bcastOthersInRm :: HasCallStack => Id -> Text -> MudStack ()
bcastOthersInRm i msg = getState >>= \ms ->
    let helper = let ((i `delete`) -> ris) = getMobRmInv i ms
                 in bcast . pure $ (msg, findMobIds ms ris)
    in isPC i ms ? unless (isIncognito . getPla i $ ms) helper :? helper


-----


blankLine :: HasCallStack => MsgQueue -> MudStack ()
blankLine = flip writeMsg BlankLine


-----


dbError :: HasCallStack => MsgQueue -> Cols -> MudStack ()
dbError mq cols = wrapSend mq cols dbErrorMsg >> sendSilentBoot mq


-----


massMsg :: HasCallStack => Msg -> MudStack ()
massMsg msg = liftIO . atomically . helper =<< getState
  where
    helper (views msgQueueTbl IM.elems -> mqs) = forM_ mqs . flip writeTQueue $ msg


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


parseExpandDesig :: HasCallStack => Id -> MudState -> Text -> Text
parseExpandDesig = parseDesigHelper (\es -> (<> parensQuote es))


parseDesigHelper :: HasCallStack => (Sing -> Text -> Text) -> Id -> MudState -> Text -> Text
parseDesigHelper f i ms = loop (getIntroduced i ms)
  where
    loop intros txt | T.singleton stdDesigDelimiter `T.isInfixOf` txt
                    , (left, desig, rest) <- extractDesig stdDesigDelimiter txt
                    = case desig of
                      d@StdDesig { desigEntSing = Just es, .. } ->
                        left                                                              <>
                        (if es `elem` intros
                           then es
                           else expandEntName i ms d ^.to (isPC desigId ms ? f es :? id)) <>
                        loop intros rest
                      d@StdDesig { desigEntSing = Nothing,  .. } ->
                        left <> expandEntName i ms d <> loop intros rest
                      _ -> patternMatchFail "parseDesigHelper loop" . showText $ desig
                    | T.singleton nonStdDesigDelimiter `T.isInfixOf` txt
                    , (left, NonStdDesig { .. }, rest) <- extractDesig nonStdDesigDelimiter txt
                    = left <> (dEntSing `elem` intros ? dEntSing :? dDesc) <> loop intros rest
                    | T.singleton corpseDesigDelimiter `T.isInfixOf` txt
                    , (left, CorpseDesig ci, rest) <- extractDesig corpseDesigDelimiter txt
                    = left <> mkCorpseAppellation i ms ci <> loop intros rest
                    | otherwise = txt
    extractDesig (T.singleton -> c) (T.breakOn c -> (left, T.breakOn c . T.tail -> (desigTxt, T.tail -> rest)))
      | desig <- deserialize . quoteWith c $ desigTxt :: Desig
      = (left, desig, rest)


expandEntName :: HasCallStack => Id -> MudState -> Desig -> Text
expandEntName i ms StdDesig { .. } = let f      = mkCapsFun desigCap
                                         (h, t) = headTail desigEntName
                                         s      = getSing desigId ms
                                     in if isPC desigId ms
                                       then f . T.concat $ [ the xth, expandSex h, " ", t ]
                                       else onFalse (isCapital s) (f . the) s
  where
    xth = let intros  = getIntroduced i ms
              idsInRm = filter ((`notElem` intros) . (`getSing` ms)) $ i `delete` desigIds
              matches = foldr (\pi -> onTrue (mkUnknownPCEntName pi ms == desigEntName) (pi :)) [] idsInRm
          in length matches > 1 |?| maybeEmp (spcR . mkOrdinal . succ) (elemIndex desigId matches)
    expandSex 'm' = "male"
    expandSex 'f' = "female"
    expandSex x   = patternMatchFail "expandEntName expandSex" . T.singleton $ x
expandEntName _ _ d = patternMatchFail "expandEntName" . showText $ d


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
                        (isHp, isMp, isPp, isFp) | isPC i ms = ( isShowingHp
                                                               , isShowingMp
                                                               , isShowingPp
                                                               , isShowingFp ) & each %~ (getPla i ms |&|)
                                                 | otherwise = dup4 True
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
  | isGmcpId i ms = send (getMsgQueue i ms) . quoteWith' (telnetGmcpLeft, telnetGmcpRight) . f i $ ms
  | otherwise     = unit


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


writeMsg :: HasCallStack => MsgQueue -> Msg -> MudStack ()
writeMsg mq = liftIO . atomically . writeTQueue mq
