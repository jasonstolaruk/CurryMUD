{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
                                  , sendMsgBoot
                                  , sendPrompt
                                  , sendPromptNl
                                  , sendSilentBoot
                                  , wrapSend
                                  , wrapSend1Nl
                                  , wrapSendPrompt
                                  , wrapSendPromptNl
                                  , writeMsg ) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Msgs.Sorry
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Misc.ANSI
import Mud.Misc.Misc
import Mud.TopLvlDefs.Chars
import Mud.Util.List (nubSort)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (each, to, views)
import Control.Lens.Operators ((%~), (&), (<>~), (^.))
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), delete, elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (elems, toList)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Output"


-- ============================================================


anglePrompt :: MsgQueue -> MudStack ()
anglePrompt = flip sendPrompt "> "


-----


bcast :: [Broadcast] -> MudStack ()
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


bcastAdmins :: Text -> MudStack ()
bcastAdmins = bcastAdminsHelper id


bcastAdminsHelper :: (Inv -> Inv) -> Text -> MudStack ()
bcastAdminsHelper f msg =
    bcastNl . pure . (colorWith adminBcastColor msg, ) =<< f . getLoggedInAdminIds <$> getState


-----


bcastAdminsExcept :: Inv -> Text -> MudStack ()
bcastAdminsExcept = bcastAdminsHelper . flip (\\)


-----


bcastIfNotIncog :: Id -> [Broadcast] -> MudStack ()
bcastIfNotIncog i bs = getState >>= \ms -> onTrue (isPC i ms) (unless (isIncognito . getPla i $ ms)) . bcast $ bs


-----


bcastIfNotIncogNl :: Id -> [Broadcast] -> MudStack ()
bcastIfNotIncogNl i = bcastIfNotIncog i . appendNlBs


appendNlBs :: [Broadcast] -> [Broadcast]
appendNlBs bs = bs ++ [(theNl, nubSort . concatMap snd $ bs)]


-----


bcastNl :: [Broadcast] -> MudStack ()
bcastNl = bcast . appendNlBs


-----


bcastOtherAdmins :: Id -> Text -> MudStack ()
bcastOtherAdmins = bcastAdminsHelper . delete


-----


bcastOthersInRm :: Id -> Text -> MudStack ()
bcastOthersInRm i msg = getState >>= \ms ->
    let helper = let ((i `delete`) -> ris) = getMobRmInv i ms
                 in bcast . pure $ (msg, findMobIds ms ris)
    in isPC i ms ? unless (isIncognito . getPla i $ ms) helper :? helper


-----


blankLine :: MsgQueue -> MudStack ()
blankLine = flip writeMsg BlankLine


-----


dbError :: MsgQueue -> Cols -> MudStack ()
dbError mq cols = wrapSend mq cols dbErrorMsg >> sendSilentBoot mq


-----


massMsg :: Msg -> MudStack ()
massMsg msg = liftIO . atomically . helper =<< getState
  where
    helper (views msgQueueTbl IM.elems -> mqs) = forM_ mqs $ flip writeTQueue msg


-----


massSend :: Text -> MudStack ()
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


multiSend :: MsgQueue -> [Text] -> MudStack ()
multiSend mq = send mq . T.unlines


-----


multiWrapSend :: MsgQueue -> Cols -> [Text] -> MudStack ()
multiWrapSend = multiWrapSendHepler multiWrapNl


multiWrapSend1Nl :: MsgQueue -> Cols -> [Text] -> MudStack ()
multiWrapSend1Nl = multiWrapSendHepler multiWrap


multiWrapSendHepler :: (Cols -> [Text] -> Text) -> MsgQueue -> Cols -> [Text] -> MudStack ()
multiWrapSendHepler f mq cols = send mq . f cols


-----


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"


-----


parseDesig :: Id -> MudState -> Text -> Text
parseDesig = parseDesigHelper (const id)


parseExpandDesig :: Id -> MudState -> Text -> Text
parseExpandDesig = parseDesigHelper (\es -> (<> parensQuote es))


parseDesigHelper :: (Sing -> Text -> Text) -> Id -> MudState -> Text -> Text
parseDesigHelper f i ms = loop (getIntroduced i ms)
  where
    loop intros txt | T.singleton stdDesigDelimiter `T.isInfixOf` txt
                    , (left, pcd, rest) <- extractDesigTxt stdDesigDelimiter txt
                    = case pcd of
                      d@StdDesig { desigEntSing = Just es, .. } ->
                        left                                                              <>
                        (if es `elem` intros
                           then es
                           else expandEntName i ms d ^.to (isPC desigId ms ? f es :? id)) <>
                        loop intros rest
                      d@StdDesig { desigEntSing = Nothing,  .. } ->
                        left <> expandEntName i ms d <> loop intros rest
                      _ -> patternMatchFail "parseDesigHelper loop" . showText $ pcd
                    | T.singleton nonStdDesigDelimiter `T.isInfixOf` txt
                    , (left, NonStdDesig { .. }, rest) <- extractDesigTxt nonStdDesigDelimiter txt
                    = left <> (dEntSing `elem` intros ? dEntSing :? dDesc) <> loop intros rest
                    | otherwise = txt
    extractDesigTxt (T.singleton -> c) (T.breakOn c -> (left, T.breakOn c . T.tail -> (pcdTxt, T.tail -> rest)))
      | pcd <- deserialize . quoteWith c $ pcdTxt :: Desig
      = (left, pcd, rest)


expandEntName :: Id -> MudState -> Desig -> Text
expandEntName i ms StdDesig { .. } =
  let f      = mkCapsFun desigShouldCap
      (h, t) = headTail desigEntName
  in if isPC desigId ms
    then T.concat [ f "the ", xth, expandSex h, " ", t ]
    else let s = getSing desigId ms in onFalse (isCapital s) (f . ("the " <>)) s
  where
    xth = let intros  = getIntroduced i ms
              idsInRm = filter ((`notElem` intros) . (`getSing` ms)) $ i `delete` desigIds
              matches = foldr (\pi acc -> onTrue (mkUnknownPCEntName pi ms == desigEntName) (pi :) acc) [] idsInRm
          in length matches > 1 |?| (<> " ") . mkOrdinal . succ . fromJust . elemIndex desigId $ matches
    expandSex 'm' = "male"
    expandSex 'f' = "female"
    expandSex x   = patternMatchFail "expandEntName expandSex" . T.singleton $ x
expandEntName _ _ d = patternMatchFail "expandEntName" . showText $ d


-----


retainedMsg :: Id -> MudState -> Text -> MudStack ()
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


send :: MsgQueue -> Text -> MudStack ()
send mq = writeMsg mq . FromServer


-----


sendCmdNotFound :: MsgQueue -> MudStack ()
sendCmdNotFound mq = send mq . nlnl $ sorryCmdNotFound


-----


sendDfltPrompt :: MsgQueue -> Id -> MudStack ()
sendDfltPrompt mq i = sendPromptNl mq . mkDfltPrompt i =<< getState


mkDfltPrompt :: Id -> MudState -> Text
mkDfltPrompt i ms = let (hps, mps, pps, fps)     = getPts i ms
                        p                        = getPla i ms
                        (isHp, isMp, isPp, isFp) = (isShowingHp, isShowingMp, isShowingPp, isShowingFp) & each %~ (p |&|)
                        marker                   = colorWith indentColor " "
                    in marker <> " " <> (spaces . dropBlanks $ [ isHp |?| f "h" hps
                                                               , isMp |?| f "m" mps
                                                               , isPp |?| f "p" pps
                                                               , isFp |?| f "f" fps ]) <> "> "
  where
    indentColor     = isNpc i ms ? toNpcColor :? promptIndentColor
    f a pair@(x, _) = commaShow x <> colorWith (mkColorTxtForXps pair) a


-----


sendMsgBoot :: MsgQueue -> Maybe Text -> MudStack ()
sendMsgBoot mq = writeMsg mq . MsgBoot . fromMaybe dfltBootMsg


sendSilentBoot :: MsgQueue -> MudStack ()
sendSilentBoot mq = writeMsg mq SilentBoot


-----


sendPrompt :: MsgQueue -> Text -> MudStack ()
sendPrompt mq = writeMsg mq . Prompt


sendPromptNl :: MsgQueue -> Text -> MudStack ()
sendPromptNl mq = writeMsg mq . PromptNl


-----


wrapSend :: MsgQueue -> Cols -> Text -> MudStack ()
wrapSend = wrapSendHepler wrapUnlinesNl


wrapSend1Nl :: MsgQueue -> Cols -> Text -> MudStack ()
wrapSend1Nl = wrapSendHepler wrapUnlines


wrapSendHepler :: (Cols -> Text -> Text) -> MsgQueue -> Cols -> Text -> MudStack ()
wrapSendHepler f mq cols = send mq . f cols


-----


wrapSendPrompt :: MsgQueue -> Cols -> Text -> MudStack ()
wrapSendPrompt = wrapSendPromptHelper sendPrompt


wrapSendPromptNl :: MsgQueue -> Cols -> Text -> MudStack ()
wrapSendPromptNl = wrapSendPromptHelper sendPromptNl


wrapSendPromptHelper :: (MsgQueue -> Text -> MudStack ()) -> MsgQueue -> Cols -> Text -> MudStack ()
wrapSendPromptHelper f mq cols = f mq . wrapUnlinesInit cols


-----


writeMsg :: MsgQueue -> Msg -> MudStack ()
writeMsg mq = liftIO . atomically . writeTQueue mq
