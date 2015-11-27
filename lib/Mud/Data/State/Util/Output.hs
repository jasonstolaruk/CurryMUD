{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, TupleSections, ViewPatterns #-}

module Mud.Data.State.Util.Output ( bcast
                                  , bcastAdmins
                                  , bcastAdminsExcept
                                  , bcastIfNotIncog
                                  , bcastIfNotIncogNl
                                  , bcastNl
                                  , bcastOtherAdmins
                                  , bcastOthersInRm
                                  , bcastSelfOthers
                                  , frame
                                  , massMsg
                                  , massSend
                                  , mkBcast
                                  , mkNTBcast
                                  , multiWrapSend
                                  , ok
                                  , parsePCDesig
                                  , prompt
                                  , retainedMsg
                                  , send
                                  , sendMsgBoot
                                  , wrapSend ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.Util.List (nubSort)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow (second)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Lens (views)
import Control.Lens.Operators ((<>~))
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\), delete, elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (elems, toList)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Output"


-- ============================================================


bcast :: [Broadcast] -> MudStack ()
bcast [] = unit
bcast bs = getState >>= \ms -> liftIO . atomically . mapM_ (sendBcastSTM ms) $ bs
  where
    sendBcastSTM ms (msg, is) = mapM_ helper is
      where
        helper targetId = case getType targetId ms of
          PCType  -> writeIt FromServer targetId
          NpcType -> maybe unit (writeIt ToNpc) . getPossessor targetId $ ms
          t       -> patternMatchFail "bcast sendBcastSTM helper" [ showText t ]
        writeIt f i = let (mq, cols) = getMsgQueueColumns i ms -- TODO: Just subtract 2 from cols here?
                      in writeTQueue mq . f . T.unlines . concatMap (wrap cols) . T.lines . parsePCDesig i ms $ msg


-----


bcastAdmins :: T.Text -> MudStack ()
bcastAdmins = bcastAdminsHelper id


bcastAdminsHelper :: (Inv -> Inv) -> T.Text -> MudStack ()
bcastAdminsHelper f msg =
    (f . getLoggedInAdminIds <$> getState) >>= bcastNl . pure . (colorWith adminBcastColor msg, )


-----


bcastAdminsExcept :: Inv -> T.Text -> MudStack ()
bcastAdminsExcept is = bcastAdminsHelper (\\ is)


-----


bcastIfNotIncog :: Id -> [Broadcast] -> MudStack ()
bcastIfNotIncog i bs = getState >>= \ms -> bcast $ if isIncognitoId i ms
                                             then map (second (filter (== i))) bs
                                             else bs


-----


bcastIfNotIncogNl :: Id -> [Broadcast] -> MudStack ()
bcastIfNotIncogNl i = bcastIfNotIncog i . appendNlBs


appendNlBs :: [Broadcast] -> [Broadcast]
appendNlBs bs = bs ++ [("\n", nubSort . concatMap snd $ bs)]


-----


bcastNl :: [Broadcast] -> MudStack ()
bcastNl = bcast . appendNlBs


-----


bcastOtherAdmins :: Id -> T.Text -> MudStack ()
bcastOtherAdmins i = bcastAdminsHelper (i `delete`)


-----


bcastOthersInRm :: Id -> T.Text -> MudStack ()
bcastOthersInRm i msg = getState >>= \ms ->
    unless (isIncognitoId i ms) $ let ((i `delete`) -> ris) = getPCRmInv i ms
                                  in bcast [(msg, findPCIds ms ris)]


-----


bcastSelfOthers :: Id -> MudState -> [Broadcast] -> [Broadcast] -> MudStack ()
bcastSelfOthers i ms toSelf toOthers = do
    bcast toSelf
    unless (isIncognitoId i ms) . bcast $ toOthers


-----


massMsg :: Msg -> MudStack ()
massMsg msg = liftIO . atomically . helperSTM =<< getState
  where
    helperSTM (views msgQueueTbl IM.elems -> mqs) = mapM_ (`writeTQueue` msg) mqs


-----


massSend :: T.Text -> MudStack ()
massSend msg = liftIO . atomically . helperSTM =<< getState
  where
    helperSTM ms@(views msgQueueTbl IM.toList -> kvs) = forM_ kvs $ \(i, mq) ->
        let cols = getColumns i ms
        in writeTQueue mq . FromServer . frame cols . wrapUnlines cols $ msg


-----


mkBcast :: Id -> T.Text -> [Broadcast]
mkBcast i = pure . (, pure i)


-----


mkNTBcast :: Id -> T.Text -> [ClassifiedBcast]
mkNTBcast i msg = [NonTargetBcast (msg, pure i)]


-----


multiWrapSend :: MsgQueue -> Cols -> [T.Text] -> MudStack ()
multiWrapSend mq cols = send mq . multiWrapNl cols


-----


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"


-----


parsePCDesig :: Id -> MudState -> T.Text -> T.Text
parsePCDesig i ms = loop (getIntroduced i ms)
  where
    loop intros txt
      | T.singleton stdDesigDelimiter `T.isInfixOf` txt
      , (left, pcd, rest) <- extractPCDesigTxt stdDesigDelimiter txt
      = case pcd of
        StdDesig { stdPCEntSing = Just pes, .. } ->
          left                                                                             <>
          (pes `elem` intros ? pes :? expandPCEntName i ms shouldCap pcEntName pcId pcIds) <>
          loop intros rest
        StdDesig { stdPCEntSing = Nothing,  .. } ->
          left <> expandPCEntName i ms shouldCap pcEntName pcId pcIds <> loop intros rest
        _ -> patternMatchFail "parsePCDesig loop" [ showText pcd ]
      | T.singleton nonStdDesigDelimiter `T.isInfixOf` txt
      , (left, NonStdDesig { .. }, rest) <- extractPCDesigTxt nonStdDesigDelimiter txt
      = left <> (nonStdPCEntSing `elem` intros ? nonStdPCEntSing :? nonStdDesc) <> loop intros rest
      | otherwise = txt
    extractPCDesigTxt (T.singleton -> c) (T.breakOn c -> (left, T.breakOn c . T.tail -> (pcdTxt, T.tail -> rest)))
      | pcd <- deserialize . quoteWith c $ pcdTxt :: PCDesig
      = (left, pcd, rest)


expandPCEntName :: Id -> MudState -> ShouldCap -> T.Text -> Id -> Inv -> T.Text
expandPCEntName i ms (mkCapsFun -> f) pen@(headTail -> (h, t)) pcIdToExpand ((i `delete`) -> pcIdsInRm) =
    T.concat [ f "the ", xth, expandSex h, " ", t ]
  where
    xth     = let matches = foldr (\pi acc -> mkUnknownPCEntName pi ms == pen ? pi : acc :? acc) [] pcIdsInRm
              in length matches > 1 |?| (<> " ") . mkOrdinal . succ . fromJust . elemIndex pcIdToExpand $ matches
    expandSex 'm'                = "male"
    expandSex 'f'                = "female"
    expandSex (T.singleton -> x) = patternMatchFail "expandPCEntName expandSex" [x]


-----


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


-----


retainedMsg :: Id -> MudState -> T.Text -> MudStack ()
retainedMsg targetId ms targetMsg@(T.uncons -> Just (x, xs))
  | isLoggedIn . getPla targetId $ ms = let (targetMq, targetCols) = getMsgQueueColumns targetId ms
                                        in wrapSend targetMq targetCols stripMarker
  | otherwise                         = tweak $ plaTbl.ind targetId.retainedMsgs <>~ pure targetMsg
  where
    stripMarker | x == fromPersonMarker = xs
                | otherwise             = targetMsg
retainedMsg _ _ _ = unit


-----


send :: MsgQueue -> T.Text -> MudStack ()
send mq = liftIO . atomically . writeTQueue mq . FromServer


-----


sendMsgBoot :: MsgQueue -> Maybe T.Text -> MudStack ()
sendMsgBoot mq = liftIO . atomically . writeTQueue mq . MsgBoot . fromMaybe dfltBootMsg


-----


wrapSend :: MsgQueue -> Cols -> T.Text -> MudStack ()
wrapSend mq cols = send mq . wrapUnlinesNl cols
