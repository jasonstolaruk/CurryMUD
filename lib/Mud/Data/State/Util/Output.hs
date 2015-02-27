{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Data.State.Util.Output ( bcast
                                  , bcastAdmins
                                  , bcastNl
                                  , bcastOthersInRm
                                  , expandPCEntName
                                  , frame
                                  , massMsg
                                  , massSend
                                  , mkBroadcast
                                  , mkDividerTxt
                                  , mkNTBroadcast
                                  , multiWrapSend
                                  , ok
                                  , parsePCDesig
                                  , prompt
                                  , send
                                  , sendMsgBoot
                                  , wrapSend ) where

import Mud.ANSI
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.Misc
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Msgs
import Mud.Util.List (nubSort)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Lens.Getter (views)
import Control.Lens.Operators ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.List (delete, elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (elems, keys)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Output"


-- ============================================================


-- TODO: Sort function definitions.


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


send :: MsgQueue -> T.Text -> MudStack ()
send mq = liftIO . atomically . sendSTM mq


sendSTM :: MsgQueue -> T.Text -> STM ()
sendSTM mq = writeTQueue mq . FromServer


wrapSend :: MsgQueue -> Cols -> T.Text -> MudStack ()
wrapSend mq cols = send mq . wrapUnlinesNl cols


multiWrapSend :: MsgQueue -> Cols -> [T.Text] -> MudStack ()
multiWrapSend mq cols = send mq . multiWrapNl cols


sendMsgBoot :: MsgQueue -> Maybe T.Text -> MudStack ()
sendMsgBoot mq = liftIO . atomically . writeTQueue mq . MsgBoot . fromMaybe dfltBootMsg


bcast :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> [Broadcast] -> MudStack ()
bcast mt mqt pcTbl plaTbl = mapM_ (\(msg, is) -> mapM_ (helper msg) is)
  where
    helper msg i | mq   <- mqt ! i
                 , cols <- (plaTbl ! i)^.columns
                 = send mq . T.unlines . concatMap (wrap cols) . T.lines . parsePCDesig i mt pcTbl $ msg


parsePCDesig :: Id -> MobTbl -> PCTbl -> T.Text -> T.Text
parsePCDesig i mt pt msg = views introduced (`helper` msg) $ pt ! i
  where
    helper intros txt
      | T.singleton stdDesigDelimiter `T.isInfixOf` txt
      , (left, pcd, rest) <- extractPCDesigTxt stdDesigDelimiter txt
      = case pcd of
        StdDesig { stdPCEntSing = Just pes, .. } ->
          left                                                                            <>
          (pes `elem` intros ? pes :? expandPCEntName i mt pt isCap pcEntName pcId pcIds) <>
          helper intros rest
        StdDesig { stdPCEntSing = Nothing,  .. } ->
          left <> expandPCEntName i mt pt isCap pcEntName pcId pcIds <> helper intros rest
        _ -> patternMatchFail "parsePCDesig helper" [ showText pcd ]
      | T.singleton nonStdDesigDelimiter `T.isInfixOf` txt
      , (left, NonStdDesig { .. }, rest) <- extractPCDesigTxt nonStdDesigDelimiter txt
      = left <> (nonStdPCEntSing `elem` intros ? nonStdPCEntSing :? nonStdDesc) <> helper intros rest
      | otherwise = txt
    extractPCDesigTxt (T.singleton -> c) (T.breakOn c -> (left, T.breakOn c . T.tail -> (pcdTxt, T.tail -> rest)))
      | pcd <- deserialize . quoteWith c $ pcdTxt :: PCDesig = (left, pcd, rest)


expandPCEntName :: Id -> MobTbl -> PCTbl -> Bool -> T.Text -> Id -> Inv -> T.Text
expandPCEntName i mt pt ic pen@(headTail -> (h, t)) pi ((i `delete`) -> pis) =
    T.concat [ leading, "he ", xth, expandSex h, " ", t ]
  where
    leading | ic        = "T"
            | otherwise = "t"
    xth = let matches = foldr (\pcI acc -> mkUnknownPCEntName pcI mt pt == pen ? pcI : acc :? acc) [] pis
          in case matches of [_] -> ""
                             _   -> (<> " ") . mkOrdinal . (+ 1) . fromJust . elemIndex pi $ matches
    expandSex 'm'                = "male"
    expandSex 'f'                = "female"
    expandSex (T.singleton -> x) = patternMatchFail "expandPCEntName expandSex" [x]


bcastNl :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> [Broadcast] -> MudStack ()
bcastNl mt mqt pcTbl plaTbl bs =
    bcast mt mqt pcTbl plaTbl . (bs ++) . concat $ [ mkBroadcast i "\n" | i <- nubSort . concatMap snd $ bs ]


bcastAdmins :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> T.Text -> MudStack ()
bcastAdmins mt mqt pcTbl plaTbl msg =
    bcastNl mt mqt pcTbl plaTbl [( adminNoticeColor <> msg <> dfltColor
                                 , [ pi | pi <- IM.keys plaTbl, getPlaFlag IsAdmin (plaTbl ! pi) ] )]


mkBroadcast :: Id -> T.Text -> [Broadcast]
mkBroadcast i msg = [(msg, [i])]


mkNTBroadcast :: Id -> T.Text -> [ClassifiedBroadcast]
mkNTBroadcast i msg = [NonTargetBroadcast (msg, [i])]


bcastOthersInRm :: Id -> InvTbl -> MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> TypeTbl -> T.Text -> MudStack ()
bcastOthersInRm i it mt mqt pcTbl plaTbl tt msg = let ri  = (pcTbl ! i)^.rmId
                                                      ris = i `delete` (it ! ri)
                                                      bs  = [(msg, findPCIds tt ris)]
                                                  in bcast mt mqt pcTbl plaTbl bs


massMsg :: Msg -> MudStack ()
massMsg msg = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = mapM_ (flip writeTQueue msg) =<< IM.elems <$> readTVar (md^.msgQueueTblTVar)


massSend :: T.Text -> MudStack ()
massSend msg = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = (,) <$> readTVar (md^.msgQueueTblTVar) <*> readTVar (md^.plaTblTVar) >>= \(mqt, pt) ->
        let helper i | mq   <- mqt ! i
                     , cols <- (pt ! i)^.columns = sendSTM mq . frame cols . wrapUnlines cols $ msg
        in forM_ (IM.keys pt) helper


frame :: Cols -> T.Text -> T.Text
frame cols | divider <- nl . mkDividerTxt $ cols = nl . (<> divider) . (divider <>)


mkDividerTxt :: Cols -> T.Text
mkDividerTxt = flip T.replicate "="


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"
