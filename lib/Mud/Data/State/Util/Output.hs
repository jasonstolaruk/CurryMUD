{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Data.State.Util.Output ( bcast
                                  , bcastAdmins
                                  , bcastNl
                                  , bcastOthersInRm
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

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Misc.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Msgs
import Mud.Util.List (nubSort)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
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


bcast :: MudState -> [Broadcast] -> MudStack ()
bcast mt mqt pcTbl plaTbl = mapM_ (\(msg, is) -> mapM_ (helper msg) is)
  where
    helper msg i = let mq   = (ms^.msgQueueTbl) ! i
                       cols = view columns $ (ms^.plaTbl) ! i
                   in sendSTM mq . T.unlines . concatMap (wrap cols) . T.lines . parsePCDesig i mt pcTbl $ msg


-----


bcastAdmins :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> T.Text -> MudStack ()
bcastAdmins mt mqt pcTbl plaTbl = liftIO . atomically . bcastAdminsSTM mt mqt pcTbl plaTbl


bcastAdminsSTM :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> T.Text -> STM ()
bcastAdminsSTM mt mqt pcTbl plaTbl msg =
    bcastNlSTM mt mqt pcTbl plaTbl [( adminNoticeColor <> msg <> dfltColor
                                 , [ pi | pi <- IM.keys plaTbl, getPlaFlag IsAdmin (plaTbl ! pi) ] )]


-----


bcastNl :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> [Broadcast] -> MudStack ()
bcastNl mt mqt pcTbl plaTbl = liftIO . atomically . bcastNlSTM mt mqt pcTbl plaTbl


bcastNlSTM :: MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> [Broadcast] -> STM ()
bcastNlSTM mt mqt pcTbl plaTbl bs =
    bcastSTM mt mqt pcTbl plaTbl . (bs ++) . concat $ [ mkBroadcast i "\n" | i <- nubSort . concatMap snd $ bs ]


-----


bcastOthersInRm :: Id -> InvTbl -> MobTbl -> MsgQueueTbl -> PCTbl -> PlaTbl -> TypeTbl -> T.Text -> MudStack ()
bcastOthersInRm i it mt mqt pcTbl plaTbl tt msg = let ri  = (pcTbl ! i)^.rmId
                                                      ris = i `delete` (it ! ri)
                                                      bs  = [(msg, findPCIds tt ris)]
                                                  in bcast mt mqt pcTbl plaTbl bs


-----


frame :: Cols -> T.Text -> T.Text
frame cols | divider <- nl . mkDividerTxt $ cols = nl . (<> divider) . (divider <>)


-----


massMsg :: Msg -> MudStack ()
massMsg msg = liftIO . atomically . helperSTM =<< ask
  where
    helperSTM md = mapM_ (`writeTQueue` msg) =<< IM.elems <$> readTVar (md^.msgQueueTblTVar)


-----


massSend :: MudState -> T.Text -> MudStack ()
massSend ms msg = liftIO . atomically $ helperSTM
  where
    helperSTM = let mqt = ms^.msgQueueTbl
                    pt  = ms^.plaTbl
                    helper i | mq   <- mqt ! i
                             , cols <- (pt ! i)^.columns
                             = writeTQueue mq . FromServer . frame cols . wrapUnlines cols $ msg
                in forM_ (IM.keys pt) helper


-----


mkBroadcast :: Id -> T.Text -> [Broadcast]
mkBroadcast i msg = [(msg, [i])]


-----


mkDividerTxt :: Cols -> T.Text
mkDividerTxt = flip T.replicate "="


-----


mkNTBroadcast :: Id -> T.Text -> [ClassifiedBroadcast]
mkNTBroadcast i msg = [NonTargetBroadcast (msg, [i])]


-----


multiWrapSend :: MsgQueue -> Cols -> [T.Text] -> MudStack ()
multiWrapSend mq cols = send mq . multiWrapNl cols


-----


ok :: MsgQueue -> MudStack ()
ok mq = send mq . nlnl $ "OK!"


-----


parsePCDesig :: Id -> MudState -> T.Text -> T.Text
parsePCDesig i ms@(view pcTbl -> pt) msg = views introduced (`helper` msg) $ pt ! i
  where
    helper intros txt
      | T.singleton stdDesigDelimiter `T.isInfixOf` txt
      , (left, pcd, rest) <- extractPCDesigTxt stdDesigDelimiter txt, mt <- ms^.mobTbl
      = case pcd of
        StdDesig { stdPCEntSing = Just pes, .. } ->
          left                                                                         <>
          (pes `elem` intros ? pes :? expandPCEntName i ms isCap pcEntName pcId pcIds) <>
          helper intros rest
        StdDesig { stdPCEntSing = Nothing,  .. } ->
          left <> expandPCEntName i ms isCap pcEntName pcId pcIds <> helper intros rest
        _ -> patternMatchFail "parsePCDesig helper" [ showText pcd ]
      | T.singleton nonStdDesigDelimiter `T.isInfixOf` txt
      , (left, NonStdDesig { .. }, rest) <- extractPCDesigTxt nonStdDesigDelimiter txt
      = left <> (nonStdPCEntSing `elem` intros ? nonStdPCEntSing :? nonStdDesc) <> helper intros rest
      | otherwise
      = txt
    extractPCDesigTxt (T.singleton -> c) (T.breakOn c -> (left, T.breakOn c . T.tail -> (pcdTxt, T.tail -> rest)))
      | pcd <- deserialize . quoteWith c $ pcdTxt :: PCDesig = (left, pcd, rest)


expandPCEntName :: Id -> MudState -> Bool -> T.Text -> Id -> Inv -> T.Text
expandPCEntName i ms ic pen@(headTail -> (h, t)) pcIdToExpand ((i `delete`) -> pcIdsInRm) =
    T.concat [ leading, "he ", xth, expandSex h, " ", t ]
  where
    leading = ic ? "T" :? "t"
    xth     = let matches = foldr (\pi acc -> mkUnknownPCEntName pi ms == pen ? pi : acc :? acc) [] pcIdsInRm
              in length matches > 1 |?| (<> " ") . mkOrdinal . succ . fromJust . elemIndex pcIdToExpand $ matches
    expandSex 'm'                = "male"
    expandSex 'f'                = "female"
    expandSex (T.singleton -> x) = patternMatchFail "expandPCEntName expandSex" [x]


-----


prompt :: MsgQueue -> T.Text -> MudStack ()
prompt mq = liftIO . atomically . writeTQueue mq . Prompt


-----


send :: MsgQueue -> T.Text -> MudStack ()
send mq = liftIO . atomically . writeTQueue mq . FromServer


-----


sendMsgBoot :: MsgQueue -> Maybe T.Text -> MudStack ()
sendMsgBoot mq = liftIO . atomically . writeTQueue mq . MsgBoot . fromMaybe dfltBootMsg


-----


wrapSend :: MsgQueue -> Cols -> T.Text -> MudStack ()
wrapSend mq cols = send mq wrapUnlinesNl cols
