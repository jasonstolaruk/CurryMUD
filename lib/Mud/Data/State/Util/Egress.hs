{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Egress ( handleEgress
                                  , mkFarewellStats ) where

import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Misc.Logging hiding (logNotice, logPla)
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iNecropolis)
import Mud.Threads.Act
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.FeelingTimer
import Mud.Threads.Regen
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Exception.Lifted (finally)
import Control.Lens (at, each, set, views)
import Control.Lens.Operators ((%~), (&), (+~), (.~), (?~), (^.))
import Control.Monad ((>=>), forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete, partition, sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import Prelude hiding (pi)
import qualified Data.Map.Lazy as M (delete, empty, keys, singleton)
import qualified Data.Text as T


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Data.State.Util.Egress"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Data.State.Util.Egress"


-- ==================================================


handleEgress :: Id -> MsgQueue -> Bool -> MudStack ()
handleEgress i mq isDropped = egressHelper `finally` writeMsg mq FinishedEgress
  where
    egressHelper = do
        logPla "handleEgress egressHelper" i "handling egress."
        stopActs i
        (ms, now) <- (,) <$> getState <*> liftIO getCurrentTime
        let tuple@(s, hoc, spirit) = ((,,) <$> uncurry getSing
                                           <*> uncurry isAdHoc
                                           <*> uncurry isSpiritId) (i, ms)
        unless (hoc || spirit) . bcastOthersInRm i . nlnl . egressMsg . serialize . mkStdDesig i ms $ DoCap
        when spirit . theBeyond i mq s $ isDropped
        helper now tuple |&| modifyState >=> \(bs, logMsgs) -> do
            unless spirit $ do { pauseEffects      i -- Already done in "handleDeath".
                               ; stopFeelings      i
                               ; stopRegen         i
                               ; throwWaitDigester i }
            closePlaLog i
            bcast bs
            bcastAdmins $ s <> " has left CurryMUD."
            forM_ logMsgs . uncurry . logPla $ "handleEgress egressHelper"
            logNotice "handleEgress egressHelper" . T.concat $ [ descSingId i ms, " has left CurryMUD." ]
            when hoc . tweak . removeAdHoc $ i
    helper now (s, hoc, spirit) ms =
        let (ms', bs, logMsgs) = peepHelper ms s spirit
            ms'' | hoc         = ms'
                 | otherwise   = updateHostMap (possessHelper . leaveParty i . movePC ms' $ spirit) s now
        in (ms'', (bs, logMsgs))
    peepHelper ms s spirit =
        let (peeperIds, peepingIds) = getPeepersPeeping i ms
            bs      = [ (nlnl .    T.concat $ [ "You are no longer peeping "
                                              , s
                                              , " "
                                              , parensQuote $ s <> spaced "has" <> txt
                                              , "." ], pure peeperId) | peeperId <- peeperIds ]
            logMsgs = [ (peeperId, T.concat   [ "no longer peeping "
                                              , s
                                              , " "
                                              , parensQuote $ s <> spaced "has" <> txt
                                              , "." ]) | peeperId <- peeperIds ]
            txt     = spirit ? "passed into the beyond" :? "disconnected"
        in (ms & plaTbl %~ stopPeeping     peepingIds
               & plaTbl %~ stopBeingPeeped peeperIds
               & plaTbl.ind i.peeping .~ []
               & plaTbl.ind i.peepers .~ [], bs, logMsgs)
      where
        stopPeeping     peepingIds pt = let f peepedId ptAcc = ptAcc & ind peepedId.peepers %~ (i `delete`)
                                        in foldr f pt peepingIds
        stopBeingPeeped peeperIds  pt = let f peeperId ptAcc = ptAcc & ind peeperId.peeping %~ (i `delete`)
                                        in foldr f pt peeperIds
    updateHostMap ms s now = flip (set $ hostTbl.at s) ms $ case getHostMap s ms of
      Nothing      -> Just . M.singleton host $ newRecord
      Just hostMap -> case hostMap^.at host of Nothing -> Just $ hostMap & at host ?~ newRecord
                                               Just r  -> Just $ hostMap & at host ?~ reviseRecord r
      where
        newRecord       = HostRecord { _noOfLogouts   = 1
                                     , _secsConnected = duration
                                     , _lastLogout    = now }
        reviseRecord r  = r & noOfLogouts   +~ 1
                            & secsConnected +~ duration
                            & lastLogout    .~ now
        host            = getCurrHostName i ms
        duration        = round $ now `diffUTCTime` conTime
        conTime         = fromJust . getConnectTime i $ ms
    movePC ms spirit = ms & invTbl     .ind ri           %~ (i `delete`)
                          & invTbl     .ind ri'          %~ (i :)
                          & msgQueueTbl.at  i            .~ Nothing
                          & mobTbl     .ind i.rmId       .~ ri'
                          & plaTbl     .ind i.logoutRmId ?~ ri
      where
        ri  = getRmId i ms
        ri' = spirit ? iNecropolis :? iLoggedOut
    possessHelper ms = let f = maybe id (\npcId -> npcTbl.ind npcId.npcPossessor .~ Nothing) . getPossessing i $ ms
                       in ms & plaTbl.ind i.possessing .~ Nothing & f


theBeyond :: Id -> MsgQueue -> Sing -> Bool -> MudStack ()
theBeyond i mq s isDropped = modifyStateSeq $ \ms ->
    let cols            = getColumns i ms
        retainedIds     = views (teleLinkMstrTbl.ind i) (map (`getIdForMobSing` ms) . M.keys) ms
        (inIds, outIds) = partition (isLoggedIn . (`getPla` ms)) retainedIds
        f targetId      = pcTbl          .ind targetId.linked %~ (s `delete`)
        g targetId      = teleLinkMstrTbl.ind targetId        %~ M.delete s
        h               = (pcTbl.ind i.linked .~ []) . (teleLinkMstrTbl.ind i .~ M.empty)
        ms'             = h . flip (foldr g) retainedIds . flip (foldr f) retainedIds $ ms
        fs              = [ unless isDropped $ do { wrapSend mq cols . colorWith spiritMsgColor $ theBeyondMsg
                                                  ; farewell i mq cols }
                          , bcast . pure $ (nlnl . linkLostMsg $ s, inIds)
                          , forM_ outIds $ \outId -> retainedMsg outId ms' (linkMissingMsg s)
                          , bcastAdmins $ s <> " passes into the beyond."
                          , logPla "theBeyond" i "passing into the beyond."
                          , logNotice "theBeyond" . T.concat $ [ descSingId i ms', " is passing into the beyond." ] ]
    in (ms', fs)


farewell :: Id -> MsgQueue -> Cols -> MudStack ()
farewell i mq cols = multiWrapSend mq cols . mkFarewellStats i cols =<< getState


-- TODO: Stats regarding time spent playing.
mkFarewellStats :: Id -> Cols -> MudState -> [Text]
mkFarewellStats i cols ms = (header :) . (<> pure footer) . concatMap (wrapIndent 2 cols) $ ts
  where
    ts     = [ T.concat [ s, ", the ", sexy, " ", r ]
             , f "Strength: "   <> str
             , f "Dexterity: "  <> dex
             , f "Health: "     <> hea
             , f "Magic: "      <> mag
             , f "Psionics: "   <> psi
             , f "Points: "     <> xpsHelper
             , f "Handedness: " <> handy
             , f "Languages: "  <> langs
             , f "Level: "      <> showText l
             , f "Experience: " <> commaShow expr ]
    header = wrapUnlines cols . T.concat $ [ "Sadly, "
                                           , s
                                           , " has passed away. Here is a final summary of "
                                           , s
                                           , "'s stats:" ]
    footer = wrapUnlinesInit cols "Thank you for playing CurryMUD! Please reconnect to play again with a new character."
    f                         = pad 12
    s                         = getSing         i ms
    (sexy, r)                 = mkPrettySexRace i ms
    (str, dex, hea, mag, psi) = calcEffAttribs  i ms & each %~ showText
    xpsHelper                 | (hps, mps, pps, fps) <- getPts i ms
                              = commas [ g "H" hps, g "M" mps, g "P" pps, g "F" fps ]
      where
        g a (_, x) = showText x |<>| a <> "P"
    handy     = pp . getHand i $ ms
    langs     = commas [ pp lang | lang <- sort . getKnownLangs i $ ms ]
    (l, expr) = getLvlExp i ms
