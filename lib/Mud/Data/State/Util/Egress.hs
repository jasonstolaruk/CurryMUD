{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Egress ( handleEgress
                                  , mkFarewellStats ) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Misc.ANSI
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Misc.Logging hiding (logNotice, logPla)
import           Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iNecropolis)
import           Mud.Threads.Act
import           Mud.Threads.Digester
import           Mud.Threads.Effect
import           Mud.Threads.FeelingTimer
import           Mud.Threads.LightTimer
import           Mud.Threads.Regen
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Padding
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow ((***))
import           Control.Exception.Lifted (finally)
import           Control.Lens (at, each, set, views)
import           Control.Lens.Operators ((?~), (.~), (&), (%~), (+~))
import           Control.Monad ((>=>), forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (delete, partition, sort)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Tuple (swap)
import           GHC.Stack (HasCallStack)
import qualified Data.Map.Strict as M (delete, empty, foldl, keys, singleton, toList)
import qualified Data.Text as T
import           System.Time.Utils (renderSecs)

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Data.State.Util.Egress"

logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Data.State.Util.Egress"

-- ==================================================

handleEgress :: HasCallStack => Id -> MsgQueue -> Bool -> MudStack ()
handleEgress i mq isDropped = egressHelper `finally` writeMsg mq FinishedEgress
  where
    egressHelper = do logPla "handleEgress egressHelper" i "handling egress."
                      stopActs i
                      (ms, now) <- (,) <$> getState <*> liftIO getCurrentTime
                      let tuple@(s, hoc, spirit) = ((,,) <$> uncurry getSing
                                                         <*> uncurry isAdHoc
                                                         <*> uncurry isSpiritId) (i, ms)
                      unless (hoc || spirit) . bcastOthersInRm i . nlnl . egressMsg . serialize . mkStdDesig i ms $ DoCap
                      helper now tuple |&| modifyState >=> \(bs, logMsgs) -> do
                          forM_ logMsgs . uncurry . logPla $ "handleEgress egressHelper helper"
                          if spirit
                            then theBeyond i mq s isDropped
                            else mapM_ (i |&|) [ stopLightTimers
                                               , pauseEffects -- Already done for spirits in "handleDeath".
                                               , stopFeelings
                                               , stopRegen
                                               , throwWaitDigester ]
                          closePlaLog i
                          bcast bs
                          bcastAdmins $ s <> " has left CurryMUD."
                          logNotice "handleEgress egressHelper helper" . T.concat $ [ descSingId i ms, " has left CurryMUD." ]
                          when hoc . tweak . removeAdHoc $ i
    helper now (s, hoc, spirit) ms =
        let (ms', bs, logMsgs) = peepHelper i ms s spirit
            ms'' | hoc         = ms'
                 | otherwise   = compose ms' [ plaTbl.ind i.disconnectTime ?~ now
                                             , updateHostMap i s now
                                             , possessHelper i
                                             , leaveParty    i
                                             , movePC        i spirit ]
         in (ms'', (bs, logMsgs))

peepHelper :: HasCallStack => Id -> MudState -> Sing -> Bool -> (MudState, [Broadcast], [(Id, Text)])
peepHelper i ms s spirit =
    let (peeperIds, peepingIds) = getPeepersPeeping i ms
        bs      | a <- "You are no longer peeping ", b <- parensQuote $ s <> " has " <> txt
                = [ (nlnl . T.concat $ [ a, s, " ", b, "." ], pure peeperId) | peeperId <- peeperIds ]
        logMsgs | a <- "no longer peeping ", b <- parensQuote $ s <> " has " <> txt
                = [ (peeperId, T.concat [ a, s, " ", b, "." ]) | peeperId <- peeperIds ]
        txt     = spirit ? "passed into the beyond" :? "disconnected"
    in (upd ms [ plaTbl %~ stopPeeping     peepingIds
               , plaTbl %~ stopBeingPeeped peeperIds
               , plaTbl.ind i.peeping .~ []
               , plaTbl.ind i.peepers .~ [] ], bs, logMsgs)
  where
    stopPeeping     peepingIds pt = let f peepedId ptAcc = ptAcc & ind peepedId.peepers %~ (i `delete`)
                                    in foldr f pt peepingIds
    stopBeingPeeped peeperIds  pt = let f peeperId ptAcc = ptAcc & ind peeperId.peeping %~ (i `delete`)
                                    in foldr f pt peeperIds

movePC :: HasCallStack => Id -> Bool -> MudState -> MudState
movePC i spirit ms = upd ms [ invTbl     .ind ri           %~ (i `delete`)
                            , invTbl     .ind ri'          %~ (i :)
                            , msgQueueTbl.at  i            .~ Nothing
                            , mobTbl     .ind i.rmId       .~ ri'
                            , plaTbl     .ind i.logoutRmId ?~ ri ]
  where
    (ri, ri') = (getRmId i ms, spirit ? iNecropolis :? iLoggedOut)

possessHelper :: HasCallStack => Int -> MudState -> MudState
possessHelper i ms = let f = maybe id (\npcId -> npcTbl.ind npcId.npcPossessor .~ Nothing) . getPossessing i $ ms
                     in upd ms [ plaTbl.ind i.possessing .~ Nothing, f ]

updateHostMap :: HasCallStack => Id -> Sing -> UTCTime -> MudState -> MudState
updateHostMap i s now ms = maybe ms helper . getConnectTime i $ ms
  where
    helper conTime    = let dur = round $ now `diffUTCTime` conTime
                        in flip (set $ hostTbl.at s) ms . Just $ case getHostMap s ms of
                          Nothing      -> M.singleton host (mkNewRecord dur)
                          Just hostMap -> let f rec = hostMap & at host ?~ rec
                                          in views (at host) (f . maybe (mkNewRecord dur) (reviseRecord dur)) hostMap
    mkNewRecord  dur  = HostRecord { _noOfLogouts = 1, _secsConnected = dur, _lastLogout = now }
    reviseRecord dur  = (noOfLogouts +~ 1) . (secsConnected +~ dur) . (lastLogout .~ now)
    host              = getCurrHostName i ms

-----

theBeyond :: HasCallStack => Id -> MsgQueue -> Sing -> Bool -> MudStack ()
theBeyond i mq s isDropped = modifyStateSeq $ \ms ->
    let cols            = getColumns i ms
        retainedIds     = views (teleLinkMstrTbl.ind i) (map (`getIdForPCSing` ms) . M.keys) ms
        (inIds, outIds) = partition (isLoggedIn . (`getPla` ms)) retainedIds
        f targetId      = pcTbl          .ind targetId.linked %~ (s `delete`)
        g targetId      = teleLinkMstrTbl.ind targetId        %~ M.delete s
        h               = (pcTbl.ind i.linked .~ []) . (teleLinkMstrTbl.ind i .~ M.empty)
        ms'             = h . flip (foldr g) retainedIds . flip (foldr f) retainedIds $ ms
        fs              = [ logPla "theBeyond" i "passing into the beyond."
                          , logNotice "theBeyond" $ descSingId i ms' <> " is passing into the beyond."
                          , unless isDropped $ do wrapSend   mq cols . colorWith spiritMsgColor $ theBeyondMsg
                                                  farewell i mq cols
                          , bcast . pure $ (nlnl . linkLostMsg $ s, inIds)
                          , forM_ outIds $ \outId -> retainedMsg outId ms' (linkMissingMsg s)
                          , bcastAdmins $ s <> " passes into the beyond." ]
    in (ms', fs)

farewell :: HasCallStack => Id -> MsgQueue -> Cols -> MudStack ()
farewell i mq cols = multiWrapSend mq cols . mkFarewellStats i =<< getState

mkFarewellStats :: HasCallStack => Id -> MudState -> [Text]
mkFarewellStats i ms = concat [ header, ts, footer ]
  where
    header = [ T.concat [ "Sadly, ", s, " has passed away. Here is a final summary of ", s, "'s stats:" ], "" ]
    ts     = [ T.concat [ s, ", the ", sexy, " ", r ]
             , f "Strength: "   <> str
             , f "Dexterity: "  <> dex
             , f "Health: "     <> hea
             , f "Magic: "      <> mag
             , f "Psionics: "   <> psi
             , f "Points: "     <> xpsHelper
             , f "Handedness: " <> handy
             , f "Languages: "  <> langs
             , f "Level: "      <> showTxt l
             , f "Experience: " <> commaShow expr
             , f "Sacrifices: " <> sacrificesHelper (getSacrificesTbl i ms)
             , "" ]
    footer = [ T.concat [ "You played ", s, " for a total of ", totalTime, " in real-world time." ]
             , ""
             , "Thank you for playing CurryMUD! Please reconnect to play again with a new character." ]
    f                         = pad 12
    s                         = getSing         i ms
    (sexy, r)                 = mkPrettySexRace i ms
    (str, dex, hea, mag, psi) = calcEffAttribs  i ms & each %~ showTxt
    xpsHelper                 | (hps, mps, pps, fps) <- getPts i ms
                              = commas [ g "H" hps, g "M" mps, g "P" pps, g "F" fps ]
      where
        g a (_, x) = showTxt x |<>| a <> "P"
    handy            = pp . getHand i $ ms
    langs            = commas [ pp lang | lang <- sort . getKnownLangs i $ ms ]
    (l, expr)        = getLvlExp i ms
    sacrificesHelper = noneOnNull . commas . map (flip quoteWith' " to " . swap . (pp *** commaShow)) . sort . M.toList
    totalTime        = maybe "(no host records)" g . getHostMap s $ ms
      where
        g = T.pack . renderSecs . M.foldl (\acc -> views secsConnected (+ acc)) 0
