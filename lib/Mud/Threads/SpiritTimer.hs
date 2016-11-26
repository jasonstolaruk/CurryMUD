{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.SpiritTimer ( mkFarewellStats
                               , runSpiritTimerAsync
                               , theBeyond
                               , throwWaitSpiritTimer ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla, logPlaOut)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (handle)
import Control.Lens (each)
import Control.Lens.Operators ((%~), (&), (.~), (?~), (^.))
import Control.Monad ((>=>), forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (delete, partition, sort)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Lazy as M (delete)
import qualified Data.Text as T


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.SpiritTimer"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.SpiritTimer"


logPlaOut :: Text -> Id -> [Text] -> MudStack ()
logPlaOut = L.logPlaOut "Mud.Threads.SpiritTimer"


-- ==================================================


runSpiritTimerAsync :: Id -> Seconds -> Inv -> MudStack ()
runSpiritTimerAsync i secs retainedIds =
    runAsync (threadSpiritTimer i secs retainedIds) >>= \a -> tweak $ plaTbl.ind i.spiritAsync ?~ a


throwWaitSpiritTimer :: Id -> MudStack ()
throwWaitSpiritTimer i = helper |&| modifyState >=> maybeVoid throwWait
  where
    helper ms = let a = ms^.plaTbl.ind i.spiritAsync
                in (ms & plaTbl.ind i.spiritAsync .~ Nothing, a)


-----


threadSpiritTimer :: Id -> Seconds -> Inv -> MudStack ()
threadSpiritTimer i secs retainedIds = handle (threadExHandler (Just i) "spirit timer") $ do
    (mq, cols) <- getMsgQueueColumns i <$> getState
    setThreadType . SpiritTimer $ i
    logPla "threadSpiritTimer" i . prd $ "spirit timer started " <> parensQuote (showText secs <> " seconds")
    handle (die (Just i) "spirit timer") . spiritTimer i mq cols retainedIds $ secs


spiritTimer :: Id -> MsgQueue -> Cols -> Inv -> Seconds -> MudStack ()
spiritTimer i mq cols retainedIds 0 = do
    logPla "spiritTimer" i "spirit timer expired."
    theBeyond i mq cols retainedIds
spiritTimer i mq cols retainedIds secs
  | secs == 75 = helper "You feel the uncanny pull of the beyond. Your time in this dimension is coming to an end."
  | secs == 45 = helper . thrice prd $ "You feel the uncanny pull of the beyond. You have VERY little time left in \
                                       \this dimension"
  | secs == 15 = helper . thrice prd $ "You are fading away"
  | otherwise  = next
  where
    helper msg = do
        wrapSend mq cols . colorWith spiritMsgColor $ msg
        logPlaOut "spiritTimer" i . pure $ msg
        next
    next = (liftIO . threadDelay $ 1 * 10 ^ 6) >> spiritTimer i mq cols retainedIds (pred secs)


theBeyond :: Id -> MsgQueue -> Cols -> Inv -> MudStack ()
theBeyond i mq cols retainedIds = modifyStateSeq $ \ms ->
    let s               = getSing i ms
        (inIds, outIds) = partition (isLoggedIn . (`getPla` ms)) retainedIds
        ms'             = flip (foldr g) retainedIds . flip (foldr f) retainedIds $ ms
        f targetId      = pcTbl          .ind targetId.linked %~ (s `delete`)
        g targetId      = teleLinkMstrTbl.ind targetId        %~ M.delete s
    in (ms', [ wrapSend mq cols . colorWith spiritMsgColor $ theBeyondMsg
             , farewell i mq cols
             , bcast . pure $ (linkLostMsg s, inIds)
             , forM_ outIds $ \i' ->　retainedMsg i' ms (linkMissingMsg s)
             , bcastAdmins $ s <> " passes into the beyond."
             , logPla "theBeyond" i "passing into the beyond."
             , logNotice "theBeyond" . T.concat $ [ descSingId i ms, " is passing into the beyond." ]
             , writeMsg mq TheBeyond ])


farewell :: Id -> MsgQueue -> Cols -> MudStack ()
farewell i mq cols = multiWrapSend mq cols . mkFarewellStats i =<< getState


mkFarewellStats :: Id -> MudState -> [Text]
mkFarewellStats i ms = [ T.concat [ s, ", the ", sexy, " ", r ]
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
  where
    f                         = pad 12
    s                         = getSing         i ms
    (sexy, r)                 = mkPrettySexRace i ms
    (str, dex, hea, mag, psi) = calcEffAttribs  i ms & each %~ showText
    xpsHelper                 | (hps, mps, pps, fps) <- getPts i ms
                              = commas [ g "h" hps, g "m" mps, g "p" pps, g "f" fps ]
      where
        g a (_, x) = showText x |<>| a <> "p"
    handy     = capitalize . pp . getHand i $ ms
    langs     = commas [ pp lang | lang <- sort . getKnownLangs i $ ms ]
    (l, expr) = getLvlExp i ms
