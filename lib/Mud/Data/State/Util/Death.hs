{-# LANGUAGE OverloadedStrings #-}

module Mud.Data.State.Util.Death (handleDeath) where

import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.Misc
import Mud.Threads.Act
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.FeelingTimer
import Mud.Threads.Regen
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Arrow ((***), first, second)
import Control.Lens (_2, at)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad (when)
import Data.Bits (setBit, zeroBits)
import Data.List (delete)
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude hiding (pi)
import qualified Data.Map.Lazy as M (elems, empty)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Data.State.Util.Death"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Data.State.Util.Death"


-- ==================================================


{-
When Taro dies:
Taro's corpse is created. Inventory, equipment, and coins are transferred from PC to corpse.
Taro's PC becomes a disembodied spirit (see below).
Those who are linked with Taro are notified of his death (via retained message?).
When the allotted time is up, Taro's spirit passes into the beyond and is sent to the Necropolis.
Taro's player is shown Taro's stats.
Taro's player is returned to the login screen.

About spirits:
A player has a certain amount of time as a spirit, depending on level.
A spirit can move freely about with no FP cost.
A spirit may be granted the ability to give out a certain number of exp bonuses (using the "bonus" command), depending on level.
A spirit retains a certain number of two-way links, depending on PS. A spirit may continue to communicate telepathically over its retained links, with no cost to PP.
Those links with the greatest volume of messages are retained. If the deceased PC's top links are all asleep, the spirit gets to retain a bonus link with a PC who is presently awake.
-}


handleDeath :: Id -> MudStack ()
handleDeath i = do
    getState >>= \ms -> when (isNpc i ms) possessHelper
    tweak . leaveParty $ i
    stopActs          i
    pauseEffects      i
    stopFeelings      i
    stopRegen         i
    throwWaitDigester i
    modifyStateSeq $ \ms -> let (ms',  fs ) = mkCorpse  i ms
                                (ms'', fs') = spiritize i ms'
                            in (ms'', logPlaHelper i ms "handleDeath" "handling death." : fs ++ fs')
  where
    possessHelper = modifyStateSeq $ \ms -> case getPossessor i ms of
      Nothing -> (ms, [])
      Just pi -> ( ms & plaTbl.ind pi.possessing   .~ Nothing
                      & npcTbl.ind i .npcPossessor .~ Nothing
                 , let (mq, cols) = getMsgQueueColumns pi ms
                       t          = aOrAnOnLower (descSingId i ms) <> " " <> parensQuote "NPC has died"
                   in [ wrapSend mq cols . prd $ "You stop possessing " <> aOrAnOnLower (getSing i ms)
                      , sendDfltPrompt mq pi
                      , logPla "handleDeath" pi . prd $ "stopped possessing " <> t ] )


logPlaHelper :: Id -> MudState -> Text -> Text -> MudStack ()
logPlaHelper i ms funName = when (isPC i ms) . logPla funName i


mkCorpse :: Id -> MudState -> (MudState, Funs)
mkCorpse i ms = let et = EntTemplate (Just "corpse")
                                     s p
                                     (getEntDesc i ms)
                                     Nothing -- TODO: Smell.
                                     zeroBits
                    ot = ObjTemplate (getCorpseWeight i ms)
                                     (getCorpseVol    i ms)
                                     Nothing -- TODO: Taste.
                                     zeroBits
                    ct = ConTemplate (getCorpseCapacity i ms `max` calcCarriedVol i ms)
                                     (setBit zeroBits . fromEnum $ IsCorpse)
                    is = M.elems (getEqMap i ms) ++ getInv i ms
                    c  = getCoins i ms
                    (_, ms', fs) = newCon ms et ot ct (is, c) . getRmId i $ ms
                in ( ms' & coinsTbl.ind i .~ mempty
                         & eqTbl   .ind i .~ M.empty
                         & invTbl  .ind i .~ []
                   , logPlaHelper i ms "mkCorpse" "corpse created." : fs )
      where
        (s, p) = ("corpse of " <>) *** ("corpses of " <>) $ if isPC i ms
          then second (<> "s") . dup . mkSerializedNonStdDesig i ms (getSing i ms) A $ Don'tCap
          else first aOrAnOnLower $ let bgns = getBothGramNos i ms in bgns & _2 .~ mkPlurFromBoth bgns


spiritize :: Id -> MudState -> (MudState, Funs)
spiritize i ms = if isPC i ms
  then ( ms & plaTbl.ind i %~ setPlaFlag IsSpirit True
            & mobTbl.ind i %~ setCurXps
       , pure . logPla "spiritize" i $ "spirit created." )
  else deleteNpc
  where
    setCurXps m = m & curHp .~ 1
                    & curMp .~ 1
                    & curPp .~ 1
                    & curFp .~ 1
    deleteNpc = let ri = getRmId i ms
                in ( ms & activeEffectsTbl.at  i  .~ Nothing
                        & coinsTbl        .at  i  .~ Nothing
                        & entTbl          .at  i  .~ Nothing
                        & eqTbl           .at  i  .~ Nothing
                        & invTbl          .at  i  .~ Nothing
                        & invTbl          .ind ri %~ (i `delete`)
                        & mobTbl          .at  i  .~ Nothing
                        & npcTbl          .at  i  .~ Nothing
                        & pausedEffectsTbl.at  i  .~ Nothing
                        & typeTbl         .at  i  .~ Nothing
                   , pure . logNotice "spiritize" $ descSingId i ms <> " has died." )
