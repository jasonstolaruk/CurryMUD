{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.Death (handleDeath) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Util.Misc
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Random
import Mud.Misc.Database
import Mud.Misc.Misc
import Mud.Threads.Act
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.FeelingTimer
import Mud.Threads.Regen
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Arrow ((***), first, second)
import Control.Lens (_1, _2, _3, at, view, views)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (zeroBits)
import Data.Function (on)
import Data.List (delete, sortBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.SQLite.Simple (fromOnly)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (delete, filterWithKey, keys, mapWithKey)
import qualified Data.Map.Lazy as M (elems, empty)


{-# ANN module ("HLint: ignore Use &&" :: String) #-}


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Data.State.Util.Death"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Data.State.Util.Death"


-- ==================================================


{-
When Taro dies:
Taro's corpse is created. Inventory, equipment, and coins are transferred from PC to corpse.
Taro's PC becomes a disembodied spirit (see below).
When the allotted time is up, Taro's spirit passes into the beyond and is sent to the Necropolis.
Taro's player is shown Taro's stats.
Taro's player is returned to the login screen.

About spirits:
A player has a certain amount of time as a spirit, depending on level.
A spirit can move freely about with no FP cost.
A spirit may be granted the ability to give out a certain number of extra exp bonuses (using the "bonus" command), depending on level.
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
    modifyStateSeq $ \ms -> second (logPlaHelper i ms "handleDeath" "handling death." :) . mkCorpse i $ ms
    spiritize         i
  where
    possessHelper = modifyStateSeq $ \ms -> case getPossessor i ms of
      Nothing -> (ms, [])
      Just pi -> ( ms & plaTbl.ind pi.possessing   .~ Nothing
                      & npcTbl.ind i .npcPossessor .~ Nothing
                 , let (mq, cols) = getMsgQueueColumns pi ms
                       t          = aOrAnOnLower (descSingId i ms) <> "; NPC has died"
                   in [ wrapSend mq cols . prd $ "You stop possessing " <> aOrAnOnLower (getSing i ms)
                      , sendDfltPrompt mq pi
                      , logPla "handleDeath" pi . prd $ "stopped possessing " <> t ] )


logPlaHelper :: Id -> MudState -> Text -> Text -> MudStack ()
logPlaHelper i ms funName = when (isPC i ms) . logPla funName i


mkCorpse :: Id -> MudState -> (MudState, Funs)
mkCorpse i ms = let et     = EntTemplate (Just "corpse")
                                         s p
                                         (getEntDesc i ms)
                                         Nothing -- TODO: Smell.
                                         zeroBits
                    ot     = ObjTemplate (getCorpseWeight i ms)
                                         (getCorpseVol    i ms)
                                         Nothing -- TODO: Taste.
                                         zeroBits
                    ct     = ConTemplate (getCorpseCapacity i ms `max` calcCarriedVol i ms)
                                         zeroBits
                    ic     = (M.elems (getEqMap i ms) ++ getInv i ms, getCoins i ms)
                    corpse | isPC i ms = PCCorpse (getSing i ms) (getSex i ms) (getRace i ms)
                           | otherwise = NpcCorpse
                    (_, ms', fs) = newCorpse ms et ot ct ic corpse . getRmId i $ ms
                in ( ms' & coinsTbl.ind i .~ mempty
                         & eqTbl   .ind i .~ M.empty
                         & invTbl  .ind i .~ []
                   , logPlaHelper i ms "mkCorpse" "corpse created." : fs )
      where
        (s, p) = if isPC i ms
          then ("corpse of a ", "") -- TODO
          else (("corpse of " <>) *** ("corpses of " <>)) . first aOrAnOnLower $ let bgns = getBothGramNos i ms
                                                                                 in bgns & _2 .~ mkPlurFromBoth bgns


spiritize :: Id -> MudStack ()
spiritize i = getState >>= \ms -> let mySing = getSing i ms in if isPC i ms
  then (withDbExHandler "spiritize" . liftIO . lookupTeleNames $ mySing) >>= \case
    Nothing                    -> uncurry dbError . getMsgQueueColumns i $ ms
    Just (procOnlySings -> ss) ->
        let triples    = [ (i', s, ia) | s <- ss, let i' = getIdForMobSing s ms, let ia = isAwake i' ms ]
            n          = calcRetainedLinks i ms
            retaineds  = take n triples
            retaineds' = (retaineds |&|) $ case filter (view _3) retaineds of
              [] -> let bonus = take 1 . filter (view _3) . drop n $ triples in (++ bonus)
              _  -> id
            asleepIds = let f i' p = and [ views linked (mySing `elem`) p
                                         , i' `notElem` map (view _1) retaineds'
                                         , not (isAwake i' ms) ]
                        in views pcTbl (IM.keys . IM.filterWithKey f . IM.delete i) ms
            (bs, fs)  = mkBcasts ms mySing retaineds'
        in do { tweaks [ plaTbl.ind i %~ setPlaFlag IsSpirit True
                       , pcTbl        %~ pcTblHelper mySing retaineds'
                       , mobTbl.ind i %~ setCurXps ]
              ; forM_ asleepIds $ \i' ->　retainedMsg i' ms . linkMissingMsg $ mySing
              ; bcast bs
              ; sequence_ (fs :: Funs)
              ; logPla "spiritize" i "spirit created." }
  else deleteNpc ms
  where
    procOnlySings xs = map snd . sortBy (flip compare `on` fst) $ [ (length g, s)
                                                                  | g@(s:_) <- sortGroup . map fromOnly $ xs ]
    pcTblHelper mySing retaineds@(map (view _1) -> retainerIds) = IM.mapWithKey helper
      where
        helper pcId | pcId == i               = linked .~ map (view _2) retaineds
                    | pcId `elem` retainerIds = id
                    | otherwise               = linked %~ (mySing `delete`)
    setCurXps m = m & curHp .~ 1
                    & curMp .~ 1
                    & curPp .~ 1
                    & curFp .~ 1
    mkBcasts ms mySing retaineds = let (toLinkRetainers, fs) = toLinkRetainersHelper
                                   in ([ toLinkLosers, toLinkRetainers ], fs)
      where
        toLinkLosers =
            let targetIds = views pcTbl (IM.keys . IM.filterWithKey f . IM.delete i) ms
                f i' p    = and [ views linked (mySing `elem`) p
                                , i' `notElem` map (view _1) retaineds
                                , isAwake i' ms ]
            in (linkLostMsg mySing, targetIds)
        -- TODO: When a spirit passes into the beyond, a retained msg should be sent to those link retainers who are asleep.
        toLinkRetainersHelper
          | targetIds <- [ i' | (i', _, ia) <- retaineds, ia ]
          , f         <- \i' -> rndmDo (calcProbSpiritizeShiver i' ms) . mkExpAction "shiver" . mkActionParams i' ms $ []
          , fs        <- pure . mapM_ f $ targetIds
          = ((linkRetainedMsg mySing, targetIds), fs)
    deleteNpc ms = let ri = getRmId i ms in do { tweaks [ activeEffectsTbl.at  i  .~ Nothing
                                                        , coinsTbl        .at  i  .~ Nothing
                                                        , entTbl          .at  i  .~ Nothing
                                                        , eqTbl           .at  i  .~ Nothing
                                                        , invTbl          .at  i  .~ Nothing
                                                        , invTbl          .ind ri %~ (i `delete`)
                                                        , mobTbl          .at  i  .~ Nothing
                                                        , npcTbl          .at  i  .~ Nothing
                                                        , pausedEffectsTbl.at  i  .~ Nothing
                                                        , typeTbl         .at  i  .~ Nothing ]
                                               ; logNotice "spiritize" $ descSingId i ms <> " has died." }
