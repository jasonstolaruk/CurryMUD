{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

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
import Mud.Threads.NpcServer
import Mud.Threads.Regen
import Mud.Threads.SpiritTimer
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Arrow ((***), first)
import Control.Lens (_1, _2, _3, at, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (zeroBits)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (delete, sortBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.SQLite.Simple (fromOnly)
import Prelude hiding (pi)
import qualified Data.IntMap.Lazy as IM (delete, filterWithKey, keys, mapWithKey)
import qualified Data.Map.Lazy as M (delete, elems, empty, filterWithKey)


default (Int)


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
A spirit retains a certain number of two-way links, depending on PS. A spirit may continue to communicate telepathically over its retained links, with no cost to PP.
Those links with the greatest volume of messages are retained. If the deceased PC's top links are all asleep, the spirit gets to retain a bonus link with a PC who is presently awake.
-}


handleDeath :: Id -> MudStack ()
handleDeath i = isNpc i <$> getState >>= \npc -> do
    logPla "handleDeath" i "handling death."
    when   npc possessHelper
    unless npc leaveChans
    tweaks [ leaveParty i
           , mobTbl.ind i.mobRmDesc .~ Nothing
           , mobTbl.ind i.tempDesc  .~ Nothing
           , mobTbl.ind i.stomach   .~ [] ]
    stopActs                    i
    pauseEffects                i
    stopFeelings                i
    stopRegen                   i
    throwWaitDigester           i
    modifyStateSeq . mkCorpse $ i
    npc ? deleteNpc :? spiritize i
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
    leaveChans = unit -- TODO
    deleteNpc  = getState >>= \ms -> let ri = getRmId i ms
                                     in do { tweaks [ activeEffectsTbl.at  i  .~ Nothing
                                                    , coinsTbl        .at  i  .~ Nothing
                                                    , entTbl          .at  i  .~ Nothing
                                                    , eqTbl           .at  i  .~ Nothing
                                                    , invTbl          .at  i  .~ Nothing
                                                    , invTbl          .ind ri %~ (i `delete`)
                                                    , mobTbl          .at  i  .~ Nothing
                                                    , pausedEffectsTbl.at  i  .~ Nothing
                                                    , typeTbl         .at  i  .~ Nothing ]
                                           ; stopWaitNpcServer i -- This removes the NPC from the "NpcTbl".
                                           ; logNotice "spiritize" $ "NPC " <> descSingId i ms <> " has died." }


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
                    corpse = bool NpcCorpse (PCCorpse (getSing i ms) (getSex i ms) . getRace i $ ms) . isPC i $ ms
                    (_, ms', fs) = newCorpse ms et ot ct ic corpse . getRmId i $ ms
                in ( ms' & coinsTbl.ind i .~ mempty
                         & eqTbl   .ind i .~ M.empty
                         & invTbl  .ind i .~ []
                   , logPla "mkCorpse" i "corpse created." : fs )
      where
        (s, p) = if isPC i ms
          then let pair @(_,    r) = getSexRace i ms
                   pair'@(sexy, _) = (pp *** pp) pair
               in ( "corpse of a " <> uncurry (|<>|) pair'
                  , "corpses of "  <> sexy |<>| plurRace r )
          else (("corpse of " <>) *** ("corpses of " <>)) . first aOrAnOnLower $ let bgns = getBothGramNos i ms
                                                                                 in bgns & _2 .~ mkPlurFromBoth bgns


spiritize :: Id -> MudStack ()
spiritize i = do
    ((mq, cols), s, secs) <- ((,,) <$> uncurry getMsgQueueColumns
                                   <*> uncurry getSing
                                   <*> uncurry calcSpiritTime) . (i, ) <$> getState
    (withDbExHandler "spiritize" . liftIO . lookupTeleNames $ s) >>= \case
      Nothing                    -> dbError mq cols
      Just (procOnlySings -> ss) -> modifyStateSeq $ \ms ->
          let triples    = [ (targetId, targetSing, isLoggedIn targetPla) | targetSing <- ss
                           , let targetId  = getIdForMobSing s        ms
                           , let targetPla = getPla          targetId ms ]
              n          = calcRetainedLinks i ms
              retaineds  | isZero secs = []
                         | otherwise   = let xs = take n triples
                                         in (xs |&|) $ case filter (view _3) xs of
                                           [] -> let bonus = take 1 . filter (view _3) . drop n $ triples in (++ bonus)
                                           _  -> id
              retainedIds   = select _1 retaineds
              retainedSings = select _2 retaineds
              asleepIds     = let f targetId targetPC = and [ views linked (s `elem`) targetPC
                                                            , targetId `notElem` retainedIds
                                                            , not . isLoggedIn . getPla targetId $ ms ]
                              in views pcTbl (IM.keys . IM.filterWithKey f . IM.delete i) ms
              (bs, fs)      = mkBcasts ms s . map dropSnd $ retaineds
          in ( ms & plaTbl.ind i    %~ setPlaFlag IsSpirit True
                  & pcTbl           %~ pcTblHelper           s retainedIds retainedSings
                  & teleLinkMstrTbl %~ teleLinkMstrTblHelper s retainedIds retainedSings
                  & mobTbl.ind i    %~ setCurrXps
             , [ logPla "spiritize" i "spirit created."
               , forM_ asleepIds $ \targetId -> retainedMsg targetId ms . linkMissingMsg $ s
               , bcast bs
               , sequence_ (fs :: Funs)
               , runSpiritTimerAsync i secs ] )
  where
    procOnlySings xs = map snd . sortBy (flip compare `on` fst) $ [ (length g, s)
                                                                  | g@(s:_) <- sortGroup . map fromOnly $ xs ]
    pcTblHelper s retainedIds retainedSings = IM.mapWithKey helper
      where
        helper pcId | pcId == i               = linked .~ retainedSings
                    | pcId `elem` retainedIds = id
                    | otherwise               = linked %~ (s `delete`)
    teleLinkMstrTblHelper s retainedIds retainedSings = IM.mapWithKey helper
      where
        helper targetId | targetId == i               = M.filterWithKey (const . (`elem` retainedSings))
                        | targetId `elem` retainedIds = id
                        | otherwise                   = M.delete s
    setCurrXps ms = ms & curHp .~ (ms^.maxHp)
                       & curMp .~ (ms^.maxMp)
                       & curPp .~ (ms^.maxPp)
                       & curFp .~ (ms^.maxFp)
    mkBcasts ms s retainedPairs = let (toLinkRetainers, fs) = toLinkRetainersHelper
                                  in ([ toLinkLosers, toLinkRetainers ], fs)
      where
        toLinkRetainersHelper =
            let targetIds = [ targetId | (targetId, targetIsLoggedIn) <- retainedPairs, targetIsLoggedIn ]
                f i'      = rndmDo (calcProbSpiritizeShiver i' ms) . mkExpAction "shiver" . mkActionParams i' ms $ []
            in ((nlnl . linkRetainedMsg $ s, targetIds), pure . mapM_ f $ targetIds)
        toLinkLosers = let targetIds           = views pcTbl (IM.keys . IM.filterWithKey f . IM.delete i) ms
                           f targetId targetPC = and [ views linked (s `elem`) targetPC
                                                     , targetId `notElem` map fst retainedPairs
                                                     , isLoggedIn . getPla targetId $ ms ]
                       in (nlnl . linkLostMsg $ s, targetIds)
