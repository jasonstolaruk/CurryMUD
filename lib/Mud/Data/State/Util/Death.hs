{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MonadComprehensions, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Data.State.Util.Death (handleDeath) where

import Mud.Cmds.ExpCmds
import Mud.Cmds.Msgs.Misc
import Mud.Cmds.Util.Misc
import Mud.Cmds.Util.Pla
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
import Mud.Threads.CorpseDecomposer
import Mud.Threads.Digester
import Mud.Threads.Effect
import Mud.Threads.FeelingTimer
import Mud.Threads.NpcServer
import Mud.Threads.Regen
import Mud.Threads.SpiritTimer
import Mud.TopLvlDefs.Misc
import Mud.Util.List
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

import Control.Arrow ((***), first)
import Control.Lens (_1, _2, _3, at, view, views)
import Control.Lens.Operators ((.~), (&), (%~), (^.), (<>~))
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (setBit, zeroBits)
import Data.Function (on)
import Data.List (delete, sortBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Prelude hiding (pi)
import qualified Data.IntMap.Strict as IM (delete, filterWithKey, keys, mapWithKey)
import qualified Data.Map.Strict as M (delete, elems, empty, filter, filterWithKey, keys, size)
import qualified Data.Text as T


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


handleDeath :: HasCallStack => Id -> MudStack ()
handleDeath i = isNpc i <$> getState >>= \npc -> do
    logPla "handleDeath" i "handling death."
    stopActs i
    tweaks [ leaveParty i
           , mobTbl.ind i.mobRmDesc .~ Nothing
           , mobTbl.ind i.tempDesc  .~ Nothing
           , mobTbl.ind i.stomach   .~ [] ]
    when   npc . possessHelper $ i
    unless npc . leaveChans    $ i
    pauseEffects                 i
    stopFeelings                 i
    stopRegen                    i
    throwWaitDigester            i
    modifyStateSeq . mkCorpse  $ i
    npc ? deleteNpc i :? spiritize i


possessHelper :: HasCallStack => Id -> MudStack ()
possessHelper i = modifyStateSeq $ \ms -> case getPossessor i ms of
  Nothing -> (ms, [])
  Just pi -> ( upd ms [ plaTbl.ind pi.possessing   .~ Nothing
                      , npcTbl.ind i .npcPossessor .~ Nothing ]
             , let (mq, cols) = getMsgQueueColumns pi ms
                   t          = aOrAnOnLower (descSingId i ms) <> "; NPC has died"
               in [ logPla "possessHelper" pi . prd $ "stopped possessing " <> t
                  , wrapSend mq cols . prd $ "You stop possessing " <> aOrAnOnLower (getSing i ms)
                  , sendDfltPrompt mq pi ] )


leaveChans :: HasCallStack => Id -> MudStack ()
leaveChans i = liftIO mkTimestamp >>= \ts -> do logPla "leaveChans" i "leaving channels."
                                                modifyStateSeq $ \ms -> foldr (helper ts) (ms, []) . getPCChans i $ ms
  where
    helper ts (Chan ci name connTbl _) pair@(ms, _) = if M.size connTbl == 1
      then pair & _1.chanTbl.at ci .~ Nothing
                & _2 <>~ let msg = asteriskQuote . prd $ "Channel deleted " <> parensQuote (s <> " has died")
                         in pure . withDbExHandler_ "leaveChans" . insertDbTblChan . ChanRec ts ci name s $ msg
      else let f    = filter (`isAwake` ms) . map (`getIdForPCSing` ms) . M.keys . M.filter id . M.delete s
               g i' = [ (leftChanMsg n name, pure i') | n <- getRelativePCName ms (i', i) ]
           in pair & _1.chanTbl.ind ci.chanConnTbl.at s .~ Nothing
                   & _2 <>~ pure (bcastNl =<< mapM g (f connTbl))
      where
        s = getSing i ms


deleteNpc :: HasCallStack => Id -> MudStack ()
deleteNpc i = getState >>= \ms -> let ri = getRmId i ms
                                  in do logNotice "deleteNpc" $ "NPC " <> descSingId i ms <> " has died."
                                        tweaks [ activeEffectTbl.at  i  .~ Nothing
                                               , coinsTbl       .at  i  .~ Nothing
                                               , entTbl         .at  i  .~ Nothing
                                               , eqTbl          .at  i  .~ Nothing
                                               , invTbl         .at  i  .~ Nothing
                                               , invTbl         .ind ri %~ (i `delete`)
                                               , mobTbl         .at  i  .~ Nothing
                                               , pausedEffectTbl.at  i  .~ Nothing
                                               , typeTbl        .at  i  .~ Nothing ]
                                        stopWaitNpcServer i {- This removes the NPC from the "NpcTbl". -}


mkCorpse :: HasCallStack => Id -> MudState -> (MudState, Funs)
mkCorpse i ms =
    let et                  = EntTemplate (Just "corpse")
                                          s p
                                          corpsePlaceholder
                                          Nothing
                                          zeroBits
        ot                  = ObjTemplate (getCorpseWeight i ms)
                                          (getCorpseVol    i ms)
                                          Nothing
                                          (onTrue (ip && r == Nymph) (`setBit` fromEnum IsHumming) zeroBits)
        ct                  = ConTemplate (getCorpseCapacity i ms `max` calcCarriedVol i ms)
                                          zeroBits
        ic                  = (M.elems (getEqMap i ms) ++ getInv i ms, getCoins i ms)
        corpse              = ip ? pcCorpse :? npcCorpse
        npcCorpse           = NpcCorpse corpsePlaceholder
        pcCorpse            = PCCorpse (getSing i ms) corpsePlaceholder (getSex i ms) r
        (corpseId, ms', fs) = newCorpse ms et ot ct ic corpse . getRmId i $ ms
        logMsg              = T.concat [ "corpse with ID ", showText corpseId, " created for ", descSingId i ms, "." ]
    in ( upd ms' [ coinsTbl.ind i .~ mempty
                 , eqTbl   .ind i .~ M.empty
                 , invTbl  .ind i .~ [] ]
       , fs ++ [ logPla "mkCorpse" i "corpse created."
               , logNotice "mkCorpse" logMsg
               , startCorpseDecomp corpseId . dup . getCorpseDecompSecs i $ ms ] )
      where
        ip        = isPC i ms
        (s, p)    | ip = ( "corpse of a " <> sexy |<>| pp r
                         , "corpses of "  <> sexy |<>| plurRace r )
                  | bgns <- getBothGramNos i ms
                  = (("corpse of " <>) *** ("corpses of " <>)) . first aOrAnOnLower $ bgns & _2 .~ mkPlurFromBoth bgns
        (sexy, r) = first pp . getSexRace i $ ms


-----


spiritize :: HasCallStack => Id -> MudStack ()
spiritize i = do
    ((mq, cols), s, secs) <- ((,,) <$> uncurry getMsgQueueColumns
                                   <*> uncurry getSing
                                   <*> uncurry calcSpiritTime) . (i, ) <$> getState
    withDbExHandler "spiritize" (lookupTeleNames s) >>= \case
      Nothing                    -> dbError mq cols
      Just (procOnlySings -> ss) -> modifyStateSeq $ \ms ->
          let triples    = [ (targetId, targetSing, isLoggedIn targetPla) | targetSing <- ss
                           , let targetId  = getIdForPCSing targetSing ms
                           , let targetPla = getPla         targetId   ms ]
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
              (bs, fs)      = mkLinkBcasts i ms s . map dropSnd $ retaineds
          in ( upd ms [ plaTbl.ind i    %~ setPlaFlag IsSpirit True
                      , pcTbl           %~ pcTblHelper           i s retainedIds retainedSings
                      , teleLinkMstrTbl %~ teleLinkMstrTblHelper i s retainedIds retainedSings
                      , mobTbl.ind i    %~ setCurrXps ]
             , [ logPla "spiritize" i . prd $ "spirit created " <> parensQuote (commaShow secs <> " seconds")
               , forM_ asleepIds $ \targetId -> retainedMsg targetId ms . linkMissingMsg $ s
               , bcast bs
               , sequence_ (fs :: Funs)
               , runSpiritTimerAsync i secs ] )
  where
    procOnlySings xs = map snd . sortBy (flip compare `on` fst) $ [ (length g, s) | g@(s:_) <- sortGroup xs ]


pcTblHelper :: HasCallStack => Id -> Sing -> Inv -> [Sing] -> PCTbl -> PCTbl
pcTblHelper i s retainedIds retainedSings = IM.mapWithKey helper
  where
    helper pcId | pcId == i               = linked .~ retainedSings
                | pcId `elem` retainedIds = id
                | otherwise               = linked %~ (s `delete`)


teleLinkMstrTblHelper :: HasCallStack => Id -> Sing -> Inv -> [Sing] -> TeleLinkMstrTbl -> TeleLinkMstrTbl
teleLinkMstrTblHelper i s retainedIds retainedSings = IM.mapWithKey helper
  where
    helper targetId | targetId == i               = M.filterWithKey (const . (`elem` retainedSings))
                    | targetId `elem` retainedIds = id
                    | otherwise                   = M.delete s


setCurrXps :: HasCallStack => Mob -> Mob
setCurrXps m = (curHp .~ (m^.maxHp)) . (curMp .~ (m^.maxMp)) . (curPp .~ (m^.maxPp)) . (curFp .~ (m^.maxFp)) $ m


mkLinkBcasts :: HasCallStack => Id -> MudState -> Sing -> [(Id, Bool)] -> ([Broadcast], Funs)
mkLinkBcasts i ms s retainedPairs = let (toLinkRetainers, fs) = toLinkRetainersHelper
                                    in ([ toLinkLosers, toLinkRetainers ], fs)
  where
    toLinkRetainersHelper =
        let targetIds = [ targetId | (targetId, targetIsLoggedIn) <- retainedPairs, targetIsLoggedIn ]
            f i'      = rndmDo_ (calcProbSpiritizeShiver i' ms) . mkExpAction "shiver" . mkActionParams i' ms $ []
        in ((nlnl . linkRetainedMsg $ s, targetIds), pure . mapM_ f $ targetIds)
    toLinkLosers = let targetIds           = views pcTbl (IM.keys . IM.filterWithKey f . IM.delete i) ms
                       f targetId targetPC = and [ views linked (s `elem`) targetPC
                                                 , targetId `notElem` map fst retainedPairs
                                                 , isLoggedIn . getPla targetId $ ms ]
                   in (nlnl . linkLostMsg $ s, targetIds)
