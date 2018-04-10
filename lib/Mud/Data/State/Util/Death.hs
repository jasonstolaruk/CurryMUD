{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MonadComprehensions, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Data.State.Util.Death (handleDeath) where

import           Mud.Cmds.ExpCmds
import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Util.Misc
import           Mud.Cmds.Util.Pla
import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Clone
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Output
import           Mud.Data.State.Util.Random
import           Mud.Misc.Database
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Misc.Misc
import           Mud.Threads.Act
import           Mud.Threads.Digester
import           Mud.Threads.Effect
import           Mud.Threads.FeelingTimer
import           Mud.Threads.NpcServer
import           Mud.Threads.Regen
import           Mud.Threads.SpiritTimer
import           Mud.TopLvlDefs.Misc
import           Mud.Util.List
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow ((&&&), (***), first)
import           Control.Lens (_1, _2, _3, at, both, view, views)
import           Control.Lens.Operators ((.~), (&), (%~), (^.), (<>~))
import           Control.Monad ((>=>), forM_, unless, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bits (setBit, zeroBits)
import           Data.Function (on)
import           Data.List ((\\), delete, sortBy)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import           Prelude hiding (pi)
import qualified Data.IntMap.Strict as IM (delete, filterWithKey, keys, mapWithKey)
import qualified Data.Map.Strict as M (delete, elems, filter, filterWithKey, keys, size)
import qualified Data.Text as T

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Data.State.Util.Death"

logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Data.State.Util.Death"

-- ==================================================

{-
When Taro (a PC) dies:
1) Taro drops all lit lights.
2) Taro's corpse is created.
3) Taro's inventory, equipment, and coins are cloned; the clones are placed on Taro's corpse.
4) Taro's PC becomes a disembodied spirit (see below).
5) When the allotted time is up, Taro's spirit passes into the beyond.
6) Taro's PC is sent to the Necropolis.
7) Taro's player is shown Taro's stats.
8) Taro's player is disconnected.

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
    dropLitLights |&| modifyState >=> \(bs, logMsgs) -> sequence_ [ logPla "handleDeath" i . slashes $ logMsgs, bcastNl bs ]
    tweaks [ leaveParty i
           , mobTbl.ind i.mobRmDesc .~ Nothing
           , mobTbl.ind i.tempDesc  .~ Nothing
           , mobTbl.ind i.stomach   .~ [] ]
    mapM_ (i |&|) [ when   npc . possessHelper
                  , unless npc . leaveChans
                  , pauseEffects
                  , stopFeelings
                  , stopRegen
                  , throwWaitDigester
                  , modifyStateSeq . mkCorpse
                  , npc ? deleteNpc :? spiritize ]
  where
    dropLitLights ms = let lightIds  = (uncurry getInv &&& (M.elems . uncurry getEqMap)) (i, ms) & both %~ filter f
                                                                                                 & uncurry (++)
                           f         = ((&&) <$> ((== LightType) . uncurry getType) <*> uncurry getLightIsLit) . (, ms)
                           g i' pair | s <- getSing i' ms
                                     = pair & _1 %~ ((prd $ "You drop your lit " <> s,           pure i         ) :)
                                            & _1 %~ ((prd $ serialize d <> " drops a lit " <> s, desigOtherIds d) :) -- TODO: "Someone drops a lit torch." -> Should show mob name.
                                            & _2 %~ ((prd $ "Dropped a lit " <> s) :)
                           d         = mkStdDesig i ms DoCap
                           ri        = getRmId i ms
                       in ( ms & invTbl.ind ri %~ addToInv ms lightIds
                               & invTbl.ind i  %~ (\\ lightIds)
                               & eqTbl .ind i  %~ M.filter (`notElem` lightIds)
                          , foldr g mempty lightIds )

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
                & _2 <>+ let msg = asteriskQuote . prd $ "Channel deleted " <> parensQuote (s <> " has died")
                         in withDbExHandler_ "leaveChans" . insertDbTblChan . ChanRec ts ci name s $ msg
      else let f    = filter (`isAwake` ms) . map (`getIdForPCSing` ms) . M.keys . M.filter id . M.delete s
               g i' = [ (leftChanMsg n name, pure i') | n <- getRelativePCName ms (i', i) ]
           in pair & _1.chanTbl.ind ci.chanConnTbl.at s .~ Nothing
                   & _2 <>+ (bcastNl =<< mapM g (f connTbl))
      where
        s = getSing i ms

deleteNpc :: HasCallStack => Id -> MudStack ()
deleteNpc i = getState >>= \ms -> let ri = getRmId i ms
                                  in do logNotice "deleteNpc" $ "NPC " <> descSingId i ms <> " has died."
                                        tweaks [ coinsTbl           .at  i  .~ Nothing
                                               , durationalEffectTbl.at  i  .~ Nothing
                                               , entTbl             .at  i  .~ Nothing
                                               , eqTbl              .at  i  .~ Nothing
                                               , invTbl             .at  i  .~ Nothing
                                               , invTbl             .ind ri %~ (i `delete`)
                                               , mobTbl             .at  i  .~ Nothing
                                               , pausedEffectTbl    .at  i  .~ Nothing
                                               , typeTbl            .at  i  .~ Nothing ]
                                        stopWaitNpcServer i -- This removes the NPC from the "NpcTbl".

mkCorpse :: HasCallStack => Id -> MudState -> (MudState, Funs)
mkCorpse i ms =
    let et  = EntTemplate (Just "corpse")
                          s p
                          corpsePlaceholder
                          Nothing
                          zeroBits
        ot  = ObjTemplate (getCorpseWeight i ms)
                          (getCorpseVol    i ms)
                          Nothing Nothing Nothing
                          (onTrue (ip && r == Nymph) (`setBit` fromEnum IsHumming) zeroBits)
        con = Con False cap zeroBits
        cap = uncurry max . (getCorpseCapacity `fanUncurry` calcCarriedVol) $ (i, ms)
        ((is, c), em, secs, ri) = ((,,,) <$> uncurry getInvCoins
                                         <*> uncurry getEqMap
                                         <*> uncurry getCorpseDecompSecs
                                         <*> uncurry getRmId) (i, ms)
        corpse    = ip ? pcCorpse :? npcCorpse
        pcCorpse  = PCCorpse (getSing i ms) corpsePlaceholder (getSex i ms) r
        npcCorpse = NpcCorpse corpsePlaceholder
        logMsg    = T.concat [ "corpse with ID ", showTxt corpseId, " created for ", descSingId i ms, "." ]
        (corpseId,   ms',  fs ) = let ic = ip ? mempties :? (sortInv ms $ is ++ M.elems em, c)
                                  in newCorpse ms et ot con ic corpse secs ri
        ((is', em'), ms'', fs') = cloneEqMap em . clone corpseId ([], ms', fs) $ is
    in (_2 <>~ [ logPla "mkCorpse" i "corpse created.", logNotice "mkCorpse" logMsg ]) $ if ip
      then ( upd ms'' [ invTbl  .ind corpseId .~ sortInv ms'' (is' ++ M.elems em')
                      , coinsTbl.ind corpseId .~ c ]
           , fs' )
      else (ms', fs)
  where
    ip        = isPla i ms
    (s, p)    | ip = ("corpse of a " <> sexy |<>| pp r, "corpses of " <> sexy |<>| plurRace r)
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
               , sequence_ fs
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
