{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards, TupleSections, TypeApplications, ViewPatterns #-}

module Mud.Threads.Act ( drinkAct
                       , eatAct
                       , sacrificeAct
                       , startAct
                       , stopAct
                       , stopActs
                       , stopNpcActs ) where

import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Destroy
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Noun
import           Mud.Data.State.Util.Output
import           Mud.Data.State.Util.Random
import           Mud.Misc.Database
import qualified Mud.Misc.Logging as L (logNotice, logPla)
import           Mud.Threads.Effect
import           Mud.Threads.Misc
import           Mud.TopLvlDefs.Misc
import           Mud.TopLvlDefs.Seconds
import           Mud.Util.List
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow ((***))
import           Control.Exception.Lifted (catch, finally, handle)
import           Control.Lens (at, view, views)
import           Control.Lens.Operators ((%~), (&), (.~), (<-~), (?~), (^.))
import           Control.Monad (join, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (delete)
import qualified Data.Map.Strict as M (elems, insert, lookup)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import           GHC.Stack (HasCallStack)
import           System.Time.Utils (renderSecs)

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Act"

logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Act"

-- ==================================================

startAct :: HasCallStack => Id -> ActType -> Fun -> MudStack ()
startAct i actType f = handle (threadStarterExHandler i fn . Just . pp $ actType) $ do
    logPla fn i $ pp actType <> " act started."
    a <- runAsync . threadAct i actType $ f
    tweak $ mobTbl.ind i.actMap.at actType ?~ a
  where
    fn = "startAct"

stopAct :: HasCallStack => Id -> ActType -> MudStack ()
stopAct i actType = views (at actType) maybeThrowDeath . getActMap i =<< getState

stopActs :: HasCallStack => Id -> MudStack ()
stopActs i = sequence_ [ logPla "stopActs" i "stopping all acts.", mapM_ throwDeathWait . M.elems . getActMap i =<< getState ]

stopNpcActs :: HasCallStack => MudStack ()
stopNpcActs = sequence_ [ logNotice "stopNpcActs" "stopping NPC acts.", mapM_ stopActs . getNpcIds =<< getState ]

threadAct :: HasCallStack => Id -> ActType -> Fun -> MudStack ()
threadAct i actType f = handle (threadExHandler (Just i) . pp $ actType) $ a `finally` b
  where
    a    = do uncurry (>>) . (setThreadType *** (`when` tweak (mobTbl.ind i.mobRmDesc .~ Nothing))) $ pair
              f
    pair = case actType of Attacking   -> (AttackingThread   i, True )
                           Drinking    -> (DrinkingThread    i, False)
                           Eating      -> (EatingThread      i, False)
                           Sacrificing -> (SacrificingThread i, True )
    b    = do logPla "threadAct" i $ pp actType <> " act finished."
              tweak $ mobTbl.ind i.actMap.at actType .~ Nothing

-- ==================================================

drinkAct :: HasCallStack => DrinkBundle -> MudStack ()
drinkAct DrinkBundle { .. } = modifyStateSeq f `finally` tweak (mobTbl.ind drinkerId.nowDrinking .~ Nothing)
  where
    distId@(DistinctLiqId i) = drinkLiq^.liqId
    f ms = let t  | ts <- [ "You begin drinking ", renderLiqNoun drinkLiq the, " from the ", drinkVesselSing ]
                  = thrice prd . T.concat $ ts
               d  = mkStdDesig drinkerId ms DoCap
               bs = pure (T.concat [ serialize d, " begins drinking from ", vo, "." ], desigOtherIds d)
               fs = pure $ sequence_ gs `catch` die (Just drinkerId) (pp Drinking)
               gs = [ multiWrapSend1Nl drinkerMq drinkerCols . dropEmpties $ [ t, drinkLiq^.liqDrinkDesc ]
                    , bcastIfNotIncogNl drinkerId bs
                    , loop 1 ]
           in (ms & mobTbl.ind drinkerId.nowDrinking ?~ (drinkLiq, drinkVesselSing), fs)
    vo               = mkSerVerbObj renderVesselSing
    renderVesselSing = drinkVesselSing |&| (isJust drinkVesselId ? aOrAn :? the)
    loop x           = do
        liftIO . delaySecs $ 1
        now <- liftIO getCurrentTime
        consume drinkerId . pure . StomachCont (Left distId) now $ False
        (ms, newCont) <- case drinkVesselId of
          Just vi -> modifyState $ \ms -> let g (_, m) = let newCont = m == 1 ? Nothing :? Just (drinkLiq, pred m)
                                                             ms'     = ms & vesselTbl.ind vi.vesselCont .~ newCont
                                                         in (ms', (ms', Right newCont))
                                          in maybe (ms, (ms, Left ())) g . getVesselCont vi $ ms
          Nothing -> (, Left ()) <$> getState
        let (stomAvail, _) = calcStomachAvailSize drinkerId ms
            d              = mkStdDesig drinkerId ms DoCap
            bcastHelper b  | ts <- [ serialize d, " finishes drinking from ", vo, b |?| " after draining it dry", "." ]
                           = bcastIfNotIncogNl drinkerId . pure $ (T.concat ts, desigOtherIds d)
        if | newCont == Right Nothing ->
               let xs = [ "You drain the ", drinkVesselSing, " dry after ", mkMouthfulTxt x, " mouthful", sOnNon1 x, "." ]
               in (>> bcastHelper True) . ioHelper x . T.concat $ xs
           | isZero stomAvail ->
               let xs = [ "You are so full after ", mkMouthfulTxt x, " mouthful", sOnNon1 x, t ]
                   t  = " that you have to stop drinking. You don't feel so good..."
               in (>> bcastHelper False) . ioHelper x . T.concat $ xs
           | x == drinkAmt    -> (>> bcastHelper False) . ioHelper x $ "You finish drinking."
           | otherwise        -> loop . succ $ x
    ioHelper m t | ts <- [ "drank ", showTxt m, " mouthful", sOnNon1 m, " of ", renderLiqNoun drinkLiq aOrAn, " "
                         , parensQuote . showTxt $ i, " from ", renderVesselSing, "." ]
                 = do logPla "drinkAct ioHelper" drinkerId . T.concat $ ts
                      wrapSend drinkerMq drinkerCols t
                      sendDfltPrompt drinkerMq drinkerId

mkMouthfulTxt :: HasCallStack => Mouthfuls -> Text
mkMouthfulTxt x | x <= 8 = showTxt x | otherwise = "many"

-----

eatAct :: HasCallStack => EatBundle -> MudStack ()
eatAct EatBundle { .. } = modifyStateSeq f `finally` tweak (mobTbl.ind eaterId.nowEating .~ Nothing)
  where
    distId@(DistinctFoodId i) = eatFood^.foodId
    f ms = let t  = thrice prd $ "You begin eating the " <> eatFoodSing
               d  = mkStdDesig eaterId ms DoCap
               bs = pure (T.concat [ serialize d, " begins eating ", vo, "." ], desigOtherIds d)
               fs = pure $ sequence_ gs `catch` die (Just eaterId) (pp Eating)
               gs = [ multiWrapSend1Nl eaterMq eaterCols . dropEmpties $ [ t, eatFood^.foodEatDesc ]
                    , bcastIfNotIncogNl eaterId bs
                    , loop 1 ]
           in (ms & mobTbl.ind eaterId.nowEating ?~ (eatFoodId, eatFoodSing), fs)
    vo     = mkSerVerbObj . aOrAn $ eatFoodSing
    loop x = do
        liftIO . delaySecs =<< view foodSecsPerMouthful . getDistinctFood i <$> getState
        now <- liftIO getCurrentTime
        consume eaterId . pure . StomachCont (Right distId) now $ False
        (m, ms) <- modifyState $ \ms -> let pair@(_, ms') = ms & foodTbl.ind eatFoodId.foodRemMouthfuls <-~ 1
                                        in (ms', pair)
        let (stomAvail, _) = calcStomachAvailSize eaterId ms
            d              = mkStdDesig eaterId ms DoCap
            bcastHelper b  | ts <- [ serialize d, " finishes eating ", b |?| "all of ", vo, "." ]
                           = bcastIfNotIncogNl eaterId . pure $ (T.concat ts, desigOtherIds d)
        if | m <= 0 -> do
               destroy . pure $ eatFoodId
               ioHelper x . T.concat $ [ "You finish eating all of the ", eatFoodSing, " after ", mkMouthfulTxt x
                                       , " mouthful", sOnNon1 x, "." ]
               bcastHelper True
           | isZero stomAvail ->
               let xs = [ "You are so full after ", mkMouthfulTxt x, " mouthful", sOnNon1 x, t ]
                   t  = " that you have to stop eating. You don't feel so good..."
               in (>> bcastHelper False) . ioHelper x . T.concat $ xs
           | x == eatAmt -> (>> bcastHelper False) . ioHelper x $ "You finish eating."
           | otherwise   -> loop . succ $ x
    ioHelper m t = do
        logPla "eatAct ioHelper" eaterId . T.concat $ [ "ate ", showTxt m, " mouthful", sOnNon1 m, " of "
                                                      , aOrAn eatFoodSing, " ", parensQuote . showTxt $ i, "." ]
        wrapSend eaterMq eaterCols t
        sendDfltPrompt eaterMq eaterId

-----

sacrificeAct :: HasCallStack => Id -> MsgQueue -> Id -> GodName -> MudStack ()
sacrificeAct i mq ci gn = handle (die (Just i) . pp $ Sacrificing) $ do
    liftIO . delaySecs $ sacrificeSecs
    modifyStateSeq $ \ms ->
        let helper f = (sacrificesTblHelper ms, fs)
              where
                fs = [ destroy . pure $ ci, f, sacrificeBonus i gn, sendDfltPrompt mq i ]
            sacrificesTblHelper = pcTbl.ind i.sacrificesTbl %~ f
              where
                f tbl = maybe (M.insert gn 1 tbl) (flip (M.insert gn) tbl . succ) . M.lookup gn $ tbl
            mkBcastHelper b i' | t <- the' . mkCorpseAppellation i' ms $ ci, vo <- serialize . VerbObj t $ DoCap
                               = ((b ? vo :? t) <> " fades away and disappears.", pure i')
        in if ((&&) <$> uncurry hasType <*> (== CorpseType) . uncurry getType) (ci, ms)
          then helper $ case findInvContaining ci ms of
            Just invId -> if | getType invId ms == RmType, mobIds <- findMobIds ms . getInv invId $ ms ->
                               bcastNl $ if i `elem` mobIds
                                 then mkBcastHelper False i : map (mkBcastHelper True) (i `delete` mobIds)
                                 else map (mkBcastHelper True) mobIds
                             | isNpcPla invId ms -> bcastNl . pure . mkBcastHelper False $ invId
                             | otherwise         -> unit
            Nothing    -> unit
          else (ms, [])

sacrificeBonus :: HasCallStack => Id -> GodName -> MudStack ()
sacrificeBonus i gn@(pp -> gn') = getSing i <$> getState >>= \s -> do
    now <- liftIO getCurrentTime
    let operation = do insertDbTblSacrifice . SacrificeRec (showTxt now) s $ gn'
                       lookupSacrifices s gn'
        next count
          | count < 2 = logHelper . prd $ "first sacrifice made to " <> gn'
          | otherwise =
              let msg = T.concat [ commaShow count, " sacrifices to ", gn', "; " ]
              in if isZero $ count `mod` 10
                then join <$> withDbExHandler "sac_bonus" (lookupSacBonusTime s gn') >>= \case
                  Nothing   -> applyBonus i s gn now
                  Just time | diff@(T.pack . renderSecs -> secs) <- round $ now `diffUTCTime` time
                            , ts <- [ msg, "not enough time has passed since the last bonus "
                                    , parensQuote $ secs <> " seconds since last bonus", "." ]
                            -> diff > fromIntegral oneDayInSecs ? applyBonus i s gn now :? logHelper (T.concat ts)
                else logHelper $ msg <> "no bonus yet."
    maybeVoid next =<< withDbExHandler "sacrifice" operation
  where
    logHelper = logPla "sacrificeBonus" i

applyBonus :: HasCallStack => Id -> Sing -> GodName -> UTCTime -> MudStack ()
applyBonus i s gn now = do
    logPla "applyBonus" i "applying bonus."
    withDbExHandler_ "sac_bonus" . insertDbTblSacBonus . SacBonusRec (showTxt now) s . pp $ gn
    let f = \case Aule      -> let a = (,) <$> rndmElem (mkXpPairs allValues) <*> rndmElem (allValues @Attrib)
                                   b = ((>>) <$> uncurry maxXp . fst <*> effectHelper Nothing (15, 15) . snd)
                               in a >>= b
                  Caila     -> f Aule
                  Celoriel  -> maxXp curPp maxPp >>  effectHelper Nothing     (5,  15) Ps
                  Dellio    -> maxXpHelper       >>  effectHelper Nothing     (5,  15) Dx
                  Drogo     -> maxXp curMp maxMp >>  effectHelper Nothing     (5,  15) Ma
                  Iminye    -> maxXpHelper       >>  effectHelper (Just "Dx") (3,  8 ) Dx
                                                 >>  effectHelper (Just "Ht") (3,  8 ) Ht
                  Itulvatar -> maxXpHelper       >> (effectHelper Nothing     (10, 15) =<< rndmElem [ St, Dx, Ht ])
                  Murgorhd  -> f Aule
                  Rhayk     -> maxXp curHp maxHp >>  effectHelper Nothing     (5,  15) St
                  Rumialys  -> maxXp curFp maxFp >>  effectHelper Nothing     (5,  15) Ht
    f gn
  where
    mkXpPairs   = map (\case Hp -> (curHp, maxHp)
                             Mp -> (curMp, maxMp)
                             Pp -> (curPp, maxPp)
                             Fp -> (curFp, maxFp))
    maxXpHelper = uncurry maxXp =<< rndmElem (mkXpPairs [ Hp, Fp ])
    effectHelper effTagSuff range attrib = let tag     = "sacrificeBonus" <> pp gn <> fromMaybeEmp effTagSuff
                                               effSub  = EffectAttrib attrib
                                               effVal  = Just . EffectRangedVal $ range
                                               effFeel = Just . EffectFeeling tag $ sacrificeBonusSecs
                                           in startEffect i . Effect (Just tag) effSub effVal sacrificeBonusSecs $ effFeel
    maxXp curXpLens maxXpLens            = tweak $ \ms -> let x = view (mobTbl.ind i.maxXpLens) ms
                                                          in ms & mobTbl.ind i.curXpLens .~ x
