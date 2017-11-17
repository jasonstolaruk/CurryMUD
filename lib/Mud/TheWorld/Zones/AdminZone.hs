{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.TheWorld.Zones.AdminZone ( adminZoneHooks
                                    , adminZoneRmActionFuns
                                    , adminZoneRmFuns
                                    , createAdminZone
                                    , getFlowerHook
                                    , lookFlowerbedHook ) where

import qualified Data.Vector.Unboxed as V (Vector, head)
import           Mud.Cmds.Msgs.Advice
import           Mud.Cmds.Msgs.Sorry
import           Mud.Cmds.Util.Pla
import           Mud.Data.Misc
import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Make
import           Mud.Data.State.Util.Misc
import           Mud.Data.State.Util.Put
import           Mud.Data.State.Util.Random
import           Mud.Misc.LocPref
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Misc.Misc
import           Mud.TheWorld.Liqs
import           Mud.TheWorld.Misc
import           Mud.TheWorld.Zones.AdminZoneIds
import           Mud.TheWorld.Zones.TutorialIds (iTutWelcome)
import           Mud.Threads.Act
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Seconds
import           Mud.TopLvlDefs.Vols
import           Mud.TopLvlDefs.Weights
import           Mud.Util.List
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Lens (_1, _2, _3, _4, view)
import           Control.Lens.Operators ((?~), (.~), (&), (%~), (<>~))
import           Control.Monad (forM_)
import           Data.Bits (setBit, zeroBits)
import           Data.Function (on)
import           Data.List ((\\), delete)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Map.Strict as M (empty, fromList, singleton)
import qualified Data.Text as T


pmf :: PatternMatchFail
pmf = U.pmf "Mud.TheWorld.Zones.AdminZone"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.AdminZone"


-- ==================================================
-- Hooks:


-- TODO: Consider visibility (here and in other zones).
adminZoneHooks :: [(HookName, HookFun)]
adminZoneHooks = [ (drinkPoolHookName,                    drinkPoolHookFun                   )
                 , (fillPoolHookName,                     fillPoolHookFun                    )
                 , (getFlowerHookName,                    getFlowerHookFun                   )
                 , (lookCeilingHookName,                  lookCeilingHookFun                 )
                 , (lookFlowerbedHookName,                lookFlowerbedHookFun               )
                 , (lookWallsHookName,                    lookWallsHookFun                   )
                 , (readLookPaperHookName,                readLookPaperHookFun               )
                 , (readLookPosterHookName,               readLookPosterHookFun              )
                 , (readLookSign_iEmptyHookName,          readLookSign_iEmptyHookFun         )
                 , (readLookSign_iLoungeEntranceHookName, readLookSign_iLoungeEntranceHookFun)
                 , (readLookSign_iTutEntranceHookName,    readLookSign_iTutEntranceHookFun   )
                 , (smellFlowerbedHookName,               smellFlowerbedHookFun              ) ]


-----


drinkPoolHook :: Hook
drinkPoolHook = Hook drinkPoolHookName . pure $ "pool"


drinkPoolHookName :: HookName
drinkPoolHookName = "AdminZone_iAtrium_drinkPool"


drinkPoolHookFun :: HookFun
drinkPoolHookFun i _ _ a@(as, (ms, _, _, _), _) | fst (calcStomachAvailSize i ms) <= 0 = a & _2._2 <>~ pure sorryFull
                                                | db <- DrinkBundle { drinkerId       = i
                                                                    , drinkerMq       = getMsgQueue i ms
                                                                    , drinkerCols     = getColumns  i ms
                                                                    , drinkVesselId   = Nothing
                                                                    , drinkVesselSing = "pool"
                                                                    , drinkLiq        = waterLiq
                                                                    , drinkAmt        = read . T.unpack . head $ as }
                                                = a & _1 .~ []
                                                    & _3 .~ (pure . startAct i Drinking . drinkAct $ db)


-----


fillPoolHook :: Hook
fillPoolHook = Hook fillPoolHookName . pure $ "pool"


fillPoolHookName :: HookName
fillPoolHookName = "AdminZone_iAtrium_fillPool"


fillPoolHookFun :: HookFun
fillPoolHookFun i Hook { .. } _ a@(as, (ms, _, _, _), _) =
    let as'                         = T.splitOn (T.singleton hookArgDelimiter) . head $ as
        (inInvs, inEqs, inRms)      = sortArgsInvEqRm InInv as'
        sorryInEq                   = inEqs |!| sorryFillInEq
        sorryInRm                   = inRms |!| sorryFillInRm
        (eiss, ecs)                 = uncurry (resolveMobInvCoins i ms inInvs) . getInvCoins i $ ms
        sorryCoins                  = ecs   |!| sorryFillCoins
        (ms', toSelfs, bs, logMsgs) = helperFillWaterRmEitherInv i (mkStdDesig i ms DoCap) eiss (ms, [], [], [])
    in a & _1    .~ []
         & _2._1 .~ ms'
         & _2._2 .~ dropBlanks ([ sorryInEq, sorryInRm, sorryCoins ] ++ toSelfs)
         & _2._3 .~ bs
         & _2._4 .~ logMsgs


helperFillWaterRmEitherInv :: Id
                           -> Desig
                           -> [Either Text Inv]
                           -> GenericIntermediateRes
                           -> GenericIntermediateRes
helperFillWaterRmEitherInv _ _        []         a = a
helperFillWaterRmEitherInv i srcDesig (eis:eiss) a = next $ case eis of
    Left msg -> sorry msg
    Right is -> helper is a
  where
    next      = helperFillWaterRmEitherInv i srcDesig eiss
    sorry msg = a & _2 <>~ pure msg
    helper []       a'                = a'
    helper (vi:vis) a'@(ms', _, _, _)
      | getType vi ms' /= VesselType = helper vis . sorry' . sorryFillType $ vs
      | otherwise                    = helper vis . (_3 <>~ bcastHelper)   $ case getVesselCont vi ms' of
          Nothing       | cans <- calcCanCarryMouthfuls vmm
                        -> if | cans < 1   -> a' & _2 <>~ sorryEnc
                              | vmm > cans -> partialMsgHelper (a' & _1.vesselTbl.ind vi.vesselCont ?~ (waterLiq, cans))
                              | otherwise  -> fillUp
          Just (vl, vm) | vl 🍧 waterLiq -> sorry' . sorryFillWaterLiqTypes . getSing vi $ ms'
                        | vm >= vmm -> sorry' . sorryFillAlready $ vs
                        | vAvail <- vmm - vm
                        , cans   <- calcCanCarryMouthfuls vAvail
                        -> if | cans < 1      -> a' & _2 <>~ sorryEnc
                              | vAvail > cans -> partialMsgHelper (a' & _1.vesselTbl.ind vi.vesselCont ?~ (waterLiq, vm + cans))
                              | otherwise     -> fillUp
      where
        (🍧) = (/=) `on` view liqId
        sorry' msg = a' & _2 <>~ pure msg
        sorryEnc   = pure $ sorryGetEnc <> "any more water."
        vs         = getSing         vi ms'
        vmm        = getMaxMouthfuls vi ms'
        calcCanCarryMouthfuls amt = let myWeight = calcWeight i ms'
                                        myMaxEnc = calcMaxEnc i ms'
                                        g        | myWeight + (amt * mouthfulWeight) > myMaxEnc
                                                 , margin <- myMaxEnc - myWeight
                                                 = floor (margin `divide` mouthfulWeight :: Double)
                                                 | otherwise = amt
                                    in g
        fillUp             = a' & _1.vesselTbl.ind vi.vesselCont ?~ (waterLiq, vmm)
                                & _2 <>~ mkFillUpMsg
                                & _4 <>~ mkFillUpMsg
        partialMsgHelper   = (_4 <>~ mkPartialFillUpMsg) . (_2 <>~ mkPartialFillUpMsg)
        mkFillUpMsg        = pure $ "You fill up the " <> vs <> " with water from the pool."
        mkPartialFillUpMsg = pure . T.concat $ [ "You partially fill the "
                                               , vs
                                               , " with water from the pool. "
                                               , head sorryEnc ]
        bcastHelper        = pure (T.concat [ serialize srcDesig
                                            , " fills "
                                            , aOrAn vs
                                            , " with water from the pool." ], i `delete` desigIds srcDesig)


-----


getFlowerHook :: Hook
getFlowerHook = Hook getFlowerHookName [ "flower", "flowers" ]


getFlowerHookName :: HookName
getFlowerHookName = "AdminZone_iAtrium_getFlower"


getFlowerHookFun :: HookFun
getFlowerHookFun i Hook { .. } v a@(_, (ms, _, _, _), _) = if calcWeight i ms + flowerWeight > calcMaxEnc i ms
  then a & _2._2 .~ pure (sorryGetEnc <> rest)
  else a & _1    %~  (\\ hookTriggers)
         & _2._2 <>~ pure msg
         & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                       in pure (serialize selfDesig <> " picks " <> rest, i `delete` desigIds selfDesig) )
         & _2._4 <>~ pure (bracketQuote hookName <> " picked flower")
         & _3    .~  pure (mkFlower i v)
  where
    msg  = "You pick " <> rest
    rest = "a flower from the flowerbed."


mkFlower :: Id -> V.Vector Int -> MudStack ()
mkFlower i v = modifyStateSeq $ \ms -> let et = EntTemplate (Just "flower")
                                                            "flower" ""
                                                            desc
                                                            (Just smell)
                                                            zeroBits
                                           ot = ObjTemplate flowerWeight
                                                            flowerVol
                                                            (Just taste)
                                                            Nothing
                                                            Nothing
                                                            (setBit zeroBits . fromEnum $ IsBiodegradable)
                                           (_, ms', fs) = newObj ms et ot i
                                       in (ms', fs)
  where
    (desc, smell, taste) = rndmIntToElem (V.head v) tuples
    tuples = [ ( "It's a fragrant daffodil sporting a collar of white petals."
               , "The powerful fragrance of the daffodil is nearly intoxicating."
               , "The petals have a pleasant floral taste. You could garnish a salad with these." )
             , ( "It's a hardy hibiscus with pink-tinged pedals surrounding a distinctly red center."
               , "The beautiful hibiscus gives off a mildly sweet fragrance."
               , "The petals have a somewhat spicy, somewhat sour taste." )
             , ( "This eye-popping chrysanthemum has a fiery-orange bloom composed of many tiny petals."
               , "Though striking in appearance, the chrysanthemum is not particularly fragrant."
               , "The petals taste slightly sour." )
             , ( "This blue lily has six large, independent petals opening widely from its base."
               , "Sure to attract a variety of loyal pollinators, the lily is markedly fragrant."
               , "The petals taste a bit like nutmeg." ) ]


-----


lookCeilingHook :: Hook
lookCeilingHook = Hook lookCeilingHookName [ "ceiling", "up" ]


lookCeilingHookName :: HookName
lookCeilingHookName = "AdminZone_iEmpty_lookCeiling"


lookCeilingHookFun :: HookFun
lookCeilingHookFun = mkGenericHookFun ceilingDesc "looks up at the ceiling." "looked at ceiling"
  where
    ceilingDesc = thrice prd "The tall ceiling looks identical to the walls: plain and white. Even if there was a \
                             \means of exit up there, you can't imagine how you'd reach it"


-----


lookFlowerbedHook :: Hook
lookFlowerbedHook = Hook lookFlowerbedHookName [ "flowerbed", "flower", "flowers" ]


lookFlowerbedHookName :: HookName
lookFlowerbedHookName = "AdminZone_iAtrium_lookFlowerbed"


lookFlowerbedHookFun :: HookFun
lookFlowerbedHookFun = mkGenericHookFun flowerbedDesc "looks at the flowerbed." "looked at flowerbed"
  where
    flowerbedDesc = "The tasteful flowerbed prominently features daffodils, hibiscuses, chrysanthemums, and lilies, \
                    \all in a pleasing array of colors."


-----


lookWallsHook :: Hook
lookWallsHook = Hook lookWallsHookName [ "walls", "wall" ]


lookWallsHookName :: HookName
lookWallsHookName = "AdminZone_iEmpty_lookWalls"


lookWallsHookFun :: HookFun
lookWallsHookFun = mkGenericHookFun wallsDesc "looks at the walls." "looked at walls"
  where
    wallsDesc = "You are enclosed by four stark, while walls, with no apparent means of exit."


-----


readLookPaperHook :: Hook
readLookPaperHook = Hook readLookPaperHookName . pure $ "paper"


readLookPaperHookName :: HookName
readLookPaperHookName = "AdminZone_iTutEntrance_readLookPaper"


readLookPaperHookFun :: HookFun
readLookPaperHookFun i Hook { .. } (V.head -> r) a@(_, (ms, _, _, _), _) =
    a &    _1 %~  (\\ hookTriggers)
      & _2._2 <>~ pure signDesc
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure ( serialize selfDesig <> " reads the piece of paper nailed to the sign."
                            , i `delete` desigIds selfDesig ) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read paper")
  where
    signDesc = dblQuote . prd $ "Your lucky number is " <> x
    x        = showTxt . rndmIntToPer $ r


-----


readLookPosterHook :: Hook
readLookPosterHook = Hook readLookPosterHookName . pure $ "poster"


readLookPosterHookName :: HookName
readLookPosterHookName = "AdminZone_iCentral_readLookPoster"


readLookPosterHookFun :: HookFun
readLookPosterHookFun = mkGenericHookFun posterDesc "reads the poster on the wall." "read poster"
  where
    posterDesc =
        "The poster reads:\n\
        \WELCOME TO THE ADMIN ZONE\n\
        \This is a restricted area where admins can hang out."


-----


readLookSign_iEmptyHook :: Hook
readLookSign_iEmptyHook = Hook readLookSign_iEmptyHookName . pure $ "sign"


readLookSign_iEmptyHookName :: HookName
readLookSign_iEmptyHookName = "AdminZone_iEmpty_readLookSign"


readLookSign_iEmptyHookFun :: HookFun
readLookSign_iEmptyHookFun = mkGenericHookFun signDesc "reads the sign on the wall." "read sign"
  where
    signDesc = "The following message has been painted on the sign in a tight, flowing script:\n\
               \\"Welcome to the Empty Room. You have been summoned here by a CurryMUD administrator who wishes to \
               \speak with you in private. As there are no exits, you will need the assistance of an administrator \
               \when the time comes for you to leave. We hope you enjoy your stay!\""


-----


readLookSign_iLoungeEntranceHook :: Hook
readLookSign_iLoungeEntranceHook = Hook readLookSign_iLoungeEntranceHookName . pure $ "sign"


readLookSign_iLoungeEntranceHookName :: HookName
readLookSign_iLoungeEntranceHookName = "AdminZone_iLoungeEntrance_readLookSign"


readLookSign_iLoungeEntranceHookFun :: HookFun
readLookSign_iLoungeEntranceHookFun = mkGenericHookFun signDesc "reads the sign affixed to the door." "read sign"
  where
    signDesc = "The small sign reads, \"Admin Lounge.\""


-----


readLookSign_iTutEntranceHook :: Hook
readLookSign_iTutEntranceHook = Hook readLookSign_iTutEntranceHookName . pure $ "sign"


readLookSign_iTutEntranceHookName :: HookName
readLookSign_iTutEntranceHookName = "AdminZone_iTutEntrance_readLookSign"


readLookSign_iTutEntranceHookFun :: HookFun
readLookSign_iTutEntranceHookFun = mkGenericHookFun signDesc "reads the sign floating above the portal." "read sign"
  where
    signDesc = "The sign reads, \"Tutorial this way. No re-entry!\"\n\
               \A small square piece of paper has been nailed to the bottom-right corner of the sign. There is a \
               \number written on it... and the number appears to be changing!"


-----


smellFlowerbedHook :: Hook
smellFlowerbedHook = Hook smellFlowerbedHookName [ "flowerbed", "flower", "flowers" ]


smellFlowerbedHookName :: HookName
smellFlowerbedHookName = "AdminZone_iAtrium_smellFlowerbed"


smellFlowerbedHookFun :: HookFun
smellFlowerbedHookFun = mkGenericHookFun smellDesc "smells the flowerbed." "smelled flowerbed"
  where
    smellDesc = "You are greeted by the gentle organic scents of florets and soil."


-- ==================================================
-- Room action functions:


-- TODO: Consider visibility (here and in other zones).
adminZoneRmActionFuns :: [(FunName, RmActionFun)]
adminZoneRmActionFuns = pure (pickRmActionFunName, pick)


-----


pickRmAction :: RmAction
pickRmAction = RmAction "pick" pickRmActionFunName


pickRmActionFunName :: FunName
pickRmActionFunName = "AdminZone_iAtrium_pick"


pick :: RmActionFun
pick p@AdviseNoArgs     = advise p [] advicePickNoArgs
pick p@(LowerNub' i as) = genericActionWithFuns p helper "pick"
  where
    helper v ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorrys                 = dropEmpties [ inInvs |!| sorryPickInInv, inEqs |!| sorryPickInEq ]
            h@Hook { .. }          = getFlowerHook
            inRms'                 = dropSynonyms hookTriggers . dropPrefixesForHooks (pure h) $ inRms
            initAcc                = (inRms', (ms, [], [], []), [])
            (_, (ms', toSelfs, bs, logMsgs), fs) | any (`elem` hookTriggers) inRms' = getHookFun hookName ms i h v initAcc
                                                 | otherwise                        = initAcc
            mkMsgForArg arg | arg `elem` hookTriggers = head toSelfs
                            | otherwise               = sorryPickNotFlower arg
        in (ms', (sorrys ++ map mkMsgForArg inRms', bs, logMsgs, fs))
pick p = pmf "pick" p


-- ==================================================
-- Room functions:


adminZoneRmFuns :: [(FunName, Fun)]
adminZoneRmFuns = [ (beepRmFunName,    beepRmFun   )
                  , (beeBuzzRmFunName, beeBuzzRmFun) ]


-----


beepRmFunName :: FunName
beepRmFunName = "AdminZone_iCentral_beep"


beepRmFun :: Fun
beepRmFun = mkRndmBcastRmFun iCentral "iCentral" beepRmFunName 25 45 beepMsg
  where
    beepMsg = "A series of blips and beeps can be heard, originating from one of the control panels."


-----


beeBuzzRmFunName :: FunName
beeBuzzRmFunName = "AdminZone_iAtrium_beeBuzz"


beeBuzzRmFun :: Fun
beeBuzzRmFun = mkRndmBcastRmFun iAtrium "iAtrium" beeBuzzRmFunName 25 oneMinInSecs beeBuzzMsg
  where
    beeBuzzMsg = "A plump bumblebee buzzes happily around the flowerbed."


-- ==================================================
-- Zone definition:


adminFlags :: Flags
adminFlags = foldl setBit zeroBits . map fromEnum $ [ IsAdmin
                                                    , IsNotFirstAdminMsg
                                                    , IsNotFirstMobSay
                                                    , IsTunedAdmin
                                                    , IsTunedQuestion ]


createAdminZone :: MudStack ()
createAdminZone = do
  logNotice "createAdminZone" "creating the admin zone."

  -- ==================================================
  -- Objects:
  let weightTuples = [ (i190Lb, "190", 19000, 66995)
                     , (i100Lb, "100", 10000, 35260)
                     , (i75Lb,  "75",  7500,  26445)
                     , (i50Lb1, "50",  5000,  17630)
                     , (i50Lb2, "50",  5000,  17630)
                     , (i25Lb1, "25",  2500,  8815 )
                     , (i25Lb2, "25",  2500,  8815 )
                     , (i10Lb1, "10",  1000,  3525 )
                     , (i10Lb2, "10",  1000,  3525 )
                     , (i5Lb1,  "5",   500,   1760 )
                     , (i5Lb2,  "5",   500,   1760 )
                     , (i1Lb1,  "1",   100,   350  )
                     , (i1Lb2,  "1",   100,   350  )
                     , (i1Lb3,  "1",   100,   350  )
                     , (i1Lb4,  "1",   100,   350  )
                     , (i1Lb5,  "1",   100,   350  ) ]
  forM_ weightTuples $ \(i, t, w, v) ->
      putObj i
             (Ent i
                  (Just "weight")
                  (t <> " lb weight") ""
                  "It's a hunk of metal cut into a rounded shape."
                  Nothing
                  zeroBits)
             (mkObj . ObjTemplate w v Nothing Nothing Nothing $ zeroBits)
  putObj iSlab
         (Ent iSlab
              (Just "slab")
              "large slab of rock" "large slabs of rock"
              "It's a hunk of grey rock cut into a rectangular block."
              Nothing
              zeroBits)
         (mkObj . ObjTemplate 19300 67000 Nothing Nothing Nothing $ zeroBits)
  forM_ [iCube1..iCube1 + 19] $ \i ->
      putObj i
             (Ent i
                  (Just "cube")
                  "cube" ""
                  "The solid white cube measures 6\" x 6\" x 6\"."
                  Nothing
                  zeroBits)
             (mkObj . ObjTemplate cubeWeight cubeVol Nothing Nothing Nothing $ zeroBits)

  -- ==================================================
  -- Players:
  putPla iRoot
         (Ent iRoot
              Nothing
              "Root" ""
              "He is the root administrator."
              Nothing
              zeroBits)
         []
         mempty
         M.empty
         (mkMob (MobTemplate Male
                             50 50 50 50 50
                             100 100 100 100
                             0 0
                             RHand
                             allValues
                             iLoggedOut
                             Nothing
                             (calcCorpseWeight Human) (calcCorpseVol Human) (calcCorpseCapacity Human)
                             (calcCorpseDecompSecs Human)
                             dfltParty))
         M.empty
         (M.singleton "Curry" True)
         (PC Human ["Curry"] ["Curry"] 0 M.empty)
         (mkPla . PlaTemplate (setBit adminFlags . fromEnum $ IsIncognito) [] $ iLounge)
  putPla iCurry
         (Ent iCurry
              Nothing
              "Curry" ""
              "He is a CurryMUD administrator."
              Nothing
              zeroBits)
         []
         mempty
         M.empty
         (mkMob (MobTemplate Male
                             50 50 50 50 50
                             100 100 100 100
                             0 0
                             RHand
                             allValues
                             iLoggedOut
                             Nothing
                             (calcCorpseWeight Human) (calcCorpseVol Human) (calcCorpseCapacity Human)
                             (calcCorpseDecompSecs Human)
                             dfltParty))
         M.empty
         (M.singleton "Root" True)
         (PC Human ["Root"] ["Root"] 0 M.empty)
         (mkPla . PlaTemplate adminFlags [] $ iLounge)

  -- ==================================================
  -- Rooms:
  putRm iWelcome
        []
        mempty
        (mkRm (RmTemplate "Welcome room"
            "Ad-hoc PCs created for new connections are placed here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0) -- This room is technically in the "unknown" zone.
            SpecialEnv
            (Just "Welcome")
            M.empty [] []))
  putRm iLoggedOut
        [ iRoot ]
        mempty
        (mkRm (RmTemplate "Logged out room"
            "PCs are placed here when their players log out."
            Nothing
            Nothing
            zeroBits
            []
            (1, 0, 0) -- This room is technically in the "unknown" zone.
            SpecialEnv
            (Just "Logged out")
            M.empty [] []))
  putRm iNecropolis
        []
        mempty
        (mkRm (RmTemplate "Necropolis"
            "PCs are placed here when they die."
            Nothing
            Nothing
            zeroBits
            []
            (2, 0, 0) -- This room is technically in the "unknown" zone.
            SpecialEnv
            (Just "Necropolis")
            M.empty [] []))
  putRm iEmpty
        []
        mempty
        (mkRm (RmTemplate "The empty room"
            "This small square room is strikingly barren. There doesn't appear to be a door or any means of exit. \
            \Notably, there is a small wooden sign affixed to the north wall."
            Nothing
            (Just "On account of there being no ventilation to speak of, the air here is markedly stale and stuffy.")
            zeroBits
            []
            (1, 1, 0)
            SpecialEnv
            (Just "Empty")
            (M.fromList [ ("look", [ readLookSign_iEmptyHook, lookWallsHook, lookCeilingHook ])
                        , ("read", [ readLookSign_iEmptyHook                                 ]) ])
            [] []))
  putRm iTrashDump
        []
        mempty
        (mkRm (RmTemplate "Trash dump"
            "Items deposited in magic trash bins end up here."
            Nothing
            (Just "This place sure does smell like shit.")
            zeroBits
            []
            (2, 1, 0)
            SpecialEnv
            (Just "Trash")
            M.empty [] []))
  putRm iClone
        []
        mempty
        (mkRm (RmTemplate "Clone room"
            "Cloned things are temporarily placed here."
            Nothing
            Nothing
            zeroBits
            []
            (3, 1, 0)
            SpecialEnv
            (Just "Clone")
            M.empty [] []))
  putRm iCentral
        []
        mempty
        (mkRm (RmTemplate "Central control room"
            "Welcome to the heart of the machine! Sprawled about this white, dome-shaped room is a cluster of \
            \electronic displays and control panels used by the admins to monitor and supervise the daily operations \
            \of CurryMUD.\n\
            \There is a large poster on the wall.\n\
            \Near the center of the room, a spiral staircase leads down. A trash bin sits adjacent to the stairs. To \
            \the east a door opens to a hallway."
            (Just "The cooling fans spinning inside the control panels give off a soothing whirring sound.")
            (Just "You vaguely detect the chemical scents of plastic and cleaning solutions.")
            zeroBits
            [ StdLink Down iBasement    1
            , StdLink East iHallwayWest 1 ]
            (0, 0, 0)
            InsideLitEnv
            (Just "Central")
            (M.fromList [ ("look", [ lookTrashHook, readLookPosterHook ])
                        , ("put",  [ putTrashHook                      ])
                        , ("read", [ readLookPosterHook                ]) ])
            [ trashRmAction ]
            [ beepRmFunName ]))
  putRm iHallwayWest
        []
        mempty
        (mkRm (RmTemplate "Hallway"
            "You are in a wide hallway leading east. A door to the west opens into the central control room."
            Nothing
            Nothing
            zeroBits
            [ StdLink West iCentral     1
            , StdLink East iHallwayEast 1 ]
            (1, 0, 0)
            InsideLitEnv
            Nothing
            M.empty [] []))
  putRm iHallwayEast
        []
        mempty
        (mkRm (RmTemplate "Hallway"
            "You are in a wide hallway leading west. To your east, the hallway opens up into an atrium."
            Nothing
            Nothing
            zeroBits
            [ StdLink West iHallwayWest 1
            , StdLink East iAtrium      1 ]
            (2, 0, 0)
            InsideLitEnv
            Nothing
            M.empty [] []))
  putRm iAtrium
        []
        mempty
        (mkRm (RmTemplate "Atrium"
            "The large, airy atrium is sparsely furnished so as to accentuate its open feel. The focal point of the \
            \atrium is a shallow pool positioned directly under a large opening in the ceiling, allowing the pool to \
            \freely collect rainwater. At each corner of the square pool, a marble column purposefully rises up to \
            \support the ceiling. Next to the pool is a raised flowerbed, surrounded by four unembellished stone \
            \benches.\n\
            \An opening in the west wall leads out into a hallway."
            (Just "You hear the sound of trees blowing in the breeze coming through the opening in the ceiling.")
            (Just "The scent of fresh air wafting in from the ceiling combines the with sweet grassy smells of the \
                  \flowerbed.")
            zeroBits
            [ StdLink West iHallwayEast 1 ]
            (3, 0, 0)
            InsideLitEnv
            (Just "Atrium")
            (M.fromList [ ("drink", [ drinkPoolHook      ])
                        , ("fill",  [ fillPoolHook       ])
                        , ("get",   [ getFlowerHook      ])
                        , ("look",  [ lookFlowerbedHook  ])
                        , ("smell", [ smellFlowerbedHook ]) ])
            [ pickRmAction     ]
            [ beeBuzzRmFunName ]))
  putRm iBasement
        []
        mempty
        (mkRm (RmTemplate "Basement"
            "You are in a dusty, unfinished basement room with round stucco walls. It smells of mold, and you spot \
            \several old cobwebs hanging from the ceiling.\n\
            \There is a single door to the north. Near the center of the room, a spiral staircase leads up. Next to the \
            \staircase lies an open manhole."
            (Just "There is a lone cricket chirping hesitantly.")
            (Just "The unmistakable scent of mildew fills your nostrils. This room really ought to be ventilated \
                  \better.")
            zeroBits
            [ StdLink    North     iWeightRm 1
            , StdLink    Up        iCentral  1
            , NonStdLink "manhole" iVoid     1 "% climbs into the manhole." "% climbs out of the manhole." ]
            (0, 0, -1)
            InsideLitEnv
            Nothing
            M.empty
            [] []))
  putRm iWeightRm
        [ i190Lb
        , i100Lb
        , i75Lb
        , i50Lb1
        , i50Lb2
        , i25Lb1
        , i25Lb2
        , i10Lb1
        , i10Lb2
        , i5Lb1
        , i5Lb2
        , i1Lb1
        , i1Lb2
        , i1Lb3
        , i1Lb4
        , i1Lb5
        , iSlab ]
        mempty
        (mkRm (RmTemplate "Weight room"
            "Feel free to lift some weights and work out!\n\
            \There is a narrow ladder leading up through an elliptical hole in the ceiling."
            Nothing
            (Just "There is a lingering scent of sweat and body odor.")
            zeroBits
            [ StdLink    South iBasement 1
            , NonStdLink "u"   iAttic    1 "% climbs up the ladder and into the hole in the ceiling."
                                           "% climbs up the ladder and out of the hole in the floor." ]
            (0, 1, -1)
            InsideLitEnv
            (Just "Weights")
            M.empty [] []))
  putRm iAttic
        [iCube1..iCube1 + 19]
        (Coins (1000, 1000, 1000))
        (mkRm (RmTemplate "Attic"
            "Though the confined attic is dusty, its cozy atmosphere creates an oddly welcoming space.\n\
            \There is an elliptical hole in the floor through which a narrow ladder leads down."
            (Just . thrice prd $ "Is that the soft squeak of a mouse you hear? Maybe you're just hearing things")
            (Just "The air here is dusty and a little stale, though not stifling.")
            zeroBits
            [ NonStdLink "d" iWeightRm 1 "% climbs down the ladder and into the hole in the floor."
                                         "% climbs down the ladder and out of the hole in the ceiling." ]
            (0, 1, 0)
            InsideLitEnv
            (Just "Attic")
            M.empty [] []))
  let voidListen = Just "It's eerily silent here."
      voidSmell  = Just "Strangely, the air here is utterly devoid of scent."
  putRm iVoid
        []
        mempty
        (mkRm (RmTemplate "The void"
            "You have stumbled into a vast empty space. You are floating.\n\
            \An open manhole hovers above you. You see a colorful round shape some distance off to the north, while to \
            \the south a door floats innocuously."
            voidListen
            voidSmell
            zeroBits
            [ StdLink    North     iTutEntrance    0
            , StdLink    South     iLoungeEntrance 0
            , NonStdLink "manhole" iBasement       0 "% climbs into the manhole." "% climbs out of the manhole." ]
            (0, 0, -2)
            InsideLitEnv
            Nothing
            M.empty [] []))
  putRm iTutEntrance
        []
        mempty
        (mkRm (RmTemplate "Portal"
            "Floating before you is a large round portal in which dazzling shapes and colors spin and dance. You feel \
            \a peculiar pulling sensation in your abdomen, as if the portal is attempting to draw you towards itself.\n\
            \A wooden sign is suspended above the portal."
            voidListen
            voidSmell
            zeroBits
            [ StdLink    South    iVoid       0
            , NonStdLink "portal" iTutWelcome 0 "% floats into the portal and promptly disappears."
                                                "% arrives in the tutorial." ]
            (0, 1, -2)
            InsideLitEnv
            (Just "Tutorial entrance")
            (M.fromList [ ("look", [ readLookSign_iTutEntranceHook, readLookPaperHook ])
                        , ("read", [ readLookSign_iTutEntranceHook, readLookPaperHook ]) ])
            [] []))
  putRm iLoungeEntrance
        []
        mempty
        (mkRm (RmTemplate "Floating door"
            "Floating before you is a polished wooden door surrounded by featureless white trimming. Hanging from a \
            \nail affixed to the door is a small sign reading, \"Admin Lounge.\""
            voidListen
            voidSmell
            zeroBits
            [ StdLink    North    iVoid   0
            , NonStdLink "lounge" iLounge 0 "% enters the lounge." "% enters the lounge." ]
            (0, -1, -2)
            InsideLitEnv
            Nothing
            (M.fromList [ ("look", [ readLookSign_iLoungeEntranceHook ])
                        , ("read", [ readLookSign_iLoungeEntranceHook ]) ])
            [] []))
  putRm iLounge
        []
        mempty
        (mkRm (RmTemplate "Admin lounge"
            "Welcome, admin! This is your private space where you can relax and take it easy."
            (Just . thrice prd $ "You can almost hear the crackle of a fire")
            (Just "There is a lingering scent of pipe tobacco in the air.")
            zeroBits
            [ NonStdLink "out" iLoungeEntrance 1 "% exits the lounge." "% exits the lounge." ]
            (0, -2, -2)
            SpecialEnv
            (Just "Lounge")
            M.empty [] []))
  putRm iInside
        []
        mempty
        (mkRm (RmTemplate "Inside"
            "This room is inside."
            Nothing
            Nothing
            zeroBits
            [ StdLink South iOutside 5 ]
            (-2, 1, -2)
            InsideUnlitEnv
            (Just "Inside")
            M.empty [] []))
  putRm iOutside
        []
        mempty
        (mkRm (RmTemplate "Outside"
            "This room is outside."
            Nothing
            Nothing
            zeroBits
            [ StdLink North iInside 5
            , StdLink South iShop   5 ]
            (-2, 0, -2)
            OutsideEnv
            (Just "Outside")
            M.empty [] []))
  putRm iShop
        []
        mempty
        (mkRm (RmTemplate "Shop"
            "This room is a shop."
            Nothing
            Nothing
            zeroBits
            [ StdLink North iOutside 5
            , StdLink South iSpecial 5 ]
            (-2, -1, -2)
            ShopEnv
            (Just "Shop")
            M.empty [] []))
  putRm iSpecial
        []
        mempty
        (mkRm (RmTemplate "Special"
            "This is a special room."
            Nothing
            Nothing
            zeroBits
            [ StdLink North iShop  5
            , StdLink South iNoEnv 5 ]
            (-2, -2, -2)
            SpecialEnv
            (Just "Special")
            M.empty [] []))
  putRm iNoEnv
        []
        mempty
        (mkRm (RmTemplate "No environment"
            "This room doesn't have an environment.\n\
            \There is a cottage to the south."
            Nothing
            Nothing
            zeroBits
            [ StdLink    North iSpecial 5
            , NonStdLink "in"  iCottage 1 "% enters the cottage." "% enters the cottage." ]
            (-2, -3, -2)
            NoEnv
            (Just "No env")
            M.empty [] []))
  putRm iCottage
        []
        mempty
        (mkRm (RmTemplate "Cottage"
            "You are inside a quaint cottage."
            Nothing
            Nothing
            zeroBits
            [ NonStdLink "out" iNoEnv 1 "% exits the cottage." "% exits the cottage." ]
            (-2, -4, -2)
            InsideLitEnv
            (Just "Cottage")
            M.empty [] []))

  -- ==================================================
  -- Room teleport names:
  putRmTeleName iAtrium    "atrium"
  putRmTeleName iCentral   "central"
  putRmTeleName iClone     "clone"
  putRmTeleName iEmpty     "empty"
  putRmTeleName iInside    "test"
  putRmTeleName iLounge    "lounge"
  putRmTeleName iTrashDump "trash"
