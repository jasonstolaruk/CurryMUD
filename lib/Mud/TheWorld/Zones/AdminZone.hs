{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.TheWorld.Zones.AdminZone ( adminZoneHooks
                                    , adminZoneRmActionFuns
                                    , adminZoneRmFuns
                                    , createAdminZone
                                    , getFlowerHook
                                    , lookFlowerbedHook ) where

import Mud.Cmds.Msgs.Advice
import Mud.Cmds.Msgs.Sorry
import Mud.Cmds.Util.Pla
import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Calc
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.New
import Mud.Data.State.Util.Put
import Mud.Data.State.Util.Random
import Mud.Misc.LocPref
import Mud.TheWorld.Liqs
import Mud.TheWorld.Misc
import Mud.TheWorld.Zones.AdminZoneIds
import Mud.TheWorld.Zones.TutorialIds (iTutWelcome)
import Mud.Threads.Act
import Mud.TopLvlDefs.Vols
import Mud.TopLvlDefs.Weights
import Mud.Util.List
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Data.Vector.Unboxed as V (Vector, head)
import qualified Mud.Misc.Logging as L (logNotice)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (_1, _2, _3, _4)
import Control.Lens.Operators ((%~), (&), (.~), (<>~))
import Control.Monad (forM_)
import Data.Bits (setBit, zeroBits)
import Data.List ((\\), delete, foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Lazy as M (empty, fromList, singleton)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.TheWorld.Zones.AdminZone"


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.AdminZone"


-- ==================================================
-- Hooks:


adminZoneHooks :: [(HookName, HookFun)]
adminZoneHooks = [ (drinkPoolHookName,                 drinkPoolHookFun                )
                 , (getFlowerHookName,                 getFlowerHookFun                )
                 , (lookCeilingHookName,               lookCeilingHookFun              )
                 , (lookFlowerbedHookName,             lookFlowerbedHookFun            )
                 , (lookWallsHookName,                 lookWallsHookFun                )
                 , (readLookPaperHookName,             readLookPaperHookFun            )
                 , (readLookPosterHookName,            readLookPosterHookFun           )
                 , (readLookSign_iEmptyHookName,       readLookSign_iEmptyHookFun      )
                 , (readLookSign_iTutEntranceHookName, readLookSign_iTutEntranceHookFun)
                 , (smellFlowerbedHookName,            smellFlowerbedHookFun           ) ]


-----


drinkPoolHook :: Hook
drinkPoolHook = Hook drinkPoolHookName . pure $ "pool"


drinkPoolHookName :: HookName
drinkPoolHookName = "AdminZone_iAtrium_drinkPool"


drinkPoolHookFun :: HookFun
drinkPoolHookFun i _ _ a@(as, (ms, _, _, _), _)
  | fst (calcStomachAvailSize i ms) <= 0 = a & _2._2 <>~ pure sorryFull
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


getFlowerHook :: Hook
getFlowerHook = Hook getFlowerHookName [ "flower", "flowers" ]


getFlowerHookName :: HookName
getFlowerHookName = "AdminZone_iAtrium_getFlower"


getFlowerHookFun :: HookFun
getFlowerHookFun i Hook { .. } v a@(_, (ms, _, _, _), _) = if calcWeight i ms + flowerWeight > calcMaxEnc i ms
  then a & _2._2 .~ pure (sorryGetEnc <> rest)
  else a & _1    %~  (\\ triggers)
         & _2._2 <>~ pure msg
         & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                       in pure (serialize selfDesig <> " picks " <> rest, i `delete` desigIds selfDesig) )
         & _2._4 <>~ pure (bracketQuote hookName <> " picked flower")
         & _3    .~  pure (mkFlower i v)
  where
    msg  = "You pick " <> rest
    rest = "a flower from the flowerbed."


mkFlower :: Id -> V.Vector Int -> MudStack ()
mkFlower i v = getUnusedId <$> getState >>= \flowerId ->
    let e = Ent flowerId
                (Just "flower")
                "flower" ""
                desc
                (Just smell)
                zeroBits
        o = Obj flowerWeight
                flowerVol
                Nothing -- TODO: Flower taste.
                (setBit zeroBits . fromEnum $ IsBiodegradable)
                Nothing
    in newObj e o i
  where
    (desc, smell) = rndmIntToElem (V.head v) descs
    descs         = [ ( "It's a fragrant daffodil sporting a collar of white petals."
                      , "The powerful fragrance of the daffodil is nearly intoxicating." )
                    , ( "It's a hardy hibiscus with pink-tinged pedals surrounding a distinctly red center."
                      , "The beautiful hibiscus gives off a mildly sweet fragrance." )
                    , ( "This eye-popping chrysanthemum has a fiery-orange bloom composed of many tiny petals."
                      , "Though striking in appearance, the chrysanthemum is not particularly fragrant." )
                    , ( "This blue lily has six large, independent petals opening widely from its base."
                      , "Sure to attract a variety of pollinators, the lily is markedly fragrant." ) ]


-----


lookCeilingHook :: Hook
lookCeilingHook = Hook lookCeilingHookName [ "ceiling", "up" ]


lookCeilingHookName :: HookName
lookCeilingHookName = "AdminZone_iEmpty_lookCeiling"


lookCeilingHookFun :: HookFun
lookCeilingHookFun = mkGenericHookFun ceilingDesc "looks up at the ceiling." "looked at ceiling"
  where
    ceilingDesc = "The tall ceiling looks identical to the walls: plain and white. Even if there was a means of exit \
                  \up there, you can't imagine how you'd reach it..."


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
    wallsDesc = "You are enclosed by four smooth, dense walls, with no means of exit in sight."


-----


readLookPaperHook :: Hook
readLookPaperHook = Hook readLookPaperHookName ["paper"]


readLookPaperHookName :: HookName
readLookPaperHookName = "AdminZone_iTutEntrance_readLookPaper"


readLookPaperHookFun :: HookFun
readLookPaperHookFun i Hook { .. } (V.head -> r) a@(_, (ms, _, _, _), _) =
    a &    _1 %~  (\\ triggers)
      & _2._2 <>~ pure signDesc
      & _2._3 <>~ ( let selfDesig = mkStdDesig i ms DoCap
                    in pure ( serialize selfDesig <> " reads the piece of paper nailed to the sign."
                            , i `delete` desigIds selfDesig ) )
      & _2._4 <>~ pure (bracketQuote hookName <> " read paper")
  where
    signDesc = "Scrawled on the piece of paper is the following message:\n\"Your lucky number is " <> x <> ".\""
    x        = showText . rndmIntToPer $ r


-----


readLookPosterHook :: Hook
readLookPosterHook = Hook readLookPosterHookName ["poster"]


readLookPosterHookName :: HookName
readLookPosterHookName = "AdminZone_iCentral_readLookPoster"


readLookPosterHookFun :: HookFun
readLookPosterHookFun = mkGenericHookFun posterDesc "reads the poster on the wall." "read poster"
  where
    posterDesc =
        "The poster reads:\n\
        \WELCOME TO THE ADMIN ZONE - What is this place?\n\
        \Greetings, admin! (You're an admin, aren't you? If not, what are you doing here?) ...Anyway, let me tell you \
        \about this area. The Admin Zone was written incrementally during the early stages of CurryMUD development. \
        \Whenever I came up with new core functionality, I'd need a way to test it; often that meant tacking on a room \
        \to this zone - the only zone that existed at the time. There was never any unifying theme for the Admin Zone, \
        \just the notion that players would likely never step foot in it (hence the name). As such, it kind of has a \
        \makeshift funhouse vibe; in the very least, it's a restricted area where admins can hang out.\n\
        \-Jason"


-----


readLookSign_iEmptyHook :: Hook
readLookSign_iEmptyHook = Hook readLookSign_iEmptyHookName ["sign"]


readLookSign_iEmptyHookName :: HookName
readLookSign_iEmptyHookName = "AdminZone_iEmpty_readLookSign"


readLookSign_iEmptyHookFun :: HookFun
readLookSign_iEmptyHookFun = mkGenericHookFun signDesc "reads the sign on the wall." "read sign"
  where
    signDesc = "The following message has been painted on the sign in a tight, flowing script:\n\
               \\"Welcome to the empty room. You have been summoned here by a CurryMUD administrator. As there are no \
               \exits, you will need the assistance of an administrator when the time comes for you to leave. We hope \
               \you enjoy your stay!\""


-----

readLookSign_iTutEntranceHook :: Hook
readLookSign_iTutEntranceHook = Hook readLookSign_iTutEntranceHookName ["sign"]


readLookSign_iTutEntranceHookName :: HookName
readLookSign_iTutEntranceHookName = "AdminZone_iTutEntrance_readLookSign"


readLookSign_iTutEntranceHookFun :: HookFun
readLookSign_iTutEntranceHookFun = mkGenericHookFun signDesc "reads the sign floating above the portal." "read sign"
  where
    signDesc = "The sign reads, \"Tutorial this way. No re-entry!\"\n\
               \A small, square piece of paper has been nailed to the bottom-right corner of the sign."


-----



smellFlowerbedHook :: Hook
smellFlowerbedHook = Hook smellFlowerbedHookName [ "flowerbed", "flower", "flowers" ]


smellFlowerbedHookName :: HookName
smellFlowerbedHookName = "AdminZone_iAtrium_smellFlowerbed"


smellFlowerbedHookFun :: HookFun
smellFlowerbedHookFun = mkGenericHookFun smellDesc "smells the flowerbed." "smelled flowerbed"
  where
    smellDesc = "You are greeted by the gentle, organic scents of florets and soil."


-- ==================================================
-- Room action functions:


adminZoneRmActionFuns :: [(FunName, RmActionFun)]
adminZoneRmActionFuns = pure (pickRmActionFunName, pick)


-----


pickRmAction :: RmAction
pickRmAction = RmAction "pick" pickRmActionFunName


pickRmActionFunName :: FunName
pickRmActionFunName = "AdminZone_iAtrium_pick"


pick :: RmActionFun
pick p@AdviseNoArgs     = advise p [] advicePickNoArgs
pick p@(LowerNub' i as) = genericActionWithHooks p helper "pick"
  where
    helper v ms =
        let (inInvs, inEqs, inRms) = sortArgsInvEqRm InRm as
            sorrys                 = dropEmpties [ inInvs |!| sorryPickInInv, inEqs |!| sorryPickInEq ]
            h@Hook { .. }          = getFlowerHook
            inRms'                 = dropSynonyms triggers . dropPrefixesForHooks (pure h) $ inRms
            initAcc                = (inRms', (ms, [], [], []), [])
            (_, (ms', toSelfs, bs, logMsgs), _) | any (`elem` triggers) inRms' = getHookFun hookName ms i h v initAcc
                                                | otherwise                    = initAcc
            mkMsgForArg arg | arg `elem` triggers = head toSelfs
                            | otherwise           = sorryPickNotFlower arg
        in (ms', (sorrys ++ map mkMsgForArg inRms', bs, logMsgs, []))
pick p = patternMatchFail "pick" [ showText p ]


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
beeBuzzRmFun = mkRndmBcastRmFun iAtrium "iAtrium" beeBuzzRmFunName 25 60 beeBuzzMsg
  where
    beeBuzzMsg = "A plump bumblebee buzzes happily around the flowerbed."


-- ==================================================
-- Zone definition:


adminFlags :: Int
adminFlags = foldl' setBit zeroBits . map fromEnum $ [ IsAdmin
                                                     , IsNotFirstAdminMsg
                                                     , IsNotFirstMobSay
                                                     , IsTunedAdmin
                                                     , IsTunedQuestion ]


createAdminZone :: MudStack ()
createAdminZone = do
  logNotice "createAdminZone" "creating the admin zone."

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
         (Mob Male
              50 50 50 50 50
              100 100
              100 100
              100 100
              100 100
              [] Nothing
              0
              RHand
              allValues
              iLoggedOut
              M.empty Nothing Nothing Nothing Nothing)
         M.empty
         (M.singleton "Curry" True)
         (PC Human ["Curry"] ["Curry"])
         (Pla "" Nothing
              (setBit adminFlags . fromEnum $ IsIncognito)
              80 24
              [] [] Nothing
              []
              (Just iLounge))
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
         (Mob Male
              50 50 50 50 50
              100 100
              100 100
              100 100
              100 100
              [] Nothing
              0
              RHand
              allValues
              iLoggedOut
              M.empty Nothing Nothing Nothing Nothing)
         M.empty
         (M.singleton "Root" True)
         (PC Human ["Root"] ["Root"])
         (Pla "" Nothing
              adminFlags
              80 24
              [] [] Nothing
              []
              (Just iLounge))

  -- ==================================================
  -- Rooms:
  putRm iLoggedOut
        [ iRoot ]
        mempty
        (Rm "Logged out room"
            "PCs are placed here when their players log out."
            Nothing
            Nothing
            zeroBits
            []
            M.empty [] [] [])
  putRm iTrashDump
        []
        mempty
        (Rm "The trash dump"
            "Items deposited in magic trash bins end up here."
            Nothing
            Nothing
            zeroBits
            []
            M.empty [] [] [])
  putRm iWelcome
        []
        mempty
        (Rm "Welcome room"
            "Ad-hoc PCs created for new connections are placed here."
            Nothing
            Nothing
            zeroBits
            []
            M.empty [] [] [])
  putRm iCentral
        []
        mempty
        (Rm "Central control room"
            "Welcome to the heart of the machine! Sprawled about this dome-shaped, white room is a cluster of \
            \electronic displays and control panels, used by the admins to monitor and supervise the daily operations \
            \of CurryMUD.\n\
            \There is a large poster on the wall.\n\
            \A spiral staircase leads down, while a door opens to a hallway leading east. A trash bin sits adjascent \
            \to the spiral staircase."
            Nothing
            (Just "You vaguely detect the chemical scents of plastic and cleaning solutions.")
            zeroBits
            [ StdLink Down iBasement, StdLink East iHallwayWest ]
            (M.fromList [ ("look", [ lookTrashHook, readLookPosterHook ])
                        , ("put",  [ putTrashHook                      ])
                        , ("read", [ readLookPosterHook                ]) ])
            [ trashRmAction ]
            [ beepRmFunName ]
            [])
  putRm iHallwayWest
        []
        mempty
        (Rm "Hallway"
            "You are in a wide hallway leading east. A door to the west opens into the central control room."
            Nothing
            Nothing
            zeroBits
            [ StdLink West iCentral, StdLink East iHallwayEast ]
            M.empty [] [] [])
  putRm iHallwayEast
        []
        mempty
        (Rm "Hallway"
            "You are in a wide hallway leading west. To your east, the hallway opens up into an atrium."
            Nothing
            Nothing
            zeroBits
            [ StdLink West iHallwayWest, StdLink East iAtrium ]
            M.empty [] [] [])
  putRm iAtrium
        []
        mempty
        (Rm "The atrium"
            "The large, airy atrium is sparsely furnished so as to accentuate its open feel. The focal point of the \
            \atrium is a shallow pool positioned directly under a large opening in the ceiling, allowing the pool to \
            \freely collect rainwater. At each corner of the square pool, a marble column purposefully rises up to \
            \support the ceiling. Next to the pool is a raised flowerbed, surrounded by four unembellished stone \
            \benches.\n\
            \An opening in the west wall leads out into a hallway."
            Nothing
            (Just "The scent of fresh air wafting in from the ceiling combines the with sweet, grassy smells of the \
                  \flowerbed.")
            zeroBits
            [ StdLink West iHallwayEast ]
            (M.fromList [ ("drink", [ drinkPoolHook      ])
                        , ("get",   [ getFlowerHook      ])
                        , ("look",  [ lookFlowerbedHook  ])
                        , ("smell", [ smellFlowerbedHook ]) ])
            [ pickRmAction     ]
            [ beeBuzzRmFunName ]
            [])
  putRm iBasement
        []
        mempty
        (Rm "The basement"
            "This dusty, unfinished basement smells of mold.\n\
            \Eight doors are positioned about the round, stucco wall at even intervals. A spiral staircase leads up. \
            \Next to the staircase lies an open manhole."
            Nothing
            Nothing
            zeroBits
            [ StdLink North     iWeightRm
            , StdLink Northeast iObjCloset
            , StdLink East      iClothCloset
            , StdLink Southeast iCoinsCloset
            , StdLink South     iConCloset
            , StdLink Southwest iWpnCloset
            , StdLink West      iArmCloset
            , StdLink Northwest iMobCloset
            , StdLink Up        iCentral
            , NonStdLink "manhole" iVoid "% climbs into the manhole." "% climbs out of the manhole." ]
            M.empty [] [] [])
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
        (Rm "Weight closet"
            "This closet holds weights."
            Nothing
            Nothing
            zeroBits
            [ StdLink    South iBasement
            , NonStdLink "u"  iAttic "% climbs up the ladder and into the hole in the ceiling."
                                     "% climbs up the ladder and out of the hole in the floor." ]
            M.empty [] [] [])
  putRm iAttic
        [ iCube1 .. iCube1 + 19 ]
        mempty
        (Rm "The attic"
            "Though the confined attic is dusty, its cozy atmosphere creates an oddly welcoming space."
            Nothing
            Nothing
            zeroBits
            [ NonStdLink "d" iWeightRm "% climbs down the ladder and into the hole in the floor."
                                       "% climbs down the ladder and out of the hole in the ceiling." ]
            M.empty [] [] [])
  putRm iObjCloset
        [ iKewpie1, iKewpie2, iPaperSml, iParchment1, iParchment2, iParchment3, iParchment4, iParchment5 ]
        mempty
        (Rm "Object closet"
            "This closet holds objects."
            Nothing
            Nothing
            zeroBits
            [ StdLink Southwest iBasement ]
            M.empty [] [] [])
  putRm iClothCloset
        [ iChemise, iTunic, iApron, iTabard, iGreyCoat, iFrockCoat, iBreeches1, iBreeches2, iTrousers1, iTrousers2 ]
        mempty
        (Rm "Clothing closet"
            "This closet holds clothing."
            Nothing
            Nothing
            zeroBits
            [ StdLink West iBasement, StdLink Down iAccessoriesCloset ]
            M.empty [] [] [])
  putRm iAccessoriesCloset
        [ iEar1
        , iEar2
        , iEar3
        , iEar4
        , iEar5
        , iEar6
        , iEar7
        , iEar8
        , iNoseRing1
        , iNoseRing2
        , iNoseRing3
        , iNeck1
        , iNeck2
        , iNeck3
        , iNeck4
        , iBracelet1
        , iBracelet2
        , iBracelet3
        , iBracelet4
        , iBracelet5
        , iBracelet6
        , iBracelet7
        , iBracelet8
        , iRing1
        , iRing2
        , iRing3
        , iRing4
        , iRing5
        , iRing6
        , iRing7
        , iRing8
        , iRing9 ]
        mempty
        (Rm "Accessories closet"
            "This closet holds accessories."
            Nothing
            Nothing
            zeroBits
            [ StdLink Up iClothCloset ]
            M.empty [] [] [])
  putRm iCoinsCloset
        []
        (Coins (100, 100, 100))
        (Rm "Coin closet"
            "This closet holds coins."
            Nothing
            Nothing
            zeroBits
            [ StdLink Northwest iBasement ]
            M.empty [] [] [])
  let conIds    = [ iSack1, iSack2, iSackSml, iSackLrg, iBack1, iBack2, iBackSml, iBackLrg ]
  let vesselIds = [ iPotionFlask1    .. iPotionFlask1    + 9 ] ++
                  [ iPotionFlaskLrg1 .. iPotionFlaskLrg1 + 9 ] ++ [ iWaterskin
                                                                  , iWaterskinLrg
                                                                  , iJarSml
                                                                  , iJar
                                                                  , iJarLrg
                                                                  , iJugSml
                                                                  , iJug
                                                                  , iJugLrg
                                                                  , iBottleSml
                                                                  , iBottle
                                                                  , iBottleLrg ]
  putRm iConCloset
        (conIds ++ vesselIds)
        mempty
        (Rm "Container closet"
            "This closet holds containers."
            Nothing
            Nothing
            zeroBits
            [ StdLink North iBasement ]
            M.empty [] [] [])
  putRm iWpnCloset
        [ iSword1, iSword2, iLongSword, iClub, iKnife1, iKnife2 ]
        mempty
        (Rm "Weapon closet"
            "This closet holds weapons."
            Nothing
            Nothing
            zeroBits
            [ StdLink Northeast iBasement ]
            M.empty [] [] [])
  putRm iArmCloset
        [ iCap, iHelm, iSandals1, iSandals2, iBoots ]
        mempty
        (Rm "Armor closet"
            "This closet holds armor."
            Nothing
            Nothing
            zeroBits
            [ StdLink East iBasement ]
            M.empty [] [] [])
  putRm iMobCloset
        [ iRockCavy1, iRockCavy2, iPidge, iSkeleton ]
        mempty
        (Rm "Mob closet"
            "This closet holds mobs."
            Nothing
            Nothing
            zeroBits
            [ StdLink Southeast iBasement ]
            M.empty [] [] [])
  let voidSmell = Just "Strangely, the air here is utterly devoid of scent."
  putRm iVoid
        []
        mempty
        (Rm "The void"
            "You have stumbled into a vast, empty space. You are floating.\n\
            \An open manhole hovers above you. You see a colorful round shape some distance off to the north, while to \
            \the south a door floats innocuously."
            Nothing
            voidSmell
            zeroBits
            [ StdLink North iTutEntrance
            , StdLink South iLoungeEntrance
            , NonStdLink "manhole" iBasement "% climbs into the manhole." "% climbs out of the manhole." ]
            M.empty [] [] [])
  putRm iTutEntrance
        []
        mempty
        (Rm "The portal"
            "Floating before you is a large round portal in which dazzling shapes and colors spin and dance. You feel \
            \a peculiar pulling sensation in your abdomen, as if the portal is attempting to draw you towards itself.\n\
            \A wooden sign is suspended above the portal."
            Nothing
            voidSmell
            zeroBits
            [ StdLink South iVoid
            , NonStdLink "portal" iTutWelcome "% floats into the portal, and promptly disappears."
                                              "% arrives in the tutorial." ]
            (M.fromList [ ("look", [ readLookSign_iTutEntranceHook, readLookPaperHook ])
                        , ("read", [ readLookSign_iTutEntranceHook, readLookPaperHook ]) ])
            [] [] [])
  putRm iLoungeEntrance
        []
        mempty
        (Rm "The floating door"
            "Floating before you is a polished wooden door surrounded by featureless white trimming. Hanging from a \
            \nail affixed to the door is a small sign reading, \"Admin Lounge.\""
            Nothing
            voidSmell
            zeroBits
            [ StdLink North iVoid
            , NonStdLink "lounge" iLounge "% enters the lounge." "% enters the lounge." ]
            M.empty [] [] [])
  putRm iLounge
        []
        mempty
        (Rm "The admin lounge"
            "Welcome, admin! Have a seat by the fire and relax for awhile."
            Nothing
            Nothing
            zeroBits
            [ NonStdLink "out" iLoungeEntrance "% exits the lounge." "% exits the lounge." ]
            M.empty [] [] [])
  putRm iEmpty
        []
        mempty
        (Rm "The empty room"
            "This small room is strikingly barren. There doesn't even seem to be a door on any of its white walls, \
            \though you can't miss the small wooden sign affixed to the north wall."
            Nothing
            (Just "On account of there being no ventilation to speak of, the air here is markedly stale and stuffy.")
            zeroBits
            []
            (M.fromList [ ("look", [ readLookSign_iEmptyHook, lookWallsHook, lookCeilingHook ])
                        , ("read", [ readLookSign_iEmptyHook                                 ]) ])
            [] [] [])

  -- ==================================================
  -- Room teleport names:
  putRmTeleName iAtrium    "atrium"
  putRmTeleName iCentral   "central"
  putRmTeleName iTrashDump "dump"
  putRmTeleName iEmpty     "empty"
  putRmTeleName iLounge    "lounge"

  -- ==================================================
  -- Objects:
  let kewpieDesc = "The kewpie doll is disgustingly cute."
  putObj iKewpie1
         (Ent iKewpie1
              (Just "doll")
              "kewpie doll" ""
              kewpieDesc
              Nothing
              zeroBits)
         (Obj dollWeight dollVol Nothing zeroBits Nothing)
  putObj iKewpie2
         (Ent iKewpie2
              (Just "doll")
              "kewpie doll" ""
              kewpieDesc
              Nothing
              zeroBits)
         (Obj dollWeight dollVol Nothing zeroBits Nothing)
  let weightDesc = "It's a hunk of metal cut into a rounded shape."
  putObj i190Lb
         (Ent i190Lb
              (Just "weight")
              "190 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 19000 66995 Nothing zeroBits Nothing)
  putObj i100Lb
         (Ent i100Lb
              (Just "weight")
              "100 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 10000 35260 Nothing zeroBits Nothing)
  putObj i75Lb
         (Ent i75Lb
              (Just "weight")
              "75 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 7500 26445 Nothing zeroBits Nothing)
  putObj i50Lb1
         (Ent i50Lb1
              (Just "weight")
              "50 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 5000 17630 Nothing zeroBits Nothing)
  putObj i50Lb2
         (Ent i50Lb2
              (Just "weight")
              "50 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 5000 17630 Nothing zeroBits Nothing)
  putObj i25Lb1
         (Ent i25Lb1
              (Just "weight")
              "25 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 2500 8815 Nothing zeroBits Nothing)
  putObj i25Lb2
         (Ent i25Lb2
              (Just "weight")
              "25 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 2500 8815 Nothing zeroBits Nothing)
  putObj i10Lb1
         (Ent i10Lb1
              (Just "weight")
              "10 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 1000 3525 Nothing zeroBits Nothing)
  putObj i10Lb2
         (Ent i10Lb2
              (Just "weight")
              "10 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 1000 3525 Nothing zeroBits Nothing)
  putObj i5Lb1
         (Ent i5Lb1
              (Just "weight")
              "5 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 500 1760 Nothing zeroBits Nothing)
  putObj i5Lb2
         (Ent i5Lb2
              (Just "weight")
              "5 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 500 1760 Nothing zeroBits Nothing)
  putObj i1Lb1
         (Ent i1Lb1
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 100 350 Nothing zeroBits Nothing)
  putObj i1Lb2
         (Ent i1Lb2
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 100 350 Nothing zeroBits Nothing)
  putObj i1Lb3
         (Ent i1Lb3
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 100 350 Nothing zeroBits Nothing)
  putObj i1Lb4
         (Ent i1Lb4
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 100 350 Nothing zeroBits Nothing)
  putObj i1Lb5
         (Ent i1Lb5
              (Just "weight")
              "1 lb weight" ""
              weightDesc
              Nothing
              zeroBits)
         (Obj 100 350 Nothing zeroBits Nothing)
  putObj iSlab
         (Ent iSlab
              (Just "slab")
              "large slab of rock" "large slabs of rock"
              "It's a hunk of grey rock cut into a rectangular block."
              Nothing
              zeroBits)
         (Obj 19300 67000 Nothing zeroBits Nothing)
  forM_ [ iCube1 .. iCube1 + 19 ] $ \i ->
      putObj i
             (Ent i
                  (Just "cube")
                  "cube" ""
                  "The solid, white cube measures 6\" x 6\" x 6\"."
                  Nothing
                  zeroBits)
             (Obj cubeWeight cubeVol Nothing zeroBits Nothing)

  -- ==================================================
  -- Writables:
  putWritable iPaperSml
              (Ent iPaperSml
                   (Just "paper")
                   "small piece of paper" "small pieces of paper"
                   "It's a rectangular piece of plain white paper."
                   Nothing
                   zeroBits)
              (Obj paperWeight paperVol Nothing zeroBits Nothing)
              (Writable (Just ( "CurryMud - A Multi-User Dungeon by Jason Stolaruk.\n\
                                \Copyright (c) 2013-2016, Jason Stolaruk and Detroit Labs LLC.\n\
                                \Version 0.1.0.0.\n\
                                \CurryMUD@gmail.com\n\
                                \CurryMUD is comprised of an original codebase written entirely in the Haskell \
                                \programming language."
                              , CommonLang ))
                        Nothing)
  let putParchment i = putWritable i
                                   (Ent i
                                        (Just "parchment")
                                        "piece of parchment" "pieces of parchment"
                                        "It's an everyday piece of parchment, made from processed animal skin."
                                        Nothing
                                        zeroBits)
                                   (Obj paperWeight paperVol Nothing zeroBits Nothing)
  putParchment iParchment1 (Writable Nothing Nothing)
  putParchment iParchment2 (Writable (Just ("You've lost it! You'll never get out of this maze...", CommonLang))
                                     Nothing)
  putParchment iParchment3 (Writable (Just ("Whatever you do, take care of your shoes.", DwarfLang))
                                     Nothing)
  putParchment iParchment4 (Writable (Just ("An asteroid crashed and nothing burned. It made me wonder.", CommonLang))
                                     (Just "Zaa"))
  putParchment iParchment5 (Writable (Just ("Toss away stuff you don't need in the end, but keep what's important and \
                                            \know who's your friend.", ElfLang))
                                           (Just "Zaa"))

  -- ==================================================
  -- Clothing:
  let earringDesc = "It's a small, but tasteful, nondescript hoop."
  putCloth iEar1
           (Ent iEar1
                (Just "earring")
                "azure earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar2
           (Ent iEar2
                (Just "earring")
                "crimson earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar3
           (Ent iEar3
                (Just "earring")
                "sea green earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar4
           (Ent iEar4
                (Just "earring")
                "onyx earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar5
           (Ent iEar5
                (Just "earring")
                "azure earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar6
           (Ent iEar6
                (Just "earring")
                "crimson earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar7
           (Ent iEar7
                (Just "earring")
                "sea green earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  putCloth iEar8
           (Ent iEar8
                (Just "earring")
                "onyx earring" ""
                earringDesc
                Nothing
                zeroBits)
           (Obj earWeight earVol Nothing zeroBits Nothing)
           Earring
  let noseRingDesc = "It's a plain copper stud, intended to be worn on the nose."
  putCloth iNoseRing1
           (Ent iNoseRing1
                (Just "nose")
                "nose ring" ""
                noseRingDesc
                Nothing
                zeroBits)
           (Obj noseWeight noseVol Nothing zeroBits Nothing)
           NoseRing
  putCloth iNoseRing2
           (Ent iNoseRing2
                (Just "nose")
                "nose ring" ""
                noseRingDesc
                Nothing
                zeroBits)
           (Obj noseWeight noseVol Nothing zeroBits Nothing)
           NoseRing
  putCloth iNoseRing3
           (Ent iNoseRing3
                (Just "nose")
                "nose ring" ""
                noseRingDesc
                Nothing
                zeroBits)
           (Obj noseWeight noseVol Nothing zeroBits Nothing)
           NoseRing
  let mkNecklaceDesc x = "It's a simple " <> x <> " chain."
  putCloth iNeck1
           (Ent iNeck1
                (Just "necklace")
                "bronze necklace" ""
                (mkNecklaceDesc "bronze")
                Nothing
                zeroBits)
           (Obj neckWeight neckVol Nothing zeroBits Nothing)
           Necklace
  putCloth iNeck2
           (Ent iNeck2
                (Just "necklace")
                "silver necklace" ""
                (mkNecklaceDesc "silver")
                Nothing
                zeroBits)
           (Obj neckWeight neckVol Nothing zeroBits Nothing)
           Necklace
  putCloth iNeck3
           (Ent iNeck3
                (Just "necklace")
                "gold necklace" ""
                (mkNecklaceDesc "gold")
                Nothing
                zeroBits)
           (Obj neckWeight neckVol Nothing zeroBits Nothing)
           Necklace
  putCloth iNeck4
           (Ent iNeck4
                (Just "necklace")
                "platinum necklace" ""
                (mkNecklaceDesc "platinum")
                Nothing
                zeroBits)
           (Obj neckWeight neckVol Nothing zeroBits Nothing)
           Necklace
  let charmBraceletDesc = "The bracelet is adorned with a variety of quaint charms in the shape of musical \
                          \instruments, fashioned out of pewter."
  putCloth iBracelet1
           (Ent iBracelet1
                (Just "bracelet")
                "charm bracelet" ""
                charmBraceletDesc
                Nothing
                zeroBits)
           (Obj 10 braceletVol Nothing zeroBits Nothing)
           Bracelet
  let bangleBraceletDesc = "The bangle bracelet is made of smooth polished wood, stained an earthy shade of brown, and \
                           \about half an inch wide."
  putCloth iBracelet2
           (Ent iBracelet2
                (Just "bracelet")
                "wooden bangle bracelet" ""
                bangleBraceletDesc
                Nothing
                zeroBits)
           (Obj 1 braceletVol Nothing zeroBits Nothing)
           Bracelet
  let beadedBraceletDesc = "This classic bracelet consist of small, spherical wooden beads, alternating black and \
                           \white in color."
  putCloth iBracelet3
           (Ent iBracelet3
                (Just "bracelet")
                "beaded bracelet" ""
                beadedBraceletDesc
                Nothing
                zeroBits)
           (Obj 2 braceletVol Nothing zeroBits Nothing)
           Bracelet
  let pearlBraceletDesc = "Lustrous white pearls are strung together to make an eye-catching, fashionable accessory."
  putCloth iBracelet4
           (Ent iBracelet4
                (Just "bracelet")
                "pearl bracelet" ""
                pearlBraceletDesc
                Nothing
                zeroBits)
           (Obj 4 braceletVol Nothing zeroBits Nothing)
           Bracelet
  putCloth iBracelet5
           (Ent iBracelet5
                (Just "bracelet")
                "charm bracelet" ""
                charmBraceletDesc
                Nothing
                zeroBits)
           (Obj 10 braceletVol Nothing zeroBits Nothing)
           Bracelet
  putCloth iBracelet6
           (Ent iBracelet6
                (Just "bracelet")
                "wooden bangle bracelet" ""
                bangleBraceletDesc
                Nothing
                zeroBits)
           (Obj 1 braceletVol Nothing zeroBits Nothing)
           Bracelet
  putCloth iBracelet7
           (Ent iBracelet7
                (Just "bracelet")
                "beaded bracelet" ""
                beadedBraceletDesc
                Nothing
                zeroBits)
           (Obj 2 braceletVol Nothing zeroBits Nothing)
           Bracelet
  putCloth iBracelet8
           (Ent iBracelet8
                (Just "bracelet")
                "pearl bracelet" ""
                pearlBraceletDesc
                Nothing
                zeroBits)
           (Obj 4 braceletVol Nothing zeroBits Nothing)
           Bracelet
  let mkRingDesc x = "It's a simple copper band prominently featuring a beautiful " <> x <> " stone."
  putCloth iRing1
           (Ent iRing1
                (Just "ring")
                "garnet ring" ""
                (mkRingDesc "garnet")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing2
           (Ent iRing2
                (Just "ring")
                "amethyst ring" ""
                (mkRingDesc "amethyst")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing3
           (Ent iRing3
                (Just "ring")
                "aquamarine ring" ""
                (mkRingDesc "aquamarine")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing4
           (Ent iRing4
                (Just "ring")
                "diamond ring" ""
                (mkRingDesc "diamond")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing5
           (Ent iRing5
                (Just "ring")
                "garnet ring" ""
                (mkRingDesc "garnet")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing6
           (Ent iRing6
                (Just "ring")
                "amethyst ring" ""
                (mkRingDesc "amethyst")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing7
           (Ent iRing7
                (Just "ring")
                "aquamarine ring" ""
                (mkRingDesc "aquamarine")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing8
           (Ent iRing8
                (Just "ring")
                "diamond ring" ""
                (mkRingDesc "diamond")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iRing9
           (Ent iRing9
                (Just "ring")
                "emerald ring" ""
                (mkRingDesc "emerald")
                Nothing
                zeroBits)
           (Obj ringWeight ringVol Nothing zeroBits Nothing)
           Ring
  putCloth iChemise
           (Ent iChemise
                (Just "chemise")
                "fine white chemise" ""
                "This voluminous frock, worn on the upper body, is fashioned out of thin, smooth linen. It hangs just \
                \below the waist, while its loose-cut, wide sleeves are elbow length."
                Nothing
                zeroBits)
           (Obj shirtWeight shirtVol Nothing zeroBits Nothing)
           Shirt
  putCloth iTunic
           (Ent iTunic
                (Just "tunic")
                "cobalt blue wool tunic" ""
                "This heavy wool tunic is waist length and short-sleeved. Decorative white embroidery along the neck, \
                \sleeves, and waist adds an eye-catching touch."
                Nothing
                zeroBits)
           (Obj tunicHeavyWeight tunicHeavyVol Nothing zeroBits Nothing)
           Shirt
  putCloth iApron
           (Ent iApron
                (Just "apron")
                "heavy brown apron" ""
                "This sturdy padded utility apron provides adequate protection while its wearer labors and toils."
                Nothing
                zeroBits)
           (Obj apronHeavyWeight apronHeavyVol Nothing zeroBits Nothing)
           Smock
  putCloth iTabard
           (Ent iTabard
                (Just "tabard")
                "sleeveless blue tabard" ""
                "This sleeveless overgarment is open at both sides and extends down to the thigh. Dyed a deep shade of \
                \blue, a contrasting bright orange trim adds a distinct accent along the hems. There is a short collar \
                \around the neck, complete with a small decorative yellow bowtie."
                Nothing
                zeroBits)
           (Obj tabardWeight tabardVol Nothing zeroBits Nothing)
           Smock
  putCloth iGreyCoat
           (Ent iGreyCoat
                (Just "coat")
                "mouse-grey coat" ""
                "Sure to keep its wearer warm in all but the coldest of weather, this heavy, long-sleeved coat reaches \
                \the knees, and features a tall collar followed by ten large silver buttons along its length."
                Nothing
                zeroBits)
           (Obj coatHeavyWeight coatHeavyVol Nothing zeroBits Nothing)
           Coat
  putCloth iFrockCoat
           (Ent iFrockCoat
                (Just "coat")
                "woman's red frock coat" ""
                "This fashionable long-sleeved coat is made of soft, bright-red fabric decorated with a fine, rich \
                \floral brochade. Six black buttons from the collar down the chest, when fastened, make this a \
                \particularly figure-flattering garment."
                Nothing
                zeroBits)
           (Obj coatWeight coatVol Nothing zeroBits Nothing)
           Coat
  let breechesDesc = "These thin, tight-fitting breeches extend just past the knees, where short drawstrings allow \
                     \them to be neatly secured."
  putCloth iBreeches1
           (Ent iBreeches1
                (Just "breeches")
                "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches"
                breechesDesc
                Nothing
                zeroBits)
           (Obj trousersWeight trousersVol Nothing zeroBits Nothing)
           Trousers
  putCloth iBreeches2
           (Ent iBreeches2
                (Just "breeches")
                "pair of knee-length yellow breeches" "pairs of knee-length yellow breeches"
                breechesDesc
                Nothing
                zeroBits)
           (Obj trousersWeight trousersVol Nothing zeroBits Nothing)
           Trousers
  let trousersDesc = "These wool trousers are loose-fitting so as to grant uninhibited movement. A rugged hemp \
                     \drawstring allows them to be snugly tightened at the waist."
  putCloth iTrousers1
           (Ent iTrousers1
                (Just "trousers")
                "pair of baggy beige trousers" "pairs of baggy beige trousers"
                trousersDesc
                Nothing
                zeroBits)
           (Obj trousersBaggyWeight trousersBaggyVol Nothing zeroBits Nothing)
           Trousers
  putCloth iTrousers2
           (Ent iTrousers2
                (Just "trousers")
                "pair of baggy beige trousers" "pairs of baggy beige trousers"
                trousersDesc
                Nothing
                zeroBits)
           (Obj trousersBaggyWeight trousersBaggyVol Nothing zeroBits Nothing)
           Trousers

  -- ==================================================
  -- Containers:
  let mkClothSackDesc x = "It's a typical cloth sack, perfect for holding your treasure. It's " <> x <> "."
  putCon iSack1
         (Ent iSack1
              (Just "sack")
              "cloth sack" ""
              (mkClothSackDesc "red")
              Nothing
              zeroBits)
         (Obj sackWeight sackVol Nothing zeroBits Nothing)
         []
         mempty
         Nothing
         (Con False sackCap)
  putCon iSack2
         (Ent iSack2
              (Just "sack")
              "cloth sack" ""
              (mkClothSackDesc "blue")
              Nothing
              zeroBits)
         (Obj sackWeight sackVol Nothing zeroBits Nothing)
         []
         mempty
         Nothing
         (Con False sackCap)
  let mkWovenSackDesc x = "The durable sack is made from a coarse, woven fabric, dyed " <> x <> " so as to give it \
                          \some flair."
  putCon iSackSml
         (Ent iSackSml
              (Just "sack")
              "small sack" ""
              (mkWovenSackDesc "light blue")
              Nothing
              zeroBits)
         (Obj sackSmlWeight sackSmlVol Nothing zeroBits Nothing)
         []
         mempty
         Nothing
         (Con False sackSmlCap)
  putCon iSackLrg
         (Ent iSackLrg
              (Just "sack")
              "large sack" ""
              (mkWovenSackDesc "red")
              Nothing
              zeroBits)
         (Obj sackLrgWeight sackLrgVol Nothing zeroBits Nothing)
         []
         mempty
         Nothing
         (Con False sackLrgCap)
  let backDesc = "The sturdy backpack is made of leather."
  putCon iBack1
         (Ent iBack1
              (Just "back")
              "backpack" ""
              backDesc
              Nothing
              zeroBits)
         (Obj backWeight backVol Nothing zeroBits Nothing)
         []
         mempty
         (Just Backpack)
         (Con True backCap)
  putCon iBack2
         (Ent iBack2
              (Just "back")
              "backpack" ""
              backDesc
              Nothing
              zeroBits)
         (Obj backWeight backVol Nothing zeroBits Nothing)
         []
         mempty
         (Just Backpack)
         (Con True backCap)
  putCon iBackSml
         (Ent iBackSml
              (Just "back")
              "small backpack" ""
              backDesc
              Nothing
              zeroBits)
         (Obj backSmlWeight backSmlVol Nothing zeroBits Nothing)
         []
         mempty
         (Just Backpack)
         (Con True backSmlCap)
  putCon iBackLrg
         (Ent iBackLrg
              (Just "back")
              "large backpack" ""
              backDesc
              Nothing
              zeroBits)
         (Obj backLrgWeight backLrgVol Nothing zeroBits Nothing)
         []
         mempty
         (Just Backpack)
         (Con True backLrgCap)

  -- ==================================================
  -- Vessels:
  let flaskIds    = [ iPotionFlask1    + i | i <- [0..9] ]
      flaskLrgIds = [ iPotionFlaskLrg1 + i | i <- [0..9] ]
      flaskConts  = repeat Nothing
  forM_ (zip flaskIds flaskConts) $ \(i, mc) ->
      putVessel i
                (Ent i
                     (Just "flask")
                     "potion flask" ""
                     "This small, glass flask complete with cork stopper is the ideal vessel for potion storage and \
                     \transportation."
                     Nothing
                     zeroBits)
                (Obj potionFlaskWeight potionFlaskVol Nothing zeroBits Nothing)
                mc
  forM_ (zip flaskLrgIds flaskConts) $ \(i, mc) ->
      putVessel i
                (Ent i
                     (Just "flask")
                     "large potion flask" ""
                     "This glass flask complete with cork stopper is the ideal vessel for potion storage and \
                     \transportation."
                     Nothing
                     zeroBits)
                (Obj potionFlaskLrgWeight potionFlaskLrgVol Nothing zeroBits Nothing)
                mc
  let waterskinDesc = "The rugged waterskin, crafted from the bladder of a bovine animal, is an indispensable piece of \
                      \equipment when it comes to travel and, often, everyday life."
  putVessel iWaterskin
            (Ent iWaterskin
                 (Just "waterskin")
                 "waterskin" ""
                 waterskinDesc
                 Nothing
                 zeroBits)
            (Obj waterskinWeight waterskinVol Nothing zeroBits Nothing)
            (Just (waterLiq, maxBound))
  putVessel iWaterskinLrg
            (Ent iWaterskinLrg
                 (Just "waterskin")
                 "large waterskin" ""
                 (waterskinDesc <> " This waterskin is particularly large, making it suitable for long journeys.")
                 Nothing
                 zeroBits)
            (Obj waterskinLrgWeight waterskinLrgVol Nothing zeroBits Nothing)
            (Just (waterLiq, maxBound))
  putVessel iJarSml
            (Ent iJarSml
                 (Just "jar")
                 "small jar" ""
                 "This versatile, small glass jar comes affixed with an airtight lid."
                 Nothing
                 zeroBits)
            (Obj jarSmlWeight jarSmlVol Nothing zeroBits Nothing)
            (Just (potHealingLiq, maxBound))
  putVessel iJar
            (Ent iJar
                 (Just "jar")
                 "jar" ""
                 "This versatile glass jar comes affixed with an airtight lid."
                 Nothing
                 zeroBits)
            (Obj jarWeight jarVol Nothing zeroBits Nothing)
            (Just (potInstantHealingLiq, maxBound))
  putVessel iJarLrg
            (Ent iJarLrg
                 (Just "jar")
                 "large jar" ""
                 "This versatile, large glass jar comes affixed with an airtight lid."
                 Nothing
                 zeroBits)
            (Obj jarLrgWeight jarLrgVol Nothing zeroBits Nothing)
            Nothing
  let jugDesc = "While capable of containing a large amount of liquid, this corked, ceramic jug is rather cumbersome."
  putVessel iJugSml
            (Ent iJugSml
                 (Just "jug")
                 "small jug" ""
                 jugDesc
                 Nothing
                 zeroBits)
            (Obj jugSmlWeight jugSmlVol Nothing zeroBits Nothing)
            (Just (potStLiq, maxBound))
  putVessel iJug
            (Ent iJug
                 (Just "jug")
                 "jug" ""
                 jugDesc
                 Nothing
                 zeroBits)
            (Obj jugWeight jugVol Nothing zeroBits Nothing)
            (Just (potInstantStLiq, maxBound))
  putVessel iJugLrg
            (Ent iJugLrg
                 (Just "jug")
                 "large jug" ""
                 jugDesc
                 Nothing
                 zeroBits)
            (Obj jugLrgWeight jugLrgVol Nothing zeroBits Nothing)
            Nothing
  let mkBottleDesc a b = T.concat [ "This "
                                  , a
                                  , "earthenware bottle is designed to be as portable and practical as possible. A \
                                    \glaze of "
                                  , b
                                  , " hues gives the vessel a glossy finish, and makes it impermeable." ]
  putVessel iBottleSml
            (Ent iBottleSml
                 (Just "bottle")
                 "small bottle" ""
                 (mkBottleDesc "small, " "light brown")
                 Nothing
                 zeroBits)
            (Obj bottleSmlWeight bottleSmlVol Nothing zeroBits Nothing)
            (Just (potTinnitusLiq, maxBound))
  putVessel iBottle
            (Ent iBottle
                 (Just "bottle")
                 "bottle" ""
                 (mkBottleDesc "" "mixed azure")
                 Nothing
                 zeroBits)
            (Obj bottleWeight bottleVol Nothing zeroBits Nothing)
            (Just (potInstantTinnitusLiq, maxBound))
  putVessel iBottleLrg
            (Ent iBottleLrg
                 (Just "bottle")
                 "large bottle" ""
                 (mkBottleDesc "large, " "rusty orange")
                 Nothing
                 zeroBits)
            (Obj bottleLrgWeight bottleLrgVol Nothing zeroBits Nothing)
            Nothing

  -- ==================================================
  -- Weapons:
  let swordDesc = "It's a sword; short but still sharp!"
  putWpn iSword1
         (Ent iSword1
              (Just "sword")
              "short sword" ""
              swordDesc
              Nothing
              zeroBits)
         (Obj swordWeight swordVol Nothing zeroBits Nothing)
         (Wpn OneHanded 1 10)
  putWpn iSword2
         (Ent iSword2
              (Just "sword")
              "short sword" ""
              swordDesc
              Nothing
              zeroBits)
         (Obj swordWeight swordVol Nothing zeroBits Nothing)
         (Wpn OneHanded 1 10)
  putWpn iLongSword
         (Ent iLongSword
              (Just "sword")
              "two-handed long sword" ""
              "With the right technique, this bulky sword could do a great deal of damage."
              Nothing
              zeroBits)
         (Obj swordLongWeight swordLongVol Nothing zeroBits Nothing)
         (Wpn TwoHanded 1 10)
  putWpn iClub
         (Ent iClub
              (Just "club")
              "wooden club" ""
              "It's a crude wooden club, the type a neanderthal might use to great effect."
              Nothing
              zeroBits)
         (Obj clubWeight clubVol Nothing zeroBits Nothing)
         (Wpn OneHanded 1 10)
  let knifeDesc = "This small knife could be useful in a pinch."
  putWpn iKnife1
         (Ent iKnife1
              (Just "knife")
              "utility knife" "utility knives"
              knifeDesc
              Nothing
              zeroBits)
         (Obj knifeWeight knifeVol Nothing zeroBits Nothing)
         (Wpn OneHanded 1 10)
  putWpn iKnife2
         (Ent iKnife2
              (Just "knife")
              "utility knife" "utility knives"
              knifeDesc
              Nothing
              zeroBits)
         (Obj knifeWeight knifeVol Nothing zeroBits Nothing)
         (Wpn OneHanded 1 10)

  -- ==================================================
  -- Armor:
  putArm iCap
         (Ent iCap
              (Just "cap")
              "knit cap" ""
              "It's a simple knit cap, designed to keep your head warm in cold weather."
              Nothing
              zeroBits)
         (Obj knitCapWeight knitCapVol Nothing zeroBits Nothing)
         (Arm Head 1)
  putArm iHelm
         (Ent iHelm
              (Just "helmet")
              "leather helmet" ""
              "The functional leather helmet provides a comfortable fit."
              Nothing
              zeroBits)
         (Obj helmLeatherWeight helmLeatherVol Nothing zeroBits Nothing)
         (Arm Head 1)
  let sandalsDesc = "These humble leather sandals offer little in the way of fashion; they will, however, adequately \
                    \protect the soles of your feet."
  putArm iSandals1
         (Ent iSandals1
              (Just "sandals")
              "pair of leather sandals" "pairs of leather sandals"
              sandalsDesc
              Nothing
              zeroBits)
         (Obj sandalsWeight sandalsVol Nothing zeroBits Nothing)
         (Arm Feet 1)
  putArm iSandals2
         (Ent iSandals2
              (Just "sandals")
              "pair of leather sandals" "pairs of leather sandals"
              sandalsDesc
              Nothing
              zeroBits)
         (Obj sandalsWeight sandalsVol Nothing zeroBits Nothing)
         (Arm Feet 1)
  putArm iBoots
         (Ent iBoots
              (Just "boots")
              "pair of leather boots" "pairs of leather boots"
              "These rugged, sturdy boots make excellent footwear for traveling across a variety of terrain."
              Nothing
              zeroBits)
         (Obj bootsWeight bootsVol Nothing zeroBits Nothing)
         (Arm Feet 1)

  -- ==================================================
  -- Mobs:
  let rockCavyDesc = "It looks like a slightly oversized guinea pig with soft, grey fur. You imagine that the rock \
                     \cavy would prefer dry, rocky areas (with low, scrubby vegetation), close to stony mountains and \
                     \hills."
  putNpc iRockCavy1
         (Ent iRockCavy1
              (Just "rock")
              "rock cavy" "rock cavies"
              rockCavyDesc
              Nothing
              zeroBits)
         []
         mempty
         M.empty
         (Mob Male
              50 50 50 50 50
              10 10
              10 10
              10 10
              10 10
              [] Nothing
              10
              NoHand
              []
              iMobCloset
              M.empty Nothing Nothing Nothing Nothing)
  putNpc iRockCavy2
         (Ent iRockCavy2
              (Just "rock")
              "rock cavy" "rock cavies"
              rockCavyDesc
              Nothing
              zeroBits)
         []
         mempty
         M.empty
         (Mob Male
              50 50 50 50 50
              10 10
              10 10
              10 10
              10 10
              [] Nothing
              10
              NoHand
              []
              iMobCloset
              M.empty Nothing Nothing Nothing Nothing)
  putNpc iPidge
         (Ent iPidge
              (Just "pidge")
              "Pidge" ""
              "Pidge is a female halfling with walnut-colored skin and large, brown eyes. She wears her silver-white \
              \hair in shoulder-length pigtails. Her small, round face is positively adorable."
              Nothing
              zeroBits)
         []
         mempty
         (M.fromList [ (ShirtS,    iPeasant'sShirt )
                     , (SmockS,    iLeatherApron   )
                     , (TrousersS, iOveralls       )
                     , (FeetS,     iTraveler'sBoots) ])
         (Mob Female
              50 50 50 50 50
              100 100
              100 100
              100 100
              100 100
              [] Nothing
              0
              RHand
              [ HalflingLang ]
              iMobCloset
              M.empty Nothing Nothing Nothing Nothing)
  putCloth iPeasant'sShirt
           (Ent iPeasant'sShirt
                (Just "shirt")
                "white peasant's shirt" ""
                "This shirt, favored by skilled laborers and lowly bumpkins alike, represents the epitome of function \
                \over fashion."
                Nothing
                zeroBits)
           (Obj shirtWeight shirtVol Nothing zeroBits Nothing)
           Shirt
  putCloth iOveralls
           (Ent iOveralls
                (Just "overalls")
                "pair of many-pocketed brown overalls" "pairs of many-pocketed brown overalls"
                "These durable overalls are adorned with a multitude of little pockets."
                Nothing
                zeroBits)
           (Obj overallsWeight overallsVol Nothing zeroBits Nothing)
           Trousers
  putCloth iLeatherApron
           (Ent iLeatherApron
                (Just "apron")
                "leather apron" ""
                "This heavy apron, though bulky, is a must for those who undertake dirty and dangerous chores."
                Nothing
                zeroBits)
           (Obj apronHeavyWeight apronHeavyVol Nothing zeroBits Nothing)
           Smock
  putArm iTraveler'sBoots
         (Ent iTraveler'sBoots (Just "boots")
              "pair of jet-black traveler's boots" "pair of jet-black traveler's boots"
              "These well-crafted, thigh-high boots are rugged and durable."
              Nothing
              zeroBits)
         (Obj bootsWeight bootsVol Nothing zeroBits Nothing)
         (Arm Feet 1)
  putNpc iSkeleton
         (Ent iSkeleton
              (Just "skeleton")
              "undead skeleton" ""
              "This mindless, bipedal skeleton has been animated and tasked with doing its master's bidding."
              Nothing
              zeroBits)
         []
         mempty
         M.empty
         (Mob NoSex
              50 50 50 50 50
              10 10
              10 10
              10 10
              10 10
              [] Nothing
              10
              RHand
              []
              iMobCloset
              M.empty Nothing Nothing Nothing Nothing)
