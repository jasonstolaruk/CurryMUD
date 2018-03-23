{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Data.State.Util.Make ( EntTemplate(..)
                                , InvId
                                , MobTemplate(..)
                                , ObjTemplate(..)
                                , PlaTemplate(..)
                                , RmTemplate(..)
                                , VesselTemplate(..)
                                , holySymbolFactory
                                , mkEnt
                                , mkMob
                                , mkObj
                                , mkPla
                                , mkRm
                                , newArm
                                , newCloth
                                , newCon
                                , newCorpse
                                , newFood
                                , newHolySymbol
                                , newLight
                                , newNpc
                                , newObj
                                , newVessel
                                , newWpn
                                , newWritable ) where

import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Calc
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Threads.Biodegrader
import           Mud.Threads.CorpseDecomposer
import           Mud.Threads.Digester
import           Mud.Threads.Regen
import           Mud.TopLvlDefs.Seconds
import           Mud.Util.Misc
import           Mud.Util.Operators

import           Control.Arrow (second)
import           Control.Lens (_2, _3, at)
import           Control.Lens.Operators ((.~), (&), (%~))
import           Control.Monad (when)
import           Data.Bits (setBit, zeroBits)
import qualified Data.Map.Strict as M (empty)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)

type InvId = Id

-- ==================================================
-- Armor

createArm :: MudState -> EntTemplate -> ObjTemplate -> Arm -> (Id, MudState, Funs)
createArm ms et ot a = let tuple@(i, _, _) = createObj ms et ot
                       in tuple & _2.armTbl.ind i .~ a

newArm :: MudState -> EntTemplate -> ObjTemplate -> Arm -> InvId -> (Id, MudState, Funs)
newArm ms et ot a invId = let (i, typeTbl.ind i .~ ArmType -> ms', fs) = createArm ms et ot a
                          in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Clothing

createCloth :: MudState -> EntTemplate -> ObjTemplate -> Cloth -> (Id, MudState, Funs)
createCloth ms et ot c = let tuple@(i, _, _) = createObj ms et ot
                         in tuple & _2.clothTbl.ind i .~ c

newCloth :: MudState -> EntTemplate -> ObjTemplate -> Cloth -> InvId -> (Id, MudState, Funs)
newCloth ms et ot c invId = let (i, typeTbl.ind i .~ ClothType -> ms', fs) = createCloth ms et ot c
                            in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Container

createCon :: MudState -> EntTemplate -> ObjTemplate -> Con -> (Inv, Coins) -> Maybe Cloth -> (Id, MudState, Funs)
createCon ms et ot con (is, c) mc = let (i, ms', fs) = createObj ms et ot
                                        ms''         = upd ms' [ clothTbl.at  i .~ mc
                                                               , coinsTbl.ind i .~ c
                                                               , conTbl  .ind i .~ con ]
                                    in (i, ms'' & invTbl.ind i .~ sortInv ms'' is, fs)

newCon :: MudState
       -> EntTemplate
       -> ObjTemplate
       -> Con
       -> (Inv, Coins)
       -> Maybe Cloth
       -> InvId
       -> (Id, MudState, Funs)
newCon ms et ot con ic mc invId = let (i, typeTbl.ind i .~ ConType -> ms', fs) = createCon ms et ot con ic mc
                                  in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Corpse

createCorpse :: MudState -> EntTemplate -> ObjTemplate -> Con -> (Inv, Coins) -> Corpse -> Seconds -> (Id, MudState, Funs)
createCorpse ms et ot con ic c secs = let tuple@(i, _, _) = createCon ms et ot con ic Nothing
                                      in tuple & _2.corpseTbl.ind i .~ c
                                               & _3 <>+ startCorpseDecomp i (dup secs)

newCorpse :: MudState
          -> EntTemplate
          -> ObjTemplate
          -> Con
          -> (Inv, Coins)
          -> Corpse
          -> Seconds
          -> InvId
          -> (Id, MudState, Funs)
newCorpse ms et ot con ic c secs invId =
    let (i, typeTbl.ind i .~ CorpseType -> ms', fs) = createCorpse ms et ot con ic c secs
    in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Entity

data EntTemplate = EntTemplate { etName  :: Maybe Text
                               , etSing  :: Sing
                               , etPlur  :: Plur
                               , etDesc  :: Text
                               , etSmell :: Maybe Text
                               , etFlags :: Flags }

mkEnt :: Id -> EntTemplate -> Ent
mkEnt i EntTemplate { .. } = Ent { _entId    = i
                                 , _entName  = etName
                                 , _sing     = etSing
                                 , _plur     = etPlur
                                 , _entDesc  = etDesc
                                 , _entSmell = etSmell
                                 , _entFlags = etFlags }

createEnt :: MudState -> EntTemplate -> (Id, MudState)
createEnt ms et = let i = getUnusedId ms in (i, upd ms [ durationalEffectTbl.ind i .~ []
                                                       , entTbl             .ind i .~ mkEnt i et
                                                       , pausedEffectTbl    .ind i .~ [] ])

-- ==================================================
-- Food

createFood :: MudState -> EntTemplate -> ObjTemplate -> Food -> (Id, MudState, Funs)
createFood ms et ot f = let tuple@(i, _, _) = createObj ms et ot
                        in tuple & _2.foodTbl.ind i .~ f

newFood :: MudState -> EntTemplate -> ObjTemplate -> Food -> InvId -> (Id, MudState, Funs)
newFood ms et ot f invId = let (i, typeTbl.ind i .~ FoodType -> ms', fs) = createFood ms et ot f
                           in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Holy symbol

createHolySymbol :: MudState -> EntTemplate -> ObjTemplate -> HolySymbol -> (Id, MudState, Funs)
createHolySymbol ms et ot h = let tuple@(i, _, _) = createObj ms et ot
                              in tuple & _2.holySymbolTbl.ind i .~ h

newHolySymbol :: MudState -> EntTemplate -> ObjTemplate -> HolySymbol -> InvId -> (Id, MudState, Funs)
newHolySymbol ms et ot h invId =
    let (i, typeTbl.ind i .~ HolySymbolType -> ms', fs) = createHolySymbol ms et ot h
    in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

holySymbolFactory :: Id -> MudState -> Int -> GodName -> (MudState, Funs)
holySymbolFactory i ms n gn = let (gn', desc, w, v, h) = ((,,,,) <$> pp
                                                                 <*> mkHolySymbolDesc
                                                                 <*> mkHolySymbolWeight
                                                                 <*> mkHolySymbolVol
                                                                 <*> HolySymbol) gn
                                  et  = EntTemplate (Just "holy")
                                                    ("holy symbol of " <> gn') ("holy symbols of " <> gn')
                                                    desc
                                                    Nothing
                                                    zeroBits
                                  ot  = ObjTemplate w
                                                    v
                                                    Nothing Nothing Nothing
                                                    (setBit zeroBits . fromEnum $ IsBiodegradable)
                                  vt  = VesselTemplate Nothing . Just $ h
                                  helper 0 pair      = pair
                                  helper x (ms', fs) = let pair = dropFst $ if gn == Iminye
                                                                    then newVessel     ms' et ot vt i
                                                                    else newHolySymbol ms' et ot h  i
                                                       in helper (pred x) . second (++ fs) $ pair
                              in helper n (ms, [])

-- ==================================================
-- Light

createLight :: MudState -> EntTemplate -> ObjTemplate -> Light -> (Id, MudState, Funs)
createLight ms et ot l = let tuple@(i, _, _) = createObj ms et ot
                         in tuple & _2.lightTbl.ind i .~ l

newLight :: MudState -> EntTemplate -> ObjTemplate -> Light -> InvId -> (Id, MudState, Funs)
newLight ms et ot l invId = let (i, typeTbl.ind i .~ LightType -> ms', fs) = createLight ms et ot l
                            in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Mob

data MobTemplate = MobTemplate { mtSex              :: Sex
                               , mtSt               :: Int
                               , mtDx               :: Int
                               , mtHt               :: Int
                               , mtMa               :: Int
                               , mtPs               :: Int
                               , mtMaxHp            :: Int
                               , mtMaxMp            :: Int
                               , mtMaxPp            :: Int
                               , mtMaxFp            :: Int
                               , mtExp              :: Exp
                               , mtLvl              :: Lvl
                               , mtHand             :: Hand
                               , mtKnownLangs       :: [Lang]
                               , mtRmId             :: Id
                               , mtStance           :: Stance
                               , mtSize             :: Maybe MobSize
                               , mtCorpseWeight     :: Weight
                               , mtCorpseVol        :: Vol
                               , mtCorpseCapacity   :: Vol
                               , mtCorpseDecompSecs :: Seconds
                               , mtParty            :: Party }

mkMob :: MobTemplate -> Mob
mkMob MobTemplate { .. } = Mob { _sex              = mtSex
                               , _st               = mtSt
                               , _dx               = mtDx
                               , _ht               = mtHt
                               , _ma               = mtMa
                               , _ps               = mtPs
                               , _curHp            = mtMaxHp
                               , _maxHp            = mtMaxHp
                               , _curMp            = mtMaxMp
                               , _maxMp            = mtMaxMp
                               , _curPp            = mtMaxPp
                               , _maxPp            = mtMaxPp
                               , _curFp            = mtMaxFp
                               , _maxFp            = mtMaxFp
                               , _exp              = mtExp
                               , _lvl              = mtLvl
                               , _hand             = mtHand
                               , _knownLangs       = mtKnownLangs
                               , _rmId             = mtRmId
                               , _lastRmId         = mtRmId
                               , _mobRmDesc        = Nothing
                               , _tempDesc         = Nothing
                               , _stance           = mtStance
                               , _mobSize          = mtSize
                               , _corpseWeight     = mtCorpseWeight
                               , _corpseVol        = mtCorpseVol
                               , _corpseCapacity   = mtCorpseCapacity
                               , _corpseDecompSecs = mtCorpseDecompSecs
                               , _party            = mtParty
                               , _stomach          = []
                               , _digesterAsync    = Nothing
                               , _feelingMap       = M.empty
                               , _actMap           = M.empty
                               , _nowAttacking     = Nothing
                               , _nowEating        = Nothing
                               , _nowDrinking      = Nothing
                               , _regenQueue       = Nothing
                               , _interp           = Nothing }

createMob :: MudState -> EntTemplate -> (Inv, Coins) -> EqMap -> MobTemplate -> (Id, MudState)
createMob ms et (is, c) em mt = let (i, ms') = createEnt ms et
                                    ms''     = upd ms' [ coinsTbl.ind i .~ c
                                                       , eqTbl   .ind i .~ em
                                                       , mobTbl  .ind i .~ mkMob mt ]
                                in (i, ms'' & invTbl.ind i .~ sortInv ms'' is)

-- ==================================================
-- NPC

createNpc :: MudState  -- The caller is responsible for running "runNpcServerAsync" before "runDigesterAsync" and "runRegenAsync".
          -> EntTemplate
          -> (Inv, Coins)
          -> EqMap
          -> MobTemplate
          -> (Id, MudState, Funs)
createNpc ms et ic em mt = let (i, ms') = createMob ms et ic em mt
                           in (i, ms', map (i |&|) [ runDigesterAsync, runRegenAsync ])

newNpc :: MudState
       -> EntTemplate
       -> (Inv, Coins)
       -> EqMap
       -> MobTemplate
       -> InvId
       -> (Id, MudState, Funs)
newNpc ms et ic em mt invId =
    let (i, typeTbl.ind i .~ NpcType -> ms', fs) = createNpc ms et ic em mt
    in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Object

data ObjTemplate = ObjTemplate { otWeight :: Weight
                               , otVol    :: Vol
                               , otTaste  :: Maybe Text
                               , otVal    :: Val
                               , otWear   :: Wear
                               , otFlags  :: Flags }

mkObj :: ObjTemplate -> Obj
mkObj ObjTemplate { .. } = Obj { _objWeight      = otWeight
                               , _objVol         = otVol
                               , _objTaste       = otTaste
                               , _objVal         = otVal
                               , _objWear        = otWear
                               , _objFlags       = otFlags
                               , _objBiodegAsync = Nothing }

createObj :: MudState -> EntTemplate -> ObjTemplate -> (Id, MudState, Funs)
createObj ms et ot = let (i, ms') = createEnt ms et
                         o        = mkObj ot
                     in (i, ms' & objTbl.ind i .~ o, pure . when (isBiodegradable o) . runBiodegAsync $ i)

newObj :: MudState -> EntTemplate -> ObjTemplate -> InvId -> (Id, MudState, Funs)
newObj ms et ot invId = let (i, typeTbl.ind i .~ ObjType -> ms', fs) = createObj ms et ot
                        in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Player

data PlaTemplate = PlaTemplate { ptPlaFlags     :: Flags
                               , ptRetainedMsgs :: [Text]
                               , ptLogoutRmId   :: Id }

mkPla :: PlaTemplate -> Pla
mkPla PlaTemplate { .. } = Pla { _currHostName   = ""
                               , _connectTime    = Nothing
                               , _loginTime      = Nothing
                               , _disconnectTime = Nothing
                               , _plaFlags       = ptPlaFlags
                               , _columns        = 80
                               , _pageLines      = 24
                               , _peepers        = []
                               , _peeping        = []
                               , _possessing     = Nothing
                               , _retainedMsgs   = ptRetainedMsgs
                               , _logoutRmId     = Just ptLogoutRmId
                               , _bonusTime      = Nothing
                               , _spiritAsync    = Nothing }

-- ==================================================
-- Room

data RmTemplate = RmTemplate { rtName      :: Text
                             , rtDesc      :: Text
                             , rtListen    :: Maybe Text
                             , rtSmell     :: Maybe Text
                             , rtFlags     :: Flags
                             , rtLinks     :: [RmLink]
                             , rtCoords    :: RmCoords
                             , rtEnv       :: RmEnv
                             , rtLabel     :: RmLabel
                             , rtHookMap   :: HookMap
                             , rtActions   :: [RmAction]
                             , rtFunNames  :: [FunName] }

mkRm :: RmTemplate -> Rm
mkRm RmTemplate { .. } = Rm { _rmName      = rtName
                            , _rmDesc      = rtDesc
                            , _rmListen    = rtListen
                            , _rmSmell     = rtSmell
                            , _rmFlags     = rtFlags
                            , _rmLinks     = rtLinks
                            , _rmCoords    = rtCoords
                            , _rmEnv       = rtEnv
                            , _rmLabel     = rtLabel
                            , _rmHookMap   = rtHookMap
                            , _rmActions   = rtActions
                            , _rmFunNames  = rtFunNames
                            , _rmFunAsyncs = [] }

-- ==================================================
-- Vessel

data VesselTemplate = VesselTemplate { vtCont :: Maybe VesselCont
                                     , vtHoly :: Maybe HolySymbol }

mkVessel :: Obj -> VesselTemplate -> Vessel
mkVessel (calcMaxMouthfuls -> m) VesselTemplate { .. } = Vessel { _vesselIsHoly       = isJust vtHoly
                                                                , _vesselMaxMouthfuls = m
                                                                , _vesselCont         = second (min m) <$> vtCont }

createVessel :: MudState -> EntTemplate -> ObjTemplate -> VesselTemplate -> (Id, MudState, Funs)
createVessel ms et ot vt = let tuple@(i, ms', _) = createObj ms et ot
                           in tuple & _2.vesselTbl    .ind i .~ mkVessel (getObj i ms') vt
                                    & _2.holySymbolTbl.at  i .~ vtHoly vt

newVessel :: MudState -> EntTemplate -> ObjTemplate -> VesselTemplate -> InvId -> (Id, MudState, Funs)
newVessel ms et ot vt invId = let (i, typeTbl.ind i .~ VesselType -> ms', fs) = createVessel ms et ot vt
                              in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Weapon

createWpn :: MudState -> EntTemplate -> ObjTemplate -> Wpn -> (Id, MudState, Funs)
createWpn ms et ot w = let tuple@(i, _, _) = createObj ms et ot
                       in tuple & _2.wpnTbl.ind i .~ w

newWpn :: MudState -> EntTemplate -> ObjTemplate -> Wpn -> InvId -> (Id, MudState, Funs)
newWpn ms et ot w invId = let (i, typeTbl.ind i .~ WpnType -> ms', fs) = createWpn ms et ot w
                          in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)

-- ==================================================
-- Writable

createWritable :: MudState -> EntTemplate -> ObjTemplate -> Writable -> (Id, MudState, Funs)
createWritable ms et ot w = let tuple@(i, _, _) = createObj ms et ot
                            in tuple & _2.writableTbl.ind i .~ w

newWritable :: MudState -> EntTemplate -> ObjTemplate -> Writable -> InvId -> (Id, MudState, Funs)
newWritable ms et ot w invId = let (i, typeTbl.ind i .~ WritableType -> ms', fs) = createWritable ms et ot w
                               in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)
