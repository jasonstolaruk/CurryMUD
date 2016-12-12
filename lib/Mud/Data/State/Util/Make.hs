{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Data.State.Util.Make where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Biodegrader
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc

import Control.Lens (at)
import Control.Lens.Operators ((%~), (&), (.~))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Map.Lazy as M (empty)


type InvId = Id


-----


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
createEnt ms et = let i = getUnusedId ms in (i, ms & activeEffectsTbl.ind i .~ []
                                                   & entTbl          .ind i .~ mkEnt i et
                                                   & pausedEffectsTbl.ind i .~ [])


-----


data ConTemplate = ConTemplate { ctCapacity :: Vol
                               , ctFlags    :: Flags }


mkCon :: ConTemplate -> Con
mkCon ConTemplate { .. } = Con { _conIsCloth  = False
                               , _conCapacity = ctCapacity
                               , _conFlags    = ctFlags }


createCon :: MudState -> EntTemplate -> ObjTemplate -> ConTemplate -> (Inv, Coins) -> (Id, MudState, Funs)
createCon ms et ot ct (is, c) = let (i, ms', fs) = createObj ms et ot
                                    ms''         = ms' & clothTbl.at  i .~ Nothing
                                                       & coinsTbl.ind i .~ c
                                                       & conTbl  .ind i .~ mkCon ct
                                in (i, ms'' & invTbl.ind i .~ sortInv ms'' is, fs)


newCon :: MudState -> EntTemplate -> ObjTemplate -> ConTemplate -> (Inv, Coins) -> InvId -> (Id, MudState, Funs)
newCon ms et ot ct ic invId = let (i, typeTbl.ind i .~ ConType -> ms', fs) = createCon ms et ot ct ic
                              in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)


-----


createCorpse :: MudState -> EntTemplate -> ObjTemplate -> ConTemplate -> (Inv, Coins) -> Corpse -> (Id, MudState, Funs)
createCorpse ms et ot ct ic corpse = let (i, ms', fs) = createCon ms et ot ct ic
                                         ms''         = ms' & corpseTbl.ind i .~ corpse
                                     in (i, ms'', fs)


newCorpse :: MudState
          -> EntTemplate
          -> ObjTemplate
          -> ConTemplate
          -> (Inv, Coins)
          -> Corpse
          -> InvId
          -> (Id, MudState, Funs)
newCorpse ms et ot ct ic corpse invId =
    let (i, typeTbl.ind i .~ CorpseType -> ms', fs) = createCorpse ms et ot ct ic corpse
    in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)


-----


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
                               , _nowEating        = Nothing
                               , _nowDrinking      = Nothing
                               , _regenQueue       = Nothing
                               , _interp           = Nothing }


-----


data ObjTemplate = ObjTemplate { otWeight :: Weight
                               , otVol    :: Vol
                               , otTaste  :: Maybe Text
                               , otFlags  :: Flags }


mkObj :: ObjTemplate -> Obj
mkObj ObjTemplate { .. } = Obj { _objWeight      = otWeight
                               , _objVol         = otVol
                               , _objTaste       = otTaste
                               , _objFlags       = otFlags
                               , _objBiodegAsync = Nothing }


createObj :: MudState -> EntTemplate -> ObjTemplate -> (Id, MudState, Funs)
createObj ms et ot = let (i, ms') = createEnt ms et
                         o        = mkObj ot
                     in (i, ms' & objTbl.ind i .~ o, pure . when (isBiodegradable o) . runBiodegAsync $ i)


newObj :: MudState -> EntTemplate -> ObjTemplate -> InvId -> (Id, MudState, Funs)
newObj ms et ot invId = let (i, typeTbl.ind i .~ ObjType -> ms', fs) = createObj ms et ot
                        in (i, ms' & invTbl.ind invId %~ addToInv ms' (pure i), fs)


-----


data PlaTemplate = PlaTemplate { ptPlaFlags   :: Flags
                               , ptLogoutRmId :: Id }


mkPla :: PlaTemplate -> Pla
mkPla PlaTemplate { .. } = Pla { _currHostName = ""
                               , _connectTime  = Nothing
                               , _plaFlags     = ptPlaFlags
                               , _columns      = 80
                               , _pageLines    = 24
                               , _peepers      = []
                               , _peeping      = []
                               , _possessing   = Nothing
                               , _retainedMsgs = []
                               , _logoutRmId   = Just ptLogoutRmId
                               , _bonusTime    = Nothing
                               , _spiritAsync  = Nothing }


-----


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
