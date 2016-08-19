{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Mud.Data.State.Util.Make where

import Mud.Data.State.MudData

import Data.Text (Text)
import qualified Data.Map.Lazy as M (empty)


data MobTemplate = MobTemplate { mtSex        :: Sex
                               , mtSt         :: Int
                               , mtDx         :: Int
                               , mtHt         :: Int
                               , mtMa         :: Int
                               , mtPs         :: Int
                               , mtMaxHp      :: Int
                               , mtMaxMp      :: Int
                               , mtMaxPp      :: Int
                               , mtMaxFp      :: Int
                               , mtExp        :: Exp
                               , mtLvl        :: Lvl
                               , mtHand       :: Hand
                               , mtKnownLangs :: [Lang]
                               , mtRmId       :: Id
                               , mtParty      :: Party }


mkMob :: MobTemplate -> Mob
mkMob MobTemplate { .. } = Mob { _sex           = mtSex
                               , _st            = mtSt
                               , _dx            = mtDx
                               , _ht            = mtHt
                               , _ma            = mtMa
                               , _ps            = mtPs
                               , _curHp         = mtMaxHp
                               , _maxHp         = mtMaxHp
                               , _curMp         = mtMaxMp
                               , _maxMp         = mtMaxMp
                               , _curPp         = mtMaxPp
                               , _maxPp         = mtMaxPp
                               , _curFp         = mtMaxFp
                               , _maxFp         = mtMaxFp
                               , _exp           = mtExp
                               , _lvl           = mtLvl
                               , _hand          = mtHand
                               , _knownLangs    = mtKnownLangs
                               , _rmId          = mtRmId
                               , _mobRmDesc     = Nothing
                               , _tempDesc      = Nothing
                               , _party         = mtParty
                               , _stomach       = []
                               , _digesterAsync = Nothing
                               , _feelingMap    = M.empty
                               , _actMap        = M.empty
                               , _nowEating     = Nothing
                               , _nowDrinking   = Nothing
                               , _regenQueue    = Nothing
                               , _interp        = Nothing }


-----


data ObjTemplate = ObjTemplate { otWeight :: Weight
                               , otVol    :: Vol
                               , otTaste  :: Maybe Text
                               , otFlags  :: Int }


mkObj :: ObjTemplate -> Obj
mkObj ObjTemplate { .. } = Obj { _objWeight      = otWeight
                               , _objVol         = otVol
                               , _objTaste       = otTaste
                               , _objFlags       = otFlags
                               , _objBiodegAsync = Nothing }


-----


data PlaTemplate = PlaTemplate { ptPlaFlags :: Int
                               , ptLastRmId :: Id }


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
                               , _lastRmId     = Just ptLastRmId
                               , _bonusTime    = Nothing }


-----


data RmTemplate = RmTemplate { rtName      :: Text
                             , rtDesc      :: Text
                             , rtListen    :: Maybe Text
                             , rtSmell     :: Maybe Text
                             , rtFlags     :: Int
                             , rtLinks     :: [RmLink]
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
                            , _rmHookMap   = rtHookMap
                            , _rmActions   = rtActions
                            , _rmFunNames  = rtFunNames
                            , _rmFunAsyncs = [] }
