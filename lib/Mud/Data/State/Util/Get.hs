{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

-- This module contains straightforward getter methods that do little or no calculation.

module Mud.Data.State.Util.Get ( getActMap
                               , getArm
                               , getArmSub
                               , getBaseAttrib
                               , getBaseAttribTuples
                               , getBaseAttribs
                               , getBaseDx
                               , getBaseHt
                               , getBaseMa
                               , getBasePs
                               , getBaseSt
                               , getBonusTime
                               , getChan
                               , getCloth
                               , getCoins
                               , getColumns
                               , getCon
                               , getConCapacity
                               , getConIsCloth
                               , getConnectTime
                               , getCorpse
                               , getCorpseCapacity
                               , getCorpseDecompSecs
                               , getCorpseVol
                               , getCorpseWeight
                               , getCurrHostName
                               , getDisconnectTime
                               , getDistinctFood
                               , getDistinctFoodForFood
                               , getDistinctLiq
                               , getDistinctLiqForLiq
                               , getDurEffects
                               , getEnt
                               , getEntDesc
                               , getEntSmell
                               , getEqMap
                               , getExp
                               , getFeelingMap
                               , getFollowers
                               , getFollowing
                               , getFood
                               , getFps
                               , getHand
                               , getHolySymbol
                               , getHolySymbolGodName
                               , getHostMap
                               , getHps
                               , getIdForPCSing
                               , getIdForRoot
                               , getInterp
                               , getIntroduced
                               , getInv
                               , getInvCoins
                               , getKnownLangs
                               , getLastRmId
                               , getLight
                               , getLightSub
                               , getLinked
                               , getLogQueue
                               , getLoginTime
                               , getLogoutRmId
                               , getLvl
                               , getLvlExp
                               , getMaxMouthfuls
                               , getMemberOf
                               , getMob
                               , getMobRm
                               , getMobRmCoins
                               , getMobRmDesc
                               , getMobRmInv
                               , getMobRmInvCoins
                               , getMobSize
                               , getMps
                               , getMsgQueue
                               , getMsgQueueColumns
                               , getMyGroup
                               , getNowDrinking
                               , getNowEating
                               , getNpc
                               , getNpcMsgQueue
                               , getObj
                               , getObjBiodegAsync
                               , getObjTaste
                               , getObjVol
                               , getObjWeight
                               , getPC
                               , getPageLines
                               , getParty
                               , getPausedEffects
                               , getPeepers
                               , getPeepersPeeping
                               , getPeeping
                               , getPickPts
                               , getPla
                               , getPossessing
                               , getPossessor
                               , getPps
                               , getPts
                               , getRace
                               , getRm
                               , getRmCoords
                               , getRmId
                               , getRmName
                               , getRndmNamesTbl
                               , getSacrificesTbl
                               , getSex
                               , getSexRace
                               , getSexRaceLvl
                               , getSing
                               , getSkillPts
                               , getStomach
                               , getTeleLinkTbl
                               , getTempDesc
                               , getType
                               , getVessel
                               , getVesselCont
                               , getVesselIsHoly
                               , getWpn
                               , getWpnSub
                               , getWritMessage
                               , getWritRecip
                               , getWritable
                               , hasRazzled
                               , hasRazzledId
                               , isAdmin
                               , isAdminId
                               , isBiodegradable
                               , isBiodegradableId
                               , isGmcp
                               , isGmcpId
                               , isHolySymbol
                               , isHumming
                               , isHummingId
                               , isIncognito
                               , isIncognitoId
                               , isNotFirstAdminMsg
                               , isNotFirstAdminMsgId
                               , isNotFirstMobSay
                               , isNotFirstModSayId
                               , isNotFirstSpiritCmdNotFound
                               , isNotFirstSpiritCmdNotFoundId
                               , isNpc
                               , isNpcPla
                               , isPla
                               , isShowingFp
                               , isShowingFpId
                               , isShowingHp
                               , isShowingHpId
                               , isShowingMp
                               , isShowingMpId
                               , isShowingPp
                               , isShowingPpId
                               , isSpirit
                               , isSpiritId
                               , isTinderbox
                               , isTinderboxId
                               , isTunedAdmin
                               , isTunedAdminId
                               , isTunedQuestion
                               , isTunedQuestionId
                               , objFlagHelper
                               , onHelper
                               , onPC
                               , onPla
                               , plaFlagHelper ) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MsgQueue
import           Mud.Data.State.MudData
import           Mud.TopLvlDefs.Seconds
import           Mud.Util.List
import qualified Mud.Util.Misc as U (blowUp)
import           Mud.Util.Misc hiding (blowUp)

import           Control.Arrow ((&&&))
import           Control.Lens (at, view, views)
import           Control.Lens.Operators ((^.))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Stack (HasCallStack)
import           Network (HostName)
import           Prelude hiding (exp)


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Data.State.Util.Get"


-- ============================================================
-- Helper functions:


onPC :: HasCallStack => (PC -> a) -> a -> Id -> MudState -> a
onPC = onHelper getPC


onPla :: HasCallStack => (Pla -> a) -> a -> Id -> MudState -> a
onPla = onHelper getPla


onHelper :: HasCallStack => (Id -> MudState -> a) -> (a -> b) -> b -> Id -> MudState -> b
onHelper f g dflt i ms | isNpc i ms = maybe dflt helper . getPossessor i $ ms
                       | otherwise  = helper i
  where
    helper i' = g . f i' $ ms


isNpc :: HasCallStack => Id -> MudState -> Bool
isNpc i = (== NpcType) . getType i


isPla :: HasCallStack => Id -> MudState -> Bool
isPla i = (== PlaType) . getType i


isNpcPla :: HasCallStack => Id -> MudState -> Bool
isNpcPla i = ((||) <$> (== NpcType) <*> (== PlaType)) . getType i


-- ============================================================
-- Getters:


getActMap :: HasCallStack => Id -> MudState -> ActMap
getActMap i = view actMap . getMob i


-----


getArm :: HasCallStack => Id -> MudState -> Arm
getArm i = view (armTbl.ind i)


-----


getArmSub :: HasCallStack => Id -> MudState -> ArmSub
getArmSub i = view armSub . getArm i


-----


getBaseAttrib :: HasCallStack => Attrib -> Id -> MudState -> Int
getBaseAttrib = \case St -> getBaseSt
                      Dx -> getBaseDx
                      Ht -> getBaseHt
                      Ma -> getBaseMa
                      Ps -> getBasePs


getBaseAttribs :: HasCallStack => Id -> MudState -> (Int, Int, Int, Int, Int)
getBaseAttribs i ms = listToTuple [ getBaseAttrib a i ms | a <- allValues ]


getBaseAttribTuples :: HasCallStack => Id -> MudState -> [(Attrib, Int)]
getBaseAttribTuples i ms = [ (a, getBaseAttrib a i ms) | a <- allValues ]


-----


getBaseDx :: HasCallStack => Id -> MudState -> Int
getBaseDx i = view dx . getMob i


-----


getBaseHt :: HasCallStack => Id -> MudState -> Int
getBaseHt i = view ht . getMob i


-----


getBaseMa :: HasCallStack => Id -> MudState -> Int
getBaseMa i = view ma . getMob i


-----


getBasePs :: HasCallStack => Id -> MudState -> Int
getBasePs i = view ps . getMob i


-----


getBaseSt :: HasCallStack => Id -> MudState -> Int
getBaseSt i = view st . getMob i


-----


getBonusTime :: HasCallStack => Id -> MudState -> Maybe UTCTime
getBonusTime i = view bonusTime . getPla i


-----


getChan :: HasCallStack => Id -> MudState -> Chan
getChan i = view (chanTbl.ind i)


-----


getCloth :: HasCallStack => Id -> MudState -> Cloth
getCloth i = view (clothTbl.ind i)


-----


getCoins :: HasCallStack => Id -> MudState -> Coins
getCoins i = view (coinsTbl.ind i)


-----


getColumns :: HasCallStack => Id -> MudState -> Cols
getColumns = onPla (view columns) 80


-----


getCon :: HasCallStack => Id -> MudState -> Con
getCon i = view (conTbl.ind i)


-----


getConCapacity :: HasCallStack => Id -> MudState -> Vol
getConCapacity i = view conCapacity . getCon i


-----


getConIsCloth :: HasCallStack => Id -> MudState -> Bool
getConIsCloth i = view conIsCloth . getCon i


-----


getConnectTime :: HasCallStack => Id -> MudState -> Maybe UTCTime
getConnectTime i = view connectTime . getPla i


-----


getCorpse :: HasCallStack => Id -> MudState -> Corpse
getCorpse i = view (corpseTbl.ind i)


-----


getCorpseCapacity :: HasCallStack => Id -> MudState -> Vol
getCorpseCapacity i = view corpseCapacity . getMob i


-----


getCorpseDecompSecs :: HasCallStack => Id -> MudState -> Seconds
getCorpseDecompSecs i = view corpseDecompSecs . getMob i


-----


getCorpseVol :: HasCallStack => Id -> MudState -> Vol
getCorpseVol i = view corpseVol . getMob i


-----


getCorpseWeight :: HasCallStack => Id -> MudState -> Weight
getCorpseWeight i = view corpseWeight . getMob i


-----


getCurrHostName :: HasCallStack => Id -> MudState -> HostName
getCurrHostName i = view currHostName . getPla i


-----


getDisconnectTime :: HasCallStack => Id -> MudState -> Maybe UTCTime
getDisconnectTime i = view disconnectTime . getPla i


-----


getDistinctFood :: HasCallStack => Id -> MudState -> DistinctFood
getDistinctFood i = view (distinctFoodTbl.ind i)


-----


getDistinctFoodForFood :: HasCallStack => Food -> MudState -> DistinctFood
getDistinctFoodForFood (view foodId -> DistinctFoodId i) = view (distinctFoodTbl.ind i)


-----


getDistinctLiq :: HasCallStack => Id -> MudState -> DistinctLiq
getDistinctLiq i = view (distinctLiqTbl.ind i)


-----


getDistinctLiqForLiq :: HasCallStack => Liq -> MudState -> DistinctLiq
getDistinctLiqForLiq (view liqId -> DistinctLiqId i) = view (distinctLiqTbl.ind i)


-----


getDurEffects :: HasCallStack => Id -> MudState -> [DurationalEffect]
getDurEffects i = view (durationalEffectTbl.ind i)


-----


getEnt :: HasCallStack => Id -> MudState -> Ent
getEnt i = view (entTbl.ind i)


-----


getEntDesc :: HasCallStack => Id -> MudState -> Text
getEntDesc i = view entDesc . getEnt i


-----


getEntSmell :: HasCallStack => Id -> MudState -> Text
getEntSmell i = views entSmell (fromMaybe noSmellMsg) . getEnt i


-----


getEqMap :: HasCallStack => Id -> MudState -> EqMap
getEqMap i = view (eqTbl.ind i)


-----


getExp :: HasCallStack => Id -> MudState -> Exp
getExp i = view exp . getMob i


-----


getFeelingMap :: HasCallStack => Id -> MudState -> FeelingMap
getFeelingMap i = view feelingMap . getMob i


-----


getFollowing :: HasCallStack => Id -> MudState -> Maybe Id
getFollowing i = view following . getParty i


-----


getFollowers :: HasCallStack => Id -> MudState -> Inv
getFollowers i = view followers . getParty i


-----


getFood :: HasCallStack => Id -> MudState -> Food
getFood i = view (foodTbl.ind i)


-----


getFps :: HasCallStack => Id -> MudState -> (Int, Int)
getFps i ms = let (_, _, _, pair) = getPts i ms in pair


-----


getHand :: HasCallStack => Id -> MudState -> Hand
getHand i = view hand . getMob i


-----


getHolySymbol :: HasCallStack => Id -> MudState -> HolySymbol
getHolySymbol i = view (holySymbolTbl.ind i)


-----


getHolySymbolGodName :: HasCallStack => Id -> MudState -> GodName
getHolySymbolGodName i = unHolySymbol . getHolySymbol i


-----


getHostMap :: HasCallStack => Sing -> MudState -> Maybe HostMap
getHostMap s = view (hostTbl.at s)


-----


getHps :: HasCallStack => Id -> MudState -> (Int, Int)
getHps i ms = let (pair, _, _, _) = getPts i ms in pair


-----


getIdForPCSing :: HasCallStack => Sing -> MudState -> Id
getIdForPCSing s = views (pcSingTbl.at s) (fromMaybe oops)
  where
    oops = blowUp "getIdForPCSing" "PC sing not found in the PC sing table" s


getIdForRoot :: HasCallStack => MudState -> Id
getIdForRoot = getIdForPCSing "Root"


-----


getInterp :: HasCallStack => Id -> MudState -> Maybe Interp
getInterp i = view interp . getMob i


-----


getIntroduced :: HasCallStack => Id -> MudState -> [Sing]
getIntroduced = onPC (view introduced) []


-----


getInv :: HasCallStack => Id -> MudState -> Inv
getInv i = view (invTbl.ind i)


-----


getInvCoins :: HasCallStack => Id -> MudState -> (Inv, Coins)
getInvCoins i = getInv i &&& getCoins i


-----


getKnownLangs :: HasCallStack => Id -> MudState -> [Lang]
getKnownLangs i = view knownLangs . getMob i


-----


getLastRmId :: HasCallStack => Id -> MudState -> Id
getLastRmId i = view lastRmId . getMob i


-----


getLight :: HasCallStack => Id -> MudState -> Light
getLight i = view (lightTbl.ind i)


-----


getLightSub :: HasCallStack => Id -> MudState -> LightSub
getLightSub i = unLight . getLight i


-----


getLinked :: HasCallStack => Id -> MudState -> [Sing]
getLinked = onPC (view linked) []


-----


getLoginTime :: HasCallStack => Id -> MudState -> Maybe UTCTime
getLoginTime i = view loginTime . getPla i


-----


getLogoutRmId :: HasCallStack => Id -> MudState -> Maybe Id
getLogoutRmId = onPla (view logoutRmId) Nothing


-----


getLogQueue :: HasCallStack => Id -> MudState -> LogQueue
getLogQueue i = views (plaLogTbl.ind i) snd


-----


getLvl :: HasCallStack => Id -> MudState -> Lvl
getLvl i = view lvl . getMob i


-----


getLvlExp :: HasCallStack => Id -> MudState -> (Lvl, Exp)
getLvlExp i = getLvl i &&& getExp i


-----


getMaxMouthfuls :: HasCallStack => Id -> MudState -> Mouthfuls
getMaxMouthfuls i = view vesselMaxMouthfuls . getVessel i


-----


getMemberOf :: HasCallStack => Id -> MudState -> Maybe Id
getMemberOf i = view memberOf . getParty i


-----


getMob :: HasCallStack => Id -> MudState -> Mob
getMob i = view (mobTbl.ind i)


-----


getMobRm :: HasCallStack => Id -> MudState -> Rm
getMobRm i ms = let ri = getRmId i ms in getRm ri ms


-----


getMobRmCoins :: HasCallStack => Id -> MudState -> Coins
getMobRmCoins i ms = let ri = getRmId i ms in getCoins ri ms


-----


getMobRmDesc :: HasCallStack => Id -> MudState -> MobRmDesc
getMobRmDesc i = view mobRmDesc . getMob i


-----


getMobRmInv :: HasCallStack => Id -> MudState -> Inv
getMobRmInv i ms = let ri = getRmId i ms in getInv ri ms


-----


getMobRmInvCoins :: HasCallStack => Id -> MudState -> (Inv, Coins)
getMobRmInvCoins i ms = let ri = getRmId i ms in getInvCoins ri ms


-----


getMobSize :: HasCallStack => Id -> MudState -> Maybe MobSize
getMobSize i = view mobSize . getMob i


-----


getMps :: HasCallStack => Id -> MudState -> (Int, Int)
getMps i ms = let (_, pair, _, _) = getPts i ms in pair


-----


getMsgQueue :: HasCallStack => Id -> MudState -> MsgQueue
getMsgQueue i = view (msgQueueTbl.ind i)


-----


getMsgQueueColumns :: HasCallStack => Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i = getMsgQueue i &&& getColumns i


-----


getMyGroup :: HasCallStack => Id -> MudState -> Inv
getMyGroup i = view myGroup . getParty i


-----


getNowDrinking :: HasCallStack => Id -> MudState -> Maybe NowDrinking
getNowDrinking i = view nowDrinking . getMob i


-----


getNowEating :: HasCallStack => Id -> MudState -> Maybe NowEating
getNowEating i = view nowEating . getMob i


-----


getNpc :: HasCallStack => Id -> MudState -> Npc
getNpc i = view (npcTbl.ind i)


-----


getNpcMsgQueue :: HasCallStack => Id -> MudState -> NpcMsgQueue
getNpcMsgQueue i = view npcMsgQueue . getNpc i


-----


getObj :: HasCallStack => Id -> MudState -> Obj
getObj i = view (objTbl.ind i)


-----


getObjBiodegAsync :: HasCallStack => Id -> MudState -> Maybe BiodegAsync
getObjBiodegAsync i = view objBiodegAsync . getObj i


-----


getObjTaste :: HasCallStack => Id -> MudState -> Text
getObjTaste i = views objTaste (fromMaybe noTasteMsg) . getObj i


-----


getObjVol :: HasCallStack => Id -> MudState -> Vol
getObjVol i = view objVol . getObj i


-----


getObjWeight :: HasCallStack => Id -> MudState -> Weight
getObjWeight i = view objWeight . getObj i


-----


getPausedEffects :: HasCallStack => Id -> MudState -> [PausedEffect]
getPausedEffects i = view (pausedEffectTbl.ind i)


-----


getPC :: HasCallStack => Id -> MudState -> PC
getPC i = view (pcTbl.ind i)


-----


getParty :: HasCallStack => Id -> MudState -> Party
getParty i = view party . getMob i


-----


getPickPts :: HasCallStack => Id -> MudState -> Int
getPickPts i = view (pickPtsTbl.ind i)


-----


getPps :: HasCallStack => Id -> MudState -> (Int, Int)
getPps i ms = let (_, _, pair, _) = getPts i ms in pair


-----


getPts :: HasCallStack => Id -> MudState -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
getPts i ms = let m = getMob i ms
              in listToTuple [ (m^.curHp, m^.maxHp), (m^.curMp, m^.maxMp), (m^.curPp, m^.maxPp), (m^.curFp, m^.maxFp) ]


-----


getPageLines :: HasCallStack => Id -> MudState -> Int
getPageLines = onPla (view pageLines) 24


-----


getPeepers :: HasCallStack => Id -> MudState -> Inv
getPeepers = onPla (view peepers) []


-----


getPeeping :: HasCallStack => Id -> MudState -> Inv
getPeeping = onPla (view peeping) []


-----


getPeepersPeeping :: HasCallStack => Id -> MudState -> (Inv, Inv)
getPeepersPeeping i = getPeepers i &&& getPeeping i


-----


getPla :: HasCallStack => Id -> MudState -> Pla
getPla i = view (plaTbl.ind i)


-----


getPossessing :: HasCallStack => Id -> MudState -> Maybe Id
getPossessing i = view possessing . getPla i


-----


getPossessor :: HasCallStack => Id -> MudState -> Maybe Id
getPossessor i = view npcPossessor . getNpc i


-----


getRace :: HasCallStack => Id -> MudState -> Race
getRace i = view race . getPC i


-----


getRm :: HasCallStack => Id -> MudState -> Rm
getRm i = view (rmTbl.ind i)


-----


getRmCoords :: HasCallStack => Id -> MudState -> RmCoords
getRmCoords i = view rmCoords . getRm i


-----


getRmId :: HasCallStack => Id -> MudState -> Id
getRmId i = view rmId . getMob i


-----


getRmName :: HasCallStack => Id -> MudState -> Text
getRmName i = view rmName . getRm i


-----


getRndmNamesTbl :: HasCallStack => Id -> MudState -> RndmNamesTbl
getRndmNamesTbl i = view (rndmNamesMstrTbl.ind i)


-----


getSacrificesTbl :: HasCallStack => Id -> MudState -> SacrificesTbl
getSacrificesTbl i = view sacrificesTbl . getPC i


-----


getSex :: HasCallStack => Id -> MudState -> Sex
getSex i = view sex . getMob i


-----


getSexRace :: HasCallStack => Id -> MudState -> (Sex, Race)
getSexRace i = getSex i &&& getRace i


-----


getSexRaceLvl :: HasCallStack => Id -> MudState -> (Sex, Race, Lvl)
getSexRaceLvl i ms | (s, r) <- getSexRace i ms = (s, r, getLvl i ms)


-----


getSing :: HasCallStack => Id -> MudState -> Sing
getSing i = view sing . getEnt i


-----


getSkillPts :: HasCallStack => Id -> MudState -> SkillPts
getSkillPts i = view skillPts . getPC i


-----


getStomach :: HasCallStack => Id -> MudState -> [StomachCont]
getStomach i = view stomach . getMob i


-----


getTeleLinkTbl :: HasCallStack => Id -> MudState -> TeleLinkTbl
getTeleLinkTbl i = view (teleLinkMstrTbl.ind i)


-----


getTempDesc :: HasCallStack => Id -> MudState -> TempDesc
getTempDesc i = view tempDesc . getMob i


-----


getType :: HasCallStack => Id -> MudState -> Type
getType i = view (typeTbl.ind i)


-----


getVessel :: HasCallStack => Id -> MudState -> Vessel
getVessel i = view (vesselTbl.ind i)


-----


getVesselCont :: HasCallStack => Id -> MudState -> Maybe VesselCont
getVesselCont i = view vesselCont . getVessel i


-----


getVesselIsHoly :: HasCallStack => Id -> MudState -> Bool
getVesselIsHoly i = view vesselIsHoly . getVessel i


-----


getWpn :: HasCallStack => Id -> MudState -> Wpn
getWpn i = view (wpnTbl.ind i)


-----


getWpnSub :: HasCallStack => Id -> MudState -> WpnSub
getWpnSub i = view wpnSub . getWpn i


-----


getWritMessage :: HasCallStack => Id -> MudState -> Maybe (Text, Lang)
getWritMessage i = view writMessage . getWritable i


-----


getWritRecip :: HasCallStack => Id -> MudState -> Maybe Sing
getWritRecip i = view writRecip . getWritable i


-----


getWritable :: HasCallStack => Id -> MudState -> Writable
getWritable i = view (writableTbl.ind i)


-- ==================================================
-- Object flag getters:


objFlagHelper :: HasCallStack => ObjFlags -> Id -> MudState -> Bool
objFlagHelper flag i = getObjFlag flag . getObj i


-----


isBiodegradable :: Obj -> Bool
isBiodegradable = getObjFlag IsBiodegradable


isBiodegradableId :: HasCallStack => Id -> MudState -> Bool
isBiodegradableId = objFlagHelper IsBiodegradable


-----


isHolySymbol :: HasCallStack => Id -> MudState -> Bool
isHolySymbol i ms = case getType i ms of HolySymbolType -> True
                                         VesselType     -> getVesselIsHoly i ms
                                         _              -> False


-----


isHumming :: Obj -> Bool
isHumming = getObjFlag IsHumming


isHummingId :: HasCallStack => Id -> MudState -> Bool
isHummingId = objFlagHelper IsHumming


-----


isTinderbox :: Obj -> Bool
isTinderbox = getObjFlag IsTinderbox


isTinderboxId :: HasCallStack => Id -> MudState -> Bool
isTinderboxId = objFlagHelper IsTinderbox


-- ==================================================
-- Player flag getters:


plaFlagHelper :: HasCallStack => PlaFlags -> Id -> MudState -> Bool
plaFlagHelper flag i = getPlaFlag flag . getPla i


-----


hasRazzled :: Pla -> Bool
hasRazzled = getPlaFlag HasRazzled


hasRazzledId :: HasCallStack => Id -> MudState -> Bool
hasRazzledId = onPla hasRazzled False


-----


isAdmin :: Pla -> Bool
isAdmin = getPlaFlag IsAdmin


isAdminId :: HasCallStack => Id -> MudState -> Bool
isAdminId = onPla isAdmin False


-----


isGmcp :: Pla -> Bool
isGmcp = getPlaFlag IsGmcp


isGmcpId :: HasCallStack => Id -> MudState -> Bool
isGmcpId = onPla isGmcp False


-----


isIncognito :: Pla -> Bool
isIncognito = getPlaFlag IsIncognito


isIncognitoId :: HasCallStack => Id -> MudState -> Bool
isIncognitoId = onPla isIncognito False


-----


isNotFirstAdminMsg :: Pla -> Bool
isNotFirstAdminMsg = getPlaFlag IsNotFirstAdminMsg


isNotFirstAdminMsgId :: HasCallStack => Id -> MudState -> Bool
isNotFirstAdminMsgId = onPla isNotFirstAdminMsg True


-----


isNotFirstMobSay :: Pla -> Bool
isNotFirstMobSay = getPlaFlag IsNotFirstMobSay


isNotFirstModSayId :: HasCallStack => Id -> MudState -> Bool
isNotFirstModSayId = onPla isNotFirstMobSay True


-----


isNotFirstSpiritCmdNotFound :: Pla -> Bool
isNotFirstSpiritCmdNotFound = getPlaFlag IsNotFirstSpiritCmdNotFound


isNotFirstSpiritCmdNotFoundId :: HasCallStack => Id -> MudState -> Bool
isNotFirstSpiritCmdNotFoundId = onPla isNotFirstSpiritCmdNotFound True


-----


isShowingHp :: Pla -> Bool
isShowingHp = getPlaFlag IsShowingHp


isShowingHpId :: HasCallStack => Id -> MudState -> Bool
isShowingHpId = plaFlagHelper IsShowingHp


-----


isShowingMp :: Pla -> Bool
isShowingMp = getPlaFlag IsShowingMp


isShowingMpId :: HasCallStack => Id -> MudState -> Bool
isShowingMpId = plaFlagHelper IsShowingMp


-----


isShowingPp :: Pla -> Bool
isShowingPp = getPlaFlag IsShowingPp


isShowingPpId :: HasCallStack => Id -> MudState -> Bool
isShowingPpId = plaFlagHelper IsShowingPp


-----


isShowingFp :: Pla -> Bool
isShowingFp = getPlaFlag IsShowingFp


isShowingFpId :: HasCallStack => Id -> MudState -> Bool
isShowingFpId = plaFlagHelper IsShowingFp


-----


isSpirit :: Pla -> Bool
isSpirit = getPlaFlag IsSpirit


isSpiritId :: HasCallStack => Id -> MudState -> Bool
isSpiritId = onPla isSpirit False


-----


isTunedAdmin :: Pla -> Bool
isTunedAdmin = getPlaFlag IsTunedAdmin


isTunedAdminId :: HasCallStack => Id -> MudState -> Bool
isTunedAdminId = plaFlagHelper IsTunedAdmin


-----


isTunedQuestion :: Pla -> Bool
isTunedQuestion = getPlaFlag IsTunedQuestion


isTunedQuestionId :: HasCallStack => Id -> MudState -> Bool
isTunedQuestionId = plaFlagHelper IsTunedQuestion
