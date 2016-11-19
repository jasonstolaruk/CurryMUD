{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

-- This module contains straightforward getter methods that do little or no calculation.

module Mud.Data.State.Util.Get where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Util.List
import Mud.Util.Misc

import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId)
import Control.Lens (at, to, view, views)
import Control.Lens.Operators ((^.))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Network (HostName)
import Prelude hiding (exp)


-- ============================================================
-- Helper functions:


onPC :: (PC -> a) -> a -> Id -> MudState -> a
onPC = onHelper getPC


onPla :: (Pla -> a) -> a -> Id -> MudState -> a
onPla = onHelper getPla


onHelper :: (Id -> MudState -> a) -> (a -> b) -> b -> Id -> MudState -> b
onHelper f g dflt i ms | isNpc i ms = maybe dflt helper . getPossessor i $ ms
                       | otherwise  = helper i
  where
    helper i' = g . f i' $ ms


isNpc :: Id -> MudState -> Bool
isNpc i = (== NpcType) . getType i


isPC :: Id -> MudState -> Bool
isPC i = (== PCType) . getType i


isNpcPC :: Id -> MudState -> Bool
isNpcPC i ms = getType i ms `elem` [ NpcType, PCType ]


-- ============================================================
-- Getters:


getActiveEffects :: Id -> MudState -> [ActiveEffect]
getActiveEffects i = view (activeEffectsTbl.ind i)


-----


getActMap :: Id -> MudState -> ActMap
getActMap i = view actMap . getMob i


-----


getArm :: Id -> MudState -> Arm
getArm i = view (armTbl.ind i)


-----


getArmSub :: Id -> MudState -> ArmSub
getArmSub i = view armSub . getArm i


-----


getBaseAttrib :: Attrib -> Id -> MudState -> Int
getBaseAttrib = \case St -> getBaseSt
                      Dx -> getBaseDx
                      Ht -> getBaseHt
                      Ma -> getBaseMa
                      Ps -> getBasePs


getBaseAttribs :: Id -> MudState -> (Int, Int, Int, Int, Int)
getBaseAttribs i ms = listToTuple [ getBaseAttrib a i ms | a <- allValues ]


getBaseAttribTuples :: Id -> MudState -> [(Attrib, Int)]
getBaseAttribTuples i ms = [ (a, getBaseAttrib a i ms) | a <- allValues ]


-----


getBaseDx :: Id -> MudState -> Int
getBaseDx i = view dx . getMob i


-----


getBaseHt :: Id -> MudState -> Int
getBaseHt i = view ht . getMob i


-----


getBaseMa :: Id -> MudState -> Int
getBaseMa i = view ma . getMob i


-----


getBasePs :: Id -> MudState -> Int
getBasePs i = view ps . getMob i


-----


getBaseSt :: Id -> MudState -> Int
getBaseSt i = view st . getMob i


-----


getBonusTime :: Id -> MudState -> Maybe UTCTime
getBonusTime i = view bonusTime . getPla i


-----


getChan :: Id -> MudState -> Chan
getChan i = view (chanTbl.ind i)


-----


getCloth :: Id -> MudState -> Cloth
getCloth i = view (clothTbl.ind i)


-----


getCoins :: Id -> MudState -> Coins
getCoins i = view (coinsTbl.ind i)


-----


getColumns :: Id -> MudState -> Cols
getColumns = onPla (view columns) 80


-----


getCon :: Id -> MudState -> Con
getCon i = view (conTbl.ind i)


-----


getConCapacity :: Id -> MudState -> Vol
getConCapacity i = view conCapacity . getCon i


-----


getConIsCloth :: Id -> MudState -> Bool
getConIsCloth i = view conIsCloth . getCon i


-----


getConnectTime :: Id -> MudState -> Maybe UTCTime
getConnectTime i = view connectTime . getPla i


-----


getCorpse :: Id -> MudState -> Corpse
getCorpse i = view (corpseTbl.ind i)


-----


getCorpseCapacity :: Id -> MudState -> Vol
getCorpseCapacity i = view corpseCapacity . getMob i


-----


getCorpseVol :: Id -> MudState -> Vol
getCorpseVol i = view corpseVol . getMob i


-----


getCorpseWeight :: Id -> MudState -> Weight
getCorpseWeight i = view corpseWeight . getMob i


-----


getCurrHostName :: Id -> MudState -> HostName
getCurrHostName i = view currHostName . getPla i


-----


getDistinctFood :: Id -> MudState -> DistinctFood
getDistinctFood i = view (distinctFoodTbl.ind i)


-----


getDistinctFoodForFood :: Food -> MudState -> DistinctFood
getDistinctFoodForFood (view foodId -> DistinctFoodId i) = view (distinctFoodTbl.ind i)


-----


getDistinctLiq :: Id -> MudState -> DistinctLiq
getDistinctLiq i = view (distinctLiqTbl.ind i)


-----


getDistinctLiqForLiq :: Liq -> MudState -> DistinctLiq
getDistinctLiqForLiq (view liqId -> DistinctLiqId i) = view (distinctLiqTbl.ind i)


-----


getEnt :: Id -> MudState -> Ent
getEnt i = view (entTbl.ind i)


-----


getEntDesc :: Id -> MudState -> Text
getEntDesc i = view entDesc . getEnt i


-----


getEntSmell :: Id -> MudState -> Text
getEntSmell i = views entSmell (fromMaybe noSmellMsg) . getEnt i


-----


getEqMap :: Id -> MudState -> EqMap
getEqMap i = view (eqTbl.ind i)


-----


getExp :: Id -> MudState -> Exp
getExp i = view exp . getMob i


-----


getFeelingMap :: Id -> MudState -> FeelingMap
getFeelingMap i = view feelingMap . getMob i


-----


getFollowing :: Id -> MudState -> Maybe Id
getFollowing i = view following . getParty i


-----


getFollowers :: Id -> MudState -> Inv
getFollowers i = view followers . getParty i


-----

getFood :: Id -> MudState -> Food
getFood i = view (foodTbl.ind i)


-----


getFps :: Id -> MudState -> (Int, Int)
getFps i ms = let (_, _, _, pair) = getPts i ms in pair


-----


getHand :: Id -> MudState -> Hand
getHand i = view hand . getMob i


-----


getHostMap :: Sing -> MudState -> Maybe HostMap
getHostMap s = view (hostTbl.at s)


-----


getHps :: Id -> MudState -> (Int, Int)
getHps i ms = let (pair, _, _, _) = getPts i ms in pair


-----


getInterp :: Id -> MudState -> Maybe Interp
getInterp i = view interp . getMob i


-----


getIntroduced :: Id -> MudState -> [Sing]
getIntroduced = onPC (view introduced) []


-----


getInv :: Id -> MudState -> Inv
getInv i = view (invTbl.ind i)


-----


getInvCoins :: Id -> MudState -> (Inv, Coins)
getInvCoins i = getInv i &&& getCoins i


-----


getKnownLangs :: Id -> MudState -> [Lang]
getKnownLangs i = view knownLangs . getMob i


-----


getLinked :: Id -> MudState -> [Sing]
getLinked = onPC (view linked) []


-----


getListenThreadId :: MudState -> ThreadId
getListenThreadId = reverseLookup Listen . view threadTbl


-----


getLogoutRmId :: Id -> MudState -> Maybe Id
getLogoutRmId = onPla (view logoutRmId) Nothing


-----


getLogQueue :: Id -> MudState -> LogQueue
getLogQueue i = view (plaLogTbl.ind i.to snd)


-----


getLvl :: Id -> MudState -> Lvl
getLvl i = view lvl . getMob i


-----


getLvlExp :: Id -> MudState -> (Lvl, Exp)
getLvlExp i = getLvl i &&& getExp i


-----


getMaxMouthfuls :: Id -> MudState -> Mouthfuls
getMaxMouthfuls i = view vesselMaxMouthfuls . getVessel i


-----


getMemberOf :: Id -> MudState -> Maybe Id
getMemberOf i = view memberOf . getParty i


-----


getMob :: Id -> MudState -> Mob
getMob i = view (mobTbl.ind i)


-----


getMobRm :: Id -> MudState -> Rm
getMobRm i ms = let ri = getRmId i ms in getRm ri ms


-----


getMobRmCoins :: Id -> MudState -> Coins
getMobRmCoins i ms = let ri = getRmId i ms in getCoins ri ms


-----


getMobRmDesc :: Id -> MudState -> MobRmDesc
getMobRmDesc i = view mobRmDesc . getMob i


-----


getMobRmInv :: Id -> MudState -> Inv
getMobRmInv i ms = let ri = getRmId i ms in getInv ri ms


-----


getMobRmInvCoins :: Id -> MudState -> (Inv, Coins)
getMobRmInvCoins i ms = let ri = getRmId i ms in getInvCoins ri ms


-----


getMobSize :: Id -> MudState -> Maybe MobSize
getMobSize i = view mobSize . getMob i


-----


getMps :: Id -> MudState -> (Int, Int)
getMps i ms = let (_, pair, _, _) = getPts i ms in pair


-----


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = view (msgQueueTbl.ind i)


-----


getMsgQueueColumns :: Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i = getMsgQueue i &&& getColumns i


-----


getMyGroup :: Id -> MudState -> Inv
getMyGroup i = view myGroup . getParty i


-----


getNowDrinking :: Id -> MudState -> Maybe NowDrinking
getNowDrinking i = view nowDrinking . getMob i


-----


getNowEating :: Id -> MudState -> Maybe NowEating
getNowEating i = view nowEating . getMob i


-----


getNpc :: Id -> MudState -> Npc
getNpc i = view (npcTbl.ind i)


-----


getNpcMsgQueue :: Id -> MudState -> NpcMsgQueue
getNpcMsgQueue i = view npcMsgQueue . getNpc i


-----


getObj :: Id -> MudState -> Obj
getObj i = view (objTbl.ind i)


-----


getObjBiodegAsync :: Id -> MudState -> Maybe BiodegAsync
getObjBiodegAsync i = view objBiodegAsync . getObj i


-----


getObjTaste :: Id -> MudState -> Text
getObjTaste i = views objTaste (fromMaybe noTasteMsg) . getObj i


-----


getObjVol :: Id -> MudState -> Vol
getObjVol i = view objVol . getObj i


-----


getObjWeight :: Id -> MudState -> Weight
getObjWeight i = view objWeight . getObj i


-----


getPausedEffects :: Id -> MudState -> [PausedEffect]
getPausedEffects i = view (pausedEffectsTbl.ind i)


-----


getPC :: Id -> MudState -> PC
getPC i = view (pcTbl.ind i)


-----


getParty :: Id -> MudState -> Party
getParty i = view party . getMob i


-----


getPickPts :: Id -> MudState -> Int
getPickPts i = view (pickPtsTbl.ind i)


-----


getPps :: Id -> MudState -> (Int, Int)
getPps i ms = let (_, _, pair, _) = getPts i ms in pair


-----


getPts :: Id -> MudState -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
getPts i ms = let m = getMob i ms
              in listToTuple [ (m^.curHp, m^.maxHp), (m^.curMp, m^.maxMp), (m^.curPp, m^.maxPp), (m^.curFp, m^.maxFp) ]


-----


getPageLines :: Id -> MudState -> Int
getPageLines = onPla (view pageLines) 24


-----


getPeepers :: Id -> MudState -> Inv
getPeepers = onPla (view peepers) []


-----


getPeeping :: Id -> MudState -> Inv
getPeeping = onPla (view peeping) []


-----


getPeepersPeeping :: Id -> MudState -> (Inv, Inv)
getPeepersPeeping i = getPeepers i &&& getPeeping i


-----


getPla :: Id -> MudState -> Pla
getPla i = view (plaTbl.ind i)


-----


getPossessing :: Id -> MudState -> Maybe Id
getPossessing i = view possessing . getPla i


-----


getPossessor :: Id -> MudState -> Maybe Id
getPossessor i = view npcPossessor . getNpc i


-----


getRace :: Id -> MudState -> Race
getRace i = view race . getPC i


-----


getRm :: Id -> MudState -> Rm
getRm i = view (rmTbl.ind i)


-----


getRmCoords :: Id -> MudState -> RmCoords
getRmCoords i = view rmCoords . getRm i


-----


getRmId :: Id -> MudState -> Id
getRmId i = view rmId . getMob i


-----


getRmName :: Id -> MudState -> Text
getRmName i = view rmName . getRm i


-----


getRndmNamesTbl :: Id -> MudState -> RndmNamesTbl
getRndmNamesTbl i = view (rndmNamesMstrTbl.ind i)


-----


getSex :: Id -> MudState -> Sex
getSex i = view sex . getMob i


-----


getSexRace :: Id -> MudState -> (Sex, Race)
getSexRace i = getSex i &&& getRace i


-----


getSexRaceLvl :: Id -> MudState -> (Sex, Race, Lvl)
getSexRaceLvl i ms | (s, r) <- getSexRace i ms = (s, r, getLvl i ms)


-----


getSing :: Id -> MudState -> Sing
getSing i = view sing . getEnt i


-----


getSkillPts :: Id -> MudState -> SkillPts
getSkillPts i = view skillPts . getPC i


-----


getStomach :: Id -> MudState -> [StomachCont]
getStomach i = view stomach . getMob i


-----


getTeleLinkTbl :: Id -> MudState -> TeleLinkTbl
getTeleLinkTbl i = view (teleLinkMstrTbl.ind i)


-----


getTempDesc :: Id -> MudState -> TempDesc
getTempDesc i = view tempDesc . getMob i


-----


getType :: Id -> MudState -> Type
getType i = view (typeTbl.ind i)


-----


getVessel :: Id -> MudState -> Vessel
getVessel i = view (vesselTbl.ind i)


-----


getVesselCont :: Id -> MudState -> Maybe VesselCont
getVesselCont i = view vesselCont . getVessel i


-----


getWpn :: Id -> MudState -> Wpn
getWpn i = view (wpnTbl.ind i)


-----


getWpnSub :: Id -> MudState -> WpnSub
getWpnSub i = view wpnSub . getWpn i


-----


getWritMessage :: Id -> MudState -> Maybe (Text, Lang)
getWritMessage i = view writMessage . getWritable i


-----


getWritRecip :: Id -> MudState -> Maybe Sing
getWritRecip i = view writRecip . getWritable i


-----


getWritable :: Id -> MudState -> Writable
getWritable i = view (writableTbl.ind i)


-- ==================================================
-- Entity flag getters:


entFlagHelper :: EntFlags -> Id -> MudState -> Bool
entFlagHelper flag i = getEntFlag flag . getEnt i


-----


isInvis :: Ent -> Bool
isInvis = getEntFlag IsInvis


isInvisId :: Id -> MudState -> Bool
isInvisId = entFlagHelper IsInvis


-- ==================================================
-- Object flag getters:


objFlagHelper :: ObjFlags -> Id -> MudState -> Bool
objFlagHelper flag i = getObjFlag flag . getObj i


-----


isBiodegradable :: Obj -> Bool
isBiodegradable = getObjFlag IsBiodegradable


isBiodegradableId :: Id -> MudState -> Bool
isBiodegradableId = objFlagHelper IsBiodegradable


-- ==================================================
-- Player flag getters:


plaFlagHelper :: PlaFlags -> Id -> MudState -> Bool
plaFlagHelper flag i = getPlaFlag flag . getPla i


-----


isAdmin :: Pla -> Bool
isAdmin = getPlaFlag IsAdmin


isAdminId :: Id -> MudState -> Bool
isAdminId = onPla isAdmin False


-----


isGmcp :: Pla -> Bool
isGmcp = getPlaFlag IsGmcp


isGmcpId :: Id -> MudState -> Bool
isGmcpId = onPla isGmcp False


-----


isIncognito :: Pla -> Bool
isIncognito = getPlaFlag IsIncognito


isIncognitoId :: Id -> MudState -> Bool
isIncognitoId = onPla isIncognito False


-----


isNotFirstAdminMsg :: Pla -> Bool
isNotFirstAdminMsg = getPlaFlag IsNotFirstAdminMsg


isNotFirstAdminMsgId :: Id -> MudState -> Bool
isNotFirstAdminMsgId = onPla isNotFirstAdminMsg True


-----


isNotFirstMobSay :: Pla -> Bool
isNotFirstMobSay = getPlaFlag IsNotFirstMobSay


isNotFirstModSayId :: Id -> MudState -> Bool
isNotFirstModSayId = onPla isNotFirstMobSay True


-----


isNotFirstSpiritCmdNotFound :: Pla -> Bool
isNotFirstSpiritCmdNotFound = getPlaFlag IsNotFirstSpiritCmdNotFound


isNotFirstSpiritCmdNotFoundId :: Id -> MudState -> Bool
isNotFirstSpiritCmdNotFoundId = onPla isNotFirstSpiritCmdNotFound True


-----


isShowingHp :: Pla -> Bool
isShowingHp = getPlaFlag IsShowingHp


isShowingHpId :: Id -> MudState -> Bool
isShowingHpId = plaFlagHelper IsShowingHp


-----


isShowingMp :: Pla -> Bool
isShowingMp = getPlaFlag IsShowingMp


isShowingMpId :: Id -> MudState -> Bool
isShowingMpId = plaFlagHelper IsShowingMp


-----


isShowingPp :: Pla -> Bool
isShowingPp = getPlaFlag IsShowingPp


isShowingPpId :: Id -> MudState -> Bool
isShowingPpId = plaFlagHelper IsShowingPp


-----


isShowingFp :: Pla -> Bool
isShowingFp = getPlaFlag IsShowingFp


isShowingFpId :: Id -> MudState -> Bool
isShowingFpId = plaFlagHelper IsShowingFp


-----


isSpirit :: Pla -> Bool
isSpirit = getPlaFlag IsSpirit


isSpiritId :: Id -> MudState -> Bool
isSpiritId = onPla isSpirit False


-----


isTunedAdmin :: Pla -> Bool
isTunedAdmin = getPlaFlag IsTunedAdmin


isTunedAdminId :: Id -> MudState -> Bool
isTunedAdminId = plaFlagHelper IsTunedAdmin


-----


isTunedQuestion :: Pla -> Bool
isTunedQuestion = getPlaFlag IsTunedQuestion


isTunedQuestionId :: Id -> MudState -> Bool
isTunedQuestionId = plaFlagHelper IsTunedQuestion
