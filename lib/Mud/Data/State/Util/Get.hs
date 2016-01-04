{-# LANGUAGE OverloadedStrings #-}

-- This module contains straightforward getter methods that do little or no calculation.

module Mud.Data.State.Util.Get where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Util.Misc

import Control.Arrow ((***))
import Control.Concurrent (ThreadId)
import Control.Lens (at, to, view)
import Control.Lens.Operators ((^.))
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


getArm :: Id -> MudState -> Arm
getArm i = view (armTbl.ind i)


-----


getArmSub :: Id -> MudState -> ArmSub
getArmSub i = view armSub . getArm i


-----


getCapacity :: Id -> MudState -> Vol
getCapacity i = view capacity . getCon i


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


getConnectTime :: Id -> MudState -> Maybe UTCTime
getConnectTime i = view connectTime . getPla i


-----


getCurrHostName :: Id -> MudState -> HostName
getCurrHostName i = view currHostName . getPla i


-----


getDx :: Id -> MudState -> Int
getDx i = view dx . getMob i


-----


getEnt :: Id -> MudState -> Ent
getEnt i = view (entTbl.ind i)


-----


getEntDesc :: Id -> MudState -> Text
getEntDesc i = view entDesc . getEnt i


-----


getEqMap :: Id -> MudState -> EqMap
getEqMap i = view (eqTbl.ind i)


-----


getExp :: Id -> MudState -> Exp
getExp i = view exp . getMob i


-----


getHand :: Id -> MudState -> Hand
getHand i = view hand . getMob i


-----


getHostMap :: Sing -> MudState -> Maybe HostMap
getHostMap s = view (hostTbl.at s)


-----


getHt :: Id -> MudState -> Int
getHt i = view ht . getMob i


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
getInvCoins i = (getInv i *** getCoins i) . dup


-----


getIsCloth :: Id -> MudState -> Bool
getIsCloth i = view isCloth . getCon i


-----


getLastRmId :: Id -> MudState -> Maybe Id
getLastRmId = onPla (view lastRmId) Nothing


-----


getLinked :: Id -> MudState -> [Sing]
getLinked = onPC (view linked) []


-----


getListenThreadId :: MudState -> ThreadId
getListenThreadId = reverseLookup Listen . view threadTbl


-----


getLogQueue :: Id -> MudState -> LogQueue
getLogQueue i = view (plaLogTbl.ind i.to snd)


-----


getMa :: Id -> MudState -> Int
getMa i = view ma . getMob i


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


getMobRmInv :: Id -> MudState -> Inv
getMobRmInv i ms = let ri = getRmId i ms in getInv ri ms


-----


getMobRmInvCoins :: Id -> MudState -> (Inv, Coins)
getMobRmInvCoins i ms = let ri = getRmId i ms in getInvCoins ri ms


-----


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = view (msgQueueTbl.ind i)


-----


getMsgQueueColumns :: Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i = (getMsgQueue i *** getColumns i) . dup


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


getPC :: Id -> MudState -> PC
getPC i = view (pcTbl.ind i)


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
getPeepersPeeping i = (getPeepers i *** getPeeping i) . dup


-----


getPla :: Id -> MudState -> Pla
getPla i = view (plaTbl.ind i)


-----


getPossessing :: Id -> MudState -> Maybe Id
getPossessing i = view possessing . getPla i


-----


getPossessor :: Id -> MudState -> Maybe Id
getPossessor i = view possessor . getNpc i


-----


getPs :: Id -> MudState -> Int
getPs i = view ps . getMob i


-----


getRace :: Id -> MudState -> Race
getRace i = view race . getPC i


-----


getRm :: Id -> MudState -> Rm
getRm i = view (rmTbl.ind i)


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
getSexRace i = (getSex i *** getRace i) . dup


-----


getSing :: Id -> MudState -> Sing
getSing i = view sing . getEnt i


-----


getSt :: Id -> MudState -> Int
getSt i = view st . getMob i


-----


getTeleLinkTbl :: Id -> MudState -> TeleLinkTbl
getTeleLinkTbl i = view (teleLinkMstrTbl.ind i)


-----


getType :: Id -> MudState -> Type
getType i = view (typeTbl.ind i)


-----


getVol :: Id -> MudState -> Vol
getVol i = view vol . getObj i


-----


getWeight :: Id -> MudState -> Int
getWeight i = view weight . getObj i


-----


getWpn :: Id -> MudState -> Wpn
getWpn i = view (wpnTbl.ind i)


-----


getWpnSub :: Id -> MudState -> WpnSub
getWpnSub i = view wpnSub . getWpn i


-----


getXps :: Id -> MudState -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
getXps i ms = let m   = getMob i ms
                  hps = (m^.curHp, m^.maxHp)
                  mps = (m^.curMp, m^.maxMp)
                  pps = (m^.curPp, m^.maxPp)
                  fps = (m^.curFp, m^.maxFp)
              in (hps, mps, pps, fps)


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
-- Player flag getters:


plaFlagHelper :: PlaFlags -> Id -> MudState -> Bool
plaFlagHelper flag i = getPlaFlag flag . getPla i


-----


isAdmin :: Pla -> Bool
isAdmin = getPlaFlag IsAdmin


isAdminId :: Id -> MudState -> Bool
isAdminId = onPla isAdmin False


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


isNotFirstLook :: Pla -> Bool
isNotFirstLook = getPlaFlag IsNotFirstLook


isNotFirstLookId :: Id -> MudState -> Bool
isNotFirstLookId = onPla isNotFirstLook True


-----


isNotFirstMobSay :: Pla -> Bool
isNotFirstMobSay = getPlaFlag IsNotFirstMobSay


isNotFirstModSayId :: Id -> MudState -> Bool
isNotFirstModSayId = onPla isNotFirstMobSay True


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
