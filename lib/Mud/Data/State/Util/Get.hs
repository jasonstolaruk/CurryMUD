module Mud.Data.State.Util.Get where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Util.Misc

import Control.Arrow ((***))
import Control.Concurrent (ThreadId)
import Control.Lens (to, view, views)
import Data.Maybe (isNothing)
import Network (HostName)
import qualified Data.IntMap.Lazy as IM (filter, keys)
import qualified Data.Text as T


getArm :: Id -> MudState -> Arm
getArm i = view (armTbl.ind i)


getArmSub :: Id -> MudState -> ArmSub
getArmSub i = view armSub . getArm i


-----


getCloth :: Id -> MudState -> Cloth
getCloth i = view (clothTbl.ind i)


-----


getCoins :: Id -> MudState -> Coins
getCoins i = view (coinsTbl.ind i)


-----


getColumns :: Id -> MudState -> Cols
getColumns i = view columns . getPla i


-----


getCon :: Id -> MudState -> Con
getCon i = view (conTbl.ind i)


-----


getEnt :: Id -> MudState -> Ent
getEnt i = view (entTbl.ind i)


getEntDesc :: Id -> MudState -> T.Text
getEntDesc i = view entDesc . getEnt i


-----


getEqMap :: Id -> MudState -> EqMap
getEqMap i = view (eqTbl.ind i)


-----


getHand :: Id -> MudState -> Hand
getHand i = view hand . getMob i


-----


getHostName :: Id -> MudState -> HostName
getHostName i = view hostName . getPla i


-----


getInterp :: Id -> MudState -> Maybe Interp
getInterp i = view interp . getPla i


-----


getIntroduced :: Id -> MudState -> [Sing]
getIntroduced i = view introduced . getPC i


-----


getInv :: Id -> MudState -> Inv
getInv i = view (invTbl.ind i)


getInvCoins :: Id -> MudState -> (Inv, Coins)
getInvCoins i = (getInv i *** getCoins i) . dup


getNonIncogInv :: Id -> MudState -> Inv
getNonIncogInv i ms = filter notIncog . getInv i $ ms
  where
    notIncog targetId | getType targetId ms /= PCType                       = True
                      | not . getPlaFlag IsIncognito . getPla targetId $ ms = True
                      | otherwise                                           = False


getNonIncogInvCoins :: Id -> MudState -> (Inv, Coins)
getNonIncogInvCoins i = (getNonIncogInv i *** getCoins i) . dup


-----


getIsCloth :: Id -> MudState -> Bool
getIsCloth i = view isCloth . getCon i


-----


getLastRmId :: Id -> MudState -> Maybe Id
getLastRmId i = view lastRmId . getPla i


-----


getListenThreadId :: MudState -> ThreadId
getListenThreadId = reverseLookup Listen . view threadTbl


-----


getLogQueue :: Id -> MudState -> LogQueue
getLogQueue i = view (plaLogTbl.ind i.to snd)


-----


getLoggedInAdminIds :: MudState -> Inv
getLoggedInAdminIds = IM.keys . IM.filter (\p -> getPlaFlag IsAdmin p && isLoggedIn p) . view plaTbl


isLoggedIn :: Pla -> Bool
isLoggedIn = views lastRmId isNothing


-----


getMob :: Id -> MudState -> Mob
getMob i = view (mobTbl.ind i)


-----


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = view (msgQueueTbl.ind i)


getMsgQueueColumns :: Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i = (getMsgQueue i *** getColumns i) . dup


-----


getObj :: Id -> MudState -> Obj
getObj i = view (objTbl.ind i)


-----


getPC :: Id -> MudState -> PC
getPC i = view (pcTbl.ind i)


getPCRm :: Id -> MudState -> Rm
getPCRm i ms = let ri = getRmId i ms in getRm ri ms


getPCRmCoins :: Id -> MudState -> Coins
getPCRmCoins i ms = let ri = getRmId i ms in getCoins ri ms


getPCRmInv :: Id -> MudState -> Inv
getPCRmInv i ms = let ri = getRmId i ms in getInv ri ms


getPCRmInvCoins :: Id -> MudState -> (Inv, Coins)
getPCRmInvCoins i ms = let ri = getRmId i ms in getInvCoins ri ms


-----


getPageLines :: Id -> MudState -> Int
getPageLines i = view pageLines . getPla i


-----


getPeepers :: Id -> MudState -> Inv
getPeepers i = view peepers . getPla i


getPeepersPeeping :: Id -> MudState -> (Inv, Inv)
getPeepersPeeping i = (getPeepers i *** getPeeping i) . dup


getPeeping :: Id -> MudState -> Inv
getPeeping i = view peeping . getPla i


-----


getPla :: Id -> MudState -> Pla
getPla i = view (plaTbl.ind i)


-----


getRace :: Id -> MudState -> Race
getRace i = view race . getPC i


-----


getRm :: Id -> MudState -> Rm
getRm i = view (rmTbl.ind i)


-----


getRmId :: Id -> MudState -> Id
getRmId i = view rmId . getPC i


-----


getSex :: Id -> MudState -> Sex
getSex i = view sex . getMob i


getSexRace :: Id -> MudState -> (Sex, Race)
getSexRace i = (getSex i *** getRace i) . dup


-----


getSing :: Id -> MudState -> Sing
getSing i = view sing . getEnt i


-----


getType :: Id -> MudState -> Type
getType i = view (typeTbl.ind i)


-----


getWpn :: Id -> MudState -> Wpn
getWpn i = view (wpnTbl.ind i)


getWpnSub :: Id -> MudState -> WpnSub
getWpnSub i = view wpnSub . getWpn i
