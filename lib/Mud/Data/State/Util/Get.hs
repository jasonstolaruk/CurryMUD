module Mud.Data.State.Util.Get where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Util.Misc
import Mud.Util.Operators

import Control.Arrow ((***))
import Control.Concurrent (ThreadId)
import Control.Lens (at, to, view, views)
import Control.Lens.Operators ((^.))
import Data.Monoid (Sum(..))
import Data.Time (UTCTime)
import Network (HostName)
import qualified Data.IntMap.Lazy as IM (filter, foldr, keys, toList)
import qualified Data.Map.Lazy as M (keys)
import qualified Data.Text as T


getAdminIds :: MudState -> Inv
getAdminIds = getAdminIdsHelper (const True)


getAdminIdsHelper :: (Pla -> Bool) -> MudState -> Inv
getAdminIdsHelper f = IM.keys . IM.filter (uncurry (&&) . (getPlaFlag IsAdmin *** f) . dup) . view plaTbl


-----


getArm :: Id -> MudState -> Arm
getArm i = view (armTbl.ind i)


-----


getArmSub :: Id -> MudState -> ArmSub
getArmSub i = view armSub . getArm i


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
getColumns i = view columns . getPla i


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


getEntDesc :: Id -> MudState -> T.Text
getEntDesc i = view entDesc . getEnt i


-----


getEqMap :: Id -> MudState -> EqMap
getEqMap i = view (eqTbl.ind i)


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
getInterp i = view interp . getPla i


-----


getIntroduced :: Id -> MudState -> [Sing]
getIntroduced i = view introduced . getPC i


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


getIdForPCSing :: Sing -> MudState -> Id
getIdForPCSing s ms = let [(i, _)] = views entTbl (IM.toList . IM.filter (views sing (== s))) ms in i


-----


getIq :: Id -> MudState -> Int
getIq i = view iq . getMob i


-----


getLastRmId :: Id -> MudState -> Maybe Id
getLastRmId i = view lastRmId . getPla i


-----


getLinked :: Id -> MudState -> [Sing]
getLinked i = view linked . getPC i


-----


getListenThreadId :: MudState -> ThreadId
getListenThreadId = reverseLookup Listen . view threadTbl


-----


getLogQueue :: Id -> MudState -> LogQueue
getLogQueue i = view (plaLogTbl.ind i.to snd)


-----


getLoggedInAdminIds :: MudState -> Inv
getLoggedInAdminIds = getAdminIdsHelper isLoggedIn


isLoggedIn :: Pla -> Bool
isLoggedIn = views lastRmId ((()#) . (Sum <$>))


-----


getLoggedInPlaIds :: MudState ->  Inv
getLoggedInPlaIds = views plaTbl (IM.keys . IM.filter (uncurry (&&) . (isLoggedIn *** not . getPlaFlag IsAdmin) . dup))


-----


-- TODO: Move.
type Lvl = Int


getLvl :: Id -> MudState -> Lvl
getLvl _ _ = 0 -- TODO


-----


getMob :: Id -> MudState -> Mob
getMob i = view (mobTbl.ind i)


-----


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = view (msgQueueTbl.ind i)


-----


getMsgQueueColumns :: Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i = (getMsgQueue i *** getColumns i) . dup


-----


getNonIncogLoggedInAdminIds :: MudState -> Inv
getNonIncogLoggedInAdminIds ms =
    let adminIds = getLoggedInAdminIds ms
    in [ adminId | adminId <- adminIds, not . getPlaFlag IsIncognito . getPla adminId $ ms ]


-----


getNonIncogInv :: Id -> MudState -> Inv
getNonIncogInv i ms = filter notIncog . getInv i $ ms
  where
    notIncog targetId | getType targetId ms /= PCType                       = True
                      | not . getPlaFlag IsIncognito . getPla targetId $ ms = True
                      | otherwise                                           = False


-----


getNonIncogInvCoins :: Id -> MudState -> (Inv, Coins)
getNonIncogInvCoins i = (getNonIncogInv i *** getCoins i) . dup


-----


getObj :: Id -> MudState -> Obj
getObj i = view (objTbl.ind i)


-----


getPC :: Id -> MudState -> PC
getPC i = view (pcTbl.ind i)


-----


getPCChans :: Id -> MudState -> [Chan]
getPCChans i ms = views chanTbl (IM.foldr helper []) ms
  where
    helper chan acc = getSing i ms `elem` (chan^.chanConnTbl.to M.keys) ? (chan : acc) :? acc


-----


getPCRm :: Id -> MudState -> Rm
getPCRm i ms = let ri = getRmId i ms in getRm ri ms


-----


getPCRmCoins :: Id -> MudState -> Coins
getPCRmCoins i ms = let ri = getRmId i ms in getCoins ri ms


-----


getPCRmInv :: Id -> MudState -> Inv
getPCRmInv i ms = let ri = getRmId i ms in getInv ri ms


-----


getPCRmInvCoins :: Id -> MudState -> (Inv, Coins)
getPCRmInvCoins i ms = let ri = getRmId i ms in getInvCoins ri ms


-----


getPCRmNonIncogInvCoins :: Id -> MudState -> (Inv, Coins)
getPCRmNonIncogInvCoins i ms = let ri = getRmId i ms in getNonIncogInvCoins ri ms


-----


getPageLines :: Id -> MudState -> Int
getPageLines i = view pageLines . getPla i


-----


getPeepers :: Id -> MudState -> Inv
getPeepers i = view peepers . getPla i


-----


getPeeping :: Id -> MudState -> Inv
getPeeping i = view peeping . getPla i


-----


getPeepersPeeping :: Id -> MudState -> (Inv, Inv)
getPeepersPeeping i = (getPeepers i *** getPeeping i) . dup


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


getRndmNamesTbl :: Id -> MudState -> RndmNamesTbl
getRndmNamesTbl i = view (rndmNamesMstrTbl.ind i)


-----


getSex :: Id -> MudState -> Sex
getSex i = view sex . getMob i


-----


getSexRace :: Id -> MudState -> (Sex, Race)
getSexRace i = (getSex i *** getRace i) . dup


-----


getSexRaceLvl :: Id -> MudState -> (Sex, Race, Lvl)
getSexRaceLvl i ms | (s, r) <- getSexRace i ms = (s, r, getLvl i ms)


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


getWeight :: Id -> MudState -> Int
getWeight i = view weight . getObj i


-----


getWpn :: Id -> MudState -> Wpn
getWpn i = view (wpnTbl.ind i)


-----


getWpnSub :: Id -> MudState -> WpnSub
getWpnSub i = view wpnSub . getWpn i
