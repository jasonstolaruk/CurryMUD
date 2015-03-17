{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Get where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData

import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((^.))
import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as IM (keys)
import qualified Data.Text as T


getAdminIds :: MudState -> Inv
getAdminIds (view plaTbl -> pt) = [ i | i <- IM.keys pt, getPlaFlag IsAdmin $ pt ! i ]


getCoins :: Id -> MudState -> Coins
getCoins i = views coinsTbl (! i)


getColumns :: Id -> MudState -> Cols
getColumns i ms = getPla i ms ^.columns


getEnt :: Id -> MudState -> Ent
getEnt i = views entTbl (! i)


getEntDesc :: Id -> MudState -> T.Text
getEntDesc i ms = getEnt i ms ^.entDesc


getEqMap :: Id -> MudState -> EqMap
getEqMap i = views eqTbl (! i)


getIntroduced :: Id -> MudState -> [Sing]
getIntroduced i ms = getPC i ms ^.introduced


getInv :: Id -> MudState -> Inv
getInv i = views invTbl (! i)


getInvCoins :: Id -> MudState -> (Inv, Coins)
getInvCoins i ms = (getInv i ms, getCoins i ms)


getLogQueue :: Id -> MudState -> LogQueue
getLogQueue i = views plaLogTbl (snd . (! i))


getMob :: Id -> MudState -> Mob
getMob i = views mobTbl (! i)


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = views msgQueueTbl (! i)


getMsgQueueColumns :: Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i ms = (getMsgQueue i ms, getColumns i ms)


getPC :: Id -> MudState -> PC
getPC i = views pcTbl (! i)


getPCRm :: Id -> MudState -> Rm
getPCRm i ms = let ri = getRmId i ms in getRm ri ms


getPCRmCoins :: Id -> MudState -> Coins
getPCRmCoins i ms = let ri = getRmId i ms in getCoins ri ms


getPCRmInv :: Id -> MudState -> Inv
getPCRmInv i ms = let ri = getRmId i ms in getInv ri ms


getPCRmInvCoins :: Id -> MudState -> (Inv, Coins)
getPCRmInvCoins i ms = let ri = getRmId i ms in getInvCoins ri ms


getPageLines :: Id -> MudState -> Int
getPageLines i ms = getPla i ms ^.pageLines


getPeepers :: Id -> MudState -> Inv
getPeepers i ms = getPla i ms ^.peepers


getPeepersPeeping :: Id -> MudState -> (Inv, Inv)
getPeepersPeeping i ms = let p = getPla i ms in (p^.peepers, p^.peeping)


getPeeping :: Id -> MudState -> Inv
getPeeping i ms = getPla i ms ^.peeping


getPla :: Id -> MudState -> Pla
getPla i = views plaTbl (! i)


getRace :: Id -> MudState -> Race
getRace i ms = getPC i ms ^.race


getRm :: Id -> MudState -> Rm
getRm i = views rmTbl (! i)


getRmId :: Id -> MudState -> Id
getRmId i ms = getPC i ms ^.rmId


getSex :: Id -> MudState -> Sex
getSex i ms = getMob i ms ^.sex


getSexRace :: Id -> MudState -> (Sex, Race)
getSexRace i ms = (getSex i ms, getRace i ms)


getSing :: Id -> MudState -> Sing
getSing i ms = getEnt i ms ^.sing


getType :: Id -> MudState -> Type
getType i = views typeTbl (! i)
