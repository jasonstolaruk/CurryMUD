{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Get where

import Mud.Data.Misc
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData

import Control.Lens.Getter (view, views)
import Control.Lens.Operators ((^.))
import Data.IntMap.Lazy ((!))
import qualified Data.IntMap.Lazy as IM (keys)


getAdminIds :: MudState -> Inv
getAdminIds (view plaTbl -> pt) = [ i | i <- IM.keys pt, getPlaFlag IsAdmin $ pt ! i ]


getColumns :: Id -> MudState -> Cols
getColumns i ms = (getPla i ms)^.columns


getEnt :: Id -> MudState -> Ent
getEnt i = views entTbl (! i)


getIntroduced :: Id -> MudState -> [Sing]
getIntroduced i ms = (getPC i ms)^.introduced


getInv :: Id -> MudState -> Inv
getInv i = views invTbl (! i)


getMob :: Id -> MudState -> Mob
getMob i = views mobTbl (! i)


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = views msgQueueTbl (! i)


getMsgQueueColumns :: Id -> MudState -> (MsgQueue, Cols)
getMsgQueueColumns i ms = (getMsgQueue i ms, getColumns i ms)


getPC :: Id -> MudState -> PC
getPC i = views pcTbl (! i)


getPCRmInv :: Id -> MudState -> Inv
getPCRmInv i ms = let ri = getRmId i ms in getInv ri ms


getPla :: Id -> MudState -> Pla
getPla i = views plaTbl (! i)


getRace :: Id -> MudState -> Race
getRace i ms = (getPC i ms)^.race


getRmId :: Id -> MudState -> Id
getRmId i ms = (getPC i ms)^.rmId


getSex :: Id -> MudState -> Sex
getSex i ms = (getMob i ms)^.sex


getSexRace :: Id -> MudState -> (Sex, Race)
getSexRace i ms = (getSex i ms, getRace i ms)


getSing :: Id -> MudState -> Sing
getSing i ms = (getEnt i ms)^.sing


getType :: Id -> MudState -> Type
getType i = views typeTbl (! i)
