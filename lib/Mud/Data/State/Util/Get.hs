module Mud.Data.State.Util.Get where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData

import Control.Lens.Getter (views)
import Control.Lens.Operators ((^.))
import Data.IntMap.Lazy ((!))


getColumns :: Id -> MudState -> Cols
getColumns i ms = (getPla i ms)^.columns


getEnt :: Id -> MudState -> Ent
getEnt i = views entTbl (! i)


getIntroduced :: Id -> MudState -> [Sing]
getIntroduced i ms = (getPC i ms)^.introduced


getMob :: Id -> MudState -> Mob
getMob i = views mobTbl (! i)


getMsgQueue :: Id -> MudState -> MsgQueue
getMsgQueue i = views msgQueueTbl (! i)


getPC :: Id -> MudState -> PC
getPC i = views pcTbl (! i)


getPla :: Id -> MudState -> Pla
getPla i = views plaTbl (! i)


getRace :: Id -> MudState -> Race
getRace i ms = (getPC i ms)^.race


getSex :: Id -> MudState -> Sex
getSex i ms = (getMob i ms)^.sex


getSexRace :: Id -> MudState -> (Sex, Race)
getSexRace i ms = (getSex i ms, getRace i ms)


getSing :: Id -> MudState -> Sing
getSing i ms = (getEnt i ms)^.sing


getType :: Id -> MudState -> Type
getType i = views typeTbl (! i)
