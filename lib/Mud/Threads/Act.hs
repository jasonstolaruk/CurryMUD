{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.Act ( drinkAct
                       , startAct ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice, logPla)

--import Control.Concurrent (threadDelay)
import Control.Exception (fromException)
import Control.Exception.Lifted (handle)
import Control.Lens (at)
import Control.Lens.Operators ((?~))
import Control.Monad (when)
--import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)


default (Int)


-----


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Act"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Act"


-- ==================================================


startAct :: Id -> ActType -> (Id -> MudStack ()) -> MudStack ()
startAct i actType f = getState >>= \ms -> do
    when (getType i ms == PCType) . logPla  "startAct" i $ "starting act: " <> pp actType
    a <- runAsync . threadAct i actType $ f
    tweak $ mobTbl.ind i.actMap.at actType ?~ a


threadAct :: Id -> ActType -> (Id -> MudStack ()) -> MudStack ()
threadAct i actType f = handle actExHandler $ do
    setThreadType $ case actType of Drinking -> DrinkingThread i
                                    Eating   -> EatingThread   i
                                    Moving   -> MovingThread   i
    f i
  where
    actExHandler e | Just PlsDie <- fromException e = undefined
                   | otherwise                      = threadExHandler tn e
    tn = quoteWith' (pp actType, showText i) " "


drinkAct :: Id -> Id -> MudStack ()
drinkAct targetId i = getState >>= \ms ->
    let (mq, cols) = getMsgQueueColumns i ms
        s          = getSing targetId ms
    in undefined -- TODO
