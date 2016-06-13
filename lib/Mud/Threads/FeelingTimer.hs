{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.FeelingTimer ( startFeeling
                                , threadFeelingTimer ) where

-- import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
-- import Mud.Data.State.Util.Get
-- import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Operators
-- import Mud.Util.Quoting
import Mud.Util.Text
-- import qualified Mud.Misc.Logging as L (logNotice, logPla)
import Mud.Data.State.Util.Misc

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (newTMQueueIO, tryReadTMQueue, writeTMQueue)
-- import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception.Lifted (catch)
import Control.Monad ((>=>), join)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import System.Time.Utils (renderSecs)
import qualified Data.Map.Lazy as M (delete, insert, lookup)
import Control.Lens.Operators ((%~), (&), (.~), (^.))


default (Int)


-----


-- logNotice :: Text -> Text -> MudStack ()
-- logNotice = L.logNotice "Mud.Threads.FeelingTimer"
--
--
-- logPla :: Text -> Id -> Text -> MudStack ()
-- logPla = L.logPla "Mud.Threads.FeelingTimer"


-- ==================================================


startFeeling :: Id -> Maybe EffectFeeling -> FeelingVal -> MudStack ()
startFeeling _ Nothing                        _  = unit
startFeeling i (Just (EffectFeeling tag dur)) fv = do
    q <- liftIO newTMQueueIO
    a <- runAsync . threadFeelingTimer i tag dur $ q
    let resetTimer = liftIO . atomically . flip writeTMQueue ResetTimer
        helper fm  = case M.lookup tag fm of Nothing               -> (M.insert tag (Feeling fv q a) fm, unit         )
                                             Just (Feeling _ q' _) -> (fm                              , resetTimer q')
    join (modifyState $ \ms -> let fm       = ms^.mobTbl.ind i.feelingMap
                                   (fm', f) = helper fm
                               in (ms & mobTbl . ind i . feelingMap .~ fm', f))


threadFeelingTimer :: Id -> FeelingTag -> Seconds -> TimerQueue -> MudStack ()
threadFeelingTimer i tag dur tq = sequence_ [ setThreadType . FeelingTimer $ i
                                            , loop 0 `catch` threadExHandler ("feeling timer " <> showText i) ]
  where
    loop secs = do
        liftIO . threadDelay $ 1 * 10 ^ 6
        tq |&| liftIO . atomically . tryReadTMQueue >=> \case
          Just Nothing | secs >= dur -> tweak $ mobTbl.ind i.feelingMap %~ (tag `M.delete`)
                       | otherwise   -> loop . succ $ secs
          Just (Just ResetTimer)     -> loop 0
          Nothing                    -> unit
