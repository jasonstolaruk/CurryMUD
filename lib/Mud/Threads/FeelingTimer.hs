{-# LANGUAGE OverloadedStrings #-}

module Mud.Threads.FeelingTimer ( startFeeling
                                , stopFeelings ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Threads.Misc
import Mud.TopLvlDefs.Seconds
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Mud.Misc.Logging as L (logExMsg, logPla)

import Control.Concurrent.Async (cancel, wait)
import Control.Exception (AsyncException(..), SomeException, fromException)
import Control.Exception.Lifted (catch, handle)
import Control.Lens.Operators ((.~), (%~))
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Data.Map.Strict as M (delete, elems, empty, insert, lookup)
import qualified Data.Text as T


logExMsg :: Text -> Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.FeelingTimer"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.FeelingTimer"


-- ==================================================


startFeeling :: HasCallStack => Id -> EffectFeeling -> FeelingVal -> MudStack ()
startFeeling i (EffectFeeling tag dur) val = handle (threadStarterExHandler i fn . Just $ tag) $ do
    let f = liftIO . ((>>) <$> cancel <*> wait) . feelingAsync
    maybeVoid f =<< M.lookup tag . getFeelingMap i <$> getState
    feel <- Feeling val dur <$> runAsync (threadFeelingTimer i tag dur)
    tweak $ mobTbl.ind i.feelingMap %~ M.insert tag feel
    logPla fn i . T.concat $ [ "started feeling with tag ", dblQuote tag, ": ", pp feel, "." ]
  where
    fn = "startFeeling"


threadFeelingTimer :: HasCallStack => Id -> FeelingTag -> Seconds -> MudStack ()
threadFeelingTimer i tag dur = sequence_ [ setThreadType . FeelingTimer $ i, loop 0 ] `catch` exHandler
  where
    loop secs | secs >= dur = sequence_ [ logHelper "is expiring.", tweak $ mobTbl.ind i.feelingMap %~ (tag `M.delete`) ]
              | otherwise   = sequence_ [ liftIO . delaySecs $ 1, loop . succ $ secs ]
    exHandler :: SomeException -> MudStack ()
    exHandler e = case fromException e of
      Just ThreadKilled -> logHelper "has been killed."
      _                 -> do t <- descSingId i <$> getState
                              logExMsg fn (T.concat [ "exception caught on ", name, " thread for ", t ]) e
    logHelper msg = logPla fn i $ name |<>| msg
    fn            = "threadFeelingTimer"
    name          = "feeling timer " <> dblQuote tag


-----


-- To stop a single feeling, cancel the async and remove the entry in the mob's feeling map. See "stopFeeling" in
-- module "Mud.Threads.Effect".
stopFeelings :: HasCallStack => Id -> MudStack ()
stopFeelings i = sequence_ [ getFeelingMap i <$> getState >>= mapM_ (liftIO . cancel . feelingAsync) . M.elems
                           , tweak $ mobTbl.ind i.feelingMap .~ M.empty ]
