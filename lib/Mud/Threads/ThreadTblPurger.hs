{-# LANGUAGE MonadComprehensions, OverloadedStrings, TypeFamilies, ViewPatterns #-}

module Mud.Threads.ThreadTblPurger ( purgeThreadTbls
                                   , threadThreadTblPurger ) where

import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Misc
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Threads.Misc
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
import           Mud.Util.Operators

import           Control.Concurrent.Async (asyncThreadId, poll)
import           Control.Exception.Lifted (catch, handle)
import           Control.Lens (at, views)
import           Control.Lens.Operators ((.~), (&))
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap.Strict as IM (assocs)
import qualified Data.Map.Strict as M (elems, keys)
import           Data.Text (Text)
import           GHC.Conc (ThreadStatus(..), threadStatus)
import           GHC.Stack (HasCallStack)

logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.ThreadTblPurger"

-- ==================================================

threadThreadTblPurger :: HasCallStack => MudStack ()
threadThreadTblPurger = handle (threadExHandler Nothing "thread table purger") $ do
    setThreadType ThreadTblPurger
    logNotice "threadThreadTblPurger" "thread table purger started."
    let loop = sequence_ [ liftIO . delaySecs $ threadTblPurgerDelay, purgeThreadTbls ]
    forever loop `catch` die Nothing "thread table purger"

purgeThreadTbls :: HasCallStack => MudStack ()
purgeThreadTbls = do logNotice "purgeThreadTbls" "purging the thread tables."
                     sequence_ [ purgePlaLogTbl, purgeTalkAsyncTbl, purgeThreadTbl ]

purgePlaLogTbl :: HasCallStack => MudStack ()
purgePlaLogTbl = views plaLogTbl (unzip . IM.assocs) <$> getState >>= \(is, map fst -> asyncs) -> do
    zipped <- [ zip is statuses | statuses <- liftIO . mapM poll $ asyncs ]
    tweak $ \ms -> let plt = views plaLogTbl (flip (foldr purger) zipped) ms in ms & plaLogTbl .~ plt
  where
    purger (_poo, Nothing) = id
    purger (i,    _poo   ) = at i .~ Nothing

purgeTalkAsyncTbl :: HasCallStack => MudStack ()
purgeTalkAsyncTbl = views talkAsyncTbl M.elems <$> getState >>= \asyncs -> do
    zipped <- [ zip asyncs statuses | statuses <- liftIO . mapM poll $ asyncs ]
    tweak $ \ms -> let tat = views talkAsyncTbl (flip (foldr purger) zipped) ms in ms & talkAsyncTbl .~ tat
  where
    purger (_poo,                Nothing) = id
    purger (asyncThreadId -> ti, _poo   ) = at ti .~ Nothing

purgeThreadTbl :: HasCallStack => MudStack ()
purgeThreadTbl = views threadTbl M.keys <$> getState >>= \threadIds -> do
    zipped <- [ zip threadIds statuses | statuses <- liftIO . mapM threadStatus $ threadIds ]
    tweak $ \ms -> let tt = views threadTbl (flip (foldr purger) zipped) ms in ms & threadTbl .~ tt
  where
    purger (ti, status) = status == ThreadFinished ? at ti .~ Nothing :? id
