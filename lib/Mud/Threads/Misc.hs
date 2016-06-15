{-# LANGUAGE DeriveDataTypeable, LambdaCase, OverloadedStrings, TupleSections #-}

module Mud.Threads.Misc ( concurrentTree
                        , dbExHandler
                        , die
                        , dieSilently
                        , fileIOExHandler
                        , findBiodegradableIds
                        , findNpcIds
                        , onNewThread
                        , plaThreadExHandler
                        , PlsDie(..)
                        , racer
                        , runAsync
                        , setThreadType
                        , stopTimer
                        , threadExHandler
                        , throwDeath
                        , throwToListenThread
                        , throwWait ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Misc.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logNotice, logPla)

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Async (Async, async, asyncThreadId, concurrently, race_, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (closeTMQueue)
import Control.Exception (AsyncException(..), Exception, IOException, SomeException, fromException, toException)
import Control.Exception.Lifted (throwTo)
import Control.Lens (at, views)
import Control.Lens.Operators ((?~))
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Conc (labelThread)
import qualified Data.IntMap.Lazy as IM (filter, keys)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)


logExMsg :: Text -> Text -> SomeException -> MudStack ()
logExMsg = L.logExMsg "Mud.Threads.Misc"


logIOEx :: Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Threads.Misc"


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.Misc"


logPla :: Text -> Id -> Text -> MudStack ()
logPla = L.logPla "Mud.Threads.Misc"


-- ==================================================


data PlsDie = PlsDie deriving (Show, Typeable)


instance Exception PlsDie


-----


concurrentTree :: [IO a] -> IO [a]
concurrentTree = foldr helper (return [])
  where
    helper ioa ioas = uncurry (:) <$> concurrently ioa ioas


-----


dbExHandler :: Text -> SomeException -> MudStack ()
dbExHandler fn e =
    logExMsg "dbExHandler" (rethrowExMsg $ "during a database operation in " <> dblQuote fn) e >> throwToListenThread e


-----


die :: Maybe Id -> Text -> PlsDie -> MudStack ()
die mi threadName = const . f $ "the " <> threadName <> " thread is dying."
  where
    f = maybe (logNotice "die") (logPla "die") mi


dieSilently :: PlsDie -> MudStack ()
dieSilently = const unit


-----


fileIOExHandler :: Text -> IOException -> MudStack ()
fileIOExHandler fn e = do
    logIOEx fn e
    let rethrow = throwToListenThread . toException $ e
    unless (any (e |&|) [ isAlreadyInUseError, isDoesNotExistError, isPermissionError ]) rethrow


-----


findBiodegradableIds :: MudState -> Inv
findBiodegradableIds = views objTbl (IM.keys . IM.filter isBiodegradable)


-----


findNpcIds :: MudState -> Inv
findNpcIds = views typeTbl (IM.keys . IM.filter (== NpcType))


-----


onNewThread :: MudStack () -> MudStack ()
onNewThread a = onEnv $ liftIO . void . forkIO . runReaderT a


-----


plaThreadExHandler :: Text -> Id -> SomeException -> MudStack ()
plaThreadExHandler threadName i e
  | Just ThreadKilled <- fromException e = closePlaLog i
  | otherwise                            = threadExHandler threadName e


threadExHandler :: Text -> SomeException -> MudStack ()
threadExHandler threadName e = do
    logExMsg "threadExHandler" (rethrowExMsg $ "on " <> threadName <> " thread") e
    throwToListenThread e


-----


racer :: MudData -> MudStack () -> MudStack () -> MudStack ()
racer md a b = liftIO . race_ (runReaderT a md) . runReaderT b $ md


-----


runAsync :: MudStack () -> MudStack (Async ())
runAsync f = onEnv $ liftIO . async . runReaderT f


-----


setThreadType :: ThreadType -> MudStack ()
setThreadType threadType = do
    ti <- liftIO $ myThreadId >>= \ti -> labelThread ti (show threadType) >> return ti
    tweak $ threadTbl.at ti ?~ threadType


-----


stopTimer :: TimerQueue -> MudStack ()
stopTimer = liftIO . atomically . closeTMQueue


-----


throwDeath :: Async () -> MudStack ()
throwDeath a = throwTo (asyncThreadId a) PlsDie


-----


throwToListenThread :: SomeException -> MudStack ()
throwToListenThread e = flip throwTo e . getListenThreadId =<< getState


-----


throwWait :: Async () -> MudStack ()
throwWait a = do { throwDeath a; liftIO . void . wait $ a }
