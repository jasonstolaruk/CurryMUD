{-# LANGUAGE DeriveDataTypeable, LambdaCase, OverloadedStrings, TupleSections #-}

module Mud.Threads.Misc ( concurrentTree
                        , dbExHandler
                        , die
                        , dieSilently
                        , fileIOExHandler
                        , onNewThread
                        , plaThreadExHandler
                        , PlsDie(..)
                        , racer
                        , runAsync
                        , setThreadType
                        , threadExHandler
                        , throwDeath
                        , throwToListenThread
                        , throwWait ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Misc.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logNotice, logPla)

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Async (Async, async, asyncThreadId, concurrently, race_, wait)
import Control.Exception (AsyncException(..), Exception, IOException, SomeException, fromException, toException)
import Control.Exception.Lifted (throwTo)
import Control.Lens (at, views)
import Control.Lens.Operators ((?~))
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Conc (labelThread)
import qualified Data.IntMap.Strict as IM (member)
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
concurrentTree = foldr helper mMempty
  where
    helper ioa ioas = uncurry (:) <$> concurrently ioa ioas


-----


dbExHandler :: Text -> SomeException -> MudStack ()
dbExHandler fn e =
    logExMsg "dbExHandler" (rethrowExMsg $ "during a database operation in " <> dblQuote fn) e >> throwToListenThread e


-----


die :: Maybe Id -> Text -> PlsDie -> MudStack () -- The "Maybe Id" parameter is used to determine logging.
die mi threadName = const . maybe (logNotice "die") (logPla "die") mi $ the threadName <> " thread is dying."


dieSilently :: PlsDie -> MudStack ()
dieSilently = const unit


-----


fileIOExHandler :: Text -> IOException -> MudStack ()
fileIOExHandler fn e = do logIOEx fn e
                          let rethrow = throwToListenThread . toException $ e
                          unless (any (e |&|) [ isAlreadyInUseError, isDoesNotExistError, isPermissionError ]) rethrow


-----


onNewThread :: Fun -> MudStack () -- Generally speaking, if you do anything on a new thread requiring a player to be
                                  -- logged in, you should first check that the player is in fact still logged in.
onNewThread f = liftIO . void . forkIO . runReaderT f =<< ask


-----


plaThreadExHandler :: Id -> Text -> SomeException -> MudStack ()
plaThreadExHandler i threadName e | Just ThreadKilled <- fromException e = closePlaLog i
                                  | otherwise                            = threadExHandler (Just i) threadName e


threadExHandler :: Maybe Id -> Text -> SomeException -> MudStack () -- The "Maybe Id" parameter is used to decorate the thread name.
threadExHandler mi threadName e = f >>= \threadName' -> do
    logExMsg "threadExHandler" (rethrowExMsg $ "on " <> threadName' <> " thread") e
    throwToListenThread e
  where
    f = case mi of Nothing -> return threadName
                   Just i  -> getState >>= \ms -> let t | views entTbl (i `IM.member`) ms = descSingId i ms
                                                        | otherwise                       = showText i
                                                  in return $ threadName |<>| t


-----


racer :: MudData -> Fun -> Fun -> MudStack ()
racer md a b = liftIO . race_ (runReaderT a md) . runReaderT b $ md


-----


runAsync :: Fun -> MudStack (Async ())
runAsync f = liftIO . async . runReaderT f =<< ask


-----


setThreadType :: ThreadType -> MudStack ()
setThreadType threadType = do ti <- liftIO $ myThreadId >>= \ti -> labelThread ti (show threadType) >> return ti
                              tweak $ threadTbl.at ti ?~ threadType


-----


throwDeath :: Async () -> MudStack ()
throwDeath a = throwTo (asyncThreadId a) PlsDie


-----


throwToListenThread :: SomeException -> MudStack ()
throwToListenThread e = maybeVoid (`throwTo` e) . getListenThreadId =<< getState


-----


throwWait :: Async () -> MudStack ()
throwWait a = sequence_ [ throwDeath a, liftIO . void . wait $ a ]
