{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Mud.Threads.Misc ( PlsDie(..)
                        , concurrentTree
                        , dbExHandler
                        , die
                        , dieSilently
                        , fileIOExHandler
                        , onNewThread
                        , plaThreadExHandler
                        , racer
                        , runAsync
                        , runEffectFun
                        , setThreadType
                        , threadExHandler
                        , threadStarterExHandler
                        , throwDeath
                        , throwToListenThread
                        , throwWait ) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Misc
import qualified Mud.Misc.Logging as L (logExMsg, logIOEx, logNotice, logPla)
import           Mud.Misc.Logging hiding (logExMsg, logIOEx, logNotice, logPla)
import           Mud.TopLvlDefs.Seconds
import qualified Mud.Util.Misc as U (blowUp)
import           Mud.Util.Misc hiding (blowUp)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Concurrent (forkIO, myThreadId)
import           Control.Concurrent.Async (Async, async, asyncThreadId, concurrently, race_, wait)
import           Control.Exception (AsyncException(..), Exception, IOException, SomeException, fromException, toException)
import           Control.Exception.Lifted (throwTo)
import           Control.Lens (at, views)
import           Control.Lens.Operators ((?~))
import           Control.Monad (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, runReaderT)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           GHC.Conc (labelThread)
import           GHC.Stack (HasCallStack)
import qualified Data.IntMap.Strict as IM (member)
import qualified Data.Text as T
import           System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Threads.Misc"


-----


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


concurrentTree :: HasCallStack => [IO a] -> IO [a]
concurrentTree = foldr (\a -> fmap (uncurry (:)) . concurrently a) mMempty


-----


dbExHandler :: HasCallStack => Text -> SomeException -> MudStack ()
dbExHandler fn e =
    logExMsg "dbExHandler" (rethrowExMsg $ "during a database operation in " <> dblQuote fn) e >> throwToListenThread e


-----


die :: HasCallStack => Maybe Id -> Text -> PlsDie -> MudStack () -- The "Maybe Id" parameter is used to determine logging.
die mi threadName = const . maybe (logNotice "die") (logPla "die") mi $ the threadName <> " thread is dying."


dieSilently :: HasCallStack => PlsDie -> MudStack ()
dieSilently = const unit


-----


fileIOExHandler :: HasCallStack => Text -> IOException -> MudStack ()
fileIOExHandler fn e = do logIOEx fn e
                          let rethrow = throwToListenThread . toException $ e
                          unless (any (e |&|) [ isAlreadyInUseError, isDoesNotExistError, isPermissionError ]) rethrow


-----


onNewThread :: HasCallStack => Fun -> MudStack () -- Generally speaking, if you do anything on a new thread requiring a
                                                  -- player to be logged in, you should first check that the player is
                                                  -- in fact still logged in.
onNewThread f = liftIO . void . forkIO . runReaderT f =<< ask


-----


plaThreadExHandler :: HasCallStack => Id -> Text -> SomeException -> MudStack ()
plaThreadExHandler i threadName e | Just ThreadKilled <- fromException e = closePlaLog i
                                  | otherwise                            = threadExHandler (Just i) threadName e


-----


racer :: HasCallStack => MudData -> Fun -> Fun -> MudStack ()
racer md a b = liftIO . race_ (runReaderT a md) . runReaderT b $ md


-----


runAsync :: HasCallStack => Fun -> MudStack (Async ())
runAsync f = liftIO . async . runReaderT f =<< ask


-----


runEffectFun :: HasCallStack => FunName -> Id -> Seconds -> MudStack ()
runEffectFun n i secs = views (effectFunTbl.at n) (maybe oops (\f -> onNewThread . f i $ secs)) =<< getState
  where
    oops = blowUp "runEffectFun" "function name not found in effect function table" n


-----


setThreadType :: HasCallStack => ThreadType -> MudStack ()
setThreadType threadType = do ti <- liftIO $ myThreadId >>= \ti -> labelThread ti (show threadType) >> return ti
                              tweak $ threadTbl.at ti ?~ threadType


-----


threadExHandler :: HasCallStack => Maybe Id -> Text -> SomeException -> MudStack () -- The "Maybe Id" parameter is used
                                                                                    -- to decorate the thread name.
threadExHandler mi threadName e = f >>= \threadName' -> do
    logExMsg "threadExHandler" (rethrowExMsg $ "on " <> threadName' <> " thread") e
    throwToListenThread e
  where
    f = case mi of Nothing -> return threadName
                   Just i  -> getState >>= \ms -> let t | views entTbl (i `IM.member`) ms = descSingId i ms
                                                        | otherwise                       = showTxt i
                                                  in return $ threadName |<>| t


-----


threadStarterExHandler :: Id -> FunName -> Maybe Text -> SomeException -> MudStack ()
threadStarterExHandler i fn maybeName e = case fromException e of
  Just ThreadKilled -> logPla fn i $ fn <> onFalse (()# name) spcR name <> " has been killed prematurely."
  _                 -> do t <- descSingId i <$> getState
                          let msg = T.concat [ "exception caught", onFalse (()# name) (" in " <>) name, " for ", t ]
                          logExMsg fn msg e
  where
    name = maybeEmp dblQuote maybeName


-----


throwDeath :: HasCallStack => Async () -> MudStack ()
throwDeath a = throwTo (asyncThreadId a) PlsDie


-----


throwToListenThread :: HasCallStack => SomeException -> MudStack ()
throwToListenThread e = maybeVoid (`throwTo` e) . getListenThreadId =<< getState


-----


throwWait :: HasCallStack => Async () -> MudStack ()
throwWait a = sequence_ [ throwDeath a, liftIO . void . wait $ a ]
