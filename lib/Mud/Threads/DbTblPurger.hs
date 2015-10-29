{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.DbTblPurger ( threadAdminChanTblPurger
                               , threadAdminMsgTblPurger
                               , threadChanTblPurger
                               , threadQuestionChanTblPurger
                               , threadTeleTblPurger ) where

import Mud.Cmds.Util.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Set
import Mud.Misc.Database
import Mud.Threads.NewMisc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted (catch, handle)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Database.SQLite.Simple (Only(..))
import qualified Data.Text as T


default (Int)


-----


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.DbTblPurger"


-- ==================================================


dbTblPurger :: T.Text -> IO [Only Int] -> IO () -> MudStack ()
dbTblPurger tblName countFun purgeFun = handle (threadExHandler threadName) $ do
    setThreadType DbTblPurger
    logNotice "dbTblPurger" $ "database table purger started for the " <> dblQuote tblName <> " table."
    let loop = (liftIO . threadDelay $ dbTblPurgerDelay * 10 ^ 6) >> helper
    forever loop `catch` die threadName
  where
    threadName = "database table purger " <> parensQuote tblName
    helper     = let fn = "dbTblPurger helper" in withDbExHandler fn countFun >>= \case
        Just [Only count] -> if count > maxDbTblRecs
                               then do
                                   withDbExHandler_ fn purgeFun
                                   logNotice fn  . T.concat $ [ "the "
                                                              , dblQuote tblName
                                                              , " table has been purged of "
                                                              , showText noOfDbTblRecsToPurge
                                                              , " records." ]
                               else logNotice fn . T.concat $ [ "the "
                                                              , dblQuote tblName
                                                              , " table presently contains "
                                                              , showText count
                                                              , " records." ]
        _ -> unit


-----


threadAdminChanTblPurger :: MudStack ()
threadAdminChanTblPurger = dbTblPurger "admin_chan" countDbTblRecsAdminChan purgeDbTblAdminChan


threadAdminMsgTblPurger :: MudStack ()
threadAdminMsgTblPurger = dbTblPurger "admin_msg" countDbTblRecsAdminMsg purgeDbTblAdminMsg


threadChanTblPurger :: MudStack ()
threadChanTblPurger = dbTblPurger "chan" countDbTblRecsChan purgeDbTblChan


threadQuestionChanTblPurger :: MudStack ()
threadQuestionChanTblPurger = dbTblPurger "question" countDbTblRecsQuestion purgeDbTblQuestion


threadTeleTblPurger :: MudStack ()
threadTeleTblPurger = dbTblPurger "tele" countDbTblRecsTele purgeDbTblTele
