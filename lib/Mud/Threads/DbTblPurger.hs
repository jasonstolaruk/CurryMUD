{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.DbTblPurger ( threadAdminChanTblPurger
                               , threadAdminMsgTblPurger
                               , threadChanTblPurger
                               , threadQuestionChanTblPurger
                               , threadTeleTblPurger ) where

import Mud.Cmds.Util.Misc
import Mud.Data.State.MudData
import Mud.Misc.Database
import Mud.Threads.Misc
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Exception.Lifted (catch, handle)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.DbTblPurger"


-- ==================================================


dbTblPurger :: Text -> IO Int -> IO () -> MudStack ()
dbTblPurger tblName countFun purgeFun = handle (threadExHandler Nothing threadName) $ do
    setThreadType DbTblPurger
    logNotice "dbTblPurger" $ "database table purger started for the " <> dblQuote tblName <> " table."
    let loop = sequence_ [ liftIO . delaySecs $ dbTblPurgerDelay, helper ]
    forever loop `catch` die Nothing threadName
  where
    threadName = "database table purger " <> parensQuote tblName
    helper     = let fn = "dbTblPurger helper" in withDbExHandler fn countFun >>= \case
        Just count -> if count > maxDbTblRecs
          then do logNotice fn . T.concat $ [ the . dblQuote $ tblName
                                            , " table is being purged of "
                                            , showText noOfDbTblRecsToPurge
                                            , " records." ]
                  withDbExHandler_ fn purgeFun
          else logNotice fn . T.concat $ [ the . dblQuote $ tblName
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
