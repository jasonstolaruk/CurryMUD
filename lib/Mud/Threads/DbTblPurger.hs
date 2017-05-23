{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Threads.DbTblPurger ( mkDbTblPurgerFuns
                               , mkDbTblPurgerHelpers ) where

import           Mud.Cmds.Util.Misc
import           Mud.Data.State.MudData
import           Mud.Misc.Database
import qualified Mud.Misc.Logging as L (logNotice)
import           Mud.Threads.Misc
import           Mud.TopLvlDefs.Misc
import           Mud.Util.Misc
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Exception.Lifted (catch, handle)
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M (Map, fromList, toList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.Threads.DbTblPurger"


-- ==================================================


mkTblPurgerMap :: M.Map DbTblName (CountDbTblRecsFun, PurgeDbTblFun)
mkTblPurgerMap = M.fromList [ ("admin_chan", (countDbTblRecsAdminChan, purgeDbTblAdminChan))
                            , ("admin_msg",  (countDbTblRecsAdminMsg,  purgeDbTblAdminMsg ))
                            , ("chan",       (countDbTblRecsChan,      purgeDbTblChan     ))
                            , ("question",   (countDbTblRecsQuestion,  purgeDbTblQuestion ))
                            , ("tele",       (countDbTblRecsTele,      purgeDbTblTele     )) ]


type DbTblPurger = DbTblName -> CountDbTblRecsFun -> PurgeDbTblFun -> MudStack ()


mkPurger :: DbTblPurger -> Funs
mkPurger f = let g (tblName, pair) = uncurry (f tblName) pair
             in map g . M.toList $ mkTblPurgerMap


mkDbTblPurgerFuns :: Funs
mkDbTblPurgerFuns = mkPurger dbTblPurger


mkDbTblPurgerHelpers :: Funs
mkDbTblPurgerHelpers = mkPurger dbTblPurgerHelper


-----


dbTblPurger :: DbTblPurger
dbTblPurger tblName countFun purgeFun = handle (threadExHandler Nothing threadName) $ do
    setThreadType DbTblPurger
    logNotice "dbTblPurger" $ "database table purger started for the " <> dblQuote tblName <> " table."
    let loop = sequence_ [ liftIO . delaySecs $ dbTblPurgerDelay, dbTblPurgerHelper tblName countFun purgeFun ]
    forever loop `catch` die Nothing threadName
  where
    threadName = "database table purger " <> parensQuote tblName


dbTblPurgerHelper :: DbTblPurger
dbTblPurgerHelper tblName countFun purgeFun = let fn = "dbTblPurgerHelper" in withDbExHandler fn countFun >>= \case
  Just count -> if count > maxDbTblRecs
    then do logNotice fn . T.concat $ [ the . dblQuote $ tblName
                                      , " table is being purged of "
                                      , showTxt noOfDbTblRecsToPurge
                                      , " records." ]
            withDbExHandler_ fn purgeFun
    else logNotice fn . T.concat $ [ the . dblQuote $ tblName
                                   , " table presently contains "
                                   , showTxt count
                                   , " records." ]
  _ -> unit
