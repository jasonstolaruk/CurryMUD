{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.TheWorld.TheWorld ( initMudData
                             , initWorld ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Misc.Logging hiding (logNotice)
import Mud.TheWorld.AdminZone
import Mud.TheWorld.AdminZoneIds (iLoggedOut, iWelcome)
import Mud.TheWorld.Tutorial
import Mud.TopLvlDefs.FilePaths
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent.STM.TMVar (newTMVarIO)
import Control.Lens (ASetter)
import Control.Lens.Operators ((%~), (&), (.~), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode)
import Data.IORef (newIORef)
import Data.List (delete, sort)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.IntMap.Lazy as IM (empty, foldrWithKey, toList, map)
import qualified Data.Map.Lazy as M (empty, fromList)
import qualified Data.Text as T
import System.Clock (Clock(..), getTime)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Random.MWC (createSystemRandom)


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.TheWorld"


-- ==================================================


initMudData :: ShouldLog -> IO MudData
initMudData shouldLog = do
    (logExLock, perLock) <- (,) <$> newTMVarIO Done <*> newTMVarIO Done
    (errorLogService, noticeLogService) <- initLogging shouldLog . Just $ logExLock
    genIO   <- createSystemRandom
    msIORef <- newIORef MudState { _armTbl           = IM.empty
                                 , _chanTbl          = IM.empty
                                 , _clothTbl         = IM.empty
                                 , _coinsTbl         = IM.empty
                                 , _conTbl           = IM.empty
                                 , _entTbl           = IM.empty
                                 , _eqTbl            = IM.empty
                                 , _hostTbl          =  M.empty
                                 , _invTbl           = IM.empty
                                 , _mobTbl           = IM.empty
                                 , _msgQueueTbl      = IM.empty
                                 , _objTbl           = IM.empty
                                 , _pcTbl            = IM.empty
                                 , _plaLogTbl        = IM.empty
                                 , _plaTbl           = IM.empty
                                 , _rmTbl            = IM.empty
                                 , _rmTeleNameTbl    = IM.empty
                                 , _rndmNamesMstrTbl = IM.empty
                                 , _talkAsyncTbl     =  M.empty
                                 , _teleLinkMstrTbl  = IM.empty
                                 , _threadTbl        =  M.empty
                                 , _typeTbl          = IM.empty
                                 , _wpnTbl           = IM.empty }
    start <- getTime Monotonic
    return MudData { _errorLog       = errorLogService
                   , _gen            = genIO
                   , _locks          = Locks logExLock perLock
                   , _mudStateIORef  = msIORef
                   , _noticeLog      = noticeLogService
                   , _startTime      = start }


initWorld :: MudStack Bool
initWorld = dropIrrelevantFilenames . sort <$> (liftIO . getDirectoryContents $ persistDir) >>= \cont ->
    ()# cont ? (createWorld >> return True) :? (loadWorld . last $ cont)


createWorld :: MudStack ()
createWorld = do
    logNotice "createWorld" "creating the world."
    createAdminZone
    createTutorial


loadWorld :: FilePath -> MudStack Bool
loadWorld dir@((persistDir </>) -> path) = do
    logNotice "loadWorld" $ "loading the world from the " <> (dblQuote . T.pack $ dir) <> " directory."
    loadEqTblRes <- loadEqTbl path
    ((loadEqTblRes :) -> res) <- mapM (path |&|) [ loadTbl armTblFile           armTbl
                                                 , loadTbl chanTblFile          chanTbl
                                                 , loadTbl clothTblFile         clothTbl
                                                 , loadTbl coinsTblFile         coinsTbl
                                                 , loadTbl conTblFile           conTbl
                                                 , loadTbl entTblFile           entTbl
                                                 , loadTbl hostTblFile          hostTbl
                                                 , loadTbl invTblFile           invTbl
                                                 , loadTbl mobTblFile           mobTbl
                                                 , loadTbl objTblFile           objTbl
                                                 , loadTbl pcTblFile            pcTbl
                                                 , loadTbl plaTblFile           plaTbl
                                                 , loadTbl rmTblFile            rmTbl
                                                 , loadTbl rmTeleNameTblFile    rmTeleNameTbl
                                                 , loadTbl rndmNamesMstrTblFile rndmNamesMstrTbl
                                                 , loadTbl teleLinkMstrTblFile  teleLinkMstrTbl
                                                 , loadTbl typeTblFile          typeTbl
                                                 , loadTbl wpnTblFile           wpnTbl ]
    modifyState $ \ms -> (foldr removeAdHoc ms . getInv iWelcome $ ms, ())
    movePCs
    return . and $ res


loadEqTbl :: FilePath -> MudStack Bool
loadEqTbl ((</> eqTblFile) -> absolute) = do
    json <- liftIO . B.readFile $ absolute
    case eitherDecode json of
      Left err -> sorry absolute err
      Right (IM.map (M.fromList . map swap . IM.toList) -> tbl) -> modifyState ((, ()) . (eqTbl .~ tbl)) >> return True


sorry :: FilePath -> String -> MudStack Bool
sorry absolute (T.pack -> err) = (logError . loadTblErrorMsg absolute $ err) >> return False


loadTbl :: (FromJSON b) => FilePath -> ASetter MudState MudState a b -> FilePath -> MudStack Bool
loadTbl tblFile lens path = let absolute = path </> tblFile in
    eitherDecode <$> (liftIO . B.readFile $ absolute) >>= \case
      Left  err -> sorry absolute err
      Right tbl -> modifyState ((, ()) . (lens .~ tbl)) >> return True


movePCs :: MudStack ()
movePCs = modifyState $ \ms ->
    let idsWithRmIds       = let pairs = IM.foldrWithKey (\i pc -> ((i, pc^.rmId) :)) [] $ ms^.pcTbl
                             in filter ((/= iLoggedOut) . snd) pairs
        helper (i, ri) ms' = ms' & invTbl.ind ri         %~ (i `delete`)
                                 & invTbl.ind iLoggedOut %~ (i :)
                                 & pcTbl .ind i.rmId     .~ iLoggedOut
                                 & plaTbl.ind i.lastRmId ?~ ri
    in (foldr helper ms idsWithRmIds, ())
