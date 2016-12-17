{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.TheWorld.TheWorld ( initMudData
                             , initWorld ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Misc.EffectFuns
import Mud.Misc.FeelingFuns
import Mud.Misc.Logging hiding (logNotice)
import Mud.TheWorld.Foods
import Mud.TheWorld.Liqs
import Mud.TheWorld.Misc
import Mud.TheWorld.Zones.AdminZone
import Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iNecropolis, iWelcome)
import Mud.TheWorld.Zones.Tutorial
import Mud.TopLvlDefs.FilePaths
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Misc.Logging as L (logNotice)

import Control.Concurrent.STM.TMVar (newTMVarIO)
import Control.Lens (ASetter, views)
import Control.Lens.Operators ((%~), (&), (.~), (?~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecode)
import Data.IORef (newIORef)
import Data.List (delete, sort)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tuple (swap)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.IntMap.Lazy as IM (empty, foldrWithKey, fromList, keys, toList, map)
import qualified Data.Map.Lazy as M (empty, fromList)
import qualified Data.Text as T
import System.Clock (Clock(..), getTime)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Random.MWC (createSystemRandom)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.TheWorld"


-- ==================================================


initMudData :: ShouldLog -> IO MudData
initMudData shouldLog = do
    (logExLock,       perLock         ) <- (,) <$> newTMVarIO Done <*> newTMVarIO Done
    (errorLogService, noticeLogService) <- initLogging shouldLog . Just $ logExLock
    genIO   <- createSystemRandom
    start   <- getTime Monotonic
    msIORef <- newIORef MudState { _activeEffectsTbl       = IM.empty
                                 , _armTbl                 = IM.empty
                                 , _chanTbl                = IM.empty
                                 , _clothTbl               = IM.empty
                                 , _coinsTbl               = IM.empty
                                 , _conTbl                 = IM.empty
                                 , _corpseDecompAsyncTbl   = IM.empty
                                 , _corpseTbl              = IM.empty
                                 , _distinctFoodTbl        = IM.empty
                                 , _distinctLiqTbl         = IM.empty
                                 , _effectFunTbl           =  M.empty
                                 , _entTbl                 = IM.empty
                                 , _eqTbl                  = IM.empty
                                 , _feelingFunTbl          =  M.empty
                                 , _foodTbl                = IM.empty
                                 , _funTbl                 =  M.empty
                                 , _hookFunTbl             =  M.empty
                                 , _hostTbl                =  M.empty
                                 , _instaEffectFunTbl      =  M.empty
                                 , _invTbl                 = IM.empty
                                 , _mobTbl                 = IM.empty
                                 , _msgQueueTbl            = IM.empty
                                 , _npcTbl                 = IM.empty
                                 , _objTbl                 = IM.empty
                                 , _pausedCorpseDecompsTbl = IM.empty
                                 , _pausedEffectsTbl       = IM.empty
                                 , _pcSingTbl              =  M.empty
                                 , _pcTbl                  = IM.empty
                                 , _pickPtsTbl             = IM.empty
                                 , _plaLogTbl              = IM.empty
                                 , _plaTbl                 = IM.empty
                                 , _rmActionFunTbl         =  M.empty
                                 , _rmTbl                  = IM.empty
                                 , _rmTeleNameTbl          = IM.empty
                                 , _rndmNamesMstrTbl       = IM.empty
                                 , _talkAsyncTbl           =  M.empty
                                 , _teleLinkMstrTbl        = IM.empty
                                 , _threadTbl              =  M.empty
                                 , _typeTbl                = IM.empty
                                 , _vesselTbl              = IM.empty
                                 , _wpnTbl                 = IM.empty
                                 , _writableTbl            = IM.empty }
    return MudData { _errorLog      = errorLogService
                   , _noticeLog     = noticeLogService
                   , _gen           = genIO
                   , _locks         = Locks logExLock perLock
                   , _startTime     = start
                   , _mudStateIORef = msIORef }


initWorld :: MudStack Bool
initWorld = dropIrrelevantFiles . sort <$> liftIO (getDirectoryContents =<< mkMudFilePath persistDirFun) >>= \cont -> do
    sequence_ [ initFunTbl
              , initEffectFunTbl
              , initInstaEffectFunTbl
              , initFeelingFunTbl
              , initHookFunTbl
              , initRmActionFunTbl
              , initDistinctFoodTbl
              , initDistinctLiqTbl ]
    ()# cont ? (createWorld >> return True) :? loadWorld (last cont)


initFunTbl :: MudStack ()
initFunTbl = tweak $ funTbl .~ M.fromList list
  where
    list = adminZoneRmFuns


initEffectFunTbl :: MudStack ()
initEffectFunTbl = tweak $ effectFunTbl .~ M.fromList list
  where
    list = effectFuns


initInstaEffectFunTbl :: MudStack ()
initInstaEffectFunTbl = tweak $ instaEffectFunTbl .~ M.fromList list
  where
    list = instaEffectFuns


initFeelingFunTbl :: MudStack ()
initFeelingFunTbl = tweak $ feelingFunTbl .~ M.fromList list
  where
    list = feelingFuns


initHookFunTbl :: MudStack ()
initHookFunTbl = tweak $ hookFunTbl .~ M.fromList list
  where
    list = concat [ commonHooks, adminZoneHooks, tutorialHooks ]


initRmActionFunTbl :: MudStack ()
initRmActionFunTbl = tweak $ rmActionFunTbl .~ M.fromList list
  where
    list = concat [ commonRmActionFuns, adminZoneRmActionFuns, tutorialRmActionFuns ]


initDistinctFoodTbl :: MudStack ()
initDistinctFoodTbl = tweak $ distinctFoodTbl .~ IM.fromList distinctFoodList


initDistinctLiqTbl :: MudStack ()
initDistinctLiqTbl = tweak $ distinctLiqTbl .~ IM.fromList distinctLiqList


createWorld :: MudStack ()
createWorld = do
    logNotice "createWorld" "creating the world."
    createAdminZone
    createTutorial


loadWorld :: FilePath -> MudStack Bool
loadWorld dir = (</> dir) <$> liftIO (mkMudFilePath persistDirFun) >>= \path -> do
    logNotice "loadWorld" $ "loading the world from the " <> showText dir <> " directory."
    loadEqTblRes <- loadEqTbl path
    ((loadEqTblRes :) -> res) <- mapM (path |&|) [ loadTbl armTblFile                 armTbl
                                                 , loadTbl chanTblFile                chanTbl
                                                 , loadTbl clothTblFile               clothTbl
                                                 , loadTbl coinsTblFile               coinsTbl
                                                 , loadTbl conTblFile                 conTbl
                                                 , loadTbl corpseTblFile              corpseTbl
                                                 , loadTbl entTblFile                 entTbl
                                                 , loadTbl foodTblFile                foodTbl
                                                 , loadTbl hostTblFile                hostTbl
                                                 , loadTbl invTblFile                 invTbl
                                                 , loadTbl mobTblFile                 mobTbl
                                                 , loadTbl objTblFile                 objTbl
                                                 , loadTbl pausedCorpseDecompsTblFile pausedCorpseDecompsTbl
                                                 , loadTbl pausedEffectsTblFile       pausedEffectsTbl
                                                 , loadTbl pcSingTblFile              pcSingTbl
                                                 , loadTbl pcTblFile                  pcTbl
                                                 , loadTbl plaTblFile                 plaTbl
                                                 , loadTbl rmTblFile                  rmTbl
                                                 , loadTbl rmTeleNameTblFile          rmTeleNameTbl
                                                 , loadTbl rndmNamesMstrTblFile       rndmNamesMstrTbl
                                                 , loadTbl teleLinkMstrTblFile        teleLinkMstrTbl
                                                 , loadTbl typeTblFile                typeTbl
                                                 , loadTbl vesselTblFile              vesselTbl
                                                 , loadTbl wpnTblFile                 wpnTbl
                                                 , loadTbl writableTblFile            writableTbl ]
    tweak $ \ms -> foldr removeAdHoc ms . getInv iWelcome $ ms
    initActiveEffectsTbl
    movePCs
    return . and $ res


loadEqTbl :: FilePath -> MudStack Bool
loadEqTbl ((</> eqTblFile) -> absolute) = do
    json <- liftIO . B.readFile $ absolute
    case eitherDecode json of
      Left err -> sorry absolute err
      Right (IM.map (M.fromList . map swap . IM.toList) -> tbl) -> tweak (eqTbl .~ tbl) >> return True


sorry :: FilePath -> String -> MudStack Bool
sorry absolute (T.pack -> err) = (logError . loadTblErrorMsg absolute $ err) >> return False


loadTbl :: (FromJSON b) => FilePath -> ASetter MudState MudState a b -> FilePath -> MudStack Bool
loadTbl tblFile lens path = let absolute = path </> tblFile in
    eitherDecode <$> (liftIO . B.readFile $ absolute) >>= \case
      Left  err -> sorry absolute err
      Right tbl -> tweak (lens .~ tbl) >> return True


initActiveEffectsTbl :: MudStack ()
initActiveEffectsTbl = tweak $ \ms -> ms & activeEffectsTbl .~ IM.fromList [ (i, []) | i <- views entTbl IM.keys ms ]


movePCs :: MudStack ()
movePCs = tweak $ \ms ->
    let idsWithRmIds       = let pairs   = views mobTbl (IM.foldrWithKey f []) ms
                                 f i mob = onTrue (isPC i ms) ((i, mob^.rmId) :)
                             in filter (((&&) <$> (/= iLoggedOut) <*> (/= iNecropolis)) . snd) pairs
        helper (i, ri) ms' = ms' & invTbl.ind ri           %~ (i `delete`)
                                 & invTbl.ind iLoggedOut   %~ (i :)
                                 & mobTbl.ind i.rmId       .~ iLoggedOut
                                 & plaTbl.ind i.logoutRmId ?~ ri
    in foldr helper ms idsWithRmIds
