{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.TheWorld.TheWorld ( initMudData
                             , initWorld ) where

import           Mud.Cmds.Msgs.Misc
import           Mud.Cmds.Util.Misc
import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Locks
import           Mud.Data.State.Util.Misc
import           Mud.Misc.EffectFuns
import           Mud.Misc.FeelingFuns
import qualified Mud.Misc.Logging as L (logErrorMsg, logNotice)
import           Mud.Misc.Logging hiding (logErrorMsg, logNotice)
import           Mud.TheWorld.Foods
import           Mud.TheWorld.Liqs
import           Mud.TheWorld.Misc
import           Mud.TheWorld.Zones.AdminZone
import           Mud.TheWorld.Zones.AdminZoneIds (iLoggedOut, iNecropolis, iWelcome)
import           Mud.TheWorld.Zones.Dalben
import           Mud.TheWorld.Zones.DalbenIds (iDalbenWelcome)
import           Mud.TheWorld.Zones.Tutorial
import           Mud.TheWorld.Zones.TutorialIds (iTutWelcome)
import           Mud.TopLvlDefs.FilePaths
import           Mud.Util.Misc
import           Mud.Util.Operators
import           Mud.Util.Text
import           Prelude hiding (log)

import           Control.Lens (ASetter, views)
import           Control.Lens.Operators ((?~), (.~), (&), (%~), (^.))
import           Control.Monad (forM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, eitherDecode)
import           Data.IORef (newIORef)
import           Data.List (delete, sort)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Tuple (swap)
import           GHC.Stack (HasCallStack)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.IntMap.Strict as IM (empty, foldrWithKey, fromList, keys, toList, map)
import qualified Data.Map.Strict as M (empty, fromList)
import qualified Data.Text as T
import           System.Clock (Clock(..), getTime)
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))
import           System.Random.MWC (createSystemRandom)


logErrorMsg :: Text -> Text -> MudStack ()
logErrorMsg = L.logErrorMsg "Mud.TheWorld.TheWorld"


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.TheWorld"


-- ==================================================


initMudData :: HasCallStack => DoOrDon'tLog -> IO MudData
initMudData log = do [ databaseLock, logLock, persLock ] <- mkLocks
                     (errorLogService, noticeLogService) <- initLogging log . Just $ logLock
                     genIO   <- createSystemRandom
                     start   <- getTime Monotonic
                     msIORef <- newIORef MudState { _armTbl                 = IM.empty
                                                  , _chanTbl                = IM.empty
                                                  , _clothTbl               = IM.empty
                                                  , _coinsTbl               = IM.empty
                                                  , _conTbl                 = IM.empty
                                                  , _corpseDecompAsyncTbl   = IM.empty
                                                  , _corpseTbl              = IM.empty
                                                  , _distinctFoodTbl        = IM.empty
                                                  , _distinctLiqTbl         = IM.empty
                                                  , _durationalEffectTbl    = IM.empty
                                                  , _effectFunTbl           =  M.empty
                                                  , _entTbl                 = IM.empty
                                                  , _eqTbl                  = IM.empty
                                                  , _feelingFunTbl          =  M.empty
                                                  , _foodTbl                = IM.empty
                                                  , _funTbl                 =  M.empty
                                                  , _holySymbolTbl          = IM.empty
                                                  , _hookFunTbl             =  M.empty
                                                  , _hostTbl                =  M.empty
                                                  , _instaEffectFunTbl      =  M.empty
                                                  , _invTbl                 = IM.empty
                                                  , _mobTbl                 = IM.empty
                                                  , _msgQueueTbl            = IM.empty
                                                  , _npcTbl                 = IM.empty
                                                  , _objTbl                 = IM.empty
                                                  , _pausedCorpseDecompsTbl = IM.empty
                                                  , _pausedEffectTbl        = IM.empty
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
                                    , _locks         = Locks databaseLock logLock persLock
                                    , _startTime     = start
                                    , _mudStateIORef = msIORef }


initWorld :: HasCallStack => MudStack Bool
initWorld = dropIrrelevantFiles . sort <$> liftIO (getDirectoryContents =<< mkMudFilePath persistDirFun) >>= \cont -> do
    sequence_ [ initFunTbl
              , initEffectFunTbl
              , initInstaEffectFunTbl
              , initFeelingFunTbl
              , initHookFunTbl
              , initRmActionFunTbl
              , initDistinctFoodTbl
              , initDistinctLiqTbl ]
    let go = createWorld >> return True
    if ()# cont
      then go
      else loadWorld (last cont) >>= \b -> onTrue b (const go) . return $ b


initFunTbl :: HasCallStack => MudStack ()
initFunTbl = tweak $ funTbl .~ M.fromList list
  where
    list = adminZoneRmFuns


initEffectFunTbl :: HasCallStack => MudStack ()
initEffectFunTbl = tweak $ effectFunTbl .~ M.fromList list
  where
    list = effectFuns


initInstaEffectFunTbl :: HasCallStack => MudStack ()
initInstaEffectFunTbl = tweak $ instaEffectFunTbl .~ M.fromList list
  where
    list = instaEffectFuns


initFeelingFunTbl :: HasCallStack => MudStack ()
initFeelingFunTbl = tweak $ feelingFunTbl .~ M.fromList list
  where
    list = feelingFuns


initHookFunTbl :: HasCallStack => MudStack ()
initHookFunTbl = tweak $ hookFunTbl .~ M.fromList list
  where
    list = concat [ commonHooks, adminZoneHooks, tutorialHooks, dalbenHooks ]


initRmActionFunTbl :: HasCallStack => MudStack ()
initRmActionFunTbl = tweak $ rmActionFunTbl .~ M.fromList list
  where
    list = concat [ commonRmActionFuns, adminZoneRmActionFuns, tutorialRmActionFuns ]


initDistinctFoodTbl :: HasCallStack => MudStack ()
initDistinctFoodTbl = tweak $ distinctFoodTbl .~ IM.fromList (map dropThr foodList)


initDistinctLiqTbl :: HasCallStack => MudStack ()
initDistinctLiqTbl = tweak $ distinctLiqTbl .~ IM.fromList (map dropThr liqList)


createWorld :: HasCallStack => MudStack ()
createWorld = getState >>= \ms -> let pairs = [ (iWelcome,       createAdminZone)
                                              , (iTutWelcome,    createTutorial )
                                              , (iDalbenWelcome, createDalben   ) ]
                                  in do logNotice "createWorld" "creating the world."
                                        forM_ pairs $ \(i, f) -> unless (hasType i ms) f



loadWorld :: HasCallStack => FilePath -> MudStack Bool
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
                                                 , loadTbl holySymbolTblFile          holySymbolTbl
                                                 , loadTbl hostTblFile                hostTbl
                                                 , loadTbl invTblFile                 invTbl
                                                 , loadTbl mobTblFile                 mobTbl
                                                 , loadTbl objTblFile                 objTbl
                                                 , loadTbl pausedCorpseDecompsTblFile pausedCorpseDecompsTbl
                                                 , loadTbl pausedEffectTblFile        pausedEffectTbl
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
    tweak . flip compose $ [ movePCs, initDurEffectTbl, removeAdHocHelper ]
    return . and $ res
  where
    removeAdHocHelper ms = foldr removeAdHoc ms . getInv iWelcome $ ms


loadEqTbl :: HasCallStack => FilePath -> MudStack Bool
loadEqTbl ((</> eqTblFile) -> absolute) = eitherDecode <$> liftIO (B.readFile absolute) >>= \case
  Left err                                                  -> sorry absolute err
  Right (IM.map (M.fromList . map swap . IM.toList) -> tbl) -> tweak (eqTbl .~ tbl) >> return True


sorry :: HasCallStack => FilePath -> String -> MudStack Bool
sorry absolute (T.pack -> err) = logErrorMsg "sorry" (loadTblErrorMsg absolute err) >> return False


loadTbl :: (HasCallStack, FromJSON b) => FilePath -> ASetter MudState MudState a b -> FilePath -> MudStack Bool
loadTbl tblFile lens path = let absolute = path </> tblFile in eitherDecode <$> liftIO (B.readFile absolute) >>= \case
  Left  err -> sorry absolute err
  Right tbl -> tweak (lens .~ tbl) >> return True


initDurEffectTbl :: HasCallStack => MudState -> MudState
initDurEffectTbl ms = ms & durationalEffectTbl .~ IM.fromList [ (i, []) | i <- views entTbl IM.keys ms ]


movePCs :: HasCallStack => MudState -> MudState
movePCs ms = let idsWithRmIds   | f     <- \i mob -> onTrue (isPla i ms) ((i, mob^.rmId) :)
                                , pairs <- views mobTbl (IM.foldrWithKey f []) ms
                                = filter (((&&) <$> (/= iLoggedOut) <*> (/= iNecropolis)) . snd) pairs
                 helper (i, ri) = flip upd [ invTbl.ind ri           %~ (i `delete`)
                                           , invTbl.ind iLoggedOut   %~ (i :)
                                           , mobTbl.ind i.rmId       .~ iLoggedOut
                                           , plaTbl.ind i.logoutRmId ?~ ri ]
             in foldr helper ms idsWithRmIds
