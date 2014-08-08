{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}
-- TODO: -Werror
{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RankNTypes #-}

module Mud.StateHelpers where -- TODO: Provide an export list.

import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TVar (modifyTVar', readTVar, TVar)
import Control.Lens (_1, at, each, folded)
import Control.Lens.Getter (Getting)
import Control.Lens.Operators ((&), (?~), (.~), (%~), (^.), (^..))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (gets)
import Data.IntMap (IntMap)
import Data.IntMap.Lazy ((!))
import Data.List ((\\), sortBy)
import Data.Monoid ((<>), mempty)
import qualified Data.IntMap.Lazy as IM (adjust, insert, keys, lookup)
import qualified Data.Map.Lazy as M (elems, filter)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, readFile)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- TODO: Determine which functions are not needed, and delete them.


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.StateHelpers"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.StateHelpers"


-- ============================================================


getWS :: MudStack WorldState
getWS = liftIO . atomically . readTVar =<< gets (^.worldStateTVar)


onWS :: (TVar WorldState -> STM a) -> MudStack a
onWS f = liftIO . atomically . f =<< gets (^.worldStateTVar)


modifyWS :: (WorldState -> WorldState) -> MudStack ()
modifyWS f = liftIO . atomically . flip modifyTVar' f =<< gets (^.worldStateTVar)


type BothGramNos = (Sing, Plur)


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


mkListFromCoins :: Coins -> [Int]
mkListFromCoins (Coins (c, g, s)) = [c, g, s]


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [cop, sil, gol] = Coins (cop, sil, gol)
mkCoinsFromList xs              = patternMatchFail "mkCoinsFromList" [ showText xs ]


{-
-- ============================================================
-- Helpers for working with both world and non-world state:


lookup_STM :: forall a . Id -> TVar (IntMap a) -> STM a
i `lookup_STM` t = maybeRet oops . IM.lookup i =<< readTVar t
  where
    oops = blowUp "lookup_STM" "value not found in state table for given key" [ showText i ]


insert_STM :: forall a . Id -> a -> TVar (IntMap a) -> STM ()
insert_STM i a t = modifyTVar' t . IM.insert i $ a


adjust_STM :: forall a . (a -> a) -> Id -> TVar (IntMap a) -> STM ()
adjust_STM f i t = modifyTVar' t . IM.adjust f $ i


keys_STM :: TVar (IntMap a) -> STM Inv
keys_STM t = IM.keys <$> readTVar t


-- ==================================================
-- Helpers for working with world state tables:


type WSTblGetting a = Getting (TVar (IntMap a)) WorldState (TVar (IntMap a))


onWorldState :: (WorldState -> STM a) -> MudStack a
onWorldState f = liftIO . atomically . f =<< gets (^.worldState)


lookupWS :: forall a . Id -> WSTblGetting a -> MudStack a
i `lookupWS` tbl = onWorldState $ \ws ->
    i `lookup_STM` (ws^.tbl)


insertWS :: forall a . Id -> a -> WSTblGetting a -> MudStack ()
insertWS i a tbl = onWorldState $ \ws ->
    insert_STM i a (ws^.tbl)


adjustWS :: forall a . (a -> a) -> Id -> WSTblGetting a -> MudStack ()
adjustWS f i tbl = onWorldState $ \ws ->
    adjust_STM f i (ws^.tbl)


findUnusedId_STM :: WorldState -> STM Id
findUnusedId_STM ws = head . (\\) [0..] <$> keys_STM (ws^.typeTbl)
-}


{-
-- ==================================================
-- Entities:


getEnt :: Id -> MudStack Ent
getEnt i = i `lookupWS` entTbl


getEnt_STM :: WorldState -> Id -> STM Ent
getEnt_STM ws i = i `lookup_STM` (ws^.entTbl)


getEntType :: Ent -> MudStack Type
getEntType e = let i = e^.entId
               in i `lookupWS` typeTbl


getEntType_STM :: WorldState -> Ent -> STM Type
getEntType_STM ws e = let i = e^.entId
                      in i `lookup_STM` (ws^.typeTbl)


getEntsInInv :: Inv -> MudStack [Ent]
getEntsInInv = mapM getEnt


getEntsInInv_STM :: WorldState -> Inv -> STM [Ent]
getEntsInInv_STM ws = mapM (getEnt_STM ws)


getEntNamesInInv :: Inv -> MudStack [T.Text]
getEntNamesInInv is = getEntsInInv is >>= \es ->
    return [ e^.name | e <- es ]


getEntNamesInInv_STM :: WorldState -> Inv -> STM [T.Text]
getEntNamesInInv_STM ws is = getEntsInInv_STM ws is >>= \es ->
    return [ e^.name | e <- es ]


getEntSingsInInv :: Inv -> MudStack [T.Text]
getEntSingsInInv is = getEntsInInv is >>= \es ->
    return [ e^.sing | e <- es ]


getEntSingsInInv_STM :: WorldState -> Inv -> STM [T.Text]
getEntSingsInInv_STM ws is = getEntsInInv_STM ws is >>= \es ->
    return [ e^.sing | e <- es ]


type BothGramNos = (Sing, Plur)


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


getEntBothGramNosInInv :: Inv -> MudStack [BothGramNos]
getEntBothGramNosInInv is = map getEntBothGramNos <$> getEntsInInv is


getEntBothGramNosInInv_STM :: WorldState -> Inv -> STM [BothGramNos]
getEntBothGramNosInInv_STM ws is = map getEntBothGramNos <$> getEntsInInv_STM ws is


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p)  = p


-- ==================================================
-- Clothing:


getCloth :: Id -> MudStack Cloth
getCloth i = i `lookupWS` clothTbl


getCloth_STM :: WorldState -> Id -> STM Cloth
getCloth_STM ws i = i `lookup_STM` (ws^.clothTbl)


-- ==================================================
-- Inventories:


getInv :: Id -> MudStack Inv
getInv i = i `lookupWS` invTbl


getInv_STM :: WorldState -> Id -> STM Inv
getInv_STM ws i = i `lookup_STM` (ws^.invTbl)


hasInv :: Id -> MudStack Bool
hasInv i = not . null <$> getInv i


hasInv_STM :: WorldState -> Id -> STM Bool
hasInv_STM ws i = not . null <$> getInv_STM ws i


hasInvOrCoins :: Id -> MudStack Bool
hasInvOrCoins i = (||) <$> hasInv i <*> hasCoins i


hasInvOrCoins_STM :: WorldState -> Id -> STM Bool
hasInvOrCoins_STM ws i = (||) <$> hasInv_STM ws i <*> hasCoins_STM ws i


type InvCoins = (Inv, Coins)


getInvCoins :: Id -> MudStack InvCoins
getInvCoins i = (,) <$> getInv i <*> getCoins i


getInvCoins_STM :: WorldState -> Id -> STM InvCoins
getInvCoins_STM ws i = (,) <$> getInv_STM ws i <*> getCoins_STM ws i


sortInv :: Inv -> MudStack Inv
sortInv is = ((^..folded._1) . sortBy nameThenSing) <$> zipped
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped = zip3 is <$> getEntNamesInInv is <*> getEntSingsInInv is


sortInv_STM :: WorldState -> Inv -> STM Inv
sortInv_STM ws is = ((^..folded._1) . sortBy nameThenSing) <$> zipped
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped = zip3 is <$> getEntNamesInInv_STM ws is <*> getEntSingsInInv_STM ws is


type FromId = Id
type ToId   = Id


addToInv :: Inv -> ToId -> MudStack ()
addToInv is ti = getInv ti >>= sortInv . (++ is) >>= \is' ->
    insertWS ti is' invTbl


addToInv_STM :: WorldState -> Inv -> ToId -> STM ()
addToInv_STM ws is ti = getInv_STM ws ti >>= sortInv_STM ws . (++ is) >>= \is' ->
    insert_STM ti is' (ws^.invTbl)


remFromInv :: Inv -> FromId -> MudStack ()
remFromInv is fi = adjustWS (deleteFirstOfEach is) fi invTbl


remFromInv_STM :: WorldState -> Inv -> FromId -> STM ()
remFromInv_STM ws is fi = adjust_STM (deleteFirstOfEach is) fi (ws^.invTbl)


moveInv :: Inv -> FromId -> ToId -> MudStack ()
moveInv [] _  _  = return ()
moveInv is fi ti = remFromInv is fi >> addToInv is ti


moveInv_STM :: WorldState -> Inv -> FromId -> ToId -> STM ()
moveInv_STM _  [] _  _  = return ()
moveInv_STM ws is fi ti = remFromInv_STM ws is fi >> addToInv_STM ws is ti


-- ==================================================
-- Coins:


getCoins :: Id -> MudStack Coins
getCoins i = i `lookupWS` coinsTbl


getCoins_STM :: WorldState -> Id -> STM Coins
getCoins_STM ws i = i `lookup_STM` (ws^.coinsTbl)


hasCoins :: Id -> MudStack Bool
hasCoins i = not . all (== 0) . mkListFromCoins <$> getCoins i


hasCoins_STM :: WorldState -> Id -> STM Bool
hasCoins_STM ws i = not . all (== 0) . mkListFromCoins <$> getCoins_STM ws i


moveCoins :: Coins -> FromId -> ToId -> MudStack ()
moveCoins c fi ti = unless (c == mempty) $ subCoins c fi >> addCoins c ti


moveCoins_STM :: WorldState -> Coins -> FromId -> ToId -> STM ()
moveCoins_STM ws c fi ti = unless (c == mempty) $ subCoins_STM ws c fi >> addCoins_STM ws c ti


addCoins :: Coins -> Id -> MudStack ()
addCoins c i = adjustWS (<> c) i coinsTbl


addCoins_STM :: WorldState -> Coins -> Id -> STM ()
addCoins_STM ws c i = adjust_STM (<> c) i $ ws^.coinsTbl


subCoins :: Coins -> Id -> MudStack ()
subCoins c i = adjustWS (<> negateCoins c) i coinsTbl


subCoins_STM :: WorldState -> Coins -> Id -> STM ()
subCoins_STM ws c i = adjust_STM (<> negateCoins c) i $ ws^.coinsTbl


negateCoins :: Coins -> Coins
negateCoins (Coins c) = Coins (each %~ negate $ c)


-- ==================================================
-- Weapons:


getWpn :: Id -> MudStack Wpn
getWpn i = i `lookupWS` wpnTbl


getWpn_STM :: WorldState -> Id -> STM Coins
getWpn_STM ws i = i `lookup_STM` (ws^.coinsTbl)


-- ==================================================
-- Armor:


getArm :: Id -> MudStack Arm
getArm i = i `lookupWS` armTbl


getArm_STM :: WorldState -> Id -> STM Arm
getArm_STM ws i = i `lookup_STM` (ws^.armTbl)


-- ==================================================
-- Equipment:


getEqMap :: Id -> MudStack EqMap
getEqMap i = i `lookupWS` eqTbl


getEqMap_STM :: WorldState -> Id -> STM EqMap
getEqMap_STM ws i = i `lookup_STM` (ws^.eqTbl)


getEq :: Id -> MudStack Inv
getEq i = M.elems <$> getEqMap i


getEq_STM :: WorldState -> Id -> STM Inv
getEq_STM ws i = M.elems <$> getEqMap_STM ws i


hasEq :: Id -> MudStack Bool
hasEq i = not . null <$> getEq i


hasEq_STM :: WorldState -> Id -> STM Bool
hasEq_STM ws i = not . null <$> getEq_STM ws i


moveReadiedItem :: Id -> EqMap -> Slot -> MudStack ()
moveReadiedItem i em s = insertWS 0 (em & at s ?~ i) eqTbl >> remFromInv [i] 0


moveReadiedItem_STM :: WorldState -> Id -> EqMap -> Slot -> STM ()
moveReadiedItem_STM ws i em s = insert_STM 0 (em & at s ?~ i) (ws^.eqTbl) >> remFromInv_STM ws [i] 0


shuffleInvUnready :: Inv -> MudStack ()
shuffleInvUnready is = M.filter (`notElem` is) <$> getEqMap 0 >>= \is' ->
    insertWS 0 is' eqTbl >> addToInv is 0


shuffleInvUnready_STM :: WorldState -> Inv -> STM ()
shuffleInvUnready_STM ws is = M.filter (`notElem` is) <$> getEqMap_STM ws 0 >>= \is' ->
    insert_STM 0 is' (ws^.eqTbl) >> addToInv_STM ws is 0


-- ==================================================
-- Mobiles:


getMob :: Id -> MudStack Mob
getMob i = i `lookupWS` mobTbl


getMob_STM :: WorldState -> Id -> STM Mob
getMob_STM ws i = i `lookup_STM` (ws^.mobTbl)


getMobGender :: Id -> MudStack Gender
getMobGender i = (^.gender) <$> getMob i


getMobGender_STM :: WorldState -> Id -> STM Gender
getMobGender_STM ws i = (^.gender) <$> getMob_STM ws i


getMobHand :: Id -> MudStack Hand
getMobHand i = (^.hand) <$> getMob i


getMobHand_STM :: WorldState -> Id -> STM Hand
getMobHand_STM ws i = (^.hand) <$> getMob_STM ws i


-- ==================================================
-- PCs:


getPC :: Id -> MudStack PC
getPC i = i `lookupWS` pcTbl


getPC_STM :: WorldState -> Id -> STM PC
getPC_STM ws i = i `lookup_STM` (ws^.pcTbl)


movePC :: Id -> Id -> MudStack ()
movePC pci ri = adjustWS (\p -> p & rmId .~ ri) pci pcTbl


movePC_STM :: WorldState -> Id -> Id -> STM ()
movePC_STM ws pci ri = adjust_STM (\p -> p & rmId .~ ri) pci (ws^.pcTbl)


-- ==================================================
-- Rooms:


getRm :: Id -> MudStack Rm
getRm i = i `lookupWS` rmTbl


getRm_STM :: WorldState -> Id -> STM Rm
getRm_STM ws i = i `lookup_STM` (ws^.rmTbl)


getPCRmId :: Id -> MudStack Id
getPCRmId i = (^.rmId) <$> getPC i


getPCRmId_STM :: WorldState -> Id -> STM Id
getPCRmId_STM ws i = (^.rmId) <$> getPC_STM ws i


getPCRm :: Id -> MudStack Rm
getPCRm i = getRm =<< getPCRmId i


getPCRm_STM :: WorldState -> Id -> STM Rm
getPCRm_STM ws i = getRm_STM ws =<< getPCRmId_STM ws i


getPCRmInvCoins :: Id -> MudStack InvCoins
getPCRmInvCoins i = getInvCoins =<< getPCRmId i


getPCRmInvCoins_STM :: WorldState -> Id -> STM InvCoins
getPCRmInvCoins_STM ws i = getInvCoins_STM ws =<< getPCRmId_STM ws i


getRmLinks :: Id -> MudStack [RmLink]
getRmLinks i = (^.rmLinks) <$> getRm i


getRmLinks_STM :: WorldState -> Id -> STM [RmLink]
getRmLinks_STM ws i = (^.rmLinks) <$> getRm_STM ws i


findExit :: LinkName -> Id -> MudStack (Maybe Id)
findExit ln i = getRmLinks i >>= \rls ->
    return $ case [ rl^.destId | rl <- rls, isValid rl ] of
      [] -> Nothing
      is -> Just . head $ is
  where
    isValid rl = ln `elem` stdLinkNames && ln == (rl^.linkName) || ln `notElem` stdLinkNames && ln `T.isInfixOf` (rl^.linkName)


findExit_STM :: WorldState -> LinkName -> Id -> STM (Maybe Id)
findExit_STM ws ln i = getRmLinks_STM ws i >>= \rls ->
    return $ case [ rl^.destId | rl <- rls, isValid rl ] of
      [] -> Nothing
      is -> Just . head $ is
  where
    isValid rl = ln `elem` stdLinkNames && ln == (rl^.linkName) || ln `notElem` stdLinkNames && ln `T.isInfixOf` (rl^.linkName)
-}


{-
-- ==================================================
-- Helpers for working with non-world state tables:


type NWSTblGetting a = Getting (TVar (IntMap a)) NonWorldState (TVar (IntMap a))


onNonWorldState :: (NonWorldState -> STM a) -> MudStack a
onNonWorldState f = liftIO . atomically . f =<< gets (^.nonWorldState)


lookupNWS :: forall a . Id -> NWSTblGetting a -> MudStack a
i `lookupNWS` tbl = onNonWorldState $ \nws ->
    i `lookup_STM` (nws^.tbl)


insertNWS :: forall a . Id -> a -> NWSTblGetting a -> MudStack ()
insertNWS i a tbl = onNonWorldState $ \nws ->
    insert_STM i a (nws^.tbl)


adjustNWS :: forall a . (a -> a) -> Id -> NWSTblGetting a -> MudStack ()
adjustNWS f i tbl = onNonWorldState $ \nws ->
    adjust_STM f i (nws^.tbl)
-}


-- ==================================================
-- Helpers for working with players:


getPla :: Id -> MudStack Pla
getPla i = gets (^.nonWorldState.plaTbl) >>= \pt -> return (pt ! i)


--getPla_STM :: NonWorldState -> Id -> STM Pla
--getPla_STM nws i = i `lookup_STM` (nws^.plaTbl)


getPlaColumns :: Id -> MudStack Int
getPlaColumns i = (^.columns) <$> getPla i


--getPlaColumns_STM :: NonWorldState -> Id -> STM Int
--getPlaColumns_STM nws i = (^.columns) <$> getPla_STM nws i


-- ==================================================
-- "output" and related helpers:


output :: T.Text -> MudStack ()
output t = getPlaColumns 0 >>= \cols ->
    mapM_ (liftIO . T.putStrLn) $ wordWrap cols t


outputIndent :: Int -> T.Text -> MudStack ()
outputIndent n t = getPlaColumns 0 >>= \cols ->
    liftIO . mapM_ T.putStrLn . wordWrapIndent n cols $ t


outputCon :: [T.Text] -> MudStack () -- Prefer over "output" when there would be more than two "<>"s.
outputCon = output . T.concat


outputConIndent :: Int -> [T.Text] -> MudStack ()
outputConIndent n = outputIndent n . T.concat


dumpFile :: FilePath -> MudStack () -- TODO: Implement paging.
dumpFile fn = takeADump =<< (liftIO . T.readFile $ fn)
  where
    takeADump contents = getPlaColumns 0 >>= \cols ->
        mapM_ (liftIO . T.putStrLn) (concat . wordWrapLines cols . T.lines $ contents)


dumpFileWithDividers :: FilePath -> MudStack ()
dumpFileWithDividers fn = divider >> dumpFile fn >> divider


divider :: MudStack ()
divider = getPlaColumns 0 >>= \cols ->
    liftIO . T.putStrLn . T.replicate cols $ "="


dispAssocList :: (Show a, Show b) => [(a, b)] -> MudStack ()
dispAssocList = mapM_ takeADump
  where
    takeADump (a, b) = outputIndent 2 $ (unquote . showText $ a) <> ": " <> showText b


dispGenericErrorMsg :: MudStack ()
dispGenericErrorMsg = output "Unfortunately, an error occured while executing your command."
