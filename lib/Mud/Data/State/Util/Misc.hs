{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, TransformListComp, TupleSections, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( aOrAnType
                                , BothGramNos
                                , findPCIds
                                , getAdminIds
                                , getEffBothGramNos
                                , getEffName
                                , getIdForPCSing
                                , getLoggedInAdminIds
                                , getLoggedInPlaIds
                                , getNonIncogInv
                                , getNonIncogInvCoins
                                , getNonIncogLoggedInAdminIds
                                , getNpcIds
                                , getNpcRm
                                , getNpcRmId
                                , getPCRmNonIncogInvCoins
                                , getState
                                , isLoggedIn
                                , mkAdminIdSingList
                                , mkAdminPlaIdSingList
                                , mkCapsFun
                                , mkPlaIdSingList
                                , mkPlurFromBoth
                                , mkSerializedNonStdDesig
                                , mkStdDesig
                                , mkUnknownPCEntName
                                , modifyState
                                , onEnv
                                , pluralize
                                , removeAdHoc
                                , setInterp
                                , sortInv
                                , tweak
                                , tweaks
                                , withLock ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.TheWorld.AdminZoneIds (iWelcome)
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Arrow ((***))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Exception.Lifted (bracket)
import Control.Lens (_1, _2, at, both, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.IORef (atomicModifyIORef, readIORef)
import Data.List (delete, foldl', sortBy)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Sum(..), (<>))
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM (filter, keys, toList)
import qualified Data.Text as T


aOrAnType :: Type -> T.Text
aOrAnType t@ClothType = pp t
aOrAnType t@ArmType   = pp t
aOrAnType t           = aOrAn . pp $ t


-----


findPCIds :: MudState -> [Id] -> [Id]
findPCIds ms haystack = [ i | i <- haystack, getType i ms == PCType ]


-----


getAdminIds :: MudState -> Inv
getAdminIds = getAdminIdsHelper (const True)


getAdminIdsHelper :: (Pla -> Bool) -> MudState -> Inv
getAdminIdsHelper f = IM.keys . IM.filter (uncurry (&&) . (isAdmin *** f) . dup) . view plaTbl


-----


type BothGramNos = (Sing, Plur)


getEffBothGramNos :: Id -> MudState -> Id -> BothGramNos
getEffBothGramNos i ms targetId =
    let targetEnt  = getEnt targetId ms
        targetSing = targetEnt^.sing
    in case targetEnt^.entName of
      Nothing -> let (pp *** pp -> (targetSexy, targetRace)) = getSexRace targetId ms
                 in if targetSing `elem` getIntroduced i ms
                   then (targetSing, "")
                   else (targetRace, plurRace targetRace) & both %~ ((targetSexy <>) . (" " <>))
      Just {} -> (targetSing, targetEnt^.plur)
  where
    plurRace "dwarf" = "dwarves"
    plurRace "elf"   = "elves"
    plurRace r       = r <> "s"


-----


getEffName :: Id -> MudState -> Id -> T.Text
getEffName i ms targetId = let targetEnt = getEnt targetId ms
                           in fromMaybe (helper $ targetEnt^.sing) $ targetEnt^.entName
  where
    helper targetSing | views (pcTbl.ind i.introduced) (targetSing `elem`) ms = uncapitalize targetSing
                      | otherwise                                             = mkUnknownPCEntName targetId ms


mkUnknownPCEntName :: Id -> MudState -> T.Text
mkUnknownPCEntName i ms = let (T.head . pp *** pp -> (h, r)) = getSexRace i ms in h `T.cons` r


-----


getIdForPCSing :: Sing -> MudState -> Id
getIdForPCSing s ms = let [(i, _)] = views entTbl (IM.toList . IM.filter (views sing (== s))) ms in i


-----


getLoggedInAdminIds :: MudState -> Inv
getLoggedInAdminIds = getAdminIdsHelper isLoggedIn


getLoggedInPlaIds :: MudState ->  Inv
getLoggedInPlaIds = views plaTbl (IM.keys . IM.filter (uncurry (&&) . (isLoggedIn *** not . isAdmin) . dup))


-----


getNonIncogInv :: Id -> MudState -> Inv
getNonIncogInv i ms = filter notIncog . getInv i $ ms
  where
    notIncog targetId | getType targetId ms /= PCType     = True
                      | not . isIncognitoId targetId $ ms = True
                      | otherwise                         = False


-----


getNonIncogInvCoins :: Id -> MudState -> (Inv, Coins)
getNonIncogInvCoins i = (getNonIncogInv i *** getCoins i) . dup


-----


getNonIncogLoggedInAdminIds :: MudState -> Inv
getNonIncogLoggedInAdminIds ms = let adminIds = getLoggedInAdminIds ms
                                 in [ adminId | adminId <- adminIds, not . isIncognitoId adminId $ ms ]


-----


getNpcIds :: MudState -> Inv
getNpcIds = views npcTbl IM.keys


-----


getNpcRm :: Id -> MudState -> Rm
getNpcRm i ms = let ri = getNpcRmId i ms in getRm ri ms


getNpcRmId :: Id -> MudState -> Id
getNpcRmId i = views invTbl (head . IM.keys . IM.filter (i `elem`))


-----


getPCRmNonIncogInvCoins :: Id -> MudState -> (Inv, Coins)
getPCRmNonIncogInvCoins i ms = let ri = getRmId i ms in getNonIncogInvCoins ri ms


-----


getState :: MudStack MudState
getState = onEnv $ liftIO . readIORef . view mudStateIORef


onEnv :: (MudData -> MudStack a) -> MudStack a
onEnv = (ask >>=)


-----


isLoggedIn :: Pla -> Bool
isLoggedIn = views lastRmId ((()#) . (Sum <$>))


-----


mkAdminIdSingList :: MudState -> [(Id, Sing)]
mkAdminIdSingList = mkIdSingListHelper id


mkIdSingListHelper :: (Bool -> Bool) -> MudState -> [(Id, Sing)]
mkIdSingListHelper f ms@(view plaTbl -> pt) = [ (i, s) | i <- IM.keys pt
                                                       , f . isAdmin $ pt ! i
                                                       , let s = getSing i ms
                                                       , then sortWith by s ]


-----


mkAdminPlaIdSingList :: MudState -> [(Id, Sing)]
mkAdminPlaIdSingList = mkIdSingListHelper (const True)


-----


mkPlaIdSingList :: MudState -> [(Id, Sing)]
mkPlaIdSingList = mkIdSingListHelper not


-----


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


-----


mkSerializedNonStdDesig :: Id -> MudState -> Sing -> AOrThe -> ShouldCap -> T.Text
mkSerializedNonStdDesig i ms s aot (mkCapsFun -> f) = let (pp *** pp -> (sexy, r)) = getSexRace i ms in
    serialize NonStdDesig { nonStdPCEntSing = s, nonStdDesc = f (pp aot) <> spaced sexy <> r }


mkCapsFun :: ShouldCap -> T.Text -> T.Text
mkCapsFun = \case DoCap    -> capitalize
                  Don'tCap -> id


-----


mkStdDesig :: Id -> MudState -> ShouldCap -> PCDesig
mkStdDesig i ms sc = StdDesig { stdPCEntSing = Just . getSing i $ ms
                              , shouldCap    = sc
                              , pcEntName    = mkUnknownPCEntName i ms
                              , pcId         = i
                              , pcIds        = findPCIds ms . getPCRmInv i $ ms }


-----


modifyState :: (MudState -> (MudState, a)) -> MudStack a
modifyState f = ask >>= \md -> liftIO .  atomicModifyIORef (md^.mudStateIORef) $ f


-----


pluralize :: BothGramNos -> Int -> T.Text
pluralize (s, p) x = x == 1 ? s :? p


-----


removeAdHoc :: Id -> MudState -> MudState
removeAdHoc i ms = ms & coinsTbl   .at  i        .~ Nothing
                      & entTbl     .at  i        .~ Nothing
                      & eqTbl      .at  i        .~ Nothing
                      & invTbl     .at  i        .~ Nothing
                      & invTbl     .ind iWelcome %~ (i `delete`)
                      & mobTbl     .at  i        .~ Nothing
                      & msgQueueTbl.at  i        .~ Nothing
                      & pcTbl      .at  i        .~ Nothing
                      & plaTbl     .at  i        .~ Nothing
                      & typeTbl    .at  i        .~ Nothing


-----


setInterp :: Id -> Maybe Interp -> MudStack ()
setInterp i mi = tweak $ plaTbl.ind i.interp .~ mi


-----


sortInv :: MudState -> Inv -> Inv
sortInv ms is = let (foldr helper ([], []) -> (pcIs, nonPCIs)) = [ (i, getType i ms) | i <- is ]
                in (pcIs ++) . sortNonPCs $ nonPCIs
  where
    helper (i, t) acc                  = let consTo lens = acc & lens %~ (i :)
                                         in t == PCType ? consTo _1 :? consTo _2
    sortNonPCs                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped nonPCIs                     = [ (i, views entName fromJust e, e^.sing) | i <- nonPCIs
                                                                                  , let e = getEnt i ms ]


-----


tweak :: (MudState -> MudState) -> MudStack ()
tweak f = modifyState $ (, ()) . f


tweaks :: [MudState -> MudState] -> MudStack ()
tweaks fs = tweak $ \ms -> foldl' (&) ms fs


-----


withLock :: Lock -> IO () -> IO ()
withLock l f = bracket (atomically . takeTMVar $ l)
                       (\Done -> atomically . putTMVar l $ Done)
                       (\Done -> f)
