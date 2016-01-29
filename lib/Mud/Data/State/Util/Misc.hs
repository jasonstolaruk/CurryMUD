{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns, OverloadedStrings, TransformListComp, TupleSections, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( aOrAnType
                                , BothGramNos
                                , dropPrefixes
                                , dropPrefixesForHooks
                                , findMobIds
                                , getAdminIds
                                , getEffBothGramNos
                                , getEffName
                                , getHookFun
                                , getIdForMobSing
                                , getLoggedInAdminIds
                                , getLoggedInPlaIds
                                , getMobRmNonIncogInvCoins
                                , getNonIncogInv
                                , getNonIncogInvCoins
                                , getNonIncogLoggedInAdminIds
                                , getNpcIds
                                , getRmActionFun
                                , getState
                                , getUnusedId
                                , isLoggedIn
                                , isNpc
                                , isPC
                                , lookupHooks
                                , mkAdminIdSingList
                                , mkAdminPlaIdSingList
                                , mkCapsFun
                                , mkCoinsMsgs
                                , mkNameCountBothList
                                , mkPlaIdSingList
                                , mkPlurFromBoth
                                , mkSerializedNonStdDesig
                                , mkStdDesig
                                , mkUnknownPCEntName
                                , modifyState
                                , newObj
                                , onEnv
                                , pcNpc
                                , pluralize
                                , procHooks
                                , removeAdHoc
                                , setInterp
                                , sortInv
                                , tweak
                                , tweaks
                                , withLock ) where
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.TheWorld.Zones.AdminZoneIds (iWelcome)
import Mud.TopLvlDefs.Chars
import Mud.Util.List
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), first)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Exception.Lifted (bracket)
import Control.Lens (_1, _2, at, both, over, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.IntMap.Lazy ((!))
import Data.IORef (atomicModifyIORef, readIORef)
import Data.List ((\\), delete, foldl', nub, sortBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Monoid (Sum(..), (<>))
import Data.Text (Text)
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM (filter, keys, toList)
import qualified Data.Map.Lazy as M (lookup)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V (Vector)
import Text.Regex.Posix ((=~))


blowUp :: Text -> Text -> [Text] -> a
blowUp = U.blowUp "Mud.Data.State.Util.Misc"


patternMatchFail :: Text -> [Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Misc"


-- ==================================================


aOrAnType :: Type -> Text
aOrAnType t@ClothType = pp t
aOrAnType t@ArmType   = pp t
aOrAnType t           = aOrAn . pp $ t


-----


findMobIds :: MudState -> [Id] -> [Id]
findMobIds ms haystack = [ i | i <- haystack
                             , uncurry (||) . ((PCType |&|) *** (NpcType |&|)) . over both (==) . dup . getType i $ ms ]


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


getEffName :: Id -> MudState -> Id -> Text
getEffName i ms targetId = let targetEnt = getEnt targetId ms
                           in fromMaybe (helper $ targetEnt^.sing) $ targetEnt^.entName
  where
    helper targetSing
      | isNpc i ms || views (pcTbl.ind i.introduced) (targetSing `notElem`) ms = mkUnknownPCEntName targetId ms
      | otherwise                                                              = uncapitalize targetSing


mkUnknownPCEntName :: Id -> MudState -> Text
mkUnknownPCEntName i ms = views entName (fromMaybe helper) . getEnt i $ ms
  where
    helper = let (T.head . pp *** pp -> (h, r)) = getSexRace i ms in h `T.cons` r


-----


getHookFun :: HookName -> MudState -> HookFun
getHookFun n = views (hookFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getHookFun" "Hook name not found in hook function table." . pure $ n


-----


getIdForMobSing :: Sing -> MudState -> Id
getIdForMobSing s ms = let [(i, _)] = views entTbl (IM.toList . IM.filter (views sing (== s))) ms in i


-----


getLoggedInAdminIds :: MudState -> Inv
getLoggedInAdminIds = getAdminIdsHelper isLoggedIn


getLoggedInPlaIds :: MudState ->  Inv
getLoggedInPlaIds = views plaTbl (IM.keys . IM.filter (uncurry (&&) . (isLoggedIn *** not . isAdmin) . dup))


-----


getMobRmNonIncogInvCoins :: Id -> MudState -> (Inv, Coins)
getMobRmNonIncogInvCoins i ms = let ri = getRmId i ms in getNonIncogInvCoins ri ms


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


getRmActionFun :: RmActionFunName -> MudState -> RmActionFun
getRmActionFun n = views (rmActionFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getRmActionFun" "Function name not found in room action function table." . pure $ n


-----


getState :: MudStack MudState
getState = onEnv $ liftIO . readIORef . view mudStateIORef


onEnv :: (MudData -> MudStack a) -> MudStack a
onEnv = (ask >>=)


-----


getUnusedId :: MudState -> Id
getUnusedId = views typeTbl (head . ([0..] \\) . IM.keys)


-----


isLoggedIn :: Pla -> Bool
isLoggedIn = views lastRmId ((()#) . (Sum <$>))


-----


lookupHooks :: Id -> MudState -> CmdName -> Maybe [Hook]
lookupHooks i ms cn = views hookMap (M.lookup cn) . getMobRm i $ ms


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


mkCoinsMsgs :: (Int -> Text -> Text) -> Coins -> [Text]
mkCoinsMsgs f (Coins (cop, sil, gol)) = catMaybes [ c, s, g ]
  where
    c = Sum cop |!| Just . f cop $ "copper piece"
    s = Sum sil |!| Just . f sil $ "silver piece"
    g = Sum gol |!| Just . f gol $ "gold piece"


-----


mkNameCountBothList :: Id -> MudState -> Inv -> [(Text, Int, BothGramNos)]
mkNameCountBothList i ms targetIds = let ens   = [ getEffName        i ms targetId | targetId <- targetIds ]
                                         cs    = mkCountList ebgns
                                         ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
                                     in nub . zip3 ens cs $ ebgns


-----


mkPlaIdSingList :: MudState -> [(Id, Sing)]
mkPlaIdSingList = mkIdSingListHelper not


-----


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p


-----


mkSerializedNonStdDesig :: Id -> MudState -> Sing -> AOrThe -> ShouldCap -> Text
mkSerializedNonStdDesig i ms s aot (mkCapsFun -> f) =
    serialize NonStdDesig { nsDesigEntSing = s, nsDesc = helper }
  where
    helper | isPC i ms = g $ let (pp *** pp -> (sexy, r)) = getSexRace i ms in sexy <> " " <> r
           | otherwise = onFalse (isCapital s) g s
    g = f . (pp aot <>) . (" " <>)


mkCapsFun :: ShouldCap -> Text -> Text
mkCapsFun = \case DoCap    -> capitalize
                  Don'tCap -> id


-----


mkStdDesig :: Id -> MudState -> ShouldCap -> Desig
mkStdDesig i ms sc = StdDesig { sDesigEntSing = Just . getSing i $ ms
                              , shouldCap     = sc
                              , desigEntName  = views entName (fromMaybe (mkUnknownPCEntName i ms)) . getEnt i $ ms
                              , desigId       = i
                              , desigIds      = findMobIds ms . getMobRmInv i $ ms }


-----


modifyState :: (MudState -> (MudState, a)) -> MudStack a
modifyState f = ask >>= \md -> liftIO .  atomicModifyIORef (md^.mudStateIORef) $ f


-----


type InvId = Id


newObj :: MudState -> Ent -> Obj -> InvId -> MudState
newObj ms e@(view entId -> i) o invId = let ms' = ms & entTbl .ind i .~ e
                                                     & objTbl .ind i .~ o
                                                     & typeTbl.ind i .~ ObjType
                                        in ms' & invTbl.ind invId %~ (sortInv ms' . (i :))


-----


pcNpc :: Id -> MudState -> MudStack () -> MudStack () -> MudStack ()
pcNpc i ms a b = case getType i ms of
  PCType  -> a
  NpcType -> b
  t       -> patternMatchFail "pcNpc" [ showText t ]


-----


pluralize :: BothGramNos -> Int -> Text
pluralize (s, p) x = x == 1 ? s :? p


-----


-- Suitable for hooks whose triggers match on any argument.
procHooks :: Id -> MudState -> V.Vector Int -> CmdName -> Args -> (Args, GenericIntermediateRes)
procHooks i ms v cn as | initAcc <- (as, (ms, [], [], [])) = case lookupHooks i ms cn of
  Nothing    -> initAcc
  Just hooks ->
    let as' = dropPrefixesForHooks hooks as
    in case filter (\Hook { triggers } -> any (`elem` triggers) as') hooks of
      []      -> initAcc
      matches ->
        let xformedArgs = foldr (\Hook { triggers } -> dropSynonyms triggers) as' matches
            hookHelper a@(_, (ms', _, _, _)) h@(Hook hn _ ) = getHookFun hn ms' i h v a
        in foldl' hookHelper (first (const xformedArgs) initAcc) matches


dropPrefixesForHooks :: [Hook] -> Args -> Args
dropPrefixesForHooks hs = let helper _     []     = []
                              helper trigs (a:as) | a' <- dropPrefixes a, a' `elem` trigs = a' : rest
                                                  | otherwise                             = a  : rest
                                where
                                  rest = helper trigs as
                          in helper (concatMap triggers hs)


dropPrefixes :: Text -> Text
dropPrefixes     (T.uncons -> Just (x, xs)) | x == allChar, ()!# xs = xs
dropPrefixes arg@(T.unpack -> arg'        )
  | triple@(_, _, c) <- arg' =~ mkRegex indexChar,  isMatch triple = T.pack c
  | triple@(_, _, c) <- arg' =~ mkRegex amountChar, isMatch triple = T.pack c
  | otherwise                                                      = arg
  where
    isMatch :: (String, String, String) -> Bool
    isMatch (a, b, c) = and [ ()# a, ()!# b, ()!# c ]
    mkRegex c         = "^[0-9]+\\" <> pure c :: String


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
setInterp i mi = tweak $ mobTbl.ind i.interp .~ mi


-----


sortInv :: MudState -> Inv -> Inv
sortInv ms is = let (foldr helper ([], []) -> (pcs, others)) = [ (i, getType i ms) | i <- is ]
                in (pcs ++) . sortOthers $ others
  where
    helper (i, t) acc                  = let consTo lens = acc & lens %~ (i :)
                                         in t == PCType ? consTo _1 :? consTo _2
    sortOthers                         = map (view _1) . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped others                      = [ (i, views entName fromJust e, e^.sing) | i <- others
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
