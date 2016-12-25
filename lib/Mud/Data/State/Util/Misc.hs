{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns, OverloadedStrings, TransformListComp, TupleSections, ViewPatterns #-}

-- This module contains state-related functions used by multiple modules.

module Mud.Data.State.Util.Misc ( addToInv
                                , descMaybeId
                                , descMaybeSingId
                                , descSingId
                                , dropPrefixes
                                , dropPrefixesForHooks
                                , expandCorpseTxt
                                , findInvContaining
                                , findMobIds
                                , getAdminIds
                                , getBothGramNos
                                , getCorpseDesc
                                , getEffBothGramNos
                                , getEffName
                                , getFeelingFun
                                , getFun
                                , getHookFun
                                , getInstaEffectFun
                                , getLogAsyncs
                                , getLoggedInAdminIds
                                , getLoggedInPlaIds
                                , getMobRmVisibleInvCoins
                                , getNonIncogLoggedInAdminIds
                                , getNpcIds
                                , getRmActionFun
                                , getState
                                , getUnusedId
                                , getVisibleInv
                                , getVisibleInvCoins
                                , isAdHoc
                                , isAwake
                                , isDead
                                , isKnownLang
                                , isLoggedIn
                                , isPCCorpse
                                , leaveParty
                                , linkDirToCmdName
                                , lookupHooks
                                , mkAdminIdSingList
                                , mkAdminPlaIdSingList
                                , mkCorpseAppellation
                                , mkCorpseTxt
                                , mkMaybeCorpseId
                                , mkMobRmDesc
                                , mkName_maybeCorpseId_count_bothList
                                , mkNameCountBothList
                                , mkPlaIdSingList
                                , mkPrettySexRace
                                , mkPrettySexRaceLvl
                                , mkSerializedNonStdDesig
                                , mkStdDesig
                                , mkUnknownPCEntName
                                , modifyState
                                , modifyStateSeq
                                , onEnv
                                , plurRace
                                , procHooks
                                , procQuoteChars
                                , removeAdHoc
                                , runEffectFun
                                , setInterp
                                , sortInv
                                , tweak
                                , tweaks ) where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Hierarchy
import Mud.Misc.Misc
import Mud.TheWorld.Zones.AdminZoneIds (iNecropolis, iWelcome)
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.List hiding (countOcc)
import Mud.Util.Misc hiding (blowUp, patternMatchFail)
import Mud.Util.Operators
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (blowUp, patternMatchFail)

import Control.Arrow ((***), (&&&), first)
import Control.Lens (_1, _2, at, both, view, views)
import Control.Lens.Operators ((%~), (&), (.~), (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Bool (bool)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.List ((\\), delete, foldl', nub, nubBy, sortBy, zip4)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Sum(..), (<>))
import Data.Text (Text)
import GHC.Exts (sortWith)
import qualified Data.IntMap.Lazy as IM ((!), filter, keys)
import qualified Data.Map.Lazy as M (lookup)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V (Vector)
import Text.Regex.Posix ((=~))


{-# ANN module ("HLint: ignore Use &&" :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Data.State.Util.Misc"


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Misc"


-- ==================================================


addToInv :: MudState -> Inv -> Inv -> Inv
addToInv ms addThese toThese = sortInv ms $ toThese ++ addThese


-----


descSingId :: Id -> MudState -> Text
descSingId i ms = quoteWith' (i |&| ((`getSing` ms) &&& parensQuote . showText)) " "


descMaybeId :: MudState -> Maybe Id -> Text
descMaybeId ms = maybe none (`descSingId` ms)


descMaybeSingId :: Maybe Id -> MudState -> Text
descMaybeSingId Nothing  _  = none
descMaybeSingId (Just x) ms = descSingId x ms


-----


expandCorpseTxt :: Text -> Text -> Text
expandCorpseTxt = T.replace (T.singleton corpseNameMarker)


-----


findInvContaining :: Id -> MudState -> Maybe Id
findInvContaining i ms = let matches = views invTbl (IM.keys . IM.filter (i `elem`)) ms
                         in ()# matches ? Nothing :? Just (head matches)


-----


findMobIds :: MudState -> Inv -> Inv
findMobIds ms haystack = [ i | i <- haystack, (||) <$> (PCType ==) <*> (NpcType ==) $ getType i ms ]


-----


getAdminIds :: MudState -> Inv
getAdminIds = getAdminIdsHelper (const True)


getAdminIdsHelper :: (Pla -> Bool) -> MudState -> Inv
getAdminIdsHelper f = views plaTbl (IM.keys . IM.filter ((&&) <$> isAdmin <*> f))


-----


getBothGramNos :: Id -> MudState -> BothGramNos
getBothGramNos i = (sing `fanView` plur) . getEnt i


getEffBothGramNos :: Id -> MudState -> Id -> BothGramNos
getEffBothGramNos i ms targetId =
    let targetEnt  = getEnt targetId ms
        targetSing = targetEnt^.sing
        pair       = (targetSing, targetEnt^.plur)
    in case targetEnt^.entName of
      Nothing -> let (pp -> targetSexy, targetRace) = getSexRace targetId ms
                 in if targetSing `elem` getIntroduced i ms
                   then (targetSing,    ""                 )
                   else (pp targetRace, plurRace targetRace) & both %~ ((targetSexy <>) . spcL)
      Just {} | getType targetId ms == CorpseType
              , isPCCorpse . getCorpse targetId $ ms -> pair & _1 .~ mkCorpseAppellation i ms targetId
              | otherwise                            -> pair


plurRace :: Race -> Text
plurRace Dwarf = "dwarves"
plurRace Elf   = "elves"
plurRace r     = pp r <> "s"


-----


getCorpseDesc :: Id -> MudState -> Text
getCorpseDesc i ms = let c    = getCorpse i ms
                         lens = bool npcCorpseDesc pcCorpseDesc . isPCCorpse $ c
                     in c^.lens


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
    helper = uncurry T.cons . first T.head . mkPrettySexRace i $ ms


-----


getFeelingFun :: FeelingTag -> MudState -> FeelingFun
getFeelingFun tag = views (feelingFunTbl.at tag) (fromMaybe oops)
  where
    oops = blowUp "getFeelingFun" "feeling tag not found in function table" tag


-----


getFun :: FunName -> MudState -> Fun
getFun n = views (funTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getFun" "function name not found in function table" n


-----


getHookFun :: HookName -> MudState -> HookFun
getHookFun n = views (hookFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getHookFun" "hook name not found in hook function table" n


-----


getInstaEffectFun :: FunName -> MudState -> InstaEffectFun
getInstaEffectFun n = views (instaEffectFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getInstaEffectFun" "function name not found in instantaneous effect function table" n


-----


getLogAsyncs :: MudData -> (LogAsync, LogAsync)
getLogAsyncs = getAsync noticeLog &&& getAsync errorLog
  where
    getAsync = flip views (fst . fromJust)


-----


getLoggedInAdminIds :: MudState -> Inv
getLoggedInAdminIds = getAdminIdsHelper isLoggedIn


getLoggedInPlaIds :: MudState ->  Inv
getLoggedInPlaIds = views plaTbl (IM.keys . IM.filter ((&&) <$> isLoggedIn <*> not . isAdmin))


-----


getMobRmVisibleInvCoins :: Id -> MudState -> (Inv, Coins)
getMobRmVisibleInvCoins i ms = let ri = getRmId i ms in getVisibleInvCoins ri ms


-----


getVisibleInv :: Id -> MudState -> Inv
getVisibleInv i ms = filter isVisible . getInv i $ ms
  where
    isVisible targetId | not . isPC targetId $ ms          = otherwise
                       | isSpiritId targetId ms            = likewise
                       | not . isIncognitoId targetId $ ms = otherwise
                       | otherwise                         = likewise


-----


getVisibleInvCoins :: Id -> MudState -> (Inv, Coins)
getVisibleInvCoins i = getVisibleInv i &&& getCoins i


-----


getNonIncogLoggedInAdminIds :: MudState -> Inv
getNonIncogLoggedInAdminIds ms = let adminIds = getLoggedInAdminIds ms
                                 in [ adminId | adminId <- adminIds, not . isIncognitoId adminId $ ms ]


-----


getNpcIds :: MudState -> Inv
getNpcIds = views npcTbl IM.keys


-----


getRmActionFun :: FunName -> MudState -> RmActionFun
getRmActionFun n = views (rmActionFunTbl.at n) (fromMaybe oops)
  where
    oops = blowUp "getRmActionFun" "function name not found in room action function table" n


-----


getState :: MudStack MudState
getState = onEnv $ liftIO . readIORef . view mudStateIORef


onEnv :: (MudData -> MudStack a) -> MudStack a
onEnv = (ask >>=)


-----


getUnusedId :: MudState -> Id
getUnusedId = views typeTbl (head . (enumFrom 0 \\) . IM.keys)


-----


isAdHoc :: Id -> MudState -> Bool
isAdHoc i = (== iWelcome) . getRmId i


-----


isAwake :: Id -> MudState -> Bool
isAwake = onPla ((&&) <$> isLoggedIn <*> not . isIncognito) True


isLoggedIn :: Pla -> Bool
isLoggedIn = views logoutRmId ((()#) . (Sum <$>))


-----


isDead :: Id -> MudState -> Bool
isDead i = (== iNecropolis) . getRmId i


-----


isKnownLang :: Id -> MudState -> Lang -> Bool
isKnownLang i ms lang | lang == CommonLang = True
                      | otherwise          = lang `elem` getKnownLangs i ms


-----


isPCCorpse :: Corpse -> Bool
isPCCorpse PCCorpse  {} = True
isPCCorpse NpcCorpse {} = False


-----


leaveParty :: Id -> MudState -> MudState
leaveParty i ms = let helper p = memberOfHelper . myGroupHelper . followersHelper . followingHelper $ ms
                        where
                          followingHelper ms' = views following (maybe ms' f) p
                            where
                              f followingId = ms' & mobTbl.ind i          .party.following .~ Nothing
                                                  & mobTbl.ind followingId.party.followers %~ delete i
                          followersHelper ms' = (mobTbl.ind i.party.followers .~ []) . foldr f ms' $ p^.followers
                            where
                              f followerId = mobTbl.ind followerId.party.following .~ Nothing
                          myGroupHelper      = mobTbl.ind i.party.myGroup .~ []
                          memberOfHelper ms' = views memberOf (maybe ms' f) p
                            where
                              f memberOfId = ms' & mobTbl.ind i         .party.memberOf .~ Nothing
                                                 & mobTbl.ind memberOfId.party.myGroup  %~ delete i
                  in helper . getParty i $ ms


-----


linkDirToCmdName :: LinkDir -> CmdName
linkDirToCmdName North     = "n"
linkDirToCmdName Northeast = "ne"
linkDirToCmdName East      = "e"
linkDirToCmdName Southeast = "se"
linkDirToCmdName South     = "s"
linkDirToCmdName Southwest = "sw"
linkDirToCmdName West      = "w"
linkDirToCmdName Northwest = "nw"
linkDirToCmdName Up        = "u"
linkDirToCmdName Down      = "d"


-----


lookupHooks :: Id -> MudState -> CmdName -> Maybe [Hook]
lookupHooks i ms cn = views rmHookMap (M.lookup cn) . getMobRm i $ ms


-----


mkAdminIdSingList :: MudState -> [(Id, Sing)]
mkAdminIdSingList = mkIdSingListHelper id


mkIdSingListHelper :: (Bool -> Bool) -> MudState -> [(Id, Sing)]
mkIdSingListHelper f ms@(view plaTbl -> pt) = [ (i, s) | i <- IM.keys pt
                                                       , f . isAdmin $ pt IM.! i
                                                       , let s = getSing i ms
                                                       , then sortWith by s ]


-----


mkAdminPlaIdSingList :: MudState -> [(Id, Sing)]
mkAdminPlaIdSingList = mkIdSingListHelper (const True)


-----


mkCorpseAppellation :: Id -> MudState -> Id -> Text
mkCorpseAppellation i ms ci
  | isPCCorpse c, ((||) <$> (== getSing i ms) <*> (`elem` getIntroduced i ms)) cs = "corpse of " <> cs
  | otherwise = s
  where
    (c, s) = (getCorpse `fanUncurry` getSing) (ci, ms)
    cs     = c^.pcCorpseSing


-----


mkCorpseTxt :: (Text, Text) -> Text
mkCorpseTxt = uncurry (middle (<>) . T.singleton $ corpseNameMarker)


-----


mkMobRmDesc :: Id -> MudState -> Text
mkMobRmDesc i ms | hasMobId i ms = case getMobRmDesc i ms of Nothing   -> ""
                                                             Just desc -> parensQuote desc
                 | otherwise     = ""


-----


mkNameCountBothList :: Id -> MudState -> Inv -> [(Text, Int, BothGramNos)]
mkNameCountBothList i ms targetIds = let ens   = [ getEffName        i ms targetId | targetId <- targetIds ]
                                         cs    = mkCountList ebgns
                                         ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
                                     in nub . zip3 ens cs $ ebgns


mkName_maybeCorpseId_count_bothList :: Id -> MudState -> Inv -> [(Text, Maybe Id, Int, BothGramNos)]
mkName_maybeCorpseId_count_bothList i ms targetIds =
    let ens   = [ getEffName i ms targetId        | targetId <- targetIds ]
        mcis  = [ mkMaybeCorpseId targetId ms     | targetId <- targetIds ]
        cs    = mkCountList ebgns
        ebgns = [ getEffBothGramNos i ms targetId | targetId <- targetIds ]
    in nubBy f . zip4 ens mcis cs $ ebgns
  where
    (a, _, c, d) `f` (a', _, c', d') = (a, c, d) == (a', c', d')


mkMaybeCorpseId :: Id -> MudState -> Maybe Id
mkMaybeCorpseId i ms | getType i ms == CorpseType = case getCorpse i ms of PCCorpse  {} -> Just i
                                                                           NpcCorpse {} -> Nothing
                     | otherwise                  = Nothing


-----


mkPlaIdSingList :: MudState -> [(Id, Sing)]
mkPlaIdSingList = mkIdSingListHelper not


-----


mkPrettySexRace :: Id -> MudState -> (Text, Text)
mkPrettySexRace i = (pp *** pp) . getSexRace i


mkPrettySexRaceLvl :: Id -> MudState -> (Text, Text, Text)
mkPrettySexRaceLvl i ms = let (s, r) = mkPrettySexRace i ms
                              l      = getLvl          i ms
                          in (s, r, showText l)


-----


mkSerializedNonStdDesig :: Id -> MudState -> Sing -> AOrThe -> ShouldCap -> Text
mkSerializedNonStdDesig i ms s aot (mkCapsFun -> f) =
    serialize NonStdDesig { dEntSing = s, dDesc = helper }
  where
    helper | isPC i ms = g . uncurry (|<>|) . mkPrettySexRace i $ ms
           | otherwise = onFalse (isCapital s) g s
    g = f . (pp aot <>) . spcL


-----


mkStdDesig :: Id -> MudState -> ShouldCap -> Desig
mkStdDesig i ms sc = StdDesig { desigEntSing   = Just . getSing i $ ms
                              , desigShouldCap = sc
                              , desigEntName   = views entName (fromMaybe (mkUnknownPCEntName i ms)) . getEnt i $ ms
                              , desigId        = i
                              , desigIds       = findMobIds ms . getMobRmInv i $ ms }


-----


modifyState :: (MudState -> (MudState, a)) -> MudStack a
modifyState f = ask >>= \md -> liftIO .  atomicModifyIORef' (md^.mudStateIORef) $ f


modifyStateSeq :: (MudState -> (MudState, Funs)) -> MudStack ()
modifyStateSeq = modifyState >=> sequence_


-----


procHooks :: Id -> MudState -> V.Vector Int -> CmdName -> Args -> HookFunRes
procHooks i ms v cn as | initAcc <- (as, (ms, [], [], []), []) = case lookupHooks i ms cn of
  Nothing    -> initAcc
  Just hooks -> case as of
    -- Process hooks that match on a particular argument. These hooks need to see the other argument(s) passed as well,
    -- not just the argument that matches the hook's trigger.
    [arg] | delim <- T.singleton hookArgDelimiter
          , delim `T.isInfixOf` arg
          , (dropPrefixes -> a, T.tail -> rest) <- T.breakOn delim arg
          -> case filter (\Hook { hookTriggers } -> a `elem` hookTriggers) hooks of
               []  -> initAcc
               [h] -> getHookFun (hookName h) ms i h v (initAcc & _1 .~ pure rest)
               xs  -> patternMatchFail "procHooks" . showText $ xs
    -- Process hooks whose triggers match on any single argument.
    _ -> let helper acc arg = case filter (\Hook { hookTriggers } -> arg `elem` hookTriggers) hooks of
               []        -> acc
               (match:_) -> acc ++ pure match
             as' = dropPrefixesForHooks hooks as
         in case foldl' helper [] as' of
           []      -> initAcc
           matches ->
             let xformedArgs = foldr (\Hook { hookTriggers } -> dropSynonyms hookTriggers) as' matches
                 hookHelper a@(_, (ms', _, _, _), _) h = getHookFun (hookName h) ms' i h v a
             in foldl' hookHelper (initAcc & _1 .~ xformedArgs) . nub $ matches


dropPrefixesForHooks :: [Hook] -> Args -> Args
dropPrefixesForHooks hs = let helper _     []     = []
                              helper trigs (a:as) | a' <- dropPrefixes a, a' `elem` trigs = a' : rest
                                                  | otherwise                             = a  : rest
                                where
                                  rest = helper trigs as
                          in helper (concatMap hookTriggers hs)


dropPrefixes :: Text -> Text
dropPrefixes     (T.uncons -> Just (x, xs)) | x == allChar, ()!# xs = xs
dropPrefixes arg@(T.unpack -> arg'        )
  | triple@(_, _, c) <- arg' =~ mkRegex indexChar,  isMatch triple = T.pack c
  | triple@(_, _, c) <- arg' =~ mkRegex amountChar, isMatch triple = T.pack c
  | otherwise                                                      = arg
  where
    isMatch :: (String, String, String) -> Bool
    isMatch (a, b, c) = and [ ()# a, ()!# b, ()!# c ]
    mkRegex c         = "^[0-9]+\\" ++ pure c :: String


-----


procQuoteChars :: Args -> Maybe Args
procQuoteChars []                    = Just []
procQuoteChars as@(T.unwords -> txt) | not $ q `T.isInfixOf` txt = Just as
                                     | odd . countOcc q $ txt    = Nothing
                                     | otherwise = Just [ fillerToSpcs w | w <- T.words . helper $ txt ]
  where
    q         = T.singleton quoteChar
    helper "" = ""
    helper t  | q `T.isInfixOf` t = let (left,   T.tail -> rest ) = T.breakOn q t
                                        (quoted, T.tail -> right) = T.breakOn q rest
                                    in left <> spcsToFiller quoted <> helper right
              | otherwise         = t


-----


removeAdHoc :: Id -> MudState -> MudState
removeAdHoc i ms = ms & activeEffectsTbl.at  i        .~ Nothing
                      & coinsTbl        .at  i        .~ Nothing
                      & entTbl          .at  i        .~ Nothing
                      & eqTbl           .at  i        .~ Nothing
                      & invTbl          .at  i        .~ Nothing
                      & invTbl          .ind iWelcome %~ (i `delete`)
                      & mobTbl          .at  i        .~ Nothing
                      & msgQueueTbl     .at  i        .~ Nothing
                      & pausedEffectsTbl.at  i        .~ Nothing
                      & pcTbl           .at  i        .~ Nothing
                      & plaTbl          .at  i        .~ Nothing
                      & rndmNamesMstrTbl.at  i        .~ Nothing
                      & teleLinkMstrTbl .at  i        .~ Nothing
                      & typeTbl         .at  i        .~ Nothing


-----


runEffectFun :: FunName -> Id -> Seconds -> MudStack ()
runEffectFun n i secs = views (effectFunTbl.at n) (maybe oops (\f -> f i secs)) =<< getState
  where
    oops = blowUp "runEffectFun" "function name not found in effect function table" n


-----


setInterp :: Id -> Maybe Interp -> MudStack ()
setInterp i mi = tweak $ mobTbl.ind i.interp .~ mi


-----


sortInv :: MudState -> Inv -> Inv
sortInv ms is = let (foldr helper mempties -> (pcs, others)) = [ (i, getType i ms) | i <- is ]
                in (pcs ++) . sortOthers $ others
  where
    helper (i, t) acc                  = let consTo lens = acc & lens %~ (i :)
                                         in t == PCType ? consTo _1 :? consTo _2
    sortOthers                         = select _1 . sortBy nameThenSing . zipped
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped others                      = [ (i, views entName fromJust e, e^.sing) | i <- others
                                                                                  , let e = getEnt i ms ]


-----


tweak :: (MudState -> MudState) -> MudStack ()
tweak f = modifyState $ (, ()) . f


tweaks :: [MudState -> MudState] -> MudStack ()
tweaks fs = tweak $ \ms -> foldl' (&) ms fs
