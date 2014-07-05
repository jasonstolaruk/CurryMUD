{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers ( addToInv
                        , BothGramNos
                        , findExit
                        , getArm
                        , getCloth
                        , getCoins
                        , getEnt
                        , getEntBothGramNos
                        , getEntBothGramNosInInv
                        , getEntNamesInInv
                        , getEntsInInv
                        , getEntType
                        , getEq
                        , getEqMap
                        , getInv
                        , getInvCoins
                        , getMob
                        , getMobGender
                        , getMobHand
                        , getPCRm
                        , getPCRmId
                        , getPCRmInvCoins
                        , getRm
                        , getRmLinks
                        , getWpn
                        , hasCoins
                        , hasEq
                        , hasInv
                        , hasInvOrCoins
                        , InvCoins
                        , mkCoinsFromList
                        , mkCoinsList
                        , mkPlurFromBoth
                        , moveCoins
                        , moveInv
                        , remFromInv
                        , sortInv ) where

import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Lens (_1, at, each, ix)
import Control.Lens.Operators ((%~), (?=), (^.), (^?!))
import Control.Monad (unless)
import Control.Monad.State (gets)
import Data.List (sortBy)
import Data.Monoid ((<>), mempty)
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.StateHelpers"


-- ==================================================
-- Entities:


getEnt :: Id -> MudStack Ent
getEnt i = gets (^?!entTbl.ix i)


getEntType :: Ent -> MudStack Type
getEntType e = let i = e^.entId
               in gets (^?!typeTbl.ix i)


getEntsInInv :: Inv -> MudStack [Ent]
getEntsInInv = mapM getEnt


getEntNamesInInv :: Inv -> MudStack [T.Text]
getEntNamesInInv is = getEntsInInv is >>= \es ->
    return [ e^.name | e <- es ]


getEntSingsInInv :: Inv -> MudStack [T.Text]
getEntSingsInInv is = getEntsInInv is >>= \es ->
    return [ e^.sing | e <- es ]


type BothGramNos = (Sing, Plur)


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


getEntBothGramNosInInv :: Inv -> MudStack [BothGramNos]
getEntBothGramNosInInv is = map getEntBothGramNos <$> getEntsInInv is


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p)  = p


-- ==================================================
-- Clothing:


getCloth :: Id -> MudStack Cloth
getCloth i = gets (^?!clothTbl.ix i)


-- ==================================================
-- Inventories:


getInv :: Id -> MudStack Inv
getInv i = gets (^?!invTbl.ix i)


hasInv :: Id -> MudStack Bool
hasInv i = not . null <$> getInv i


hasInvOrCoins :: Id -> MudStack Bool
hasInvOrCoins i = do
    hi <- hasInv   i
    hc <- hasCoins i
    return (hi || hc)


type InvCoins = (Inv, Coins)


getInvCoins :: Id -> MudStack InvCoins
getInvCoins i = (,) <$> getInv i <*> getCoins i


addToInv :: Inv -> Id -> MudStack ()
addToInv is ti = getInv ti >>= sortInv . (++ is) >>= (invTbl.at ti ?=)


remFromInv :: Inv -> FromId -> MudStack ()
remFromInv is fi = getInv fi >>= \fis ->
    invTbl.at fi ?= deleteFirstOfEach is fis


moveInv :: Inv -> FromId -> ToId -> MudStack ()
moveInv [] _  _  = return ()
moveInv is fi ti = remFromInv is fi >> addToInv is ti


sortInv :: Inv -> MudStack Inv
sortInv is = (map (^._1) . sortBy nameThenSing) <$> zipped
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped = zip3 is <$> getEntNamesInInv is <*> getEntSingsInInv is


-- ==================================================
-- Coins:


getCoins :: Id -> MudStack Coins
getCoins i = gets (^?!coinsTbl.ix i)


mkCoinsList :: Coins -> [Int]
mkCoinsList (Coins (c, g, s)) = [c, g, s]


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [cop, sil, gol] = Coins (cop, sil, gol)
mkCoinsFromList xs              = patternMatchFail "mkCoinsFromList" [ showText xs ]


hasCoins :: Id -> MudStack Bool
hasCoins i = not . all (== 0) . mkCoinsList <$> getCoins i


type FromId = Id
type ToId   = Id


moveCoins :: Coins -> FromId -> ToId -> MudStack ()
moveCoins c fi ti = unless (c == mempty) $ subCoins c fi >> addCoins c ti


addCoins :: Coins -> Id -> MudStack ()
addCoins c i = getCoins i >>= \c' ->
    coinsTbl.at i ?= c' <> c


subCoins :: Coins -> Id -> MudStack ()
subCoins c i = getCoins i >>= \c' ->
    coinsTbl.at i ?= c' <> negateCoins c


negateCoins :: Coins -> Coins
negateCoins (Coins c) = Coins (each %~ negate $ c)


-- ==================================================
-- Weapons:


getWpn :: Id -> MudStack Wpn
getWpn i = gets (^?!wpnTbl.ix i)


-- ==================================================
-- Armor:


getArm :: Id -> MudStack Arm
getArm i = gets (^?!armTbl.ix i)


-- ==================================================
-- Equipment:


getEqMap :: Id -> MudStack EqMap
getEqMap i = gets (^?!eqTbl.ix i)


getEq :: Id -> MudStack Inv
getEq i = M.elems <$> getEqMap i


hasEq :: Id -> MudStack Bool
hasEq i = not . null <$> getEq i


-- ==================================================
-- Mobiles:


getMob :: Id -> MudStack Mob
getMob i = gets (^?!mobTbl.ix i)


getMobGender :: Id -> MudStack Gender
getMobGender i = (^.gender) <$> getMob i


getMobHand :: Id -> MudStack Hand
getMobHand i = (^.hand) <$> getMob i


-- ==================================================
-- Rooms:


getRm :: Id -> MudStack Rm
getRm i = gets (^?!rmTbl.ix i)


getPCRmId :: MudStack Id
getPCRmId = gets (^.pc.rmId)


getPCRm :: MudStack Rm
getPCRm = getPCRmId >>= getRm


getPCRmInvCoins :: MudStack InvCoins
getPCRmInvCoins = getPCRmId >>= getInvCoins


getRmLinks :: Id -> MudStack [RmLink]
getRmLinks i = (^.rmLinks) <$> getRm i


findExit :: LinkName -> Id -> MudStack (Maybe Id)
findExit ln i = getRmLinks i >>= \rls ->
    case [ rl^.destId | rl <- rls, isValid rl ] of
      [] -> return Nothing
      is -> return (Just . head $ is)
  where
    isValid rl = ln `elem` stdLinkNames && ln == (rl^.linkName) || ln `notElem` stdLinkNames && ln `T.isInfixOf` (rl^.linkName)
