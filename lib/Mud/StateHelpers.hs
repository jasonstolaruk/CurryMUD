{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers ( addToInv
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
                        , hasInv
                        , mkCoinsAmtList
                        , mkPlurFromBoth
                        , moveCoins
                        , moveInv
                        , remFromInv
                        , sortInv
                        , sumCoins ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util

import Control.Applicative ((<$>), (<*>))
import Control.Lens (_1, at, ix)
import Control.Lens.Operators ((?=), (^.), (^?!))
import Control.Monad.State (gets)
import Data.List (foldl', sortBy)
import Data.Monoid ((<>))
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


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


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


getEntBothGramNosInInv :: Inv -> MudStack [BothGramNos]
getEntBothGramNosInInv is = map getEntBothGramNos <$> getEntsInInv is


mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p)  = p


-----


getCloth :: Id -> MudStack Cloth
getCloth i = gets (^?!clothTbl.ix i)


-----


getWpn :: Id -> MudStack Wpn
getWpn i = gets (^?!wpnTbl.ix i)


-----


getArm :: Id -> MudStack Arm
getArm i = gets (^?!armTbl.ix i)


-----


getCoins :: Id -> MudStack Coins
getCoins i = gets (^?!coinsTbl.ix i)


mkCoinsAmtList :: Coins -> [Int]
mkCoinsAmtList (c, g, s) = [c, g, s]


hasCoins :: Id -> MudStack Bool
hasCoins i = not . all (== 0) . mkCoinsAmtList <$> getCoins i


moveCoins :: Coins -> Id -> Id -> MudStack ()
moveCoins c fi ti | c == noCoins = return ()
                  | otherwise    = subCoins c fi >> addCoins c ti


addCoins :: Coins -> Id -> MudStack ()
addCoins c i = getCoins i >>= \c' ->
    coinsTbl.at i ?= c' `plusCoins` c


subCoins :: Coins -> Id -> MudStack ()
subCoins c i = getCoins i >>= \c' ->
    coinsTbl.at i ?= c' `minusCoins` c


plusCoins :: Coins -> Coins -> Coins
plusCoins = opCoins (+)


minusCoins :: Coins -> Coins -> Coins
minusCoins = opCoins (-)


opCoins :: (Int -> Int -> Int) -> Coins -> Coins -> Coins
opCoins op (cop, sil, gol) (cop', sil', gol') = (cop `op` cop', sil `op` sil', gol `op` gol') -- TODO: Is there a nifty way to do this using lenses?


sumCoins :: [Coins] -> Coins
sumCoins = foldl' plusCoins noCoins


-----


getInv :: Id -> MudStack Inv
getInv i = gets (^?!invTbl.ix i)


hasInv :: Id -> MudStack Bool
hasInv i = not . null <$> getInv i


getInvCoins :: Id -> MudStack InvCoins
getInvCoins i = (,) <$> getInv i <*> getCoins i


addToInv :: Inv -> Id -> MudStack ()
addToInv is ti = getInv ti >>= sortInv . (++ is) >>= (invTbl.at ti ?=)


remFromInv :: Inv -> Id -> MudStack ()
remFromInv is fi = getInv fi >>= \fis ->
    invTbl.at fi ?= deleteFirstOfEach is fis


moveInv :: Inv -> Id -> Id -> MudStack ()
moveInv [] _  _  = return ()
moveInv is fi ti = remFromInv is fi >> addToInv is ti


sortInv :: Inv -> MudStack Inv
sortInv is = (map (^._1) . sortBy nameThenSing) <$> zipped
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') <> (s `compare` s')
    zipped = zip3 is <$> getEntNamesInInv is <*> getEntSingsInInv is


-----


getEqMap :: Id -> MudStack EqMap
getEqMap i = gets (^?!eqTbl.ix i)


getEq :: Id -> MudStack Inv
getEq i = M.elems <$> getEqMap i


-----


getMob :: Id -> MudStack Mob
getMob i = gets (^?!mobTbl.ix i)


getMobGender :: Id -> MudStack Gender
getMobGender i = (^.gender) <$> getMob i


getMobHand :: Id -> MudStack Hand
getMobHand i = (^.hand) <$> getMob i


-----


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
