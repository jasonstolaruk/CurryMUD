{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers ( addToInv
                        , findExit
                        , gerToMes
                        , getArm
                        , getCloth
                        , getEnt
                        , getEntBothGramNos
                        , getEntBothGramNosInInv
                        , getEntIds
                        , getEntNamesInInv
                        , getEntSingsInInv
                        , getEntsInInv
                        , getEntsInInvByName
                        , getEntType
                        , getEq
                        , getEqMap
                        , getInv
                        , getMob
                        , getMobHand
                        , getMobGender
                        , getPCEq 
                        , getPCEqMap
                        , getPCInv
                        , getPCMobHand
                        , getPCMobGender
                        , getPCRm
                        , getPCRmId
                        , getPCRmInv
                        , getRm
                        , getRmLinks
                        , getWpn
                        , makePlurFromBoth
                        , moveInv
                        , procGetEntResPCInv
                        , procGetEntResRm
                        , remFromInv
                        , sortInv ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp)
import qualified Mud.Util as U (blowUp)

import Control.Applicative ((<$>), (<*>))
import Control.Lens (_1, at, ix)
import Control.Lens.Operators ((?=), (^.), (^?!))
import Control.Monad.State (gets)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed)
import qualified Data.Map.Lazy as M (elems)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.StateHelpers"


getEnt :: Id -> MudStack Ent
getEnt i = gets (^?!entTbl.ix i)


getEntType :: Ent -> MudStack Type
getEntType e = let i = e^.entId
               in gets (^?!typeTbl.ix i)


getEntIds :: [Ent] -> Inv
getEntIds es = [ e^.entId | e <- es ]


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


makePlurFromBoth :: BothGramNos -> Plur
makePlurFromBoth (s, "") = s <> "s"
makePlurFromBoth (_, p)  = p


-----


getEntsInInvByName :: T.Text -> Inv -> MudStack GetEntResult
getEntsInInvByName searchName is
  | searchName == [allChar]^.packed = (Mult (length is) searchName . Just) <$> getEntsInInv is
  | T.head searchName == allChar = getMultEnts (maxBound :: Int) (T.tail searchName) is
  | isDigit (T.head searchName) = let numText = T.takeWhile isDigit searchName
                                      numInt  = either (oops numText) (^._1) $ decimal numText
                                      rest    = T.drop (T.length numText) searchName
                                  in parse rest numInt
  | otherwise = getMultEnts 1 searchName is
  where
    oops numText = blowUp "getEntsInInvByName" "unable to convert Text to Int" [ showText numText ]
    parse rest numInt
      | T.length rest < 2 = return (Sorry searchName)
      | otherwise = let delim = T.head rest
                        rest' = T.tail rest
                    in case () of _ | delim == amountChar -> getMultEnts   numInt rest' is -- TODO: Change to a multi-way if.
                                    | delim == indexChar  -> getIndexedEnt numInt rest' is
                                    | otherwise           -> return (Sorry searchName)


getMultEnts :: Amount -> T.Text -> Inv -> MudStack GetEntResult
getMultEnts a n is
  | a < 1     = return (Sorry n)
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findFullNameForAbbrev n
  where
    notFound = return (Mult a n Nothing)
    found fullName = (Mult a n . Just . takeMatchingEnts fullName) <$> getEntsInInv is
    takeMatchingEnts fn = take a . filter (\e -> e^.name == fn)


getIndexedEnt :: Index -> T.Text -> Inv -> MudStack GetEntResult
getIndexedEnt x n is
  | x < 1     = return (Sorry n)
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findFullNameForAbbrev n
  where
    notFound = return (Indexed x n (Left ""))
    found fullName = filter (\e -> e^.name == fullName) <$> getEntsInInv is >>= \matches ->
        if length matches < x
          then let both = getEntBothGramNos . head $ matches
               in return (Indexed x n (Left . makePlurFromBoth $ both))
          else return (Indexed x n (Right $ matches !! (x - 1)))


gerToMes :: GetEntResult -> MudStack (Maybe [Ent])
gerToMes ger = case ger of
  (Mult    _ _ (Just es)) -> return (Just es)
  (Indexed _ _ (Right e)) -> return (Just [e])
  _                       -> return Nothing


procGetEntResRm :: GetEntResult -> MudStack (Maybe [Ent])
procGetEntResRm ger = case ger of
  Sorry n                 -> output ("You don't see " <> aOrAn n <> " here.")             >> return Nothing
  (Mult 1 n Nothing)      -> output ("You don't see " <> aOrAn n <> " here.")             >> return Nothing
  (Mult _ n Nothing)      -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Mult _ _ (Just es))    -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't see ", showText x, " ", p, " here." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


procGetEntResPCInv :: GetEntResult -> MudStack (Maybe [Ent])
procGetEntResPCInv ger = case ger of
  Sorry n                 -> output ("You don't have " <> aOrAn n <> ".")             >> return Nothing
  (Mult 1 n Nothing)      -> output ("You don't have " <> aOrAn n <> ".")             >> return Nothing
  (Mult _ n Nothing)      -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Mult _ _ (Just es))    -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't have ", showText x, " ", p, "." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


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


getInv :: Id -> MudStack Inv
getInv i = gets (^?!invTbl.ix i)


getPCInv :: MudStack Inv
getPCInv = getInv 0


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


getPCEqMap :: MudStack EqMap
getPCEqMap = getEqMap 0


getEq :: Id -> MudStack Inv
getEq i = M.elems <$> getEqMap i


getPCEq :: MudStack Inv
getPCEq = getEq 0


-----


getMob :: Id -> MudStack Mob
getMob i = gets (^?!mobTbl.ix i)


getMobGender :: Id -> MudStack Gender
getMobGender i = (^.gender) <$> getMob i


getPCMobGender :: MudStack Gender
getPCMobGender = getMobGender 0


getMobHand :: Id -> MudStack Hand
getMobHand i = (^.hand) <$> getMob i


getPCMobHand :: MudStack Hand
getPCMobHand = getMobHand 0


-----


getRm :: Id -> MudStack Rm
getRm i = gets (^?!rmTbl.ix i)


getPCRmId :: MudStack Id
getPCRmId = gets (^.pc.rmId)


getPCRm :: MudStack Rm
getPCRm = getPCRmId >>= getRm


getPCRmInv :: MudStack Inv
getPCRmInv = getPCRmId >>= \i ->
    gets (^?!invTbl.ix i)


getRmLinks :: Id -> MudStack [RmLink]
getRmLinks i = (^.rmLinks) <$> getRm i


findExit :: LinkName -> Id -> MudStack (Maybe Id)
findExit ln i = getRmLinks i >>= \rls ->
    case [ rl^.destId | rl <- rls, isValid rl ] of
      [] -> return Nothing
      is -> return (Just . head $ is)
  where
    isValid rl = ln `elem` stdLinkNames && ln == (rl^.linkName) || ln `notElem` stdLinkNames && ln `T.isInfixOf` (rl^.linkName)
