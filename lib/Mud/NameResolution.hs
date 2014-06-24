{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.NameResolution ( procGcrPCInv
                          , procGcrRm
                          , procGecrMisPCInv
                          , procGecrMisRm
                          , resolveEntCoinNames ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>))
import Control.Lens (_1)
import Control.Lens.Operators ((^.))
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.NameResolution"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.NameResolution"


resolveEntCoinNames :: Rest -> InvCoins -> MudStack ([GetEntsCoinsRes], [Maybe Inv], GetCoinsRes)
resolveEntCoinNames rs ic@(_, c) = do
    gecrs <- mapM (mkGecr ic) rs
    let (gecrs', cs) = extractCoinsFromGecrs gecrs
    mess :: [Maybe [Ent]] <- mapM extractMesFromGecr gecrs'
    let miss :: [Maybe Inv] = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
    let c'  = sumCoins cs
    let gcr = mkGcr c c'
    return (gecrs', miss, gcr)


mkGecr :: InvCoins -> T.Text -> MudStack GetEntsCoinsRes -- TODO: Impact of alphabetical case?
mkGecr ic@(is, c) n
  | n == [allChar]^.packed = getEntsInInv is >>= \es -> return (Mult (length is) n (Just es) (Just c))
  | T.head n == allChar    = mkGecrMult (maxBound :: Int) (T.tail n) ic
  | isDigit (T.head n)     = let numText = T.takeWhile isDigit n
                                 numInt  = either (oops numText) (^._1) $ decimal numText
                                 rest    = T.drop (T.length numText) n
                             in if numText /= "0" then parse rest numInt else return (Sorry n)
  | otherwise              = mkGecrMult 1 n ic
  where
    oops numText = blowUp "mkGecr" "unable to convert Text to Int" [ showText numText ]
    parse rest numInt
      | T.length rest < 2 = return (Sorry n)
      | otherwise = let delim = T.head rest
                        rest' = T.tail rest
                    in if | delim == amountChar -> mkGecrMult    numInt rest' ic
                          | delim == indexChar  -> mkGecrIndexed numInt rest' is
                          | otherwise           -> return (Sorry n)


mkGecrMult :: Amount -> T.Text -> InvCoins -> MudStack GetEntsCoinsRes
mkGecrMult a n (is, c) = if n `elem` allCoinNames
                           then mkGecrMultForCoins a n c
                           else mkGecrMultForEnts  a n is


-- TODO: Is there a nifty way to do this using lenses?
mkGecrMultForCoins :: Amount -> T.Text -> Coins -> MudStack GetEntsCoinsRes
mkGecrMultForCoins a n c = let (cop, sil, gol) = c in case n of
  "cp"    -> let a' = if a == (maxBound :: Int) then cop else a in helper (a', 0,  0 )
  "sp"    -> let a' = if a == (maxBound :: Int) then sil else a in helper (0,  a', 0 )
  "gp"    -> let a' = if a == (maxBound :: Int) then gol else a in helper (0,  0,  a')
  "coin"  -> aggregate
  "coins" -> aggregate
  _       -> patternMatchFail "mkGecrMultForCoins" [n]
  where
    helper c' = return $ if c' == noCoins
                           then (Mult a (expand n) Nothing Nothing  )
                           else (Mult a n          Nothing (Just c'))
    expand "cp" = "copper piece"
    expand "sp" = "silver piece"
    expand "gp" = "gold piece"
    expand _    = "coin"
    aggregate   = if a == (maxBound :: Int)
                    then helper c
                    else undefined


mkGecrMultForEnts :: Amount -> T.Text -> Inv -> MudStack GetEntsCoinsRes
mkGecrMultForEnts a n is = getEntNamesInInv is >>= maybe notFound found . findFullNameForAbbrev n
  where
    notFound       = return (Mult a n Nothing Nothing)
    found fullName = getEntsInInv is >>= \es ->
        return (Mult a n (Just . takeMatchingEnts fullName $ es) Nothing)
    takeMatchingEnts fn = take a . filter (\e -> e^.name == fn)


mkGecrIndexed :: Index -> T.Text -> Inv -> MudStack GetEntsCoinsRes
mkGecrIndexed x n is = if n `elem` allCoinNames
                         then return SorryIndexedCoins
                         else getEntNamesInInv is >>= maybe notFound found . findFullNameForAbbrev n
  where
    notFound       = return (Indexed x n (Left ""))
    found fullName = filter (\e -> e^.name == fullName) <$> getEntsInInv is >>= \matches ->
        if length matches < x
          then let both = getEntBothGramNos . head $ matches
               in return (Indexed x n (Left . mkPlurFromBoth $ both))
          else return (Indexed x n (Right $ matches !! (x - 1)))


extractCoinsFromGecrs :: [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [Coins])
extractCoinsFromGecrs = foldl' helper ([], [])
  where
    helper (gecrs, cs) gecr@(Mult _ _ (Just _) (Just c)) = (gecr : gecrs, c : cs)
    helper (gecrs, cs)      (Mult _ _ _        (Just c)) = (gecrs, c : cs)
    helper (gecrs, cs) gecr                              = (gecr : gecrs, cs)


extractMesFromGecr :: GetEntsCoinsRes -> MudStack (Maybe [Ent])
extractMesFromGecr gecr = case gecr of
  (Mult    _ _ Nothing   _) -> return Nothing
  (Mult    _ _ (Just es) _) -> return (Just es)
  (Indexed _ _ (Right e)  ) -> return (Just [e])
  _                         -> return Nothing


pruneDupIds :: Inv -> [Maybe Inv] -> [Maybe Inv]
pruneDupIds _       []               = []
pruneDupIds uniques (Nothing : rest) = Nothing : pruneDupIds uniques rest
pruneDupIds uniques (Just is : rest) = let is' = deleteFirstOfEach uniques is
                                       in Just is' : pruneDupIds (is' ++ uniques) rest


mkGcr :: ActualCoins -> RequestedCoins -> GetCoinsRes -- TODO: Is there a nifty way to do this using lenses?
mkGcr c c' = let (cop,  sil,  gol ) = c
                 (cop', sil', gol') = c'
             in (helper cop cop', helper sil sil', helper gol gol')
  where
    helper actual requested = if requested <= actual
                                then Right requested
                                else Left  (actual, requested)


sorryIndexedCoins :: MudStack ()
sorryIndexedCoins = output $ "Sorry, but " <> dblQuote ([indexChar]^.packed) <> " cannot be used with coins."


procGecrMisPCInv :: (Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe Inv) -> MudStack ()
procGecrMisPCInv _ (_,                     Just []) = return () -- Nothing left after eliminating duplicate IDs. -- TODO: Put this comment wherever appropriate.
procGecrMisPCInv _ (Mult 1 n Nothing  _,   Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGecrMisPCInv _ (Mult _ n Nothing  _,   Nothing) = output $ "You don't have any " <> n <> "s."
procGecrMisPCInv f (Mult _ _ (Just _) _,   Just is) = f is
procGecrMisPCInv _ (Indexed _ n (Left ""), Nothing) = output $ "You don't have any " <> n <> "s."
procGecrMisPCInv _ (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, "." ]
procGecrMisPCInv f (Indexed _ _ (Right _), Just is) = f is
procGecrMisPCInv _ (SorryIndexedCoins,     Nothing) = sorryIndexedCoins
procGecrMisPCInv _ (Sorry n,               Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGecrMisPCInv _ gecrMis = patternMatchFail "procGecrMisPCInv" [ showText gecrMis ]


procGecrMisRm :: (Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe Inv) -> MudStack ()
procGecrMisRm _ (_,                     Just []) = return () -- Nothing left after eliminating duplicate IDs.
procGecrMisRm _ (Mult 1 n Nothing  _,   Nothing) = output $ "You don't see " <> aOrAn n <> " here."
procGecrMisRm _ (Mult _ n Nothing  _,   Nothing) = output $ "You don't see any " <> n <> "s here."
procGecrMisRm f (Mult _ _ (Just _) _,   Just is) = f is
procGecrMisRm _ (Indexed _ n (Left ""), Nothing) = output $ "You don't see any " <> n <> "s here."
procGecrMisRm _ (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't see ", showText x, " ", p, " here." ]
procGecrMisRm f (Indexed _ _ (Right _), Just is) = f is
procGecrMisRm _ (SorryIndexedCoins,     Nothing) = sorryIndexedCoins
procGecrMisRm _ (Sorry n,               Nothing) = output $ "You don't see " <> aOrAn n <> " here."
procGecrMisRm _ gecrMis = patternMatchFail "procGecrMisRm" [ showText gecrMis ]



procGcrPCInv :: (Coins -> MudStack ()) -> GetCoinsRes -> MudStack ()
procGcrPCInv f (cpRes, spRes, gpRes) = do
    mcp <- helper cpRes "copper pieces"
    msp <- helper spRes "silver pieces"
    mgp <- helper gpRes "gold pieces"
    f (fromMaybe 0 mcp, fromMaybe 0 msp, fromMaybe 0 mgp) -- TODO: Is there a nifty way to do this using lenses?
  where
    helper res cn = case res of
      (Left (actual, requested)) -> if actual == 0
                                      then output ("You don't have any " <> cn <> ".")                       >> return Nothing
                                      else outputCon [ "You don't have ", showText requested, " ", cn, "." ] >> return Nothing
      (Right requested)          -> return (Just requested)


procGcrRm :: (Coins -> MudStack ()) -> GetCoinsRes -> MudStack ()
procGcrRm f (cpRes, spRes, gpRes) = do
    mcp <- helper cpRes "copper pieces"
    msp <- helper spRes "silver pieces"
    mgp <- helper gpRes "gold pieces"
    f (fromMaybe 0 mcp, fromMaybe 0 msp, fromMaybe 0 mgp) -- TODO: Is there a nifty way to do this using lenses?
  where
    helper res cn = case res of
      (Left (actual, requested)) -> if actual == 0
                                      then output ("You don't see any " <> cn <> " here.")                      >> return Nothing
                                      else outputCon [ "There aren't ", showText requested, " ", cn, " here." ] >> return Nothing
      (Right requested)          -> return (Just requested)
