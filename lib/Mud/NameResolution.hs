-- {-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.NameResolution ( procEnscPCInv
                          , procEnscRm
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
import Data.Monoid ((<>), mconcat, mempty)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.NameResolution"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.NameResolution"


resolveEntCoinNames :: Rest -> InvCoins -> MudStack ([GetEntsCoinsRes], [Maybe Inv], [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)])
resolveEntCoinNames rs ic@(_, c) = do
    gecrs :: [GetEntsCoinsRes] <- mapM (mkGecr ic) rs
    let (gecrs', enscs) :: ([GetEntsCoinsRes], [EmptyNoneSome Coins]) = extractEnscsFromGecrs gecrs
    mess :: [Maybe [Ent]] <- mapM extractMesFromGecr gecrs'
    let miss :: [Maybe Inv] = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
    let reconciled :: [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)] = reconcileCoins c . distillEnscs $ enscs
    return (gecrs', miss, reconciled)


mkGecr :: InvCoins -> T.Text -> MudStack GetEntsCoinsRes -- TODO: Impact of alphabetical case?
mkGecr ic@(is, c) n
  | n == [allChar]^.packed = getEntsInInv is >>= \es -> return (Mult (length is) n (Just es) (Just . SomeOf $ c))
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


mkGecrMultForCoins :: Amount -> T.Text -> Coins -> MudStack GetEntsCoinsRes
mkGecrMultForCoins a n c@(Coins (cop, sil, gol))
  | c == mempty = return (Mult a n Nothing . Just $ Empty)
  | otherwise = case n of
    "cp"    -> helper . Coins $ let a' = if a == (maxBound :: Int) then cop else a in (a', 0,  0 )
    "sp"    -> helper . Coins $ let a' = if a == (maxBound :: Int) then sil else a in (0,  a', 0 )
    "gp"    -> helper . Coins $ let a' = if a == (maxBound :: Int) then gol else a in (0,  0,  a')
    "coin"  -> aggregate
    "coins" -> aggregate
    _       -> patternMatchFail "mkGecrMultForCoins" [n]
  where
    helper c' = return $ if c' == mempty
                           then (Mult a n Nothing (Just . NoneOf . Coins $ case n of
                                                                             "cp" -> (a, 0, 0)
                                                                             "sp" -> (0, a, 0)
                                                                             "gp" -> (0, 0, a)))
                           else (Mult a n Nothing (Just . SomeOf $ c')) -- TODO: Make a NoneOf is existing amoint is 0.
    aggregate = if a == (maxBound :: Int)
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


extractEnscsFromGecrs :: [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [EmptyNoneSome Coins])
extractEnscsFromGecrs = foldl' helper ([], [])
  where
    helper (gecrs, enscs) gecr@(Mult _ _ (Just _) (Just ensc)) = (gecr : gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@(Mult _ _ (Just _) Nothing    ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs)      (Mult _ _ Nothing  (Just ensc)) = (gecrs, ensc : enscs)


extractMesFromGecr :: GetEntsCoinsRes -> MudStack (Maybe [Ent])
extractMesFromGecr gecr = case gecr of
  (Mult    _ _ (Just es) _) -> return (Just es)
  (Indexed _ _ (Right e)  ) -> return (Just [e])
  _                         -> return Nothing


pruneDupIds :: Inv -> [Maybe Inv] -> [Maybe Inv]
pruneDupIds _       []               = []
pruneDupIds uniques (Nothing : rest) = Nothing : pruneDupIds uniques rest
pruneDupIds uniques (Just is : rest) = let is' = deleteFirstOfEach uniques is
                                       in Just is' : pruneDupIds (is' ++ uniques) rest


distillEnscs :: [EmptyNoneSome Coins] -> [EmptyNoneSome Coins]
distillEnscs enscs
  | Empty `elem` enscs = [Empty]
  | otherwise          = let someOfs = filter isSomeOf enscs
                             noneOfs = filter isNoneOf enscs
                         in distillSomeOfs someOfs ++ distillNoneOfs noneOfs
  where
    isSomeOf (SomeOf _)     = True
    isSomeOf _              = False
    isNoneOf (NoneOf _)     = True
    isNoneOf _              = False
    distillSomeOfs []       = []
    distillSomeOfs someOfs  = let cs = map fromEnsCoins someOfs :: [Coins]
                                  c = foldr (<>) mempty cs :: Coins
                              in [SomeOf c] :: [EmptyNoneSome Coins]
    distillNoneOfs []       = []
    distillNoneOfs noneOfs  = let cs = map fromEnsCoins noneOfs :: [Coins]
                                  c = foldr (<>) mempty cs :: Coins
                              in [NoneOf c] :: [EmptyNoneSome Coins]
    fromEnsCoins (SomeOf c) = c
    fromEnsCoins (NoneOf c) = c


reconcileCoins :: Coins -> [EmptyNoneSome Coins] -> [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)]
reconcileCoins _                       []    = []
reconcileCoins (Coins (cop, sil, gol)) enscs = concat . map helper $ enscs
  where
    helper Empty                               = [ Left Empty ]
    helper (NoneOf c)                          = [ Left . NoneOf $ c ]
    helper (SomeOf (Coins (cop', sil', gol'))) = concat [ if cop' /= 0 then [mkEitherCop] else []
                                                        , if sil' /= 0 then [mkEitherSil] else []
                                                        , if gol' /= 0 then [mkEitherGol] else [] ]
      where
        mkEitherCop | cop' <= cop = Right . SomeOf . Coins $ (cop', 0, 0)
                    | otherwise   = Left  . SomeOf . Coins $ (cop', 0, 0)
        mkEitherSil | sil' <= sil = Right . SomeOf . Coins $ (0, sil', 0)
                    | otherwise   = Left  . SomeOf . Coins $ (0, sil', 0)
        mkEitherGol | gol' <= gol = Right . SomeOf . Coins $ (0, 0, gol')
                    | otherwise   = Left  . SomeOf . Coins $ (0, 0, gol')


sorryIndexedCoins :: MudStack ()
sorryIndexedCoins = output $ "Sorry, but " <> dblQuote ([indexChar]^.packed) <> " cannot be used with coins."


-- TODO: Compare and refactor.
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


-- TODO: Rename. Compare and refactor.
procEnscPCInv :: (Coins -> MudStack ()) -> Either (EmptyNoneSome Coins) (EmptyNoneSome Coins) -> MudStack ()
procEnscPCInv _ (Left Empty) = output "You don't have any coins."
procEnscPCInv _ (Left  (NoneOf (Coins (cop, sil, gol)))) = output "You don't have any coins (of one or more types)."
procEnscPCInv _ (Right (SomeOf (Coins (cop, sil, gol)))) = output "You have enough coins (of one or more types)."
procEnscPCInv _ (Left  (SomeOf (Coins (cop, sil, gol)))) = output "You don't have enough coins (of one or more types)."


procEnscRm :: (Coins -> MudStack ()) -> Either (EmptyNoneSome Coins) (EmptyNoneSome Coins) -> MudStack ()
procEnscRm _ (Left Empty) = output "You don't see any coins here."
procEnscRm _ (Left  (NoneOf (Coins (cop, sil, gol)))) = output "You don't see any coins (of one or more types)."
procEnscRm _ (Right (SomeOf (Coins (cop, sil, gol)))) = output "You see enough coins (of one or more types)."
procEnscRm _ (Left  (SomeOf (Coins (cop, sil, gol)))) = output "You don't see enough coins (of one or more types)."
