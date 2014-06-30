-- {-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.NameResolution ( procReconciledCoinsPCInv
                          , procGecrMisCon
                          , procGecrMisPCInv
                          , procGecrMisRm
                          , procGecrMrolMiss
                          , procReconciledCoinsCon
                          , procReconciledCoinsRm
                          , resolveEntCoinNames
                          , resolveEntName
                          , resolveEntCoinNamesWithRols
                          , ringHelp ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>))
import Control.Lens (_1, _2, dropping, folded, to)
import Control.Lens.Operators ((^.), (^..))
import Control.Monad (unless)
import Data.Char (isDigit, toUpper)
import Data.List (foldl')
import Data.Monoid ((<>), mempty)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.NameResolution"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.NameResolution"


resolveEntName :: T.Text -> InvCoins -> MudStack (Maybe [Ent])
resolveEntName n ic = mkGecr ic n >>= extractMesFromGecr


resolveEntCoinNames :: Rest -> InvCoins -> MudStack ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNames rs ic@(_, c) = do
    gecrs :: [GetEntsCoinsRes] <- mapM (mkGecr ic) rs
    let (gecrs', enscs) :: ([GetEntsCoinsRes], [EmptyNoneSome Coins]) = extractEnscsFromGecrs gecrs
    mess :: [Maybe [Ent]] <- mapM extractMesFromGecr gecrs'
    let miss :: [Maybe Inv] = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
    let rcs :: [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)] = reconcileCoins c . distillEnscs $ enscs
    return (gecrs', miss, rcs)


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
  | otherwise = return $ case n of
    "cp"    | cop == 0               -> Mult a n Nothing . Just . NoneOf . Coins $ (a,   0,   0  )
            | a == (maxBound :: Int) -> Mult a n Nothing . Just . SomeOf . Coins $ (cop, 0,   0  )
            | otherwise              -> Mult a n Nothing . Just . SomeOf . Coins $ (a,   0,   0  )
    "sp"    | sil == 0               -> Mult a n Nothing . Just . NoneOf . Coins $ (0,   a,   0  )
            | a == (maxBound :: Int) -> Mult a n Nothing . Just . SomeOf . Coins $ (0,   sil, 0  )
            | otherwise              -> Mult a n Nothing . Just . SomeOf . Coins $ (0,   a,   0  )
    "gp"    | gol == 0               -> Mult a n Nothing . Just . NoneOf . Coins $ (0,   0,   a  )
            | a == (maxBound :: Int) -> Mult a n Nothing . Just . SomeOf . Coins $ (0,   0,   gol)
            | otherwise              -> Mult a n Nothing . Just . SomeOf . Coins $ (0,   0,   a  )
    "coin"  -> aggregate
    "coins" -> aggregate
    _       -> patternMatchFail "mkGecrMultForCoins" [n]
  where
    aggregate | a == (maxBound :: Int) = Mult a n Nothing . Just . SomeOf $ c
              | otherwise              = undefined


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
    helper (gecrs, enscs) gecr@(Mult    _ _ (Just _) (Just ensc)) = (gecr : gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@(Mult    _ _ (Just _) Nothing    ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs)      (Mult    _ _ Nothing  (Just ensc)) = (gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@(Mult    _ _ Nothing  Nothing    ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@(Indexed _ _ _                   ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@(Sorry   _                       ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@(SorryIndexedCoins               ) = (gecr : gecrs, enscs)


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
    fromEnsCoins ensc       = patternMatchFail "distillEnscs" [ showText ensc ]


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


resolveEntCoinNamesWithRols :: Rest -> InvCoins -> MudStack ([GetEntsCoinsRes], [Maybe RightOrLeft], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNamesWithRols rs ic@(_, c) = do
    gecrMrols :: [(GetEntsCoinsRes, Maybe RightOrLeft)] <- mapM (mkGecrWithRol ic) rs
    let gecrs = gecrMrols^..folded._1
    let mrols = gecrMrols^..folded._2
    let (gecrs', enscs) :: ([GetEntsCoinsRes], [EmptyNoneSome Coins]) = extractEnscsFromGecrs gecrs
    mess :: [Maybe [Ent]] <- mapM extractMesFromGecr gecrs'
    let miss :: [Maybe Inv] = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
    let rcs :: [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)] = reconcileCoins c . distillEnscs $ enscs
    return (gecrs', mrols, miss, rcs)


mkGecrWithRol :: InvCoins -> T.Text -> MudStack (GetEntsCoinsRes, Maybe RightOrLeft) -- TODO: Impact of alphabetical case?
mkGecrWithRol ic n = let (a, b) = T.break (== slotChar) n
                     in if | T.null b        -> mkGecr ic n >>= \gecr -> return (gecr, Nothing)
                           | T.length b == 1 -> sorry
                           | otherwise -> do
                               gecr <- mkGecr ic a
                               let parsed = reads (b^..unpacked.dropping 1 (folded.to toUpper)) :: [(RightOrLeft, String)]
                               case parsed of [(rol, _)] -> return (gecr, Just rol)
                                              _          -> sorry
  where
    sorry = return (Sorry n, Nothing)


-----


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
procGecrMisPCInv _ gecrMis                          = patternMatchFail "procGecrMisPCInv" [ showText gecrMis ]


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
procGecrMisRm _ gecrMis                          = patternMatchFail "procGecrMisRm" [ showText gecrMis ]


procGecrMisCon :: ConName -> (Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe Inv) -> MudStack ()
procGecrMisCon _  _ (_,                     Just []) = return () -- Nothing left after eliminating duplicate IDs.
procGecrMisCon cn _ (Mult 1 n Nothing  _,   Nothing) = outputCon [ "The ", cn, " doesn't contain ", aOrAn n, "." ]
procGecrMisCon cn _ (Mult _ n Nothing  _,   Nothing) = outputCon [ "The ", cn, " doesn't contain any ", n, "s." ]
procGecrMisCon _  f (Mult _ _ (Just _) _,   Just is) = f is
procGecrMisCon cn _ (Indexed _ n (Left ""), Nothing) = outputCon [ "The ", cn, " doesn't contain any ", n, "s." ]
procGecrMisCon cn _ (Indexed x _ (Left p),  Nothing) = outputCon [ "The ", cn, " doesn't contain ", showText x, " ", p, "." ]
procGecrMisCon _  f (Indexed _ _ (Right _), Just is) = f is
procGecrMisCon _  _ (SorryIndexedCoins,     Nothing) = sorryIndexedCoins
procGecrMisCon cn _ (Sorry n,               Nothing) = outputCon [ "The ", cn, " doesn't contain ", aOrAn n, "." ]
procGecrMisCon _  _ gecrMis                          = patternMatchFail "procGecrMisCon" [ showText gecrMis ]


-- TODO: Compare and refactor.
procReconciledCoinsPCInv :: (Coins -> MudStack ()) -> ReconciledCoins -> MudStack ()
procReconciledCoinsPCInv _ (Left Empty)                             = output "You don't have any coins."
procReconciledCoinsPCInv _ (Left  (NoneOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) . output $ "You don't have any copper pieces."
    unless (sil == 0) . output $ "You don't have any silver pieces."
    unless (gol == 0) . output $ "You don't have any gold pieces."
procReconciledCoinsPCInv f (Right (SomeOf c                      )) = f c
procReconciledCoinsPCInv _ (Left  (SomeOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) . output $ "You don't have " <> showText cop <> " copper pieces."
    unless (sil == 0) . output $ "You don't have " <> showText sil <> " silver pieces."
    unless (gol == 0) . output $ "You don't have " <> showText gol <> " gold pieces."
procReconciledCoinsPCInv _ rc = patternMatchFail "procReconciledCoinsPCInv" [ showText rc ]


procReconciledCoinsRm :: (Coins -> MudStack ()) -> ReconciledCoins -> MudStack ()
procReconciledCoinsRm _ (Left Empty)                             = output $ "You don't see any coins here."
procReconciledCoinsRm _ (Left  (NoneOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) . output $ "You don't see any copper pieces here."
    unless (sil == 0) . output $ "You don't see any silver pieces here."
    unless (gol == 0) . output $ "You don't see any gold pieces here."
procReconciledCoinsRm f (Right (SomeOf c                      )) = f c
procReconciledCoinsRm _ (Left  (SomeOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) . output $ "You don't see " <> showText cop <> " copper pieces here."
    unless (sil == 0) . output $ "You don't see " <> showText sil <> " silver pieces here."
    unless (gol == 0) . output $ "You don't see " <> showText gol <> " gold pieces here."
procReconciledCoinsRm _ rc = patternMatchFail "procReconciledCoinsRm" [ showText rc ]


procReconciledCoinsCon :: ConName -> (Coins -> MudStack ()) -> ReconciledCoins -> MudStack ()
procReconciledCoinsCon cn _ (Left Empty)                             = output $ "The " <> cn <> " doesn't contain any coins."
procReconciledCoinsCon cn _ (Left  (NoneOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) . output $ "The " <> cn <> " doesn't contain any copper pieces."
    unless (sil == 0) . output $ "The " <> cn <> " doesn't contain any silver pieces."
    unless (gol == 0) . output $ "The " <> cn <> " doesn't contain any gold pieces."
procReconciledCoinsCon _  f (Right (SomeOf c                      )) = f c
procReconciledCoinsCon cn _ (Left  (SomeOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) . outputCon $ [ "The ", cn, "doesn't contain ", showText cop, " copper pieces." ]
    unless (sil == 0) . outputCon $ [ "The ", cn, "doesn't contain ", showText sil, " silver pieces." ]
    unless (gol == 0) . outputCon $ [ "The ", cn, "doesn't contain ", showText gol, " gold pieces." ]
procReconciledCoinsCon _  _ rc = patternMatchFail "procReconciledCoinsCon" [ showText rc ]


procGecrMrolMiss :: (Maybe RightOrLeft -> Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe RightOrLeft, Maybe Inv) -> MudStack ()
procGecrMrolMiss _ (_,                     _,    Just []) = return () -- Nothing left after eliminating duplicate IDs. -- TODO: Put this comment wherever appropriate.
procGecrMrolMiss _ (Mult 1 n Nothing  _,   _,    Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGecrMrolMiss _ (Mult _ n Nothing  _,   _,    Nothing) = output $ "You don't have any " <> n <> "s."
procGecrMrolMiss f (Mult _ _ (Just _) _,   mrol, Just is) = f mrol is
procGecrMrolMiss _ (Indexed _ n (Left ""), _,    Nothing) = output $ "You don't have any " <> n <> "s."
procGecrMrolMiss _ (Indexed x _ (Left p),  _,    Nothing) = outputCon [ "You don't have ", showText x, " ", p, "." ]
procGecrMrolMiss f (Indexed _ _ (Right _), mrol, Just is) = f mrol is
procGecrMrolMiss _ (SorryIndexedCoins,     _,    Nothing) = sorryIndexedCoins
procGecrMrolMiss _ (Sorry n,               _,    Nothing) = sorryMrol n
procGecrMrolMiss _ gecrMisMrol                            = patternMatchFail "procGecrMrolMiss" [ showText gecrMisMrol ]


sorryMrol :: T.Text -> MudStack ()
sorryMrol n
  | slotChar `elem` n^.unpacked = outputCon [ "Please specify ", dblQuote "r", " or ", dblQuote "l", ".\n", ringHelp ]
  | otherwise = output $ "You don't have " <> aOrAn n <> "."


ringHelp :: T.Text -- TODO: This isn't being wrapped correctly.
ringHelp = T.concat [ "For rings, specify ", dblQuote "r", " or ", dblQuote "l", " immediately followed by:\n"
                    , dblQuote "i", " for index finger,\n"
                    , dblQuote "m", " for middle finter,\n"
                    , dblQuote "r", " for ring finger,\n"
                    , dblQuote "p", " for pinky finger." ]
