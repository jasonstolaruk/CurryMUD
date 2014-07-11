{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.NameResolution ( procReconciledCoinsPCInv
                          , procGecrMisCon
                          , procGecrMisPCEq
                          , procGecrMisPCInv
                          , procGecrMisPCInvForInv
                          , procGecrMisRm
                          , procGecrMisRmForInv
                          , procGecrMrolMiss
                          , procReconciledCoinsCon
                          , procReconciledCoinsRm
                          , ReconciledCoins
                          , resolveEntCoinNames
                          , resolveEntCoinNamesWithRols
                          , ringHelp ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Applicative ((<$>))
import Control.Lens (_1, _2, dropping, folded, over, to)
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


-- ==================================================
-- Resolving entity and coin names:


type ReconciledCoins = Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)


resolveEntCoinNames :: Rest -> InvCoins -> MudStack ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNames rs ic@(_, c) = mapM (mkGecr ic . T.toLower) rs >>= expandGecrs c


mkGecr :: InvCoins -> T.Text -> MudStack GetEntsCoinsRes
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
  | c == mempty                 = return  (Mult a n Nothing . Just $ Empty)
  | n `elem` aggregateCoinNames = return  (Mult a n Nothing . Just . SomeOf $ if a == (maxBound :: Int) then c else c')
  | otherwise                   = return . Mult a n Nothing . Just $ case n of
    "cp"    | cop == 0               -> NoneOf . Coins $ (a,   0,   0  )
            | a == (maxBound :: Int) -> SomeOf . Coins $ (cop, 0,   0  )
            | otherwise              -> SomeOf . Coins $ (a,   0,   0  )
    "sp"    | sil == 0               -> NoneOf . Coins $ (0,   a,   0  )
            | a == (maxBound :: Int) -> SomeOf . Coins $ (0,   sil, 0  )
            | otherwise              -> SomeOf . Coins $ (0,   a,   0  )
    "gp"    | gol == 0               -> NoneOf . Coins $ (0,   0,   a  )
            | a == (maxBound :: Int) -> SomeOf . Coins $ (0,   0,   gol)
            | otherwise              -> SomeOf . Coins $ (0,   0,   a  )
    _                                -> patternMatchFail "mkGecrMultForCoins" [n]
  where
    c'        = mkCoinsFromList . distributeAmt a . mkCoinsList $ c


distributeAmt :: Int -> [Int] -> [Int]
distributeAmt _   []     = []
distributeAmt amt (c:cs) = let diff = amt - c
                           in if diff >= 0
                                then c   : distributeAmt diff cs
                                else amt : distributeAmt 0    cs


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


expandGecrs :: Coins -> [GetEntsCoinsRes] -> MudStack ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
expandGecrs c gecrs = do
    let (gecrs', enscs) = extractEnscsFromGecrs gecrs
    mess <- mapM extractMesFromGecr gecrs'
    let miss = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
    let rcs  = reconcileCoins c . distillEnscs $ enscs
    return (gecrs', miss, rcs)


extractEnscsFromGecrs :: [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [EmptyNoneSome Coins])
extractEnscsFromGecrs = over _1 reverse . foldl' helper ([], [])
  where
    helper (gecrs, enscs) gecr@(Mult    _ _ (Just _) (Just ensc)) = (gecr : gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@(Mult    _ _ (Just _) Nothing    ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs)      (Mult    _ _ Nothing  (Just ensc)) = (gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@(Mult    _ _ Nothing  Nothing    ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@Indexed {}                         = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@(Sorry   _                       ) = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@SorryIndexedCoins                  = (gecr : gecrs, enscs)


extractMesFromGecr :: GetEntsCoinsRes -> MudStack (Maybe [Ent])
extractMesFromGecr gecr = return $ case gecr of
  (Mult    _ _ (Just es) _) -> Just es
  (Indexed _ _ (Right e)  ) -> Just [e]
  _                         -> Nothing


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
                         in distill SomeOf someOfs : [ distill NoneOf noneOfs ]
  where
    isSomeOf (SomeOf _)     = True
    isSomeOf _              = False
    isNoneOf (NoneOf _)     = True
    isNoneOf _              = False
    distill f               = f . foldr ((<>) . fromEnsCoins) mempty
    fromEnsCoins (SomeOf c) = c
    fromEnsCoins (NoneOf c) = c
    fromEnsCoins ensc       = patternMatchFail "distillEnscs fromEnsCoins" [ showText ensc ]


reconcileCoins :: Coins -> [EmptyNoneSome Coins] -> [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)]
reconcileCoins _                       []    = []
reconcileCoins (Coins (cop, sil, gol)) enscs = concatMap helper enscs
  where
    helper Empty                               = [ Left Empty        ]
    helper (NoneOf c)                          = [ Left . NoneOf $ c ]
    helper (SomeOf (Coins (cop', sil', gol'))) = concat [ [ mkEitherCop | cop' /= 0 ]
                                                        , [ mkEitherSil | sil' /= 0 ]
                                                        , [ mkEitherGol | gol' /= 0 ] ]
      where
        mkEitherCop | cop' <= cop = Right . SomeOf . Coins $ (cop', 0,    0   )
                    | otherwise   = Left  . SomeOf . Coins $ (cop', 0,    0   )
        mkEitherSil | sil' <= sil = Right . SomeOf . Coins $ (0,    sil', 0   )
                    | otherwise   = Left  . SomeOf . Coins $ (0,    sil', 0   )
        mkEitherGol | gol' <= gol = Right . SomeOf . Coins $ (0,    0,    gol')
                    | otherwise   = Left  . SomeOf . Coins $ (0,    0,    gol')


-- ============================================================
-- Resolving entity and coin names with right/left indicators:


resolveEntCoinNamesWithRols :: Rest -> InvCoins -> MudStack ([GetEntsCoinsRes], [Maybe RightOrLeft], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNamesWithRols rs ic@(_, c) = do
    gecrMrols <- mapM (mkGecrWithRol ic . T.toLower) rs
    let gecrs = gecrMrols^..folded._1
    let mrols = gecrMrols^..folded._2
    (gecrs', miss, rcs) <- expandGecrs c gecrs
    return (gecrs', mrols, miss, rcs)


mkGecrWithRol :: InvCoins -> T.Text -> MudStack (GetEntsCoinsRes, Maybe RightOrLeft)
mkGecrWithRol ic n = let (a, b) = T.break (== slotChar) n
                     in if | T.null b        -> mkGecr ic n >>= \gecr -> return (gecr, Nothing)
                           | T.length b == 1 -> sorry
                           | otherwise -> mkGecr ic a >>= \gecr ->
                               let parsed = reads (b^..unpacked.dropping 1 (folded.to toUpper)) :: [(RightOrLeft, String)]
                               in case parsed of [(rol, _)] -> return (gecr, Just rol)
                                                 _          -> sorry
  where
    sorry = return (Sorry n, Nothing)


-- ==================================================
-- Processing "GetEntsCoinsRes":


procGecrMisPCInv :: ShouldNewLine -> (Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe Inv) -> MudStack ()
procGecrMisPCInv _   _ (_,                     Just []) = return () -- Nothing left after eliminating duplicate IDs.
procGecrMisPCInv snl _ (Mult 1 n Nothing  _,   Nothing) = output ("You don't have " <> aOrAn n <> ".")             >> maybeNewLine snl
procGecrMisPCInv snl _ (Mult _ n Nothing  _,   Nothing) = output ("You don't have any " <> n <> "s." )             >> maybeNewLine snl
procGecrMisPCInv _   f (Mult _ _ (Just _) _,   Just is) = f is
procGecrMisPCInv snl _ (Indexed _ n (Left ""), Nothing) = output ("You don't have any " <> n <> "s." )             >> maybeNewLine snl
procGecrMisPCInv snl _ (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, "." ] >> maybeNewLine snl
procGecrMisPCInv _   f (Indexed _ _ (Right _), Just is) = f is
procGecrMisPCInv snl _ (SorryIndexedCoins,     Nothing) = sorryIndexedCoins                                        >> maybeNewLine snl
procGecrMisPCInv snl _ (Sorry n,               Nothing) = output ("You don't have " <> aOrAn n <> ".")             >> maybeNewLine snl
procGecrMisPCInv _   _ gecrMis                          = patternMatchFail "procGecrMisPCInv" [ showText gecrMis ]


sorryIndexedCoins :: MudStack ()
sorryIndexedCoins = output $ "Sorry, but " <> dblQuote ([indexChar]^.packed) <> " cannot be used with coins."


procGecrMisPCInvForInv :: (GetEntsCoinsRes, Maybe Inv) -> MudStack Inv
procGecrMisPCInvForInv (Mult 1 n Nothing  _,   Nothing) = output ("You don't have " <> aOrAn n <> ".")             >> return []
procGecrMisPCInvForInv (Mult _ n Nothing  _,   Nothing) = output ("You don't have any " <> n <> "s." )             >> return []
procGecrMisPCInvForInv (Mult _ _ (Just _) _,   Just is) = return is
procGecrMisPCInvForInv (Indexed _ n (Left ""), Nothing) = output ("You don't have any " <> n <> "s." )             >> return []
procGecrMisPCInvForInv (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, "." ] >> return []
procGecrMisPCInvForInv (Indexed _ _ (Right _), Just is) = return is
procGecrMisPCInvForInv (SorryIndexedCoins,     Nothing) = sorryIndexedCoins                                        >> return []
procGecrMisPCInvForInv (Sorry n,               Nothing) = output ("You don't have " <> aOrAn n <> ".")             >> return []
procGecrMisPCInvForInv gecrMis                          = patternMatchFail "procGecrMisPCInvForInv" [ showText gecrMis ]


procGecrMisRm :: ShouldNewLine -> (Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe Inv) -> MudStack ()
procGecrMisRm _   _ (_,                     Just []) = return () -- Nothing left after eliminating duplicate IDs.
procGecrMisRm snl _ (Mult 1 n Nothing  _,   Nothing) = output ("You don't see " <> aOrAn n <> " here.")             >> maybeNewLine snl
procGecrMisRm snl _ (Mult _ n Nothing  _,   Nothing) = output ("You don't see any " <> n <> "s here.")              >> maybeNewLine snl
procGecrMisRm _   f (Mult _ _ (Just _) _,   Just is) = f is
procGecrMisRm snl _ (Indexed _ n (Left ""), Nothing) = output ("You don't see any " <> n <> "s here.")              >> maybeNewLine snl
procGecrMisRm snl _ (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't see ", showText x, " ", p, " here." ] >> maybeNewLine snl
procGecrMisRm _   f (Indexed _ _ (Right _), Just is) = f is
procGecrMisRm snl _ (SorryIndexedCoins,     Nothing) = sorryIndexedCoins                                            >> maybeNewLine snl
procGecrMisRm snl _ (Sorry n,               Nothing) = output ("You don't see " <> aOrAn n <> " here.")             >> maybeNewLine snl
procGecrMisRm _   _ gecrMis                          = patternMatchFail "procGecrMisRm" [ showText gecrMis ]


procGecrMisRmForInv :: (GetEntsCoinsRes, Maybe Inv) -> MudStack Inv
procGecrMisRmForInv (Mult 1 n Nothing  _,   Nothing) = output ("You don't see " <> aOrAn n <> " here.")             >> return []
procGecrMisRmForInv (Mult _ n Nothing  _,   Nothing) = output ("You don't see any " <> n <> "s here.")              >> return []
procGecrMisRmForInv (Mult _ _ (Just _) _,   Just is) = return is
procGecrMisRmForInv (Indexed _ n (Left ""), Nothing) = output ("You don't see any " <> n <> "s here.")              >> return []
procGecrMisRmForInv (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't see ", showText x, " ", p, " here." ] >> return []
procGecrMisRmForInv (Indexed _ _ (Right _), Just is) = return is
procGecrMisRmForInv (SorryIndexedCoins,     Nothing) = sorryIndexedCoins                                            >> return []
procGecrMisRmForInv (Sorry n,               Nothing) = output ("You don't see " <> aOrAn n <> " here.")             >> return []
procGecrMisRmForInv gecrMis                          = patternMatchFail "procGecrMisRmForInv" [ showText gecrMis ]


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


procGecrMrolMiss :: (Maybe RightOrLeft -> Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe RightOrLeft, Maybe Inv) -> MudStack ()
procGecrMrolMiss _ (_,                     _,    Just []) = return () -- Nothing left after eliminating duplicate IDs.
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
  | slotChar `elem` n^.unpacked = mapM_ output . T.lines . T.concat $ [ "Please specify ", dblQuote "r", " or ", dblQuote "l", ".\n", ringHelp ]
  | otherwise                   = output $ "You don't have " <> aOrAn n <> "."


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", dblQuote "r", " or ", dblQuote "l", " immediately followed by:\n"
                    , dblQuote "i", " for index finger,\n"
                    , dblQuote "m", " for middle finter,\n"
                    , dblQuote "r", " for ring finger,\n"
                    , dblQuote "p", " for pinky finger." ]


procGecrMisPCEq :: (Inv -> MudStack ()) -> (GetEntsCoinsRes, Maybe Inv) -> MudStack ()
procGecrMisPCEq _ (_,                     Just []) = return () -- Nothing left after eliminating duplicate IDs.
procGecrMisPCEq _ (Mult 1 n Nothing  _,   Nothing) = output $ "You don't have " <> aOrAn n <> " among your readied equipment."
procGecrMisPCEq _ (Mult _ n Nothing  _,   Nothing) = output $ "You don't have any " <> n <> "s among your readied equipment."
procGecrMisPCEq f (Mult _ _ (Just _) _,   Just is) = f is
procGecrMisPCEq _ (Indexed _ n (Left ""), Nothing) = output $ "You don't have any " <> n <> "s among your readied equipment."
procGecrMisPCEq _ (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, " among your readied equipment." ]
procGecrMisPCEq f (Indexed _ _ (Right _), Just is) = f is
procGecrMisPCEq _ (SorryIndexedCoins,     Nothing) = sorryIndexedCoins
procGecrMisPCEq _ (Sorry n,               Nothing) = output $ "You don't have " <> aOrAn n <> " among your readied equipment."
procGecrMisPCEq _ gecrMis                          = patternMatchFail "procGecrMisPCEq" [ showText gecrMis ]


-- ==================================================
-- Processing "ReconciledCoins":


procReconciledCoinsPCInv :: ShouldNewLine -> (Coins -> MudStack ()) -> ReconciledCoins -> MudStack ()
procReconciledCoinsPCInv snl _ (Left Empty)                             = output "You don't have any coins." >> maybeNewLine snl
procReconciledCoinsPCInv snl _ (Left  (NoneOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) $ output "You don't have any copper pieces."                      >> maybeNewLine snl
    unless (sil == 0) $ output "You don't have any silver pieces."                      >> maybeNewLine snl
    unless (gol == 0) $ output "You don't have any gold pieces."                        >> maybeNewLine snl
procReconciledCoinsPCInv _   f (Right (SomeOf c                      )) = f c
procReconciledCoinsPCInv snl _ (Left  (SomeOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) $ output ("You don't have " <> showText cop <> " copper pieces.") >> maybeNewLine snl
    unless (sil == 0) $ output ("You don't have " <> showText sil <> " silver pieces.") >> maybeNewLine snl
    unless (gol == 0) $ output ("You don't have " <> showText gol <> " gold pieces."  ) >> maybeNewLine snl
procReconciledCoinsPCInv _   _ rc = patternMatchFail "procReconciledCoinsPCInv" [ showText rc ]


procReconciledCoinsRm :: ShouldNewLine -> (Coins -> MudStack ()) -> ReconciledCoins -> MudStack ()
procReconciledCoinsRm snl _ (Left Empty)                             = output "You don't see any coins here." >> maybeNewLine snl
procReconciledCoinsRm snl _ (Left  (NoneOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) $ output "You don't see any copper pieces here."                      >> maybeNewLine snl
    unless (sil == 0) $ output "You don't see any silver pieces here."                      >> maybeNewLine snl
    unless (gol == 0) $ output "You don't see any gold pieces here."                        >> maybeNewLine snl
procReconciledCoinsRm _   f (Right (SomeOf c                      )) = f c
procReconciledCoinsRm snl _ (Left  (SomeOf (Coins (cop, sil, gol)))) = do
    unless (cop == 0) $ output ("You don't see " <> showText cop <> " copper pieces here.") >> maybeNewLine snl
    unless (sil == 0) $ output ("You don't see " <> showText sil <> " silver pieces here.") >> maybeNewLine snl
    unless (gol == 0) $ output ("You don't see " <> showText gol <> " gold pieces here."  ) >> maybeNewLine snl
procReconciledCoinsRm _   _ rc = patternMatchFail "procReconciledCoinsRm" [ showText rc ]


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
