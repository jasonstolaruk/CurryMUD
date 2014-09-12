{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ScopedTypeVariables #-}

module Mud.NameResolution ( ReconciledCoins
                          , procGecrMisCon
                          , procGecrMisPCEq
                          , procGecrMisPCInv
                          , procGecrMisReady
                          , procGecrMisRm
                          , procReconciledCoinsCon
                          , procReconciledCoinsPCInv
                          , procReconciledCoinsRm
                          , ringHelp
                          , resolveEntCoinNames
                          , resolveEntCoinNamesWithRols ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Lens (_1, _2, dropping, folded, over, to)
import Control.Lens.Operators ((^.), (^..))
import Data.Char (isDigit, toUpper)
import Data.IntMap.Lazy ((!))
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


resolveEntCoinNames :: WorldState -> Rest -> Inv -> Coins -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNames ws rs is c = expandGecrs c . map (mkGecr ws is c . T.toLower) $ rs


mkGecr :: WorldState -> Inv -> Coins -> T.Text -> GetEntsCoinsRes
mkGecr ws is c n
  | n == [allChar]^.packed = let es = [ (ws^.entTbl) ! i | i <- is ]
                             in Mult (length is) n (Just es) (Just . SomeOf $ c)
  | T.head n == allChar    = mkGecrMult ws (maxBound :: Int) (T.tail n) is c
  | isDigit (T.head n)     = let numText = T.takeWhile isDigit n
                                 numInt  = either (oops numText) (^._1) $ decimal numText
                                 rest    = T.drop (T.length numText) n
                             in if numText /= "0" then parse rest numInt else Sorry n
  | otherwise              = mkGecrMult ws 1 n is c
  where
    oops numText = blowUp "mkGecr" "unable to convert Text to Int" [ showText numText ]
    parse rest numInt
      | T.length rest < 2 = Sorry n
      | otherwise = let delim = T.head rest
                        rest' = T.tail rest
                    in if | delim == amountChar -> mkGecrMult    ws numInt rest' is c
                          | delim == indexChar  -> mkGecrIndexed ws numInt rest' is
                          | otherwise           -> Sorry n


mkGecrMult :: WorldState -> Amount -> T.Text -> Inv -> Coins -> GetEntsCoinsRes
mkGecrMult ws a n is c = if n `elem` allCoinNames
                           then mkGecrMultForCoins   a n c
                           else mkGecrMultForEnts ws a n is


mkGecrMultForCoins :: Amount -> T.Text -> Coins -> GetEntsCoinsRes
mkGecrMultForCoins a n c@(Coins (cop, sil, gol))
  | c == mempty                 = Mult a n Nothing . Just $ Empty
  | n `elem` aggregateCoinNames = Mult a n Nothing . Just . SomeOf $ if a == (maxBound :: Int) then c else c'
  | otherwise                   = Mult a n Nothing . Just $ case n of
    "cp" | cop == 0               -> NoneOf . Coins $ (a,   0,   0  )
         | a == (maxBound :: Int) -> SomeOf . Coins $ (cop, 0,   0  )
         | otherwise              -> SomeOf . Coins $ (a,   0,   0  )
    "sp" | sil == 0               -> NoneOf . Coins $ (0,   a,   0  )
         | a == (maxBound :: Int) -> SomeOf . Coins $ (0,   sil, 0  )
         | otherwise              -> SomeOf . Coins $ (0,   a,   0  )
    "gp" | gol == 0               -> NoneOf . Coins $ (0,   0,   a  )
         | a == (maxBound :: Int) -> SomeOf . Coins $ (0,   0,   gol)
         | otherwise              -> SomeOf . Coins $ (0,   0,   a  )
    _                             -> patternMatchFail "mkGecrMultForCoins" [n]
  where
    c' = mkCoinsFromList . distributeAmt a . mkListFromCoins $ c


distributeAmt :: Int -> [Int] -> [Int]
distributeAmt _   []     = []
distributeAmt amt (c:cs) = let diff = amt - c
                           in if diff >= 0
                                then c   : distributeAmt diff cs
                                else amt : distributeAmt 0    cs


mkGecrMultForEnts :: WorldState -> Amount -> T.Text -> Inv -> GetEntsCoinsRes
mkGecrMultForEnts ws a n is = let es  = [ (ws^.entTbl) ! i | i <- is ]
                                  ens = [ e^.name          | e <- es ]
                              in maybe notFound (found es) . findFullNameForAbbrev n $ ens
  where
    notFound            = Mult a n Nothing Nothing
    found es fn         = Mult a n (Just . takeMatchingEnts fn $ es) Nothing
    takeMatchingEnts fn = take a . filter (\e -> e^.name == fn)


mkGecrIndexed :: WorldState -> Index -> T.Text -> Inv -> GetEntsCoinsRes
mkGecrIndexed ws x n is = if n `elem` allCoinNames
                            then SorryIndexedCoins
                            else let es  = [ (ws^.entTbl) ! i | i <- is ]
                                     ens = [ e^.name          | e <- es ]
                                 in maybe notFound (found es) . findFullNameForAbbrev n $ ens
  where
    notFound    = Indexed x n (Left "")
    found es fn = let matches = filter (\e -> e^.name == fn) es
                  in if length matches < x
                       then let both = getEntBothGramNos . head $ matches
                            in Indexed x n (Left . mkPlurFromBoth $ both)
                     else Indexed x n (Right $ matches !! (x - 1))


expandGecrs :: Coins -> [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
expandGecrs c gecrs = let (gecrs', enscs) = extractEnscsFromGecrs  gecrs
                          mess            = map extractMesFromGecr gecrs'
                          miss            = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
                          rcs             = reconcileCoins c . distillEnscs $ enscs
                      in (gecrs', miss, rcs)


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


extractMesFromGecr :: GetEntsCoinsRes -> Maybe [Ent]
extractMesFromGecr = \case (Mult    _ _ (Just es) _) -> Just es
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
                         in distill SomeOf someOfs ++ distill NoneOf noneOfs
  where
    isSomeOf (SomeOf _)     = True
    isSomeOf _              = False
    isNoneOf (NoneOf _)     = True
    isNoneOf _              = False
    distill _ []            = []
    distill f enscs'        = [ f . foldr ((<>) . fromEnsCoins) mempty $ enscs' ]
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


resolveEntCoinNamesWithRols :: WorldState -> Rest -> Inv -> Coins -> ([GetEntsCoinsRes], [Maybe RightOrLeft], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNamesWithRols ws rs is c = let gecrMrols           = map (mkGecrWithRol ws is c . T.toLower) rs
                                             (gecrs, mrols)      = (,) (gecrMrols^..folded._1) (gecrMrols^..folded._2)
                                             (gecrs', miss, rcs) = expandGecrs c gecrs
                                         in (gecrs', mrols, miss, rcs)


mkGecrWithRol :: WorldState -> Inv -> Coins -> T.Text -> (GetEntsCoinsRes, Maybe RightOrLeft)
mkGecrWithRol ws is c n = let (a, b) = T.break (== slotChar) n
                              parsed = reads (b^..unpacked.dropping 1 (folded.to toUpper)) :: [ (RightOrLeft, String) ]
                          in if | T.null b        -> (mkGecr ws is c n, Nothing)
                                | T.length b == 1 -> sorry
                                | otherwise       -> case parsed of [ (rol, _) ] -> (mkGecr ws is c a, Just rol)
                                                                    _            -> sorry
  where
    sorry = (Sorry n, Nothing)


-- ==================================================
-- Processing "GetEntsCoinsRes":


sorryIndexedCoins :: T.Text
sorryIndexedCoins = "Sorry, but " <> dblQuote ([indexChar]^.packed) <> " cannot be used with coins.\n"


procGecrMisPCInv :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisPCInv (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisPCInv (Mult 1 n Nothing  _,   Nothing) = Left $ "You don't have " <> aOrAn n <> ".\n"
procGecrMisPCInv (Mult _ n Nothing  _,   Nothing) = Left $ "You don't have any " <> n <> "s.\n"
procGecrMisPCInv (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisPCInv (Indexed _ n (Left ""), Nothing) = Left $ "You don't have any " <> n <> "s.\n"
procGecrMisPCInv (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "You don't have ", showText x, " ", p, ".\n" ]
procGecrMisPCInv (Indexed _ _ (Right _), Just is) = Right is
procGecrMisPCInv (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisPCInv (Sorry n,               Nothing) = Left $ "You don't have " <> aOrAn n <> ".\n"
procGecrMisPCInv gecrMis                          = patternMatchFail "procGecrMisPCInv" [ showText gecrMis ]


procGecrMisReady :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisReady (Sorry n, Nothing) = Left . sorryBadSlot $ n
procGecrMisReady gecrMis            = procGecrMisPCInv gecrMis


sorryBadSlot :: T.Text -> T.Text
sorryBadSlot n
  | slotChar `elem` n^.unpacked = T.concat [ "Please specify ", mkSlotTxt "r", " or ", mkSlotTxt "l", ".\n", ringHelp ]
  | otherwise                   = "You don't have " <> aOrAn n <> "."


mkSlotTxt :: T.Text -> T.Text
mkSlotTxt = dblQuote . ([slotChar]^.packed <>)


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", mkSlotTxt "r", " or ", mkSlotTxt "l", " immediately followed by:\n"
                    , dblQuote "i", " for index finger,\n"
                    , dblQuote "m", " for middle finger,\n"
                    , dblQuote "r", " for ring finger, or\n"
                    , dblQuote "p", " for pinky finger.\n" ]


procGecrMisRm :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisRm (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisRm (Mult 1 n Nothing  _,   Nothing) = Left $ "You don't see " <> aOrAn n <> " here.\n"
procGecrMisRm (Mult _ n Nothing  _,   Nothing) = Left $ "You don't see any " <> n <> "s here.\n"
procGecrMisRm (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisRm (Indexed _ n (Left ""), Nothing) = Left $ "You don't see any " <> n <> "s here.\n"
procGecrMisRm (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "You don't see ", showText x, " ", p, " here.\n" ]
procGecrMisRm (Indexed _ _ (Right _), Just is) = Right is
procGecrMisRm (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisRm (Sorry n,               Nothing) = Left $ "You don't see " <> aOrAn n <> " here.\n"
procGecrMisRm gecrMis                          = patternMatchFail "procGecrMisRm" [ showText gecrMis ]


procGecrMisCon :: ConName -> (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisCon _  (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisCon cn (Mult 1 n Nothing  _,   Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain ", aOrAn n, ".\n" ]
procGecrMisCon cn (Mult _ n Nothing  _,   Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain any ", n, "s.\n"  ]
procGecrMisCon _  (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisCon cn (Indexed _ n (Left ""), Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain any ", n, "s.\n"  ]
procGecrMisCon cn (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain ", showText x, " ", p, ".\n" ]
procGecrMisCon _  (Indexed _ _ (Right _), Just is) = Right is
procGecrMisCon _  (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisCon cn (Sorry n,               Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain ", aOrAn n, ".\n" ]
procGecrMisCon _  gecrMis                          = patternMatchFail "procGecrMisCon" [ showText gecrMis ]


procGecrMisPCEq :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisPCEq (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisPCEq (Mult 1 n Nothing  _,   Nothing) = Left $ "You don't have " <> aOrAn n <> " among your readied equipment.\n"
procGecrMisPCEq (Mult _ n Nothing  _,   Nothing) = Left $ "You don't have any " <> n <> "s among your readied equipment.\n"
procGecrMisPCEq (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisPCEq (Indexed _ n (Left ""), Nothing) = Left $ "You don't have any " <> n <> "s among your readied equipment.\n"
procGecrMisPCEq (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "You don't have ", showText x, " ", p, " among your readied equipment.\n" ]
procGecrMisPCEq (Indexed _ _ (Right _), Just is) = Right is
procGecrMisPCEq (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisPCEq (Sorry n,               Nothing) = Left $ "You don't have " <> aOrAn n <> " among your readied equipment.\n"
procGecrMisPCEq gecrMis                          = patternMatchFail "procGecrMisPCEq" [ showText gecrMis ]


-- ==================================================
-- Processing "ReconciledCoins":


procReconciledCoinsPCInv :: ReconciledCoins -> Either T.Text Coins
procReconciledCoinsPCInv (Left  Empty)                            = Left "You don't have any coins.\n"
procReconciledCoinsPCInv (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . T.concat $ [ c, s, g ]
  where
    c = if cop /= 0 then "You don't have any copper pieces.\n" else ""
    s = if sil /= 0 then "You don't have any silver pieces.\n" else ""
    g = if gol /= 0 then "You don't have any gold pieces.\n"   else ""
procReconciledCoinsPCInv (Right (SomeOf c                      )) = Right c
procReconciledCoinsPCInv (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . T.concat $ [ c, s, g ]
  where
    c = if cop /= 0 then "You don't have " <> showText cop <> " copper pieces.\n" else ""
    s = if sil /= 0 then "You don't have " <> showText sil <> " silver pieces.\n" else ""
    g = if gol /= 0 then "You don't have " <> showText gol <> " gold pieces.\n"   else ""
procReconciledCoinsPCInv rc = patternMatchFail "procReconciledCoinsPCInv" [ showText rc ]


procReconciledCoinsRm :: ReconciledCoins -> Either T.Text Coins
procReconciledCoinsRm (Left  Empty)                            = Left "You don't see any coins here.\n"
procReconciledCoinsRm (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . T.concat $ [ c, s, g ]
  where
    c = if cop /= 0 then "You don't see any copper pieces here.\n" else ""
    s = if sil /= 0 then "You don't see any silver pieces here.\n" else ""
    g = if gol /= 0 then "You don't see any gold pieces here.\n"   else ""
procReconciledCoinsRm (Right (SomeOf c                      )) = Right c
procReconciledCoinsRm (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . T.concat $ [ c, s, g ]
  where
    c = if cop /= 0 then "You don't see " <> showText cop <> " copper pieces here.\n" else ""
    s = if sil /= 0 then "You don't see " <> showText sil <> " silver pieces here.\n" else ""
    g = if gol /= 0 then "You don't see " <> showText gol <> " gold pieces here.\n"   else ""
procReconciledCoinsRm rc = patternMatchFail "procReconciledCoinsRm" [ showText rc ]


procReconciledCoinsCon :: ConName -> ReconciledCoins -> Either T.Text Coins
procReconciledCoinsCon cn (Left  Empty)                            = Left $ "The " <> cn <> " doesn't contain any coins.\n"
procReconciledCoinsCon cn (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . T.concat $ [ c, s, g ]
  where
    c = if cop /= 0 then "The " <> cn <> " doesn't contain any copper pieces.\n" else ""
    s = if sil /= 0 then "The " <> cn <> " doesn't contain any silver pieces.\n" else ""
    g = if gol /= 0 then "The " <> cn <> " doesn't contain any gold pieces.\n"   else ""
procReconciledCoinsCon _  (Right (SomeOf c                      )) = Right c
procReconciledCoinsCon cn (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . T.concat $ [ c, s, g ]
  where
    c = if cop /= 0 then T.concat [ "The ", cn, "doesn't contain ", showText cop, " copper pieces.\n" ] else ""
    s = if sil /= 0 then T.concat [ "The ", cn, "doesn't contain ", showText sil, " silver pieces.\n" ] else ""
    g = if gol /= 0 then T.concat [ "The ", cn, "doesn't contain ", showText gol, " gold pieces.\n"   ] else ""
procReconciledCoinsCon _ rc = patternMatchFail "procReconciledCoinsCon" [ showText rc ]
