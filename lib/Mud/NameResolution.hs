{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

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
import Data.Text.Strict.Lens (unpacked)
import qualified Data.Text as T


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.NameResolution"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.NameResolution"


-- ==================================================
-- Resolving entity and coin names:


type ReconciledCoins = Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)


resolveEntCoinNames :: Id -> WorldState -> Rest -> Inv -> Coins -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNames i ws (map T.toLower -> rs) is c = expandGecrs c [ mkGecr i ws is c r | r <- rs ]


mkGecr :: Id -> WorldState -> Inv -> Coins -> T.Text -> GetEntsCoinsRes
mkGecr i ws is c n@(headTail -> (h, t))
  | n == T.pack [allChar]
  , es <- [ (ws^.entTbl) ! i' | i' <- is ]                  = Mult (length is) n (Just es) (Just . SomeOf $ c)
  | h == allChar                                            = mkGecrMult i ws (maxBound :: Int) t is c
  | isDigit h
  , (numText, rest) <- T.span isDigit n
  , numInt <- either (oops numText) fst . decimal $ numText = if numText /= "0" then parse rest numInt else Sorry n
  | otherwise                                               = mkGecrMult i ws 1 n is c
  where
    oops numText = blowUp "mkGecr" "unable to convert Text to Int" [ showText numText ]
    parse rest numInt
      | T.length rest < 2               = Sorry n
      | (delim, rest') <- headTail rest = if | delim == amountChar -> mkGecrMult    i ws numInt rest' is c
                                             | delim == indexChar  -> mkGecrIndexed i ws numInt rest' is
                                             | otherwise           -> Sorry n


mkGecrMult :: Id -> WorldState -> Amount -> T.Text -> Inv -> Coins -> GetEntsCoinsRes
mkGecrMult i ws a n is c | n `elem` allCoinNames = mkGecrMultForCoins     a n c
                         | otherwise             = mkGecrMultForEnts i ws a n is


mkGecrMultForCoins :: Amount -> T.Text -> Coins -> GetEntsCoinsRes
mkGecrMultForCoins a n c@(Coins (cop, sil, gol)) = Mult a n Nothing . Just $ helper
  where
    helper | c == mempty                 = Empty
           | n `elem` aggregateCoinNames = SomeOf $ if a == (maxBound :: Int)
             then c
             else mkCoinsFromList . distributeAmt a . mkListFromCoins $ c
           | otherwise = case n of
             "cp" | cop == 0               -> NoneOf . Coins $ (a,   0,   0  )
                  | a == (maxBound :: Int) -> SomeOf . Coins $ (cop, 0,   0  )
                  | otherwise              -> SomeOf . Coins $ (a,   0,   0  )
             "sp" | sil == 0               -> NoneOf . Coins $ (0,   a,   0  )
                  | a == (maxBound :: Int) -> SomeOf . Coins $ (0,   sil, 0  )
                  | otherwise              -> SomeOf . Coins $ (0,   a,   0  )
             "gp" | gol == 0               -> NoneOf . Coins $ (0,   0,   a  )
                  | a == (maxBound :: Int) -> SomeOf . Coins $ (0,   0,   gol)
                  | otherwise              -> SomeOf . Coins $ (0,   0,   a  )
             _                             -> patternMatchFail "mkGecrMultForCoins helper" [n]


distributeAmt :: Int -> [Int] -> [Int]
distributeAmt _   []     = []
distributeAmt amt (c:cs) | diff <- amt - c, diff >= 0 = c   : distributeAmt diff cs
                         | otherwise                  = amt : distributeAmt 0    cs


mkGecrMultForEnts :: Id -> WorldState -> Amount -> T.Text -> Inv -> GetEntsCoinsRes
mkGecrMultForEnts i ws a n is | ens <- [ getEffName i ws i' | i' <- is ] =
    maybe notFound (found ens) . findFullNameForAbbrev n $ ens
  where
    notFound                    = Mult a n Nothing Nothing
    found (zip is -> zipped) fn = Mult a n (Just . takeMatchingEnts zipped $ fn) Nothing
    takeMatchingEnts zipped  fn = let matches = filter (\(_, en) -> en == fn) zipped
                                  in take a [ (ws^.entTbl) ! i' | (i', _) <- matches ]


mkGecrIndexed :: Id -> WorldState -> Index -> T.Text -> Inv -> GetEntsCoinsRes
mkGecrIndexed i ws x n is
  | n `elem` allCoinNames                    = SorryIndexedCoins
  | ens <- [ getEffName i ws i' | i' <- is ] = maybe notFound (found ens) . findFullNameForAbbrev n $ ens
  where
    notFound                                                               = Indexed x n . Left $ ""
    found ens fn | matches <- filter (\(_, en) -> en == fn) . zip is $ ens = Indexed x n $ if length matches < x
      then Left . mkPlurFromBoth . getEffBothGramNos i ws . fst . head $ matches
      else Right . ((ws^.entTbl) !) . fst $ matches !! (x - 1)


expandGecrs :: Coins -> [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
expandGecrs c (extractEnscsFromGecrs -> (gecrs, enscs)) =
    let mess = map extractMesFromGecr gecrs
        miss = pruneDupIds [] . (fmap . fmap . fmap) (^.entId) $ mess
        rcs  = reconcileCoins c . distillEnscs $ enscs
    in (gecrs, miss, rcs)


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
distillEnscs enscs | Empty `elem` enscs               = [Empty]
                   | someOfs <- filter isSomeOf enscs
                   , noneOfs <- filter isNoneOf enscs = distill SomeOf someOfs ++ distill NoneOf noneOfs
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


resolveEntCoinNamesWithRols :: Id -> WorldState -> Rest -> Inv -> Coins -> ([GetEntsCoinsRes], [Maybe RightOrLeft], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNamesWithRols i ws (map T.toLower -> rs) is c
  | gecrMrols           <- map (mkGecrWithRol i ws is c) rs
  , (gecrs, mrols)      <- (,) (gecrMrols^..folded._1) (gecrMrols^..folded._2)
  , (gecrs', miss, rcs) <- expandGecrs c gecrs = (gecrs', mrols, miss, rcs)


mkGecrWithRol :: Id -> WorldState -> Inv -> Coins -> T.Text -> (GetEntsCoinsRes, Maybe RightOrLeft)
mkGecrWithRol i ws is c n
  | (a, b) <- T.break (== slotChar) n
  , parsed <- reads (b^..unpacked.dropping 1 (folded.to toUpper)) :: [ (RightOrLeft, String) ] =
      if | T.null b        -> (mkGecr i ws is c n, Nothing)
         | T.length b == 1 -> sorry
         | otherwise       -> case parsed of [(rol, _)] -> (mkGecr i ws is c a, Just rol)
                                             _          -> sorry
  where
    sorry = (Sorry n, Nothing)


-- ==================================================
-- Processing "GetEntsCoinsRes":


sorryIndexedCoins :: T.Text
sorryIndexedCoins = nl $ "Sorry, but " <> (dblQuote . T.pack $ [indexChar]) <> " cannot be used with coins."


procGecrMisPCInv :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisPCInv (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisPCInv (Mult 1 n Nothing  _,   Nothing) = Left $ "You don't have " <> aOrAn n <> "."
procGecrMisPCInv (Mult _ n Nothing  _,   Nothing) = Left $ "You don't have any " <> n <> "s."
procGecrMisPCInv (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisPCInv (Indexed _ n (Left ""), Nothing) = Left $ "You don't have any " <> n <> "s."
procGecrMisPCInv (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "You don't have ", showText x, " ", p, "." ]
procGecrMisPCInv (Indexed _ _ (Right _), Just is) = Right is
procGecrMisPCInv (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisPCInv (Sorry n,               Nothing) = Left $ "You don't have " <> aOrAn n <> "."
procGecrMisPCInv gecrMis                          = patternMatchFail "procGecrMisPCInv" [ showText gecrMis ]


procGecrMisReady :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisReady (Sorry n, Nothing) = Left . sorryBadSlot $ n
procGecrMisReady gecrMis            = procGecrMisPCInv gecrMis


sorryBadSlot :: T.Text -> T.Text
sorryBadSlot n
  | slotChar `elem` T.unpack n = T.concat [ "Please specify ", mkSlotTxt "r", " or ", mkSlotTxt "l", ".", nl' ringHelp ]
  | otherwise                  = "You don't have " <> aOrAn n <> "."


mkSlotTxt :: T.Text -> T.Text
mkSlotTxt = dblQuote . (T.pack [slotChar] <>)


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", mkSlotTxt "r", " or ", mkSlotTxt "l", nl " immediately followed by:"
                    , dblQuote "i", nl " for index finger,"
                    , dblQuote "m", nl " for middle finger,"
                    , dblQuote "r", nl " for ring finger, or"
                    , dblQuote "p", nl " for pinky finger." ]


procGecrMisRm :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisRm (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisRm (Mult 1 n Nothing  _,   Nothing) = Left $ "You don't see " <> aOrAn n <> " here."
procGecrMisRm (Mult _ n Nothing  _,   Nothing) = Left $ "You don't see any " <> n <> "s here."
procGecrMisRm (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisRm (Indexed _ n (Left ""), Nothing) = Left $ "You don't see any " <> n <> "s here."
procGecrMisRm (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "You don't see ", showText x, " ", p, " here." ]
procGecrMisRm (Indexed _ _ (Right _), Just is) = Right is
procGecrMisRm (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisRm (Sorry n,               Nothing) = Left $ "You don't see " <> aOrAn n <> " here."
procGecrMisRm gecrMis                          = patternMatchFail "procGecrMisRm" [ showText gecrMis ]


procGecrMisCon :: ConName -> (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisCon _  (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisCon cn (Mult 1 n Nothing  _,   Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain ", aOrAn n, "." ]
procGecrMisCon cn (Mult _ n Nothing  _,   Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain any ", n, "s."  ]
procGecrMisCon _  (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisCon cn (Indexed _ n (Left ""), Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain any ", n, "s."  ]
procGecrMisCon cn (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain ", showText x, " ", p, "." ]
procGecrMisCon _  (Indexed _ _ (Right _), Just is) = Right is
procGecrMisCon _  (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisCon cn (Sorry n,               Nothing) = Left . T.concat $ [ "The ", cn, " doesn't contain ", aOrAn n, "." ]
procGecrMisCon _  gecrMis                          = patternMatchFail "procGecrMisCon" [ showText gecrMis ]


procGecrMisPCEq :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisPCEq (_,                     Just []) = Left "" -- Nothing left after eliminating duplicate IDs.
procGecrMisPCEq (Mult 1 n Nothing  _,   Nothing) = Left $ "You don't have " <> aOrAn n <> " among your readied equipment."
procGecrMisPCEq (Mult _ n Nothing  _,   Nothing) = Left $ "You don't have any " <> n <> "s among your readied equipment."
procGecrMisPCEq (Mult _ _ (Just _) _,   Just is) = Right is
procGecrMisPCEq (Indexed _ n (Left ""), Nothing) = Left $ "You don't have any " <> n <> "s among your readied equipment."
procGecrMisPCEq (Indexed x _ (Left p),  Nothing) = Left . T.concat $ [ "You don't have ", showText x, " ", p, " among your readied equipment." ]
procGecrMisPCEq (Indexed _ _ (Right _), Just is) = Right is
procGecrMisPCEq (SorryIndexedCoins,     Nothing) = Left sorryIndexedCoins
procGecrMisPCEq (Sorry n,               Nothing) = Left $ "You don't have " <> aOrAn n <> " among your readied equipment."
procGecrMisPCEq gecrMis                          = patternMatchFail "procGecrMisPCEq" [ showText gecrMis ]


-- ==================================================
-- Processing "ReconciledCoins":


procReconciledCoinsPCInv :: ReconciledCoins -> Either [T.Text] Coins
procReconciledCoinsPCInv (Left  Empty)                            = Left ["You don't have any coins."]
procReconciledCoinsPCInv (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = if cop /= 0 then Just "You don't have any copper pieces." else Nothing
    s = if sil /= 0 then Just "You don't have any silver pieces." else Nothing
    g = if gol /= 0 then Just "You don't have any gold pieces."   else Nothing
procReconciledCoinsPCInv (Right (SomeOf c                      )) = Right c
procReconciledCoinsPCInv (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = if cop /= 0 then Just $ "You don't have " <> showText cop <> " copper pieces." else Nothing
    s = if sil /= 0 then Just $ "You don't have " <> showText sil <> " silver pieces." else Nothing
    g = if gol /= 0 then Just $ "You don't have " <> showText gol <> " gold pieces."   else Nothing
procReconciledCoinsPCInv rc = patternMatchFail "procReconciledCoinsPCInv" [ showText rc ]


extractCoinsTxt :: [Maybe T.Text] -> [T.Text]
extractCoinsTxt []           = []
extractCoinsTxt (Nothing:xs) =     extractCoinsTxt xs
extractCoinsTxt (Just  x:xs) = x : extractCoinsTxt xs


procReconciledCoinsRm :: ReconciledCoins -> Either [T.Text] Coins
procReconciledCoinsRm (Left  Empty)                            = Left ["You don't see any coins here."]
procReconciledCoinsRm (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = if cop /= 0 then Just "You don't see any copper pieces here." else Nothing
    s = if sil /= 0 then Just "You don't see any silver pieces here." else Nothing
    g = if gol /= 0 then Just "You don't see any gold pieces here."   else Nothing
procReconciledCoinsRm (Right (SomeOf c                      )) = Right c
procReconciledCoinsRm (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = if cop /= 0 then Just $ "You don't see " <> showText cop <> " copper pieces here." else Nothing
    s = if sil /= 0 then Just $ "You don't see " <> showText sil <> " silver pieces here." else Nothing
    g = if gol /= 0 then Just $ "You don't see " <> showText gol <> " gold pieces here."   else Nothing
procReconciledCoinsRm rc = patternMatchFail "procReconciledCoinsRm" [ showText rc ]


procReconciledCoinsCon :: ConName -> ReconciledCoins -> Either [T.Text] Coins
procReconciledCoinsCon cn (Left  Empty)                            = Left [ "The " <> cn <> " doesn't contain any coins." ]
procReconciledCoinsCon cn (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = if cop /= 0 then Just $ "The " <> cn <> " doesn't contain any copper pieces." else Nothing
    s = if sil /= 0 then Just $ "The " <> cn <> " doesn't contain any silver pieces." else Nothing
    g = if gol /= 0 then Just $ "The " <> cn <> " doesn't contain any gold pieces."   else Nothing
procReconciledCoinsCon _  (Right (SomeOf c                      )) = Right c
procReconciledCoinsCon cn (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = if cop /= 0 then Just . T.concat $ [ "The ", cn, "doesn't contain ", showText cop, " copper pieces." ] else Nothing
    s = if sil /= 0 then Just . T.concat $ [ "The ", cn, "doesn't contain ", showText sil, " silver pieces." ] else Nothing
    g = if gol /= 0 then Just . T.concat $ [ "The ", cn, "doesn't contain ", showText gol, " gold pieces."   ] else Nothing
procReconciledCoinsCon _ rc = patternMatchFail "procReconciledCoinsCon" [ showText rc ]
