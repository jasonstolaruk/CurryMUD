{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-unused-do-bind #-}
{-# LANGUAGE LambdaCase, MultiWayIf, NamedFieldPuns, OverloadedStrings, PatternSynonyms, RebindableSyntax, ViewPatterns #-}

module Mud.NameResolution ( ReconciledCoins
                          , procGecrMisCon
                          , procGecrMisPCEq
                          , procGecrMisPCInv
                          , procGecrMisReady
                          , procGecrMisRm
                          , procReconciledCoinsCon
                          , procReconciledCoinsPCInv
                          , procReconciledCoinsRm
                          , resolveEntCoinNames
                          , resolveEntCoinNamesWithRols
                          , ringHelp ) where

import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (blowUp, patternMatchFail)
import qualified Mud.Util as U (blowUp, patternMatchFail)

import Control.Lens (_1, _2, dropping, folded, over, to)
import Control.Lens.Getter (view)
import Control.Lens.Operators ((^.), (^..))
import Data.Char (isDigit, toUpper)
import Data.IntMap.Lazy ((!))
import Data.List (foldl')
import Data.Monoid ((<>), mempty)
import Data.String (fromString)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (unpacked)
import Formatting ((%), sformat)
import Formatting.Holey (Holey)
import Data.Text.Internal.Builder (Builder)
import Formatting.Formatters (int, stext)
import Prelude hiding ((>>))
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


(>>) :: Holey Builder (T.Text -> a) b -> Holey Builder c a -> Holey Builder c b
a >> b = a % stext % b


blowUp :: T.Text -> T.Text -> [T.Text] -> a
blowUp = U.blowUp "Mud.NameResolution"


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.NameResolution"


-- ==================================================
-- Resolving entity and coin names:


type ReconciledCoins = Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)


resolveEntCoinNames :: Id -> WorldState -> Args -> Inv -> Coins -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNames i ws (map T.toLower -> as) is c = expandGecrs c [ mkGecr i ws is c a | a <- as ]


mkGecr :: Id -> WorldState -> Inv -> Coins -> T.Text -> GetEntsCoinsRes
mkGecr i ws is c n@(headTail' -> (h, t))
  | n == T.singleton allChar
  , es <- [ (ws^.entTbl) ! i' | i' <- is ]                  = Mult { amount          = length is
                                                                   , nameSearchedFor = n
                                                                   , entsRes         = Just es
                                                                   , coinsRes        = Just . SomeOf $ c }
  | h == allChar                                            = mkGecrMult i ws (maxBound :: Int) t is c
  | isDigit h
  , (numText, rest) <- T.span isDigit n
  , numInt <- either (oops numText) fst . decimal $ numText = if numText /= "0" then parse rest numInt else Sorry n
  | otherwise                                               = mkGecrMult i ws 1 n is c
  where
    oops numText = blowUp "mkGecr" "unable to convert Text to Int" [ showText numText ]
    parse rest numInt
      | T.length rest < 2                = Sorry n
      | (delim, rest') <- headTail' rest = if | delim == amountChar -> mkGecrMult    i ws numInt rest' is c
                                              | delim == indexChar  -> mkGecrIndexed i ws numInt rest' is
                                              | otherwise           -> Sorry n


mkGecrMult :: Id -> WorldState -> Amount -> T.Text -> Inv -> Coins -> GetEntsCoinsRes
mkGecrMult i ws a n is c | n `elem` allCoinNames = mkGecrMultForCoins     a n c
                         | otherwise             = mkGecrMultForEnts i ws a n is


mkGecrMultForCoins :: Amount -> T.Text -> Coins -> GetEntsCoinsRes
mkGecrMultForCoins a n c@(Coins (cop, sil, gol)) = Mult { amount          = a
                                                        , nameSearchedFor = n
                                                        , entsRes         = Nothing
                                                        , coinsRes        = Just helper }
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
    uncurry (Mult a n) . maybe notFound (found ens) . findFullNameForAbbrev n $ ens
  where
    notFound                    = (Nothing, Nothing)
    found (zip is -> zipped) fn = (Just . takeMatchingEnts zipped $ fn, Nothing)
    takeMatchingEnts zipped  fn | matches <- filter (\(_, en) -> en == fn) zipped
                                = take a [ (ws^.entTbl) ! i' | (i', _) <- matches ]


mkGecrIndexed :: Id -> WorldState -> Index -> T.Text -> Inv -> GetEntsCoinsRes
mkGecrIndexed i ws x n is
  | n `elem` allCoinNames = SorryIndexedCoins
  | ens <- [ getEffName i ws i' | i' <- is ] = Indexed x n . maybe notFound (found ens) . findFullNameForAbbrev n $ ens
  where
    notFound = Left ""
    found ens fn | matches <- filter (\(_, en) -> en == fn) . zip is $ ens = if length matches < x
      then Left . mkPlurFromBoth . getEffBothGramNos i ws . fst . head $ matches
      else Right . ((ws^.entTbl) !) . fst $ matches !! (x - 1)


expandGecrs :: Coins -> [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
expandGecrs c (extractEnscsFromGecrs -> (gecrs, enscs))
  | mess <- map extractMesFromGecr gecrs
  , miss <- pruneDupIds [] . (fmap . fmap . fmap) (view entId) $ mess
  , rcs  <- reconcileCoins c . distillEnscs $ enscs
  = (gecrs, miss, rcs)


extractEnscsFromGecrs :: [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [EmptyNoneSome Coins])
extractEnscsFromGecrs = over _1 reverse . foldl' helper ([], [])
  where
    helper (gecrs, enscs) gecr@Mult { entsRes = Just _,  coinsRes = Just ensc } = (gecr : gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@Mult { entsRes = Just _,  coinsRes = Nothing   } = (gecr : gecrs, enscs)
    helper (gecrs, enscs)      Mult { entsRes = Nothing, coinsRes = Just ensc } = (gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@Mult { entsRes = Nothing, coinsRes = Nothing   } = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@Indexed {}                         = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@Sorry   {}                         = (gecr : gecrs, enscs)
    helper (gecrs, enscs) gecr@SorryIndexedCoins                  = (gecr : gecrs, enscs)


extractMesFromGecr :: GetEntsCoinsRes -> Maybe [Ent]
extractMesFromGecr = \case Mult    { entsRes = Just es } -> Just es
                           Indexed { entRes  = Right e } -> Just [e]
                           _                             -> Nothing


pruneDupIds :: Inv -> [Maybe Inv] -> [Maybe Inv]
pruneDupIds _       []                                              = []
pruneDupIds uniques (Nothing : rest)                                = Nothing : pruneDupIds uniques rest
pruneDupIds uniques (Just (deleteFirstOfEach uniques -> is) : rest) = Just is : pruneDupIds (is ++ uniques) rest


distillEnscs :: [EmptyNoneSome Coins] -> [EmptyNoneSome Coins]
distillEnscs enscs | Empty `elem` enscs               = [Empty]
                   | someOfs <- filter isSomeOf enscs
                   , noneOfs <- filter isNoneOf enscs = distill SomeOf someOfs ++ distill NoneOf noneOfs
  where
    isSomeOf (SomeOf _) = True
    isSomeOf _          = False
    isNoneOf (NoneOf _) = True
    isNoneOf _          = False
    distill  _ []                                         = []
    distill  f (foldr ((<>) . fromEnsCoins) mempty -> cs) = [ f cs ]
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


resolveEntCoinNamesWithRols :: Id         ->
                               WorldState ->
                               Args       ->
                               Inv        ->
                               Coins      ->
                               ([GetEntsCoinsRes], [Maybe RightOrLeft], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNamesWithRols i ws (map T.toLower -> as) is c
  | gecrMrols           <- map (mkGecrWithRol i ws is c) as
  , (gecrs, mrols)      <- (gecrMrols^..folded._1, gecrMrols^..folded._2)
  , (gecrs', miss, rcs) <- expandGecrs c gecrs
  = (gecrs', mrols, miss, rcs)


mkGecrWithRol :: Id -> WorldState -> Inv -> Coins -> T.Text -> (GetEntsCoinsRes, Maybe RightOrLeft)
mkGecrWithRol i ws is c n@(T.break (== slotChar) -> (a, b))
  | T.null b        = (mkGecr i ws is c n, Nothing)
  | T.length b == 1 = sorry
  | parsed <- reads (b^..unpacked.dropping 1 (folded.to toUpper)) :: [ (RightOrLeft, String) ] =
      case parsed of [(rol, _)] -> (mkGecr i ws is c a, Just rol)
                     _          -> sorry
  where
    sorry = (Sorry n, Nothing)


-- ==================================================
-- Processing "GetEntsCoinsRes":


-- "DupIdsNull" applies when nothing is left after having eliminated duplicate IDs.
pattern DupIdsNull       <- (_,                                                                          Just [])
pattern SorryOne     n   <- (Mult { amount = 1, nameSearchedFor = (aOrAn -> n), entsRes = Nothing },     Nothing)
pattern NoneMult     n   <- (Mult {             nameSearchedFor = n,            entsRes = Nothing },     Nothing)
pattern FoundMult    res <- (Mult {                                             entsRes = Just _  },     Just (Right -> res))
pattern NoneIndexed  n   <- (Indexed {                          nameSearchedFor = n, entRes = Left "" }, Nothing)
pattern SorryIndexed x p <- (Indexed { index = x,                                    entRes = Left p  }, Nothing)
pattern FoundIndexed res <- (Indexed {                                               entRes = Right _ }, Just (Right -> res))
pattern SorryCoins       <- (SorryIndexedCoins,                                                          Nothing)
pattern GenericSorry n   <- (Sorry { nameSearchedFor = (aOrAn -> n) },                                   Nothing)


procGecrMisPCInv :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisPCInv DupIdsNull         | res <- dupIdsRes               = res
procGecrMisPCInv (SorryOne     (don'tHaveInv    -> res))             = res
procGecrMisPCInv (NoneMult     (don'tHaveAnyInv -> res))             = res
procGecrMisPCInv (FoundMult                        res)              = res
procGecrMisPCInv (NoneIndexed  (don'tHaveAnyInv -> res))             = res
procGecrMisPCInv (SorryIndexed x p) | res <- don'tHaveIndexedInv x p = res
procGecrMisPCInv (FoundIndexed                     res)              = res
procGecrMisPCInv SorryCoins         | res <- sorryIndexedCoins       = res
procGecrMisPCInv (GenericSorry (don'tHaveInv    -> res))             = res
procGecrMisPCInv gecrMis = patternMatchFail "procGecrMisPCInv" [ showText gecrMis ]


dupIdsRes :: Either T.Text Inv
dupIdsRes = Left ""


don'tHaveInv :: T.Text -> Either T.Text Inv
don'tHaveInv = Left . sformat (do { "You don't have "; "." })


don'tHaveAnyInv :: T.Text -> Either T.Text Inv
don'tHaveAnyInv = Left . sformat (do { "You don't have any "; "s." })


don'tHaveIndexedInv :: Int -> T.Text -> Either T.Text Inv
don'tHaveIndexedInv x = Left . sformat (do { "You don't have " % int % " "; "." }) x


sorryIndexedCoins :: Either T.Text Inv
sorryIndexedCoins =
    Left . nl . sformat (do { "Sorry, but "; " cannot be used with coins." }) . dblQuote . T.singleton $ indexChar


procGecrMisReady :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisReady (Sorry (sorryBadSlot -> txt), Nothing) = Left txt
procGecrMisReady gecrMis                                = procGecrMisPCInv gecrMis


sorryBadSlot :: T.Text -> T.Text
sorryBadSlot n
  | T.singleton slotChar `T.isInfixOf` n = sformat m (mkSlotTxt "r") (mkSlotTxt "l") (nl' ringHelp)
  | otherwise                            = sformat (do { "You don't have "; "." }) . aOrAn $ n
  where
    m = do
        "Please specify "
        " or "
        "."
        ""


mkSlotTxt :: T.Text -> T.Text
mkSlotTxt = dblQuote . (T.singleton slotChar <>)


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", mkSlotTxt "r", " or ", mkSlotTxt "l", nl " immediately followed by:"
                    , dblQuote "i", nl " for index finger,"
                    , dblQuote "m", nl " for middle finger,"
                    , dblQuote "r", nl " for ring finger, or"
                    , dblQuote "p", nl " for pinky finger." ]


procGecrMisRm :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisRm DupIdsNull         | res <- dupIdsRes           = res
procGecrMisRm (SorryOne     (don'tSee    -> res))             = res
procGecrMisRm (NoneMult     (don'tSeeAny -> res))             = res
procGecrMisRm (FoundMult                    res)              = res
procGecrMisRm (NoneIndexed  (don'tSeeAny -> res))             = res
procGecrMisRm (SorryIndexed x p) | res <- don'tSeeIndexed x p = res
procGecrMisRm (FoundIndexed                 res)              = res
procGecrMisRm SorryCoins         | res <- sorryIndexedCoins   = res
procGecrMisRm (GenericSorry (don'tSee    -> res))             = res
procGecrMisRm gecrMis = patternMatchFail "procGecrMisRm" [ showText gecrMis ]


don'tSee :: T.Text -> Either T.Text Inv
don'tSee = Left . sformat (do { "You don't see "; " here." })


don'tSeeAny :: T.Text -> Either T.Text Inv
don'tSeeAny = Left . sformat (do { "You don't see any "; "s here." })


don'tSeeIndexed :: Int -> T.Text -> Either T.Text Inv
don'tSeeIndexed x = Left . sformat (do { "You don't see " % int % " "; " here." }) x


procGecrMisCon :: ConName -> (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisCon _  DupIdsNull         | res <- dupIdsRes                    = res
procGecrMisCon cn (SorryOne     (doesn'tContain    cn -> res))             = res
procGecrMisCon cn (NoneMult     (doesn'tContainAny cn -> res))             = res
procGecrMisCon _  (FoundMult                             res)              = res
procGecrMisCon cn (NoneIndexed  (doesn'tContainAny cn -> res))             = res
procGecrMisCon cn (SorryIndexed x p) | res <- doesn'tContainIndexed cn x p = res
procGecrMisCon _  (FoundIndexed                          res)              = res
procGecrMisCon _  SorryCoins         | res <- sorryIndexedCoins            = res
procGecrMisCon cn (GenericSorry (doesn'tContain    cn -> res))             = res
procGecrMisCon _  gecrMis = patternMatchFail "procGecrMisCon" [ showText gecrMis ]


doesn'tContain :: T.Text -> T.Text -> Either T.Text Inv
doesn'tContain cn = Left . sformat m cn
  where
    m = do
        "The "
        " doesn't contain "
        "."


doesn'tContainAny :: T.Text -> T.Text -> Either T.Text Inv
doesn'tContainAny cn = Left . sformat m cn
  where
    m = do
        "The "
        " doesn't contain any "
        "s."


doesn'tContainIndexed :: T.Text -> Int -> T.Text -> Either T.Text Inv
doesn'tContainIndexed cn x = Left . sformat m cn x
  where
    m = do
        "The "
        " doesn't contain " % int % " "
        "."


procGecrMisPCEq :: (GetEntsCoinsRes, Maybe Inv) -> Either T.Text Inv
procGecrMisPCEq DupIdsNull         | res <- dupIdsRes              = res
procGecrMisPCEq (SorryOne     (don'tHaveEq    -> res))             = res
procGecrMisPCEq (NoneMult     (don'tHaveAnyEq -> res))             = res
procGecrMisPCEq (FoundMult                       res)              = res
procGecrMisPCEq (NoneIndexed  (don'tHaveAnyEq -> res))             = res
procGecrMisPCEq (SorryIndexed x p) | res <- don'tHaveIndexedEq x p = res
procGecrMisPCEq (FoundIndexed                    res)              = res
procGecrMisPCEq SorryCoins         | res <- sorryIndexedCoins      = res
procGecrMisPCEq (GenericSorry (don'tHaveEq    -> res))             = res
procGecrMisPCEq gecrMis = patternMatchFail "procGecrMisPCEq" [ showText gecrMis ]


don'tHaveEq :: T.Text -> Either T.Text Inv
don'tHaveEq = Left . sformat (do { "You don't have "; " among your readied equipment." })


don'tHaveAnyEq :: T.Text -> Either T.Text Inv
don'tHaveAnyEq = Left . sformat (do { "You don't have any "; "s among your readied equipment." })


don'tHaveIndexedEq :: Int -> T.Text -> Either T.Text Inv
don'tHaveIndexedEq x = Left . sformat (do { "You don't have " % int % " "; " among your readied equipment." }) x


-- ==================================================
-- Processing "ReconciledCoins":


procReconciledCoinsPCInv :: ReconciledCoins -> Either [T.Text] Coins
procReconciledCoinsPCInv (Left  Empty)                            = Left ["You don't have any coins."]
procReconciledCoinsPCInv (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop "You don't have any copper pieces."
    s = msgOnNonzero sil "You don't have any silver pieces."
    g = msgOnNonzero gol "You don't have any gold pieces."
procReconciledCoinsPCInv (Right (SomeOf c                      )) = Right c
procReconciledCoinsPCInv (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat ("You don't have " % int % " copper pieces.") $ cop
    s = msgOnNonzero sil . sformat ("You don't have " % int % " silver pieces.") $ sil
    g = msgOnNonzero gol . sformat ("You don't have " % int % " gold pieces."  ) $ gol
procReconciledCoinsPCInv rc = patternMatchFail "procReconciledCoinsPCInv" [ showText rc ]


extractCoinsTxt :: [Maybe T.Text] -> [T.Text]
extractCoinsTxt []           = []
extractCoinsTxt (Nothing:xs) =     extractCoinsTxt xs
extractCoinsTxt (Just  x:xs) = x : extractCoinsTxt xs


msgOnNonzero :: Int -> T.Text -> Maybe T.Text
msgOnNonzero x msg = if x /= 0 then Just msg else Nothing


procReconciledCoinsRm :: ReconciledCoins -> Either [T.Text] Coins
procReconciledCoinsRm (Left  Empty)                            = Left ["You don't see any coins here."]
procReconciledCoinsRm (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop "You don't see any copper pieces here."
    s = msgOnNonzero sil "You don't see any silver pieces here."
    g = msgOnNonzero gol "You don't see any gold pieces here."
procReconciledCoinsRm (Right (SomeOf c                      )) = Right c
procReconciledCoinsRm (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat ("You don't see " % int % " copper pieces here.") $ cop
    s = msgOnNonzero sil . sformat ("You don't see " % int % " silver pieces here.") $ sil
    g = msgOnNonzero gol . sformat ("You don't see " % int % " gold pieces here."  ) $ gol
procReconciledCoinsRm rc = patternMatchFail "procReconciledCoinsRm" [ showText rc ]


procReconciledCoinsCon :: ConName -> ReconciledCoins -> Either [T.Text] Coins
procReconciledCoinsCon cn (Left  Empty)                            = doesn'tContainAnyCoins cn
procReconciledCoinsCon cn (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat (do { "The "; " doesn't contain any copper pieces." }) $ cn
    s = msgOnNonzero sil . sformat (do { "The "; " doesn't contain any silver pieces." }) $ cn
    g = msgOnNonzero gol . sformat (do { "The "; " doesn't contain any gold pieces."   }) $ cn
procReconciledCoinsCon _  (Right (SomeOf c                      )) = Right c
procReconciledCoinsCon cn (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat (do { "The "; "doesn't contain " % int % " copper pieces." }) cn $ cop
    s = msgOnNonzero sil . sformat (do { "The "; "doesn't contain " % int % " silver pieces." }) cn $ sil
    g = msgOnNonzero gol . sformat (do { "The "; "doesn't contain " % int % " gold pieces."   }) cn $ gol
procReconciledCoinsCon _ rc = patternMatchFail "procReconciledCoinsCon" [ showText rc ]


doesn'tContainAnyCoins :: T.Text -> Either [T.Text] Coins
doesn'tContainAnyCoins cn = Left [ sformat (do { "The "; " doesn't contain any coins." }) cn ]
