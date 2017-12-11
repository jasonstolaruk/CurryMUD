{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternSynonyms, RankNTypes, RebindableSyntax, ViewPatterns #-}

module Mud.Misc.NameResolution ( ReconciledCoins
                               , procGecrMisCon
                               , procGecrMisMobEq
                               , procGecrMisMobInv
                               , procGecrMisReady
                               , procGecrMisRm
                               , procReconciledCoinsCon
                               , procReconciledCoinsMobInv
                               , procReconciledCoinsRm
                               , resolveEntCoinNames
                               , resolveEntCoinNamesWithRols
                               , ringHelp ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Coins
import           Mud.Data.State.Util.Get
import           Mud.Data.State.Util.Misc
import           Mud.Misc.ANSI
import           Mud.Misc.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.TopLvlDefs.Misc
import qualified Mud.Util.Misc as U (blowUp, pmf)
import           Mud.Util.Misc hiding (blowUp, pmf)
import           Mud.Util.Operators
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow (first)
import           Control.Lens (view)
import           Control.Monad (guard)
import           Data.Char (isDigit)
import           Data.List ((\\), foldl')
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Text.Read (decimal)
import           Formatting (Format, (%), sformat)
import           Formatting.Formatters (int, stext)
import           Prelude hiding ((>>))
import qualified Data.Text as T
import qualified Prelude ((>>))

(>>) :: Format (Text -> r) a -> Format r' r -> Format r' a
a >> b = a % stext % b

-----

blowUp :: BlowUp a
blowUp = U.blowUp "Mud.Misc.NameResolution"

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Misc.NameResolution"

-- ==================================================
-- Resolving entity and coin names:

type ReconciledCoins = Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)

resolveEntCoinNames :: Id -> MudState -> Args -> Inv -> Coins -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNames i ms (map T.toLower -> as) is c = expandGecrs c [ mkGecr i ms is c a | a <- as ]

expandGecrs :: Coins -> [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [Maybe Inv], [ReconciledCoins])
expandGecrs c (extractEnscsFromGecrs -> (gecrs, enscs)) | mess <- map extractMesFromGecr gecrs
                                                        , miss <- pruneDupIds $ view entId `fmap3` mess
                                                        , rcs  <- reconcileCoins c . distillEnscs $ enscs
                                                        = (gecrs, miss, rcs)

extractEnscsFromGecrs :: [GetEntsCoinsRes] -> ([GetEntsCoinsRes], [EmptyNoneSome Coins])
extractEnscsFromGecrs = first reverse . foldl' helper mempties
  where
    helper (gecrs, enscs) gecr | isSorryGecr gecr                               = (gecr : gecrs,        enscs)
    helper (gecrs, enscs) gecr@Mult { entsRes = Just {}, coinsRes = Just ensc } = (gecr : gecrs, ensc : enscs)
    helper (gecrs, enscs) gecr@Mult { entsRes = Just {}, coinsRes = Nothing   } = (gecr : gecrs,        enscs)
    helper (gecrs, enscs)      Mult { entsRes = Nothing, coinsRes = Just ensc } = (gecrs,        ensc : enscs)
    helper (gecrs, enscs) gecr@Mult { entsRes = Nothing, coinsRes = Nothing   } = (gecr : gecrs,        enscs)
    helper (gecrs, enscs) gecr@Indexed {}                                       = (gecr : gecrs,        enscs)
    helper _              x                                                     = pmf "extractEnscsFromGecrs helper" x

isSorryGecr :: GetEntsCoinsRes -> Bool
isSorryGecr Sorry {}          = True
isSorryGecr SorryIndexedCoins = True
isSorryGecr _                 = False

extractMesFromGecr :: GetEntsCoinsRes -> Maybe [Ent]
extractMesFromGecr gecr = guard (not . isSorryGecr $ gecr) Prelude.>> case gecr of
  Mult    { entsRes = Just es } -> Just es
  Indexed { entRes  = Right e } -> Just . pure $ e
  _                             -> Nothing

pruneDupIds :: [Maybe Inv] -> [Maybe Inv]
pruneDupIds = dropJustNulls . pruneThem []
  where
    pruneThem _       []               = []
    pruneThem uniques (Nothing : rest) = Nothing : pruneThem uniques rest
    pruneThem uniques (Just is : rest) = let is' = is \\ uniques in Just is' : pruneThem (is' ++ uniques) rest
    dropJustNulls = foldr helper []
      where
        helper Nothing   acc = Nothing : acc
        helper (Just []) acc = acc
        helper x         acc = x : acc

reconcileCoins :: Coins -> [EmptyNoneSome Coins] -> [Either (EmptyNoneSome Coins) (EmptyNoneSome Coins)]
reconcileCoins (Coins (cop, sil, gol)) enscs = guard (()!# enscs) Prelude.>> concatMap helper enscs
  where
    helper Empty                                        = pure . Left $ Empty
    helper (NoneOf c)                                   = pure . Left . NoneOf $ c
    helper (SomeOf (Coins (someCop, someSil, someGol))) = concat [ [ mkEitherCop | isNonZero someCop ]
                                                                 , [ mkEitherSil | isNonZero someSil ]
                                                                 , [ mkEitherGol | isNonZero someGol ] ]
      where
        mkEitherCop | someCop <= cop = Right . SomeOf . Coins $ (someCop, 0,       0      )
                    | otherwise      = Left  . SomeOf . Coins $ (someCop, 0,       0      )
        mkEitherSil | someSil <= sil = Right . SomeOf . Coins $ (0,       someSil, 0      )
                    | otherwise      = Left  . SomeOf . Coins $ (0,       someSil, 0      )
        mkEitherGol | someGol <= gol = Right . SomeOf . Coins $ (0,       0,       someGol)
                    | otherwise      = Left  . SomeOf . Coins $ (0,       0,       someGol)

distillEnscs :: [EmptyNoneSome Coins] -> [EmptyNoneSome Coins]
distillEnscs enscs | Empty `elem` enscs               = pure Empty
                   | someOfs <- filter isSomeOf enscs
                   , noneOfs <- filter isNoneOf enscs = distill SomeOf someOfs ++ distill NoneOf noneOfs
  where
    isSomeOf     (SomeOf _) = True
    isSomeOf     _          = False
    isNoneOf     (NoneOf _) = True
    isNoneOf     _          = False
    distill      f enscs'   = guard (()!# enscs') Prelude.>> (pure . f . foldr ((<>) . fromEnsCoins) mempty $ enscs')
    fromEnsCoins (SomeOf c) = c
    fromEnsCoins (NoneOf c) = c
    fromEnsCoins ensc       = pmf "distillEnscs fromEnsCoins" ensc

mkGecr :: Id -> MudState -> Inv -> Coins -> Text -> GetEntsCoinsRes
mkGecr i ms searchIs searchCoins searchName@(headTail -> (h, t))
  | searchName == T.singleton allChar
  , allEs <- [ getEnt si ms | si <- searchIs ] = Mult { amount          = length searchIs
                                                      , nameSearchedFor = searchName
                                                      , entsRes         = Just allEs
                                                      , coinsRes        = Just . SomeOf $ searchCoins }
  | h == allChar = mkGecrMult i ms maxBound t searchIs searchCoins
  | isDigit h
  , (numText, rest) <- T.span isDigit searchName
  , numInt <- decimal numText |&| either (oops numText) fst
  = numText /= "0" ? parse rest numInt :? Sorry searchName
  | otherwise = mkGecrMult i ms 1 searchName searchIs searchCoins
  where
    oops = blowUp "mkGecr" "unable to convert Text to Int"
    parse rest numInt | T.length rest < 2               = Sorry searchName
                      | (delim, rest') <- headTail rest =
                          if | delim == amountChar -> mkGecrMult    i ms numInt rest' searchIs searchCoins
                             | delim == indexChar  -> mkGecrIndexed i ms numInt rest' searchIs
                             | otherwise           -> Sorry searchName

mkGecrMult :: Id -> MudState -> Amount -> Text -> Inv -> Coins -> GetEntsCoinsRes
mkGecrMult i ms a n is c | n `elem` allCoinNames = mkGecrMultForCoins     a n c
                         | otherwise             = mkGecrMultForEnts i ms a n is

mkGecrMultForCoins :: Amount -> Text -> Coins -> GetEntsCoinsRes
mkGecrMultForCoins a n c@(Coins (cop, sil, gol)) = Mult { amount          = a
                                                        , nameSearchedFor = n
                                                        , entsRes         = Nothing
                                                        , coinsRes        = Just helper }
  where
    helper | ()# c                       = Empty
           | n `elem` aggregateCoinNames = SomeOf $ if a == maxBound
             then c
             else coinsFromList . distributeAmt a . coinsToList $ c
           | otherwise = case n of
             "cp" | isZero cop    -> NoneOf . Coins $ (a,   0,   0  )
                  | a == maxBound -> SomeOf . Coins $ (cop, 0,   0  )
                  | otherwise     -> SomeOf . Coins $ (a,   0,   0  )
             "sp" | isZero sil    -> NoneOf . Coins $ (0,   a,   0  )
                  | a == maxBound -> SomeOf . Coins $ (0,   sil, 0  )
                  | otherwise     -> SomeOf . Coins $ (0,   a,   0  )
             "gp" | isZero gol    -> NoneOf . Coins $ (0,   0,   a  )
                  | a == maxBound -> SomeOf . Coins $ (0,   0,   gol)
                  | otherwise     -> SomeOf . Coins $ (0,   0,   a  )
             _                    -> pmf "mkGecrMultForCoins helper" n

distributeAmt :: Int -> [Int] -> [Int]
distributeAmt _   []     = []
distributeAmt amt (c:cs) | diff <- amt - c, diff >= 0 = c   : distributeAmt diff cs
                         | otherwise                  = amt : distributeAmt 0    cs

mkGecrMultForEnts :: Id -> MudState -> Amount -> Text -> Inv -> GetEntsCoinsRes
mkGecrMultForEnts i ms a n is = let effNames = [ getEffName i ms targetId | targetId <- is ] in
    uncurry (Mult a n) (findFullNameForAbbrev n effNames |&| maybe notFound (found effNames))
  where
    notFound                          = (Nothing, Nothing)
    found (zip is -> zipped) fullName = (Just . takeMatchingEnts zipped $ fullName, Nothing)
    takeMatchingEnts zipped  fullName = take a [ getEnt targetId ms | (targetId, effName) <- zipped
                                                                    , effName == fullName ]

mkGecrIndexed :: Id -> MudState -> Index -> Text -> Inv -> GetEntsCoinsRes
mkGecrIndexed i ms x n is
  | n `elem` allCoinNames = SorryIndexedCoins
  | otherwise             = let effNames = [ getEffName i ms targetId | targetId <- is ]
                            in Indexed x n (findFullNameForAbbrev n effNames |&| maybe notFound (found effNames))
  where
    notFound = Left ""
    found effNames fn | matches <- filter ((== fn) . snd) . zip is $ effNames = if length matches < x
      then Left . mkPlurFromBoth . getEffBothGramNos i ms . fst . head $ matches
      else Right . (`getEnt` ms) . fst $ matches !! pred x

-- ============================================================
-- Resolving entity and coin names with right/left indicators:

resolveEntCoinNamesWithRols :: Id
                            -> MudState
                            -> Args
                            -> Inv
                            -> Coins
                            -> ([GetEntsCoinsRes], [Maybe RightOrLeft], [Maybe Inv], [ReconciledCoins])
resolveEntCoinNamesWithRols i ms (map T.toLower -> as) is c =
    let (unzip -> (gecrs, mrols)) = map (mkGecrWithRol i ms is c) as
        (gecrs', miss, rcs)       = expandGecrs c gecrs
    in (gecrs', mrols, miss, rcs)

mkGecrWithRol :: Id -> MudState -> Inv -> Coins -> Text -> (GetEntsCoinsRes, Maybe RightOrLeft)
mkGecrWithRol i ms is c n@(T.breakOn (T.singleton slotChar) -> (a, b))
  | ()# b           = (mkGecr i ms is c n, Nothing)
  | T.length b == 1 = sorry
  | parsed <- reads . T.unpack . T.toUpper . T.drop 1 $ b = case parsed of [(rol, _)] -> (mkGecr i ms is c a, Just rol)
                                                                           _          -> sorry
  where
    sorry = (Sorry n, Nothing)

-- ==================================================
-- Processing "GetEntsCoinsRes":

pattern DupIdsEmpty  ::                              (a,               Maybe [b])
pattern SorryOne     ::             Text          -> (GetEntsCoinsRes, Maybe a  )
pattern NoneMult     ::             Text          -> (GetEntsCoinsRes, Maybe a  )
pattern FoundMult    :: forall a b. Either a b    -> (GetEntsCoinsRes, Maybe b  )
pattern NoneIndexed  ::             Text          -> (GetEntsCoinsRes, Maybe a  )
pattern SorryIndexed ::             Index -> Plur -> (GetEntsCoinsRes, Maybe a  )
pattern FoundIndexed :: forall a b. Either a b    -> (GetEntsCoinsRes, Maybe b  )
pattern SorryCoins   ::                              (GetEntsCoinsRes, Maybe a  )
pattern GenericSorry ::             Text          -> (GetEntsCoinsRes, Maybe a  )

-- "DupIdsEmpty" applies when nothing is left after having eliminated duplicate IDs.
pattern DupIdsEmpty      <- (_,                                                                         Just [])
pattern SorryOne     n   <- (Mult { amount = 1,   nameSearchedFor = (aOrAn -> n), entsRes = Nothing  }, Nothing)
pattern NoneMult     n   <- (Mult {               nameSearchedFor = n,            entsRes = Nothing  }, Nothing)
pattern FoundMult    res <- (Mult {                                               entsRes = Just {}  }, Just (Right -> res))
pattern NoneIndexed  n   <- (Indexed {            nameSearchedFor = n,            entRes  = Left ""  }, Nothing)
pattern SorryIndexed x p <- (Indexed { index = x,                                 entRes  = Left p   }, Nothing)
pattern FoundIndexed res <- (Indexed {                                            entRes  = Right {} }, Just (Right -> res))
pattern SorryCoins       <- (SorryIndexedCoins,                                                         Nothing)
pattern GenericSorry n   <- (Sorry   {            nameSearchedFor = (aOrAn -> n)                     }, Nothing)

procGecrMisMobInv :: (GetEntsCoinsRes, Maybe Inv) -> Either Text Inv
procGecrMisMobInv DupIdsEmpty        | res <- dupIdsRes               = res
procGecrMisMobInv (SorryOne     (don'tHaveInv    -> res))             = res
procGecrMisMobInv (NoneMult     (don'tHaveAnyInv -> res))             = res
procGecrMisMobInv (FoundMult                        res)              = res
procGecrMisMobInv (NoneIndexed  (don'tHaveAnyInv -> res))             = res
procGecrMisMobInv (SorryIndexed x p) | res <- don'tHaveIndexedInv x p = res
procGecrMisMobInv (FoundIndexed                     res)              = res
procGecrMisMobInv SorryCoins         | res <- sorryIndexedCoins       = res
procGecrMisMobInv (GenericSorry (don'tHaveInv    -> res))             = res
procGecrMisMobInv gecrMis                                             = pmf "procGecrMisMobInv" gecrMis

dupIdsRes :: Either Text Inv
dupIdsRes = Left ""

don'tHaveInv :: Text -> Either Text Inv
don'tHaveInv = Left . sformat ("You don't have " >> ".")

don'tHaveAnyInv :: Text -> Either Text Inv
don'tHaveAnyInv = Left . sformat ("You don't have any " >> "s.")

don'tHaveIndexedInv :: Int -> Text -> Either Text Inv
don'tHaveIndexedInv x = Left . sformat (do { "You don't have " % int % " "; "." }) x

sorryIndexedCoins :: Either Text Inv
sorryIndexedCoins =
    Left . sformat ("Sorry, but " >> " cannot be used with coins.") . dblQuote . T.singleton $ indexChar

procGecrMisReady :: (GetEntsCoinsRes, Maybe Inv) -> Either Text Inv
procGecrMisReady (Sorry (sorryBadSlot -> txt), Nothing) = Left txt
procGecrMisReady gecrMis                                = procGecrMisMobInv gecrMis

sorryBadSlot :: Text -> Text
sorryBadSlot n
  | T.singleton slotChar `T.isInfixOf` n = sformat m (mkSlotTxt "r") (mkSlotTxt "l") . nlPrefix $ ringHelp
  | otherwise                            = sformat ("You don't have " >> ".") . aOrAn $ n
  where
    m = "Please specify " >> " or " >> "." >> ""

mkSlotTxt :: Text -> Text
mkSlotTxt = colorWith quoteColor . dblQuote . T.cons slotChar

ringHelp :: Text
ringHelp = T.concat [ "For rings, specify ", mkSlotTxt "r", " or ", mkSlotTxt "l", nl " immediately followed by:"
                    , colorWith quoteColor "\"i\"" <> nl " for index finger,"
                    , colorWith quoteColor "\"m\"" <> nl " for middle finger,"
                    , colorWith quoteColor "\"r\"" <> nl " for ring finger, or"
                    , colorWith quoteColor "\"p\"" <> nl " for pinky finger." ]

procGecrMisMobEq :: (GetEntsCoinsRes, Maybe Inv) -> Either Text Inv
procGecrMisMobEq DupIdsEmpty        | res <- dupIdsRes              = res
procGecrMisMobEq (SorryOne     (don'tHaveEq    -> res))             = res
procGecrMisMobEq (NoneMult     (don'tHaveAnyEq -> res))             = res
procGecrMisMobEq (FoundMult                       res )             = res
procGecrMisMobEq (NoneIndexed  (don'tHaveAnyEq -> res))             = res
procGecrMisMobEq (SorryIndexed x p) | res <- don'tHaveIndexedEq x p = res
procGecrMisMobEq (FoundIndexed                    res )             = res
procGecrMisMobEq SorryCoins         | res <- sorryIndexedCoins      = res
procGecrMisMobEq (GenericSorry (don'tHaveEq    -> res))             = res
procGecrMisMobEq gecrMis                                            = pmf "procGecrMisMobEq" gecrMis

don'tHaveEq :: Text -> Either Text Inv
don'tHaveEq = Left . sformat ("You don't have " >> " among your readied equipment.")

don'tHaveAnyEq :: Text -> Either Text Inv
don'tHaveAnyEq = Left . sformat ("You don't have any " >> "s among your readied equipment.")

don'tHaveIndexedEq :: Int -> Text -> Either Text Inv
don'tHaveIndexedEq x = Left . sformat (do { "You don't have " % int % " "; " among your readied equipment." }) x

procGecrMisRm :: (GetEntsCoinsRes, Maybe Inv) -> Either Text Inv
procGecrMisRm DupIdsEmpty        | res <- dupIdsRes           = res
procGecrMisRm (SorryOne     (don'tSee    -> res))             = res
procGecrMisRm (NoneMult     (don'tSeeAny -> res))             = res
procGecrMisRm (FoundMult                    res)              = res
procGecrMisRm (NoneIndexed  (don'tSeeAny -> res))             = res
procGecrMisRm (SorryIndexed x p) | res <- don'tSeeIndexed x p = res
procGecrMisRm (FoundIndexed                 res)              = res
procGecrMisRm SorryCoins         | res <- sorryIndexedCoins   = res
procGecrMisRm (GenericSorry (don'tSee    -> res))             = res
procGecrMisRm gecrMis                                         = pmf "procGecrMisRm" gecrMis

don'tSee :: Text -> Either Text Inv
don'tSee = Left . sformat ("You don't see " >> " here.")

don'tSeeAny :: Text -> Either Text Inv
don'tSeeAny = Left . sformat ("You don't see any " >> "s here.")

don'tSeeIndexed :: Int -> Text -> Either Text Inv
don'tSeeIndexed x = Left . sformat (do { "You don't see " % int % " "; " here." }) x

procGecrMisCon :: ConName -> (GetEntsCoinsRes, Maybe Inv) -> Either Text Inv
procGecrMisCon _  DupIdsEmpty        | res <- dupIdsRes                    = res
procGecrMisCon cn (SorryOne     (doesn'tContain    cn -> res))             = res
procGecrMisCon cn (NoneMult     (doesn'tContainAny cn -> res))             = res
procGecrMisCon _  (FoundMult                             res)              = res
procGecrMisCon cn (NoneIndexed  (doesn'tContainAny cn -> res))             = res
procGecrMisCon cn (SorryIndexed x p) | res <- doesn'tContainIndexed cn x p = res
procGecrMisCon _  (FoundIndexed                          res)              = res
procGecrMisCon _  SorryCoins         | res <- sorryIndexedCoins            = res
procGecrMisCon cn (GenericSorry (doesn'tContain    cn -> res))             = res
procGecrMisCon _  gecrMis                                                  = pmf "procGecrMisCon" gecrMis

doesn'tContain :: Text -> Text -> Either Text Inv
doesn'tContain cn = Left . sformat m cn
  where
    m = "The " >> " doesn't contain " >> "."

doesn'tContainAny :: Text -> Text -> Either Text Inv
doesn'tContainAny cn = Left . sformat m cn
  where
    m = "The " >> " doesn't contain any " >> "s."

doesn'tContainIndexed :: Text -> Int -> Text -> Either Text Inv
doesn'tContainIndexed cn x = Left . sformat m cn x
  where
    m = do { "The "; " doesn't contain " % int % " "; "." }

-- ==================================================
-- Processing "ReconciledCoins":

procReconciledCoinsMobInv :: ReconciledCoins -> Either [Text] Coins
procReconciledCoinsMobInv (Left  Empty)                            = Left . pure $ "You don't have any coins."
procReconciledCoinsMobInv (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop "You don't have any copper pieces."
    s = msgOnNonzero sil "You don't have any silver pieces."
    g = msgOnNonzero gol "You don't have any gold pieces."
procReconciledCoinsMobInv (Right (SomeOf c                      )) = Right c
procReconciledCoinsMobInv (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat ("You don't have " % int % " copper pieces.") $ cop
    s = msgOnNonzero sil . sformat ("You don't have " % int % " silver pieces.") $ sil
    g = msgOnNonzero gol . sformat ("You don't have " % int % " gold pieces."  ) $ gol
procReconciledCoinsMobInv rc = pmf "procReconciledCoinsMobInv" rc

extractCoinsTxt :: [Maybe Text] -> [Text]
extractCoinsTxt []           = []
extractCoinsTxt (Nothing:xs) =     extractCoinsTxt xs
extractCoinsTxt (Just  x:xs) = x : extractCoinsTxt xs

msgOnNonzero :: Int -> Text -> Maybe Text
msgOnNonzero x msg = guard (isNonZero x) Prelude.>> return msg

procReconciledCoinsRm :: ReconciledCoins -> Either [Text] Coins
procReconciledCoinsRm (Left  Empty)                            = Left . pure $ "You don't see any coins here."
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
procReconciledCoinsRm rc = pmf "procReconciledCoinsRm" rc

procReconciledCoinsCon :: ConName -> ReconciledCoins -> Either [Text] Coins
procReconciledCoinsCon cn (Left  Empty)                            = doesn'tContainAnyCoins cn
procReconciledCoinsCon cn (Left  (NoneOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat ("The " >> " doesn't contain any copper pieces.") $ cn
    s = msgOnNonzero sil . sformat ("The " >> " doesn't contain any silver pieces.") $ cn
    g = msgOnNonzero gol . sformat ("The " >> " doesn't contain any gold pieces."  ) $ cn
procReconciledCoinsCon _  (Right (SomeOf c                      )) = Right c
procReconciledCoinsCon cn (Left  (SomeOf (Coins (cop, sil, gol)))) = Left . extractCoinsTxt $ [ c, s, g ]
  where
    c = msgOnNonzero cop . sformat (do { "The "; "doesn't contain " % int % " copper pieces." }) cn $ cop
    s = msgOnNonzero sil . sformat (do { "The "; "doesn't contain " % int % " silver pieces." }) cn $ sil
    g = msgOnNonzero gol . sformat (do { "The "; "doesn't contain " % int % " gold pieces."   }) cn $ gol
procReconciledCoinsCon _ rc = pmf "procReconciledCoinsCon" rc

doesn'tContainAnyCoins :: Text -> Either [Text] Coins
doesn'tContainAnyCoins cn = Left [ sformat ("The " >> " doesn't contain any coins.") cn ]
