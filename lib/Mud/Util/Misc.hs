{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts, MonadComprehensions, OverloadedStrings, RankNTypes, TypeFamilies #-}

module Mud.Util.Misc ( atLst1
                     , atomicWriteIORef'
                     , blowUp
                     , BlowUp
                     , boolToMaybe
                     , compose
                     , concatMapM
                     , divide
                     , divideRound
                     , dropFst
                     , dropIrrelevantFiles
                     , dropSnd
                     , dropThr
                     , dup
                     , dup3
                     , dup4
                     , dupFirst
                     , dupSecond
                     , eitherRet
                     , emptied
                     , errorWithStack
                     , fanUncurry
                     , fanView
                     , fmap2
                     , fmap3
                     , formatTimeHelper
                     , fromEither
                     , fromLeft
                     , fromMaybeEmp
                     , fromRight
                     , ifThenElse
                     , ind
                     , intDivide
                     , isNonZero
                     , isVowel
                     , isZero
                     , listToMaybe
                     , lookupMapValue
                     , max0
                     , max1
                     , maybeEmp
                     , maybeRet
                     , maybeVoid
                     , mempties
                     , middle
                     , mIf
                     , minusFifth
                     , minusHalf
                     , minusQuarter
                     , minusTenth
                     , minusThird
                     , mkDateTimeTxt
                     , mkTimestamp
                     , mMempty
                     , mUnless
                     , mWhen
                     , onFalse
                     , onLeft
                     , onTrue
                     , panicMsg
                     , patternMatchFail
                     , PatternMatchFail
                     , percent
                     , plusFifth
                     , plusHalf
                     , plusQuarter
                     , plusTenth
                     , plusThird
                     , printPanicMsg
                     , safeCoerce
                     , safePerformIO
                     , sortEithers
                     , strictId
                     , thrice
                     , twice
                     , unadulterated
                     , uncurry3
                     , uncurry4
                     , uncurry5
                     , unit ) where

import Mud.Util.List
import Mud.Util.Operators
import Mud.Util.Quoting

import Control.Arrow ((&&&), Arrow, first, second)
import Control.Lens (_1, _2, Lens', lens, view)
import Control.Lens.Getter (Getting)
import Control.Lens.Operators ((%~))
import Control.Monad (guard, join)
import Control.Monad.Reader.Class (MonadReader)
import Data.Bool (bool)
import Data.Function (on)
import Data.IORef (IORef, atomicWriteIORef)
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), (<>))
import Data.Text (Text)
import Data.Time (FormatTime, defaultTimeLocale, formatTime, getZonedTime)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import qualified Data.IntMap.Lazy as IM (IntMap, insert, lookup)
import qualified Data.Map.Lazy as M (Map, assocs)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)


default (Int, Double)


-----


infixl 7 `divide`, `divideRound`, `intDivide`, `percent`


-- ==================================================


atLst1 :: (Eq a, Num a) => a -> a
atLst1 x = case signum x of -1 -> 1
                            0  -> 1
                            _  -> x


atomicWriteIORef' :: IORef a -> a -> IO ()
atomicWriteIORef' ior = (atomicWriteIORef ior $!)


boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe b = (guard b >>) . return


concatMapM  :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f


type BlowUp a = Text -> Text -> Text -> a


blowUp :: Text -> BlowUp a
blowUp modName funName msg t = error . T.unpack . T.concat $ [ modName, " ", funName, ": ", msg ] ++ (t |!| [ "; ", t ])


compose :: a -> [a -> a] -> a
compose = foldr ($)


divide :: (Integral a, Fractional b) => a -> a -> b
divide = (/) `on` fromIntegral


divideRound :: (Integral a) => a -> a -> a
x `divideRound` y = round $ x `divide` y


dropFst :: (a, b, c) -> (b, c)
dropFst (_, x, y) = (x, y)


dropSnd :: (a, b, c) -> (a, c)
dropSnd (x, _, z) = (x, z)


dropThr :: (a, b, c) -> (a, b)
dropThr (x, y, _) = (x, y)


dropIrrelevantFiles :: [FilePath] -> [FilePath]
dropIrrelevantFiles = foldr ((.) . delete) id [ ".", "..", ".DS_Store" ]


dup :: a -> (a, a)
dup x = (x, x)


dup3 :: a -> (a, a, a)
dup3 x = (x, x, x)


dup4 :: a -> (a, a, a, a)
dup4 x = (x, x, x, x)


dupFirst :: (a -> b) -> a -> (b, a)
dupFirst f = first f . dup


dupSecond :: (a -> b) -> a -> (a, b)
dupSecond f = second f . dup


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return


emptied :: (Monad m, Monoid b) => m a -> m b
emptied m = m >> mMempty


errorWithStack :: HasCallStack => String -> a
errorWithStack msg = error $ msg ++ "\n" ++ prettyCallStack callStack


-- "(&&&)" is the "fanout" operator.
fanUncurry :: (a -> b -> c) -> (a -> b -> c') -> (a, b) -> (c, c')
f `fanUncurry` g = uncurry f &&& uncurry g


fanView :: (MonadReader s (a b), Arrow a) => Getting c s c -> Getting c' s c' -> a b (c, c')
a `fanView` b = view a &&& view b


fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b) -- Nice when used infix.
fmap2 = fmap . fmap


fmap3 :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
fmap3 = fmap2 . fmap


formatTimeHelper :: (FormatTime a) => a -> Text
formatTimeHelper = T.pack . formatTime defaultTimeLocale "%Z: %F %T"


fromEither :: Either a a -> a -- TODO: Eew!
fromEither (Right a) = a
fromEither (Left  a) = a


fromLeft :: (Show a, Show b) => Either a b -> a -- TODO: Eew!
fromLeft (Left x) = x
fromLeft x        = blowUp "Mud.Util.Misc" "fromLeft" "Right" . T.pack . show $ x


fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Right x) = x
fromRight x         = blowUp "Mud.Util.Misc" "fromRight" "Left" . T.pack . show $ x


fromMaybeEmp :: (Monoid a) => Maybe a -> a
fromMaybeEmp = maybeEmp id


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y


ind :: HasCallStack => Int -> Lens' (IM.IntMap a) a
ind k = lens getter (flip (IM.insert k))
  where
    getter = fromMaybe (errorWithStack $ "key " ++ show k ++ " is not an element of the map") . (k `IM.lookup`)


{-
"intDivide" is integer division, similar in effect to "divideRound".
"intDivide" and "divideRound" do produce different results in some cases due to the behavior of "round". See
"test_division_compare_results".
Profiling showed that "divideRound" is more efficient than "intDivide".
-}
intDivide :: (Integral a) => a -> a -> a
x `intDivide` y = (x + y `div` 2) `div` y


isNonZero :: (Eq a, Num a) => a -> Bool
isNonZero = not . isZero


isVowel :: Char -> Bool
isVowel = (`elem` ("aeiou" :: String))


isZero :: (Eq a, Num a) => a -> Bool
isZero = (()#) . Sum


listToMaybe :: (Show a) => [a] -> Maybe a
listToMaybe []  = Nothing
listToMaybe [a] = Just a
listToMaybe xs  = patternMatchFail "Mud.Util.Misc" "listToMaybe" xs


middle :: (a -> b -> a) -> b -> a -> b -> a
middle f mid x y = x `f` mid `f` y


mIf :: (Monad m) => m Bool -> m a -> m a -> m a
mIf p x = (p >>=) . flip bool x


mMempty :: (Monad a, Monoid b) => a b
mMempty = return mempty


mUnless :: (Monad m) => m Bool -> m () -> m ()
mUnless p = mIf p unit


mWhen :: (Monad m) => m Bool -> m () -> m ()
mWhen p x = mIf p x unit


max0 :: (Num a, Ord a) => a -> a
max0 = (`max` 0)


max1 :: (Num a, Ord a) => a -> a
max1 = (`max` 1)


maybeEmp :: (Monoid b) => (a -> b) -> Maybe a -> b
maybeEmp = maybe mempty


maybeRet :: (Monad m) => m a -> Maybe a -> m a
maybeRet = flip maybe return


maybeVoid :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe unit


mempties :: (Monoid a, Monoid b) => (a, b)
mempties = (mempty, mempty)


minusFifth :: Int -> Int
minusFifth x = round (fromIntegral x * 0.80 :: Double)


minusHalf :: Int -> Int
minusHalf x = round (fromIntegral x * 0.50 :: Double)


minusQuarter :: Int -> Int
minusQuarter x = round (fromIntegral x * 0.75 :: Double)


minusTenth :: Int -> Int
minusTenth x = round (fromIntegral x * 0.90 :: Double)


minusThird :: Int -> Int
minusThird x = round (fromIntegral x * 0.66 :: Double)


mkDateTimeTxt :: IO (Text, Text)
mkDateTimeTxt = helper <$> (T.words . T.pack . show) `fmap` getZonedTime
  where
    helper = (,) <$> head <*> (T.init . T.dropWhileEnd (/= '.') . head . tail)


mkTimestamp :: IO Text
mkTimestamp = [ bracketQuote . uncurry (|<>|) $ pair | pair <- mkDateTimeTxt ]


onFalse :: Bool -> (a -> a) -> a -> a
onFalse b f x | b         = x
              | otherwise = f x


onTrue :: Bool -> (a -> a) -> a -> a
onTrue b f x | b         = f x
             | otherwise = x


onLeft :: (Show a, Show b) => (a -> c) -> Either a b -> Either c b
onLeft f (Left  a) = Left . f $ a
onLeft _ x         = blowUp "Mud.Util.Misc" "onLeft" "Right" . T.pack . show $ x


panicMsg :: Text
panicMsg = "panic! " <> parensQuote ("the " <> singleQuote "impossible" <> " happened")


type PatternMatchFail a b = Text -> a -> b


patternMatchFail :: (Show a) => Text -> PatternMatchFail a b
patternMatchFail modName funName = blowUp modName funName "pattern match failure" . T.pack . show


percent :: Int -> Int -> Int
percent x y = 100 * x `divideRound` y


plusFifth :: Int -> Int
plusFifth x = round (fromIntegral x * 1.20 :: Double)


plusHalf :: Int -> Int
plusHalf x = round (fromIntegral x * 1.50 :: Double)


plusQuarter :: Int -> Int
plusQuarter x = round (fromIntegral x * 1.25 :: Double)


plusTenth :: Int -> Int
plusTenth x = round (fromIntegral x * 1.10 :: Double)


plusThird :: Int -> Int
plusThird x = round (fromIntegral x * 1.33 :: Double)


printPanicMsg :: IO ()
printPanicMsg = hPutStrLn stderr . T.unpack $ panicMsg <> ": see the logs for details"


lookupMapValue :: (Eq v) => v -> M.Map k v -> Maybe k
lookupMapValue v = lookupValue v . M.assocs


safeCoerce :: a ~ b => a -> b
safeCoerce x = x


safePerformIO :: IO a -> IO a
safePerformIO = (return =<<)


sortEithers :: [Either l r] -> ([r], [l])
sortEithers = foldr helper mempties
  where
    helper (Right a) = _1 %~ (a :)
    helper (Left  b) = _2 %~ (b :)


{-# ANN strictId ("HLint: ignore Redundant seq" :: String) #-}
strictId :: a -> a
strictId = join seq


twice :: (a -> a) -> a -> a
twice f = f . f


thrice :: (a -> a) -> a -> a
thrice f = f . twice f


unadulterated :: (Monad m, Applicative f) => a -> m (f a)
unadulterated = return . pure


uncurry3 :: (a -> b -> c -> d) -> (,,) a b c -> d
uncurry3 f (a, b, c) = f a b c


uncurry4 :: (a -> b -> c -> d -> e) -> (,,,) a b c d -> e
uncurry4 f (a, b, c, d) = f a b c d


uncurry5 :: (a -> b -> c -> d -> e -> f) -> (,,,,) a b c d e -> f
uncurry5 f (a, b, c, d, e) = f a b c d e


unit :: (Monad m) => m ()
unit = return $ let in ()
