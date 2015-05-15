{-# LANGUAGE LambdaCase, MonadComprehensions, RankNTypes, OverloadedStrings, ViewPatterns #-}

module Mud.Util.Misc ( (?)
                     , (|!|)
                     , (|$|)
                     , (|?|)
                     , Cond(..)
                     , atLst1
                     , blowUp
                     , divide
                     , dropFst
                     , dropIrrelevantFilenames
                     , dup
                     , eitherRet
                     , emptied
                     , ifThenElse
                     , ind
                     , isEmpty
                     , isVowel
                     , mIf
                     , mUnless
                     , mWhen
                     , maybeRet
                     , maybeVoid
                     , mkDateTimeTxt
                     , mkTimestamp
                     , notEmpty
                     , patternMatchFail
                     , reverseLookup
                     , toMaybe
                     , uncurry3
                     , uncurry4
                     , unit ) where

import Mud.Util.Quoting

import Control.Applicative ((<$>), (<*>))
import Control.Lens (Lens', lens)
import Control.Monad (guard)
import Data.Function (on)
import Data.IntMap.Lazy ((!))
import Data.List (delete)
import Data.Monoid ((<>), Monoid, mempty)
import Data.Time (getZonedTime)
import qualified Data.IntMap.Lazy as IM (IntMap, insert)
import qualified Data.Map.Lazy as M (Map, assocs)
import qualified Data.Text as T


infixl 0 ?
infixl 1 :?, |!|, |?|
infixl 7 `divide`
infixr 0 |$|


-- ==================================================


data Cond a = a :? a


(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y


-- mempty on mempty.
(|!|) :: (Eq a, Monoid a, Monoid b) => a -> b -> b
a |!| b = isEmpty a ? mempty :? b


-- mempty on False.
(|?|) :: (Monoid a) => Bool -> a -> a
a |?| b = a ? b :? mempty


-- TODO: GHC 7.10 will have a similar function named "(&)"...
(|$|) :: a -> (a -> b) -> b
(|$|) = flip ($)


atLst1 :: (Eq a, Num a) => a -> a
atLst1 x = case signum x of -1 -> 1
                            0  -> 1
                            _  -> x


blowUp :: T.Text -> T.Text -> T.Text -> [T.Text] -> a
blowUp modName funName msg (bracketQuote . T.intercalate ", " . map singleQuote -> vals) =
    error . T.unpack . T.concat $ [ modName, " ", funName, ": ", msg, ". ", vals ]


divide :: (Integral a, Fractional b) => a -> a -> b
divide = (/) `on` fromIntegral


dropFst :: (a, b, c) -> (b, c)
dropFst (_, x, y) = (x, y)


dropIrrelevantFilenames :: [FilePath] -> [FilePath]
dropIrrelevantFilenames = foldr ((.) . delete) id [ ".", "..", ".DS_Store" ]


dup :: a -> (a, a)
dup x = (x, x)


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return


emptied :: (Monad m, Monoid b) => m a -> m b
emptied m = m >> return mempty


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y


ind :: Int -> Lens' (IM.IntMap a) a
ind k = lens (! k) (flip (IM.insert k))


isEmpty :: (Eq m, Monoid m) => m -> Bool
isEmpty = (== mempty)


isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")


maybeRet :: Monad m => m a -> Maybe a -> m a
maybeRet = flip maybe return


maybeVoid :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe (return ())


mIf :: (Monad m) => m Bool -> m a -> m a -> m a
mIf p x y = p >>= \case True  -> x
                        False -> y


mUnless :: (Monad m) => m Bool -> m () -> m ()
mUnless p = mIf p (return ())


mWhen :: (Monad m) => m Bool -> m () -> m ()
mWhen p x = mIf p x . return $ ()


mkDateTimeTxt :: IO (T.Text, T.Text)
mkDateTimeTxt = helper <$> (T.words . T.pack . show) `fmap` getZonedTime
  where
    helper = (,) <$> head <*> (T.init . T.dropWhileEnd (/= '.') . head . tail)


mkTimestamp :: IO T.Text
mkTimestamp = [ bracketQuote $ date <> " " <> time | (date, time) <- mkDateTimeTxt ]


notEmpty :: (Eq m, Monoid m) => m -> Bool
notEmpty = not . isEmpty


patternMatchFail :: T.Text -> T.Text -> [T.Text] -> a
patternMatchFail modName funName = blowUp modName funName "pattern match failure"


reverseLookup :: (Eq v) => v -> M.Map k v -> k
reverseLookup v = fst . head . filter ((== v) . snd) . M.assocs


toMaybe :: Bool -> a -> Maybe a
toMaybe b = (guard b >>) . return


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c


uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d


unit :: ()
unit = let in ()
