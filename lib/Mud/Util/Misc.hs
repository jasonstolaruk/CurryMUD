{-# LANGUAGE LambdaCase, MonadComprehensions, OverloadedStrings, ViewPatterns #-}

module Mud.Util.Misc ( (?)
                     , (|!|)
                     , (|$|)
                     , (|?|)
                     , Cond(..)
                     , blowUp
                     , dup
                     , eitherRet
                     , emptied
                     , ifThenElse
                     , isVowel
                     , mIf
                     , maybeRet
                     , maybeVoid
                     , mkDateTimeTxt
                     , mkTimestamp
                     , notEmpty
                     , patternMatchFail
                     , reverseLookup
                     , toMaybe
                     , uncurry3
                     , uncurry4 ) where

import Mud.Util.Quoting

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Monoid ((<>), Monoid, mempty)
import Data.Time (getZonedTime)
import qualified Data.Map.Lazy as M (Map, assocs)
import qualified Data.Text as T


infixl 1 :?
data Cond a = a :? a


infixl 0 ?
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y


-- mempty on False.
infixl 1 |?|
(|?|) :: (Monoid a) => Bool -> a -> a
a |?| b = a ? b :? mempty


-- mempty on mempty.
infixl 1 |!|
(|!|) :: (Eq a, Monoid a, Monoid b) => a -> b -> b
a |!| b = (a == mempty) ? mempty :? b


infixr 0 |$| -- TODO: GHC 7.10 will have a similar function named "(&)"...
(|$|) :: a -> (a -> b) -> b
(|$|) = flip ($)


blowUp :: T.Text -> T.Text -> T.Text -> [T.Text] -> a
blowUp modName funName msg (bracketQuote . T.intercalate ", " . map singleQuote -> vals) =
    error . T.unpack . T.concat $ [ modName, " ", funName, ": ", msg, ". ", vals ]


dup :: a -> (a, a)
dup x = (x, x)


eitherRet :: (Monad m) => (a -> m b) -> Either a b -> m b
eitherRet = flip either return


-- TODO: Use this.
emptied :: (Monad m, Monoid b) => m a -> m b
emptied m = m >> return mempty


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y


isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")


maybeRet :: Monad m => m a -> Maybe a -> m a
maybeRet = flip maybe return


maybeVoid :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe (return ())


mIf :: (Monad m) => m Bool -> m a -> m a -> m a
mIf p x y = p >>= \case True  -> x
                        False -> y


mkDateTimeTxt :: IO (T.Text, T.Text)
mkDateTimeTxt = helper <$> (T.words . T.pack . show) `fmap` getZonedTime
  where
    helper = (,) <$> head <*> (T.init . T.dropWhileEnd (/= '.') . head . tail)


mkTimestamp :: IO T.Text
mkTimestamp = [ bracketQuote $ date <> " " <> time | (date, time) <- mkDateTimeTxt ]


notEmpty :: (Eq m, Monoid m) => m -> Bool
notEmpty = (/= mempty)


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
