{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Operators ( (!#)
                          , (#)
                          , (<>+)
                          , (?)
                          , (|!|)
                          , (|#|)
                          , (|&|)
                          , (|<>|)
                          , (|?|)
                          , Cond(..) ) where

import Control.Lens.Operators ((<>~))
import Control.Lens.Setter (ASetter)
import Control.Monad (unless)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Monoid ()
import Data.Text (Text)

infixl 0 ?
infixl 0 |&|
infixl 1 :?, |!|, |?|
infixl 8 |#|
infixl 9 !#, #

infixr 4 <>+
infixr 6 |<>|

-- ==================================================

data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

-----

(#) :: (Eq m, Monoid m) => () -> m -> Bool
(#) = isMempty
  where
    () `isMempty` x = x == mempty

(!#) :: (Eq m, Monoid m) => () -> m -> Bool
(!#) = isNotMempty
  where
    () `isNotMempty` x = not $ ()# x

-----

(|!|) :: (Eq a, Monoid a, Monoid b) => a -> b -> b
(|!|) = memptyOnMempty
  where
    m `memptyOnMempty` a = bool a mempty $ ()# m

-----

(|?|) :: (Monoid a) => Bool -> a -> a
(|?|) = memptyOnFalse
  where
    memptyOnFalse = flip (bool mempty)

-----

(|#|) :: (Eq a, Monoid a, Monad m) => a -> (a -> m ()) -> m ()
(|#|) = unlessMempty
  where
    x `unlessMempty` f = unless (()# x) . f $ x

-----

(|&|) :: a -> (a -> b) -> b
(|&|) = (&)

-----

(|<>|) :: Text -> Text -> Text
a |<>| b = a <> " " <> b

-----

(<>+) :: (Applicative f, Monoid (f a)) => ASetter s t (f a) (f a) -> a -> s -> t
a <>+ b = a <>~ pure b
