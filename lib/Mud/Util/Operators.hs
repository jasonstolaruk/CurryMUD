{-# LANGUAGE OverloadedStrings #-}

module Mud.Util.Operators ( (!#)
                          , (?)
                          , (#)
                          , (|!|)
                          , (|?|)
                          , (|&|)
                          , (|#|)
                          , (|<>|)
                          , Cond(..) ) where

import Control.Monad (unless)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)


infixl 0 ?
infixl 0 |&|
infixl 1 :?, |!|, |?|
infixl 8 |#|
infixl 9 !#, #

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
    m `memptyOnMempty` a = bool mempty a $ ()# m


-----


(|?|) :: (Monoid a) => Bool -> a -> a
(|?|) = memptyOnFalse
  where
    b `memptyOnFalse` a = bool a mempty b


-----


(|#|) :: (Eq a, Monoid a, Monad m) => a -> (a -> m ()) -> m ()
(|#|) = unlessMempty
  where
    x `unlessMempty` f = unless (()# x) . f $ x


-----


(|<>|) :: Text -> Text -> Text
a |<>| b = a <> " " <> b


-----


(|&|) :: a -> (a -> b) -> b
(|&|) = (&)
