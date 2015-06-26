module Mud.Util.Operators ( (!#)
                          , (#)
                          , (?)
                          , (|!|)
                          , (|#|)
                          , (|&|)
                          , (|?|)
                          , Cond(..) ) where

import Control.Monad (unless)
import Data.Function ((&))


infixl 0 ?
infixl 0 |&|
infixl 1 :?, |!|, |?|
infixl 8 |#|
infixl 9 !#, #


-- ==================================================


-- not mempty?
(!#) :: (Eq m, Monoid m) => () -> m -> Bool
()!# x = not $ ()# x


-- mempty?
(#) :: (Eq m, Monoid m) => () -> m -> Bool
()# x = x == mempty


data Cond a = a :? a


(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y


-- mempty on mempty
(|!|) :: (Eq a, Monoid a, Monoid b) => a -> b -> b
a |!| b = ()# a ? mempty :? b


-- unless mempty
(|#|) :: (Eq a, Monoid a, Monad m) => a -> (a -> m ()) -> m ()
x |#| f = unless (()# x) . f $ x


-- mempty on False
(|?|) :: (Monoid a) => Bool -> a -> a
a |?| b = a ? b :? mempty


(|&|) :: a -> (a -> b) -> b
(|&|) = (&)
