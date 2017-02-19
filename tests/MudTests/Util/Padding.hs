{-# LANGUAGE OverloadedStrings #-}

module MudTests.Util.Padding where

import Mud.Util.Padding

import Control.Applicative (liftA2)
import Control.Lens (both)
import Control.Lens.Operators ((&), (%~))
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers (NonNegative(..))
import Test.Tasty.QuickCheck ((==>), Property, choose, forAll)


prop_quoteWithAndPad_length :: Text -> Property
prop_quoteWithAndPad_length t = forAll (choose (3, 50)) $ \len ->
    let res = quoteWithAndPad ("[", "]") len t
    in T.length res == len


prop_quoteWithAndPad_quotes :: Char -> Char -> Text -> Property
prop_quoteWithAndPad_quotes left right t = let f = not . isSpace in liftA2 (&&) (f . fst) (f . snd) (left, right) ==>
    forAll (choose (3, 50)) $ \len -> let quotes    = (left, right) & both %~ T.singleton
                                          res       = quoteWithAndPad quotes len t
                                          grabRight = T.head . T.dropWhile isSpace . T.reverse
                                      in liftA2 (&&) ((== left) . T.head) ((== right) . grabRight) res


prop_padOrTrunc_pads :: NonNegative Int -> Text -> Property
prop_padOrTrunc_pads (NonNegative x) t = T.length t <= x ==>
    (T.length . padOrTrunc x $ t) == x


prop_padOrTrunc_truncates :: NonNegative Int -> Text -> Property
prop_padOrTrunc_truncates (NonNegative x) t = T.length t > x ==>
    (T.length . padOrTrunc x $ t) == x
