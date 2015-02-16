{-# LANGUAGE OverloadedStrings #-}

module MudTests.Util.Padding where

import Mud.Util.Padding

import Control.Lens (both, over)
import Data.Char (isSpace)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers (NonNegative(..))
import Test.Tasty.QuickCheck ((==>), Property, choose, forAll)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


prop_quoteWithAndPad_length :: T.Text -> Property
prop_quoteWithAndPad_length t = forAll (choose (3, 50)) $ \len ->
    let res = quoteWithAndPad ("[", "]") len t
    in T.length res == len


prop_quoteWithAndPad_quotes :: Char -> Char -> T.Text -> Property
prop_quoteWithAndPad_quotes left right t = (not . isSpace $ left) &&
                                           (not . isSpace $ right) ==>
  forAll (choose (3, 50)) $ \len ->
      let quotes    = over both T.singleton (left, right)
          res       = quoteWithAndPad quotes len t
          grabRight = T.head . T.dropWhile isSpace . T.reverse
      in T.head res == left && grabRight res == right


prop_padOrTrunc_pads :: NonNegative Int -> T.Text -> Property
prop_padOrTrunc_pads (NonNegative x) t = T.length t <= x ==>
  (T.length . padOrTrunc x $ t) == x


prop_padOrTrunc_truncates :: NonNegative Int -> T.Text -> Property
prop_padOrTrunc_truncates (NonNegative x) t = T.length t > x ==>
  (T.length . padOrTrunc x $ t) == x
