{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MudTests.Util.Wrapping where

import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Text
import Mud.Util.Wrapping
import MudTests.TestUtil
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (isDigit, isSpace)
import Data.Monoid ((<>))
import Test.Tasty.QuickCheck ((==>), Property, choose, forAll)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "MudTests.Util.Wrapping"


-- ==================================================


prop_wrap :: Property
prop_wrap = forAll genCols               $ \c ->
            forAll (genTextLongerThan c) $ \t ->
    all (\(T.length -> l) -> l <= c) . wrap c $ t


prop_wrapIndent_wraps :: Property
prop_wrapIndent_wraps = forAll (choose (0, maxCols + 10)) $ \n ->
                        forAll genCols                    $ \c ->
                        forAll (genTextLongerThan c)      $ \t ->
    all (\(T.length -> l) -> l <= c) . wrapIndent n c $ t


prop_wrapIndent_indents :: Property
prop_wrapIndent_indents = forAll (choose (0, maxCols + 10)) $ \n ->
                          forAll genCols                    $ \c ->
                          forAll (genTextLongerThan c)      $ \t ->
    let res = wrapIndent n c t
    in resIsIndented (adjustIndent n c) res


resIsIndented :: Int -> [T.Text] -> Bool
resIsIndented n (t:wrapped) = (not . T.null $ t) && all lineIsIndented wrapped
  where
    lineIsIndented (T.splitAt n -> (indent, rest)) = T.all isSpace indent && (not . T.null $ rest)
resIsIndented _ ls = patternMatchFail "resIsIndented" ls


prop_xformLeading :: Char -> Char -> Property
prop_xformLeading a b = forAll (choose (0, 10))           $ \numOfLeading ->
                        forAll (genTextOfRandLen (0, 10)) $ \rest ->
                        (T.null . T.takeWhile (== a) $ rest) ==>
    let leading    = T.replicate numOfLeading . T.singleton $ a
        t          = leading <> rest
        res        = xformLeading a b t
        resLeading = T.take numOfLeading res
    in T.length res == T.length t &&
       T.all (== b) resLeading &&
       T.drop numOfLeading res == rest


prop_wrapLineWithIndentTag :: Property
prop_wrapLineWithIndentTag = forAll genCols                       $ \c ->
                             forAll (genTextOfRandLen (0, c * 2)) $ \t ->
                             forAll (choose (1, maxCols + 10))    $ \n ->
                             T.null t || (not . isDigit . T.last $ t) ==>
    let res = wrapLineWithIndentTag c $ t <> showText n `T.snoc` indentTagChar
    in if T.length t <= c
      then res == [t]
      else resIsIndented (adjustIndent n c) res


prop_calcIndent :: Property
prop_calcIndent = forAll (genTextOfRandLen (0, 10)) $ \firstWord ->
                  forAll (choose (1, 15))           $ \numOfFollowingSpcs ->
                  forAll (genTextOfRandLen (0, 10)) $ \rest ->
                  T.all (not . isSpace) firstWord &&
                  (T.null . T.takeWhile isSpace $ rest) ==>
    let t = firstWord <> T.replicate numOfFollowingSpcs " " <> rest
    in calcIndent t == T.length firstWord + numOfFollowingSpcs
