{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MudTests.Util.Wrapping where

import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Operators
import Mud.Util.Text
import Mud.Util.Wrapping
import MudTests.TestUtil
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Char (isDigit, isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty.QuickCheck ((==>), Property, choose, forAll)


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "MudTests.Util.Wrapping"


-- ==================================================


prop_wrap :: Property
prop_wrap = forAll genCols               $ \c ->
            forAll (genTextLongerThan c) $ \t ->
    all ((<= c) . T.length) . wrap c $ t


prop_wrapIndent_wraps :: Property
prop_wrapIndent_wraps = forAll (choose (0, maxCols + 10)) $ \n ->
                        forAll genCols                    $ \c ->
                        forAll (genTextLongerThan c)      $ \t ->
    all ((<= c) . T.length) . wrapIndent n c $ t


prop_wrapIndent_indents :: Property
prop_wrapIndent_indents = forAll (choose (0, maxCols + 10)) $ \n ->
                          forAll genCols                    $ \c ->
                          forAll (genTextLongerThan c)      $ \t ->
    let res = wrapIndent n c t
    in resIsIndented (adjustIndent n c) res


resIsIndented :: Int -> [Text] -> Bool
resIsIndented n (t:wrapped) = ()!# t && all lineIsIndented wrapped
  where
    lineIsIndented (T.splitAt n -> (indent, rest)) = T.all isSpace indent && ()!# rest
resIsIndented _ xs = patternMatchFail "resIsIndented" . showText $ xs


prop_xformLeading :: Char -> Char -> Property
prop_xformLeading a b = forAll (choose (0, 10))           $ \noOfLeading ->
                        forAll (genTextOfRandLen (0, 10)) $ \rest ->
                        ((()#) . T.takeWhile (== a) $ rest) ==>
    let leading    = T.replicate noOfLeading . T.singleton $ a
        t          = leading <> rest
        res        = xformLeading a b t
        resLeading = T.take noOfLeading res
    in T.length res == T.length t &&
       T.all (== b) resLeading    &&
       T.drop noOfLeading res == rest


prop_wrapLineWithIndentTag :: Property
prop_wrapLineWithIndentTag = forAll genCols                       $ \c ->
                             forAll (genTextOfRandLen (0, c * 2)) $ \t ->
                             forAll (choose (1, maxCols + 10))    $ \n ->
                             ()# t || (not . isDigit . T.last $ t) ==>
    let res = wrapLineWithIndentTag c $ t <> showText n `T.snoc` indentTagChar
    in if T.length t <= c
      then res == pure t
      else resIsIndented (adjustIndent n c) res


prop_calcIndent :: Property
prop_calcIndent = forAll (genTextOfRandLen (0, 10)) $ \firstWord ->
                  forAll (choose (1, 15))           $ \noOfFollowingSpcs ->
                  forAll (genTextOfRandLen (0, 10)) $ \rest ->
                  T.all (not . isSpace) firstWord &&
                  ((()#) . T.takeWhile isSpace $ rest) ==>
    let t = firstWord <> T.replicate noOfFollowingSpcs " " <> rest
    in calcIndent t == T.length firstWord + noOfFollowingSpcs
