{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module MudTests.UtilTests where

import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import MudTests.TestHelpers
import qualified Mud.Util as U (patternMatchFail)

import Control.Applicative ((<$>), (<*>))
import Control.Lens (_1, _2, both, over, to)
import Control.Lens.Operators ((^.))
import Data.Char (chr, isDigit, isSpace)
import Data.List (elemIndices, group, sort)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers (NonEmptyList(..), NonNegative(..))
import Test.Tasty.QuickCheck ((==>), Property, choose, forAll)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "UtilTests"


-- ==================================================


prop_wordWrap :: Property
prop_wordWrap = forAll genCols               $ \c ->
                forAll (genTextLongerThan c) $ \t ->
    all (\(T.length -> l) -> l <= c) . wordWrap c $ t


prop_wordWrapIndent_wraps :: Property
prop_wordWrapIndent_wraps = forAll (choose (0, maxCols + 10)) $ \n ->
                            forAll genCols                    $ \c ->
                            forAll (genTextLongerThan c)      $ \t ->
    all (\(T.length -> l) -> l <= c) . wordWrapIndent n c $ t


prop_wordWrapIndent_indents :: Property
prop_wordWrapIndent_indents = forAll (choose (0, maxCols + 10)) $ \n ->
                              forAll genCols                    $ \c ->
                              forAll (genTextLongerThan c)      $ \t ->
    let res = wordWrapIndent n c t
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
    let res = wrapLineWithIndentTag c $ t <> showText n <> T.singleton indentTagChar
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


prop_aOrAn :: T.Text -> Property
prop_aOrAn t = (not . T.null . T.strip $ t) ==>
  let (a, b) = T.break isSpace . aOrAn $ t
  in a == if isVowel . T.head . T.tail $ b then "an" else "a"


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


prop_findFullNameForAbbrev_findsNothing :: NonEmptyList Char -> [T.Text] -> Property
prop_findFullNameForAbbrev_findsNothing (NonEmpty (T.pack -> needle)) hay = any (not . T.null) hay &&
                                                                            all (not . (needle `T.isInfixOf`)) hay ==>
  isNothing . findFullNameForAbbrev needle $ hay


prop_findFullNameForAbbrev_findsMatch :: NonEmptyList Char -> [T.Text] -> Property
prop_findFullNameForAbbrev_findsMatch (NonEmpty (T.pack -> needle)) hay = any (not . T.null) hay &&
                                                                          all (not . (needle `T.isInfixOf`)) hay ==>
  let nonNull = head . filter (not . T.null) $ hay
      match   = needle <> nonNull
      hay'    = match : hay
  in findFullNameForAbbrev needle hay' == Just match


prop_countOcc :: Int -> [Int] -> Bool
prop_countOcc needle hay = countOcc needle hay == matches
  where
    matches = length . elemIndices needle $ hay


prop_mkCountList :: [Int] -> Bool
prop_mkCountList xs = mkCountList xs == mkCountList' xs
  where
    mkCountList' xs'@(group . sort -> grouped) | ((`map` grouped) -> elemCountList) <- (,) <$> head <*> length =
      let getCountForElem x = (head . filter (^._1.to (== x)) $ elemCountList)^._2
      in map getCountForElem xs'


prop_deleteFirstOfEach :: [Int] -> [Int] -> Property
prop_deleteFirstOfEach delThese fromThis = all (\x -> countOcc x fromThis < 2) delThese ==>
  let res = deleteFirstOfEach delThese fromThis
  in all (`notElem` res) delThese


-- --------------------------------------------------


test_stripControl :: T.Text
test_stripControl = stripControl . quoteWith controlCodes $ "test"
  where
    controlCodes = T.pack $ [ '\0' .. '\31' ] ++ [ '\127' .. (maxBound :: Char) ]


telnetCodes :: T.Text
telnetCodes = T.pack . map chr $ [ 255, 252, 3, 255, 250, 201, 67, 111, 114, 101, 46, 83, 117, 112, 112, 111, 114, 116, 115, 46, 83, 101, 116, 32, 91, 93, 255, 240 ]


test_stripTelnet_null :: T.Text
test_stripTelnet_null = stripTelnet ""


test_stripTelnet_telnetCodes :: T.Text
test_stripTelnet_telnetCodes = stripTelnet telnetCodes


test_stripTelnet_leading :: T.Text
test_stripTelnet_leading = stripTelnet $ telnetCodes <> "test"


test_stripTelnet_trailing :: T.Text
test_stripTelnet_trailing = stripTelnet $ "test" <> telnetCodes


test_stripTelnet_leadingAndTrailing :: T.Text
test_stripTelnet_leadingAndTrailing = stripTelnet $ quoteWith telnetCodes "test"


test_stripTelnet_intercalated :: T.Text
test_stripTelnet_intercalated = stripTelnet $ T.intercalate telnetCodes [ "test1", "test2", "test3", "test4", "test5" ]


test_stripTelnet_malformed1 :: T.Text
test_stripTelnet_malformed1 = stripTelnet . T.singleton $ telnetIAC


test_stripTelnet_malformed2 :: T.Text
test_stripTelnet_malformed2 = stripTelnet . T.pack $ [ telnetIAC, telnetSB, 'a' ]


test_stripTelnet_malformed3 :: T.Text
test_stripTelnet_malformed3 = stripTelnet . T.pack $ telnetIAC : telnetSB : "test"


test_stripTelnet_malformed4 :: T.Text
test_stripTelnet_malformed4 = stripTelnet $ "test" <> T.singleton telnetIAC


test_stripTelnet_malformed5 :: T.Text
test_stripTelnet_malformed5 = stripTelnet $ "test" <> T.pack [ telnetIAC, telnetSB, 'a' ]


test_stripTelnet_malformed6 :: T.Text
test_stripTelnet_malformed6 = stripTelnet $ "test" <> T.pack (telnetIAC : telnetSB : "TEST")
