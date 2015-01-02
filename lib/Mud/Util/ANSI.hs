{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.ANSI ( colorizeFileTxt
                     , extractANSI
                     , insertANSI ) where

import Mud.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Util.ANSI"


-- ==================================================


colorizeFileTxt :: T.Text -> T.Text -> T.Text
colorizeFileTxt c t | T.last t == '\n' = nl . T.concat $ [ c, T.init t, dfltColorANSI ]
                    | otherwise        = c <> t <> dfltColorANSI


-----


type EscSeq = T.Text


extractANSI :: T.Text -> [(T.Text, EscSeq)]
extractANSI t
  | ansiCSI `notInfixOf` t = [(t, "")]
  | otherwise              =
      let (t',                                    rest)            = T.break (== ansiEsc)          t
          ((`T.snoc` ansiSGRDelimiter) -> escSeq, T.tail -> rest') = T.break (== ansiSGRDelimiter) rest
      in if T.null rest' then [(t', escSeq)] else (t', escSeq) : extractANSI rest'


-----


insertANSI :: [(T.Text, EscSeq)] -> [T.Text] -> [T.Text]
insertANSI [(_, "")] wrapped                                        = wrapped
insertANSI extracted (T.intercalate (T.singleton breakMarker) -> t) =
    T.split (== breakMarker) . loopOverExtractedList extracted $ t


loopOverExtractedList :: [(T.Text, EscSeq)] -> T.Text -> T.Text
loopOverExtractedList []                  ys = ys
loopOverExtractedList [("", escSeq)]      "" = escSeq
loopOverExtractedList ((xs, escSeq):rest) ys
  | T.null xs = escSeq <> loopOverExtractedList rest ys
  | otherwise = let left         = loopOverExtractedTxt xs ys
                    (Just right) = left `T.stripPrefix` ys
                in left <> escSeq <> loopOverExtractedList rest right


loopOverExtractedTxt :: T.Text -> T.Text -> T.Text
loopOverExtractedTxt a@(T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys))
  | x == y            = x            `T.cons` loopOverExtractedTxt xs ys
  | y == indentFiller = indentFiller `T.cons` loopOverExtractedTxt a  ys
  | y == breakMarker  = breakMarker  `T.cons` loopOverExtractedTxt a  ys
loopOverExtractedTxt "" _ = ""
loopOverExtractedTxt a  b = patternMatchFail "loopOverExtractedTxt" [ a, b ]
