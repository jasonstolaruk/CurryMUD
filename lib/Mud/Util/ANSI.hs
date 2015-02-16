{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Util.ANSI ( colorizeFileTxt
                     , dropANSI
                     , extractANSI
                     , insertANSI ) where

import Mud.ANSI
import Mud.TopLvlDefs.Chars
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Util.ANSI"


-- ==================================================


colorizeFileTxt :: T.Text -> T.Text -> T.Text
colorizeFileTxt c t | T.last t == '\n' = nl . T.concat $ [ c, T.init t, dfltColor ]
                    | otherwise        = c <> t <> dfltColor


-----


dropANSI :: T.Text -> T.Text
dropANSI t | ansiCSI `notInfixOf` t = t
           | otherwise              = let (left, rest)      = T.breakOn   (T.singleton ansiEsc) t
                                          (T.tail -> right) = T.dropWhile (/= ansiSGRDelimiter) rest
                                      in T.null right ? left :? left <> dropANSI right


-----


type EscSeq = T.Text


extractANSI :: T.Text -> [(T.Text, EscSeq)]
extractANSI t | (T.length -> l, rest) <- T.span (== ' ') t
              , l > 0     = helper $ T.replicate l (T.singleton indentFiller) <> rest
              | otherwise = helper t
  where
    helper txt
      | ansiCSI `notInfixOf` txt = [(txt, "")]
      | (txt',                                  rest)            <- T.breakOn (T.singleton ansiEsc)          txt
      , ((`T.snoc` ansiSGRDelimiter) -> escSeq, T.tail -> rest') <- T.breakOn (T.singleton ansiSGRDelimiter) rest
      = T.null rest' ? [(txt', escSeq)] :? (txt', escSeq) : helper rest'


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
  | left         <- loopOverExtractedTxt xs ys
  , (Just right) <- left `T.stripPrefix` ys = left <> escSeq <> loopOverExtractedList rest right
loopOverExtractedList xs ys = patternMatchFail "loopOverExtractedList" [ showText xs, ys ]


loopOverExtractedTxt :: T.Text -> T.Text -> T.Text
loopOverExtractedTxt a@(T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys))
  | x == y            = x            `T.cons` loopOverExtractedTxt xs ys
  | y == indentFiller = indentFiller `T.cons` loopOverExtractedTxt a  ys
  | y == breakMarker  = breakMarker  `T.cons` loopOverExtractedTxt a  ys
loopOverExtractedTxt "" _ = ""
loopOverExtractedTxt a  b = patternMatchFail "loopOverExtractedTxt" [ a, b ]
