{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Cmds.Util.Abbrev (styleAbbrevs) where

import           Mud.Data.Misc
import           Mud.Misc.ANSI
import           Mud.TopLvlDefs.Misc
import           Mud.Util.List (nubSort)
import qualified Mud.Util.Misc as U (pmf)
import           Mud.Util.Misc hiding (pmf)
import           Mud.Util.Quoting
import           Mud.Util.Text

import           Control.Arrow ((&&&), first)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)
import qualified Data.Text as T

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Cmds.Util.Abbrev"

-- ==================================================

type FullWord = Text

styleAbbrevs :: HasCallStack => DoOrDon'tCoins -> DoOrDon'tQuote -> [FullWord] -> [Text]
styleAbbrevs coins quote fws = let abbrevs = mkAbbrevs coins fws
                                   f fw    = onTrue (quote == DoQuote) bracketQuote . maybe fw g . lookup fw $ abbrevs
                                   g       = uncurry (<>) . first (quoteWith' (abbrevColor, dfltColor'))
                               in map f fws

type Abbrev         = Text
type Rest           = Text
type PrevWordInList = Text

mkAbbrevs :: HasCallStack => DoOrDon'tCoins -> [FullWord] -> [(FullWord, (Abbrev, Rest))]
mkAbbrevs coins = helper "" . nubSort
  where
    helper :: HasCallStack => PrevWordInList -> [FullWord] -> [(FullWord, (Abbrev, Rest))]
    helper _      []     = []
    helper ""     (x:xs) = (id &&& first T.singleton . headTail) x : helper x xs
    helper prev a@(x:xs) | coins == DoCoins, abbrev `elem` coinNames = helper abbrev a
                         | pair <- case abbrev `T.stripPrefix` x of Nothing   -> (x,      ""  )
                                                                    Just rest -> (abbrev, rest) = (x, pair) : helper x xs
      where
        abbrev = calcAbbrev x prev

calcAbbrev :: HasCallStack => Text -> Text -> Text
calcAbbrev (T.uncons -> Just (x, _ )) ""                                  = T.singleton x
calcAbbrev (T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys)) | x == y = T.cons      x (calcAbbrev xs ys)
                                                                 | x /= y = T.singleton x
calcAbbrev x                          y                                   = pmf "calcAbbrev" (x, y)
