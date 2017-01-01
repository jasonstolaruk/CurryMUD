{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Mud.Cmds.Util.Abbrev (styleAbbrevs) where

import Mud.Data.Misc
import Mud.Misc.ANSI
import Mud.Util.List (nubSort)
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Arrow ((&&&), first)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Abbrev"


-- ==================================================


type FullWord = Text


styleAbbrevs :: HasCallStack => ShouldQuote -> [FullWord] -> [Text]
styleAbbrevs sq fws = let abbrevs   = mkAbbrevs fws
                          helper fw = f . maybe fw (uncurry (<>)) . lookup fw $ abbrevs
                          f         = onTrue (sq == DoQuote) bracketQuote . quoteWith' (abbrevColor, dfltColor')
                      in map helper fws


type Abbrev         = Text
type Rest           = Text
type PrevWordInList = Text


mkAbbrevs :: HasCallStack => [FullWord] -> [(FullWord, (Abbrev, Rest))]
mkAbbrevs = helper "" . nubSort
  where
    helper :: HasCallStack => PrevWordInList -> [FullWord] -> [(FullWord, (Abbrev, Rest))]
    helper _    []     = []
    helper ""   (x:xs) = (id &&& first T.singleton . headTail) x : helper x xs
    helper prev (x:xs) = let abbrev = calcAbbrev x prev
                         in (: helper x xs) . (x, ) $ case abbrev `T.stripPrefix` x of Nothing   -> (x,      ""  )
                                                                                       Just rest -> (abbrev, rest)


calcAbbrev :: HasCallStack => Text -> Text -> Text
calcAbbrev (T.uncons -> Just (x, _ )) ""                                  = T.singleton x
calcAbbrev (T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys)) | x == y = T.cons      x (calcAbbrev xs ys)
                                                                 | x /= y = T.singleton x
calcAbbrev x                          y                                   = patternMatchFail "calcAbbrev" . showText $ (x, y)
