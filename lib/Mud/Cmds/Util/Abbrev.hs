{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Cmds.Util.Abbrev ( styleAbbrevs ) where

import Mud.ANSI
import Mud.Data.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (_1, over)
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Abbrevs"


-- ==================================================


type FullWord = T.Text


styleAbbrevs :: ShouldBracketQuote -> [FullWord] -> [FullWord]
styleAbbrevs sbq fws = let abbrevs   = mkAbbrevs fws
                           helper fw = let [(_, (abbrev, rest))] = filter ((fw ==) . fst) abbrevs
                                           f                     = case sbq of DoBracket    -> bracketQuote
                                                                               Don'tBracket -> id
                                       in f . T.concat $ [ abbrevColor, abbrev, dfltColor, rest ]
                       in map helper fws


type Abbrev         = T.Text
type Rest           = T.Text
type PrevWordInList = T.Text


mkAbbrevs :: [FullWord] -> [(FullWord, (Abbrev, Rest))]
mkAbbrevs = helper "" . sort . nub
  where
    helper :: PrevWordInList -> [FullWord] -> [(FullWord, (Abbrev, Rest))]
    helper _    []       = []
    helper ""   (en:ens) = (en, over _1 T.singleton . headTail' $ en) : helper en ens
    helper prev (en:ens) = let abbrev = calcAbbrev en prev
                           in (en, (abbrev, fromJust $ abbrev `T.stripPrefix` en)) : helper  en ens


calcAbbrev :: T.Text -> T.Text -> T.Text
calcAbbrev (T.uncons -> Just (x, _ )) ""                                  = T.singleton x
calcAbbrev (T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys)) | x == y = T.singleton x <> calcAbbrev xs ys
                                                                 | x /= y = T.singleton x
calcAbbrev x                          y                                   = patternMatchFail "calcAbbrev" [ x, y ]
