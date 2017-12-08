{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Misc.Misc ( BothGramNos
                     , dropSynonyms
                     , mkCapsFun
                     , mkCoinsMsgs
                     , mkPlurFromBoth
                     , parseWrap
                     , parseWrapXform
                     , pluralize ) where

import           Mud.Data.Misc
import           Mud.Data.State.MudData
import           Mud.Util.Operators
import           Mud.Util.Text
import           Mud.Util.Token
import           Mud.Util.Wrapping

import           Data.Maybe (catMaybes)
import           Data.Monoid (Sum(..), (<>))
import           Data.Text (Text)
import qualified Data.Text as T

dropSynonyms :: [Text] -> [Text] -> [Text]
dropSynonyms _        []                         = []
dropSynonyms synonyms (x:xs) | x `elem` synonyms = x : filter (`notElem` synonyms) xs
                             | otherwise         = x : dropSynonyms synonyms xs

-----

mkCapsFun :: DoOrDon'tCap -> Text -> Text
mkCapsFun = \case DoCap    -> capitalize
                  Don'tCap -> id

-----

mkCoinsMsgs :: (Int -> Text -> Text) -> Coins -> [Text]
mkCoinsMsgs f (Coins (cop, sil, gol)) = catMaybes [ c, s, g ]
  where
    c = Sum cop |!| Just . f cop $ "copper piece"
    s = Sum sil |!| Just . f sil $ "silver piece"
    g = Sum gol |!| Just . f gol $ "gold piece"

-----

type BothGramNos = (Sing, Plur)

mkPlurFromBoth :: BothGramNos -> Plur
mkPlurFromBoth (s, "") = s <> "s"
mkPlurFromBoth (_, p ) = p

-----

parseWrap :: ServerSettings -> Cols -> Text -> [Text]
parseWrap s cols = concat . wrapLines cols . T.lines . parseTokens s

parseWrapXform :: ServerSettings -> Cols -> Text -> [Text]
parseWrapXform s cols = map xformLeadingSpaceChars . parseWrap s cols

-----

pluralize :: BothGramNos -> Int -> Text
pluralize (s, p) x = x == 1 ? s :? p
