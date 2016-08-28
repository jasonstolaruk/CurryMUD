{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Mud.Misc.Misc ( BothGramNos
                     , mkCapsFun
                     , mkCoinsMsgs
                     , mkPlurFromBoth
                     , parseWrapHelper
                     , pluralize
                     , procRulesMsg
                     , raceToLang
                     , renderLiqNoun
                     , renderNoun
                     , withLock ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.Operators
import Mud.Util.Text
import Mud.Util.Token
import Mud.Util.Wrapping

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Exception.Lifted (bracket)
import Control.Lens (to)
import Control.Lens.Operators ((^.))
import Data.Maybe (catMaybes)
import Data.Monoid (Sum(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T


mkCapsFun :: ShouldCap -> Text -> Text
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


parseWrapHelper :: Cols -> Text -> [Text]
parseWrapHelper cols = concat . wrapLines cols . T.lines . parseTokens


-----


pluralize :: BothGramNos -> Int -> Text
pluralize (s, p) x = x == 1 ? s :? p


-----


procRulesMsg :: Cols -> [Text]
procRulesMsg cols = map xformLeadingSpaceChars . parseWrapHelper cols $ rulesMsg


-----


raceToLang :: Race -> Lang
raceToLang Dwarf     = DwarfLang
raceToLang Elf       = ElfLang
raceToLang Felinoid  = FelinoidLang
raceToLang Hobbit    = HobbitLang
raceToLang Human     = HumanLang
raceToLang Lagomorph = LagomorphLang
raceToLang Nymph     = NymphLang
raceToLang Vulpenoid = VulpenoidLang


-----


renderLiqNoun :: Liq -> (Text -> Text) -> Text
renderLiqNoun l f = l^.liqNoun.to (renderNoun f)


-----


renderNoun :: (Text -> Text) -> Noun -> Text
renderNoun _ (Don'tArticle t) = t
renderNoun f (DoArticle    t) = f t


-----


withLock :: Lock -> IO () -> IO ()
withLock l f = bracket (atomically . takeTMVar $ l)
                       (\Done -> atomically . putTMVar l $ Done)
                       (\Done -> f)
