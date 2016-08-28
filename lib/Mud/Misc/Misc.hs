{-# LANGUAGE LambdaCase, OverloadedStrings #-}

-- TODO: Move anything else to this module?
-- TODO: Sort exports.
module Mud.Misc.Misc ( BothGramNos
                     , dummy -- TODO
                     , mkCapsFun
                     , mkCoinsMsgs
                     , mkPlurFromBoth
                     , pluralize
                     , procRulesMsg
                     , raceToLang
                     , renderNoun
                     , renderLiqNoun
                     , withLock ) where

import Mud.Cmds.Msgs.Misc
import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Operators
import Mud.Util.Text
import Mud.Util.Token
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Exception.Lifted (bracket)
import Control.Lens (to) -- TODO (_1, _2, at, both, over, to, view, views)
import Control.Lens.Operators ((^.)) -- TODO ((%~), (&), (.~), (^.))
import Data.Maybe (catMaybes)
import Data.Monoid (Sum(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Misc.Misc"


-- ==================================================


procRulesMsg :: Cols -> [Text] -- TODO: Pull out.
procRulesMsg cols = map xformLeadingSpaceChars . concat . wrapLines cols . T.lines . parseTokens $ rulesMsg


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


mkCapsFun :: ShouldCap -> Text -> Text
mkCapsFun = \case DoCap    -> capitalize
                  Don'tCap -> id


-----


pluralize :: BothGramNos -> Int -> Text
pluralize (s, p) x = x == 1 ? s :? p


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


renderNoun :: (Text -> Text) -> Noun -> Text
renderNoun _ (Don'tArticle t) = t
renderNoun f (DoArticle    t) = f t


renderLiqNoun :: Liq -> (Text -> Text) -> Text
renderLiqNoun l f = l^.liqNoun.to (renderNoun f)


-----


withLock :: Lock -> IO () -> IO ()
withLock l f = bracket (atomically . takeTMVar $ l)
                       (\Done -> atomically . putTMVar l $ Done)
                       (\Done -> f)


-----


dummy :: a -- TODO
dummy = patternMatchFail "dummy" [("dummy" :: Text)]
