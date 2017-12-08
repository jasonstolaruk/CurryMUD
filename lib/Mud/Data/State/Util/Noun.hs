module Mud.Data.State.Util.Noun ( renderLiqNoun
                                , renderNoun ) where

import Mud.Data.State.MudData

import Control.Lens (to)
import Control.Lens.Operators ((^.))
import Data.Text (Text)

renderLiqNoun :: Liq -> (Text -> Text) -> Text
renderLiqNoun l f = l^.liqNoun.to (renderNoun f)

renderNoun :: (Text -> Text) -> Noun -> Text
renderNoun _ (Don'tArticle t) = t
renderNoun f (DoArticle    t) = f t
