{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Mud.Data.State.Util.Coins ( coinsFromList
                                 , coinsToList
                                 , negateCoins ) where

import Mud.Data.State.MudData
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (each)
import Control.Lens.Operators ((%~), (^..))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Coins"


-- ============================================================


coinsFromList :: [Int] -> Coins
coinsFromList [ cop, sil, gol ] = Coins (cop, sil, gol)
coinsFromList xs                = patternMatchFail "coinsFromList" [ showText xs ]


coinsToList :: Coins -> [Int]
coinsToList (Coins c) = c^..each


negateCoins :: Coins -> Coins
negateCoins (Coins (each %~ negate -> c)) = Coins c
