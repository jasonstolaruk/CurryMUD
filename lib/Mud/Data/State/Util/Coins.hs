{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}

module Mud.Data.State.Util.Coins ( mkCoinsFromList
                                 , mkListFromCoins
                                 , negateCoins ) where

import Mud.Data.State.State
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Control.Lens (each)
import Control.Lens.Operators ((%~))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Data.State.Util.Coins"


-- ============================================================


mkCoinsFromList :: [Int] -> Coins
mkCoinsFromList [ cop, sil, gol ] = Coins (cop, sil, gol)
mkCoinsFromList xs                = patternMatchFail "mkCoinsFromList" [ showText xs ]


mkListFromCoins :: Coins -> [Int]
mkListFromCoins (Coins (c, g, s)) = [ c, g, s ]


negateCoins :: Coins -> Coins
negateCoins (Coins (each %~ negate -> c)) = Coins c
