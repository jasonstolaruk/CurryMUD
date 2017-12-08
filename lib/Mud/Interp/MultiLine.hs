{-# LANGUAGE OverloadedStrings #-}

module Mud.Interp.MultiLine (interpMutliLine) where

import           Mud.Data.State.ActionParams.ActionParams
import           Mud.Data.State.MudData
import           Mud.Data.State.Util.Misc
import           Mud.TopLvlDefs.Chars
import           Mud.Util.Misc (PatternMatchFail)
import qualified Mud.Util.Misc as U (pmf)

import           Data.Text (Text)
import qualified Data.Text as T

pmf :: PatternMatchFail
pmf = U.pmf "Mud.Interp.MultiLine"

-- ==================================================

interpMutliLine :: ([Text] -> MudStack ()) -> [Text] -> Interp
interpMutliLine f ts cn (NoArgs'' _       ) | cn == T.singleton multiLineEndChar = f ts
interpMutliLine f ts cn (WithArgs i _ _ as) = nextLine f i $ ts ++ pure (T.unwords $ cn : as)
interpMutliLine _ _  _  p                   = pmf "interpMutliLine" p

nextLine :: ([Text] -> MudStack ()) -> Id -> [Text] -> MudStack ()
nextLine f i = setInterp i . Just . interpMutliLine f
