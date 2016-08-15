{-# LANGUAGE OverloadedStrings #-}

module Mud.Interp.MultiLine ( multiLineInterp
                            , promptMultiLine ) where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.TopLvlDefs.Chars
import Mud.Util.Misc (PatternMatchFail)
import Mud.Util.Text
import qualified Mud.Util.Misc as U (patternMatchFail)

import Data.Text (Text)
import qualified Data.Text as T


patternMatchFail :: (Show a) => PatternMatchFail a b
patternMatchFail = U.patternMatchFail "Mud.Interp.MultiLine"


-- ==================================================


multiLineInterp :: ([Text] -> MudStack ()) -> [Text] -> Interp
multiLineInterp f ts "" (NoArgs'  i mq     )                                      = nextLine f i mq $ "" : ts
multiLineInterp f ts cn (NoArgs'' _        ) | cn == T.singleton multiLineEndChar = f ts
multiLineInterp f ts cn (WithArgs i mq _ as)                                      = nextLine f i mq $ t  : ts
  where
    t = T.unwords $ cn : as
multiLineInterp _ _ _ p = patternMatchFail "multiLineInterp" . showText $ p


nextLine :: ([Text] -> MudStack ()) -> Id -> MsgQueue -> [Text] -> MudStack ()
nextLine f i mq ts = let next = setInterp i . Just . multiLineInterp f $ ts in anglePrompt mq >> next


promptMultiLine :: MsgQueue -> MudStack ()
promptMultiLine = anglePrompt
