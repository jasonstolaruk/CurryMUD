{-# LANGUAGE OverloadedStrings #-}

module Mud.Interp.MultiLine ( interpMutliLine
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


interpMutliLine :: ([Text] -> MudStack ()) -> [Text] -> Interp
interpMutliLine f ts cn (NoArgs'' _        ) | cn == T.singleton multiLineEndChar = f ts
interpMutliLine f ts cn (WithArgs i mq _ as)                                      = nextLine f i mq $ ts ++ pure t
  where
    t = T.unwords $ cn : as
interpMutliLine _ _ _ p = patternMatchFail "interpMutliLine" . showText $ p


nextLine :: ([Text] -> MudStack ()) -> Id -> MsgQueue -> [Text] -> MudStack ()
nextLine f i mq ts = let next = setInterp i . Just . interpMutliLine f $ ts in promptMultiLine mq >> next


promptMultiLine :: MsgQueue -> MudStack ()
promptMultiLine = flip sendPromptNoNl "> "
