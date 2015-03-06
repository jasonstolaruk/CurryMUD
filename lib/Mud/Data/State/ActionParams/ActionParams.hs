{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, RebindableSyntax, RecordWildCards, ViewPatterns #-}

module Mud.Data.State.ActionParams.ActionParams ( ActionParams(..)
                                                , Cols
                                                , pattern AdviseNoArgs
                                                , pattern AdviseOneArg
                                                , pattern Advising
                                                , pattern Ignoring
                                                , pattern Lower
                                                , pattern Lower'
                                                , pattern LowerNub
                                                , pattern LowerNub'
                                                , pattern Msg
                                                , pattern MsgWithTarget
                                                , pattern NoArgs
                                                , pattern NoArgs'
                                                , pattern NoArgs''
                                                , pattern OneArg
                                                , pattern OneArg'
                                                , pattern OneArgNubbed
                                                , pattern WithArgs ) where

import Mud.Data.State.ActionParams.Util
import Mud.Data.State.MsgQueue
import Mud.Util.Quoting

import Data.List (nub)
import Data.String (fromString)
import Formatting ((%), sformat)
import Formatting.Formatters (string)
import Prelude hiding ((>>))
import qualified Data.Text as T


type Id   = Int
type Cols = Int


data ActionParams = ActionParams { plaId       :: Id
                                 , plaMsgQueue :: MsgQueue
                                 , plaCols     :: Cols
                                 , args        :: Args }


instance Show ActionParams where
  show ActionParams { .. } = showIt (show plaId) (show plaCols) (show args)
    where
      showIt i cols = T.unpack . sformat m i cols
      m = do
          "ActionParams {plaId = "
          ", plaMsgQueue = elided, plaCols = "
          ", args = "
          "}"
      a >> b = a % string % b


-- ==================================================
-- Patterns matching type "ActionParams":


pattern AdviseNoArgs <- NoArgs' _ _


pattern AdviseOneArg a <- WithArgs _ _ _ [a]


pattern Advising mq cols <- WithArgs _ mq cols _


pattern Ignoring mq cols as <- WithArgs _ mq cols (dblQuote . T.unwords -> as)


pattern Lower i mq cols as <- WithArgs i mq cols (map T.toLower -> as)


pattern Lower' i as <- Lower i _ _ as


pattern LowerNub i mq cols as <- WithArgs i mq cols (nub . map T.toLower -> as)


pattern LowerNub' i as <- LowerNub i _ _ as


pattern Msg i mq msg <- WithArgs i mq _ (formatMsgArgs -> msg)


pattern MsgWithTarget i mq cols target msg <- WithArgs i mq cols (formatMsgWithTargetArgs -> (target, msg))


pattern NoArgs i mq cols = WithArgs i mq cols []


pattern NoArgs' i mq <- NoArgs i mq _


pattern NoArgs'' i <- NoArgs' i _


pattern OneArg i mq cols a <- WithArgs i mq cols [(T.toLower -> a)]


pattern OneArg' i a <- OneArg i _ _ a


pattern OneArgNubbed i mq cols a <- WithArgs i mq cols (nub . map T.toLower -> [a])


pattern WithArgs i mq cols as = ActionParams { plaId       = i
                                             , plaMsgQueue = mq
                                             , plaCols     = cols
                                             , args        = as }
