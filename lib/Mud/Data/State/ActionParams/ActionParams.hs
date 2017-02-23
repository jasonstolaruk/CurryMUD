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
                                                , pattern Msg'
                                                , pattern MsgWithTarget
                                                , pattern NoArgs
                                                , pattern NoArgs'
                                                , pattern NoArgs''
                                                , pattern Nubbed
                                                , pattern OneArg
                                                , pattern OneArg'
                                                , pattern OneArgLower
                                                , pattern OneArgLower'
                                                , pattern OneArgNubbed
                                                , pattern WithArgs
                                                , pattern WithTarget ) where

import Mud.Data.State.ActionParams.Misc
import Mud.Data.State.MsgQueue
import Mud.Util.List
import Mud.Util.Quoting

import Data.List (nub)
import Data.String (fromString)
import Data.Text (Text)
import Formatting ((%), sformat)
import Formatting.Formatters (string)
import Prelude hiding ((>>))
import qualified Data.Text as T


type Id   = Int
type Cols = Int
type Args = [Text]


data ActionParams = ActionParams { myId        :: Id
                                 , plaMsgQueue :: MsgQueue
                                 , plaCols     :: Cols
                                 , args        :: Args }


instance Show ActionParams where
  show ActionParams { .. } = showIt (show myId) (show plaCols) (show args)
    where
      showIt i cols = T.unpack . sformat m i cols
      m = do
          "ActionParams {myId = "
          ", plaMsgQueue = elided, plaCols = "
          ", args = "
          "}"
      a >> b = a % string % b


-- ==================================================
-- Patterns matching type "ActionParams":


pattern AdviseNoArgs :: ActionParams
pattern AdviseNoArgs <- NoArgs' _ _


pattern AdviseOneArg :: ActionParams
pattern AdviseOneArg <- WithArgs _ _ _ [_]


pattern Advising :: MsgQueue -> Cols -> ActionParams
pattern Advising mq cols <- WithArgs _ mq cols _


pattern Ignoring :: MsgQueue -> Cols -> Text -> ActionParams
pattern Ignoring mq cols as <- WithArgs _ mq cols (dblQuote . T.unwords -> as)


pattern Lower :: Id -> MsgQueue -> Cols -> [Text] -> ActionParams
pattern Lower i mq cols as <- WithArgs i mq cols (map T.toLower -> as)


pattern Lower' :: Id -> [Text] -> ActionParams
pattern Lower' i as <- Lower i _ _ as


pattern LowerNub :: Id -> MsgQueue -> Cols -> [Text] -> ActionParams
pattern LowerNub i mq cols as <- WithArgs i mq cols (nub . map T.toLower -> as)


pattern LowerNub' :: Id -> [Text] -> ActionParams
pattern LowerNub' i as <- LowerNub i _ _ as


pattern Msg :: Id -> MsgQueue -> Cols -> Text -> ActionParams
pattern Msg i mq cols msg <- WithArgs i mq cols (formatMsgArgs -> msg)


pattern Msg' :: Id -> MsgQueue -> Text -> ActionParams
pattern Msg' i mq msg <- Msg i mq _ msg


pattern MsgWithTarget :: Id -> MsgQueue -> Cols -> Text -> Text -> ActionParams
pattern MsgWithTarget i mq cols target msg <- WithArgs i mq cols (formatMsgWithTargetArgs -> (target, msg))


pattern NoArgs :: Id -> MsgQueue -> Cols -> ActionParams
pattern NoArgs i mq cols = WithArgs i mq cols []


pattern NoArgs' :: Id -> MsgQueue -> ActionParams
pattern NoArgs' i mq <- NoArgs i mq _


pattern NoArgs'' :: Id -> ActionParams
pattern NoArgs'' i <- NoArgs' i _


pattern Nubbed :: Id -> MsgQueue -> Cols -> Args -> ActionParams
pattern Nubbed i mq cols as <- WithArgs i mq cols (nub -> as)


pattern OneArg :: Id -> MsgQueue -> Cols -> Text -> ActionParams
pattern OneArg i mq cols a <- WithArgs i mq cols [a]


pattern OneArg' :: Id -> Text -> ActionParams
pattern OneArg' i a <- OneArg i _ _ a


pattern OneArgLower :: Id -> MsgQueue -> Cols -> Text -> ActionParams
pattern OneArgLower i mq cols a <- OneArg i mq cols (T.toLower -> a)


pattern OneArgLower' :: Id -> Text -> ActionParams
pattern OneArgLower' i a <- OneArgLower i _ _ a


pattern OneArgNubbed :: Id -> MsgQueue -> Cols -> Text -> ActionParams
pattern OneArgNubbed i mq cols a <- WithArgs i mq cols (nub . map T.toLower -> [a])


pattern WithArgs :: Id -> MsgQueue -> Cols -> Args -> ActionParams
pattern WithArgs i mq cols as = ActionParams { myId        = i
                                             , plaMsgQueue = mq
                                             , plaCols     = cols
                                             , args        = as }


pattern WithTarget :: Id -> MsgQueue -> Cols -> Text -> Text -> ActionParams
pattern WithTarget i mq cols target rest <- WithArgs i mq cols (headTail -> (target, T.unwords -> rest))
