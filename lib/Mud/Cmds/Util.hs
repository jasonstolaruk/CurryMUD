{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Util ( HelpTopic
                     , advise
                     , dispCmdList
                     , prefixCmd
                     , sendGenericErrorMsg
                     , withoutArgs ) where

import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Util as U (patternMatchFail)

import Data.List (foldl', intercalate, sort)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util"


-- ==================================================


type HelpTopic = T.Text


advise :: ActionParams -> [HelpTopic] -> T.Text -> MudStack ()
advise (Advising mq cols) []  msg = wrapSend mq cols msg
advise (Advising mq cols) [h] msg
  | msgs <- [ msg, "For more information, type " <> (dblQuote . ("help " <>) $ h) <> "." ] = multiWrapSend mq cols msgs
advise (Advising mq cols) hs  msg
  | msgs <- [ msg, "See also the following help topics: " <> helpTopics <> "." ]           = multiWrapSend mq cols msgs
  where
    helpTopics = dblQuote . T.intercalate (dblQuote ", ") $ hs
advise p hs msg = patternMatchFail "advise" [ showText p, showText hs, msg ]


-----


dispCmdList :: [Cmd] -> Action
dispCmdList cmds (NoArgs   _ mq cols) =
    send mq . nl . T.unlines . concatMap (wordWrapIndent (maxCmdLen + 1) cols) . mkCmdListText $ cmds
dispCmdList cmds (LowerNub _ mq cols as) | matches <- [ grepTextList a . mkCmdListText $ cmds | a <- as ] =
    send mq . nl . T.unlines . concatMap (wordWrapIndent (maxCmdLen + 1) cols) . intercalate [""] $ matches
dispCmdList _ p = patternMatchFail "dispCmdList" [ showText p ]


mkCmdListText :: [Cmd] -> [T.Text]
mkCmdListText = sort . T.lines . T.concat . foldl' helper []
  where
    helper acc Cmd { .. } | cmdTxt <- nl $ padOrTrunc (maxCmdLen + 1) cmdName <> cmdDesc = cmdTxt : acc


-----


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd (T.singleton -> prefix) cn = prefix <> cn


-----


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = wrapSend mq cols genericErrorMsg


-----


withoutArgs :: Action -> ActionParams -> MudStack ()
withoutArgs act p = ignore p >> act p { args = [] }


ignore :: Action
ignore (Ignoring mq cols as) = send mq . wrapUnlines cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" [ showText p ]
