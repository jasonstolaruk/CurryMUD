{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Util.Misc ( HelpTopic
                          , advise
                          , dispCmdList
                          , fileIOExHandler
                          , prefixCmd
                          , sendGenericErrorMsg
                          , withoutArgs ) where

import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Msgs
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logIOEx)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Exception (IOException)
import Control.Exception.Lifted (throwIO)
import Data.List (foldl', intercalate, sort)
import Data.Monoid ((<>))
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Misc"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Util.Misc"


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
    send mq . nl . T.unlines . concatMap (wrapIndent (succ maxCmdLen) cols) . mkCmdListText $ cmds
dispCmdList cmds (LowerNub _ mq cols as) | matches <- [ grepTextList a . mkCmdListText $ cmds | a <- as ] =
    send mq . nl . T.unlines . concatMap (wrapIndent (succ maxCmdLen) cols) . intercalate [""] $ matches
dispCmdList _ p = patternMatchFail "dispCmdList" [ showText p ]


mkCmdListText :: [Cmd] -> [T.Text]
mkCmdListText = sort . T.lines . T.concat . foldl' helper []
  where
    helper acc Cmd { .. } | cmdTxt <- nl $ padOrTrunc (succ maxCmdLen) cmdName <> cmdDesc = cmdTxt : acc


-----


fileIOExHandler :: T.Text -> IOException -> MudStack ()
fileIOExHandler fn e
  | isAlreadyInUseError e = logIt
  | isDoesNotExistError e = logIt
  | isPermissionError   e = logIt
  | otherwise             = throwIO e
  where
    logIt = logIOEx fn e


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
