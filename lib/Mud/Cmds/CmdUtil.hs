{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.CmdUtil ( HelpTopic
                        , advise
                        , dispCmdList
                        , isNonStdLink
                        , linkDirToCmdName
                        , mkSerializedNonStdDesig
                        , prefixCmd
                        , readFileExHandler
                        , getRecordUptime
                        , sendGenericErrorMsg
                        , withoutArgs ) where

import Control.Exception (IOException)
import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TopLvlDefs
import Mud.Util hiding (patternMatchFail)
import qualified Mud.Logging as L (logIOEx, logIOExRethrow)
import qualified Mud.Util as U (patternMatchFail)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import Control.Exception.Lifted (catch)
import Control.Applicative ((<$>))

import Data.List (foldl', intercalate, sort)
import Data.Monoid ((<>))
import System.IO.Error (isDoesNotExistError, isPermissionError)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.CmdUtil"


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.CmdUtil"


logIOExRethrow :: T.Text -> IOException -> MudStack ()
logIOExRethrow = L.logIOExRethrow "Mud.Cmds.CmdUtil"


-- ==================================================


prefixCmd :: Char -> CmdName -> T.Text
prefixCmd (T.singleton -> prefix) cn = prefix <> cn


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


withoutArgs :: Action -> ActionParams -> MudStack ()
withoutArgs act p = ignore p >> act p { args = [] }


ignore :: Action
ignore (Ignoring mq cols as) = send mq . wrapUnlines cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" [ showText p ]


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


readFileExHandler :: T.Text -> IOException -> MudStack ()
readFileExHandler fn e
  | isDoesNotExistError e = logIOEx        fn e
  | isPermissionError   e = logIOEx        fn e
  | otherwise             = logIOExRethrow fn e


mkSerializedNonStdDesig :: Id -> WorldState -> Sing -> AOrThe -> T.Text
mkSerializedNonStdDesig i ws s (capitalize . pp -> aot) | (pp -> s', pp -> r) <- getSexRace i ws =
    serialize NonStdDesig { nonStdPCEntSing = s
                          , nonStdDesc      = T.concat [ aot, " ", s', " ", r ] }


linkDirToCmdName :: LinkDir -> CmdName
linkDirToCmdName North     = "n"
linkDirToCmdName Northeast = "ne"
linkDirToCmdName East      = "e"
linkDirToCmdName Southeast = "se"
linkDirToCmdName South     = "s"
linkDirToCmdName Southwest = "sw"
linkDirToCmdName West      = "w"
linkDirToCmdName Northwest = "nw"
linkDirToCmdName Up        = "u"
linkDirToCmdName Down      = "d"


isNonStdLink :: RmLink -> Bool
isNonStdLink (NonStdLink {}) = True
isNonStdLink _               = False


getRecordUptime :: MudStack (Maybe Integer)
getRecordUptime = (liftIO . doesFileExist $ uptimeFile) >>= \case
  True  -> liftIO readUptime `catch` (\e -> readFileExHandler "getRecordUptime" e >> return Nothing)
  False -> return Nothing
  where
    readUptime = Just . read <$> readFile uptimeFile


sendGenericErrorMsg :: MsgQueue -> Cols -> MudStack ()
sendGenericErrorMsg mq cols = wrapSend mq cols genericErrorMsg
