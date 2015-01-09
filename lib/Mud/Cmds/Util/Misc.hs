{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, ParallelListComp, PatternSynonyms, ViewPatterns #-}

module Mud.Cmds.Util.Misc ( advise
                          , dispCmdList
                          , fileIOExHandler
                          , grep
                          , pager
                          , prefixCmd
                          , sendGenericErrorMsg
                          , withoutArgs ) where

import Mud.ANSI
import Mud.Cmds.Util.Abbrev
import Mud.Data.Misc
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.Interp.Pager
import Mud.TopLvlDefs.Misc
import Mud.TopLvlDefs.Msgs
import Mud.Util.ANSI
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Padding
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Logging as L (logIOEx)
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Exception (IOException)
import Control.Exception.Lifted (throwIO)
import Control.Monad (void)
import Data.List (intercalate)
import Data.Monoid ((<>))
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)
import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-----


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util.Misc"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Util.Misc"


-- ==================================================


advise :: ActionParams -> [HelpName] -> T.Text -> MudStack ()
advise (Advising mq cols) []  msg = wrapSend mq cols msg
advise (Advising mq cols) [h] msg =
    let msgs = [ msg, T.concat [ "For more information, type "
                               , quoteColor
                               , dblQuote $ "help " <> h
                               , dfltColor
                               , "." ] ]
    in multiWrapSend mq cols msgs
advise (Advising mq cols) hs  msg =
    let msgs = [ msg, "For more information, see the following help articles: " <> helpTopics <> "." ]
    in multiWrapSend mq cols msgs
  where
    helpTopics = dblQuote . T.intercalate (dblQuote ", ") $ hs
advise p hs msg = patternMatchFail "advise" [ showText p, showText hs, msg ]


-----


dispCmdList :: [Cmd] -> Action
dispCmdList cmds (NoArgs   i mq cols) =
    pager i mq . concatMap (wrapIndent (succ maxCmdLen) cols) . mkCmdListText $ cmds
dispCmdList cmds (LowerNub i mq cols as)
  | cmdListTxt                       <- mkCmdListText cmds
  , (filter (not . null) -> matches) <- [ grep a cmdListTxt | a <- as ] = if null matches
    then wrapSend mq cols "No matches found."
    else pager i mq . concatMap (wrapIndent (succ maxCmdLen) cols) . intercalate [""] $ matches
dispCmdList _ p = patternMatchFail "dispCmdList" [ showText p ]


mkCmdListText :: [Cmd] -> [T.Text]
mkCmdListText cmds = let (styleAbbrevs Don'tBracket -> cmdNames) = [ cmdName cmd | cmd <- cmds ]
                         cmdDescs                                = [ cmdDesc cmd | cmd <- cmds ]
                     in  [ pad (succ maxCmdLen) n <> d | n <- cmdNames | d <- cmdDescs ]


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


-- TODO: Since we're lowering here, we only need to nub caller-side...
grep :: T.Text -> [T.Text] -> [T.Text]
grep (T.toLower -> needle) haystack = let haystack' = zip haystack [ T.toLower . dropANSI $ hay | hay <- haystack ]
                                      in [ fst match | match <- haystack', needle `T.isInfixOf` snd match ]


-----


pager :: Id -> MsgQueue -> [T.Text] -> MudStack ()
pager i mq txt@(length -> txtLen) = getPlaPageLines i >>= \pageLen ->
    if txtLen + 3 <= pageLen
      then send mq . nl . T.unlines $ txt
      else let (page, rest) = splitAt (pageLen - 2) txt in do
        send mq . T.unlines $ page
        sendPagerPrompt mq (pageLen - 2) txtLen
        void . modifyPla i interp . Just $ interpPager pageLen txtLen (page, rest)


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
