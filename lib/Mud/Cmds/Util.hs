{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, RecordWildCards, ViewPatterns #-}

module Mud.Cmds.Util ( HelpTopic
                     , advise
                     , dispCmdList
                     , fileIOExHandler
                     , prefixCmd
                     , sendGenericErrorMsg
                     , styleAbbrevs
                     , withoutArgs ) where

import Mud.ANSI
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
import Control.Lens (_1, over)
import Data.List (foldl', intercalate, nub, sort)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Cmds.Util"


-----


logIOEx :: T.Text -> IOException -> MudStack ()
logIOEx = L.logIOEx "Mud.Cmds.Util"


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


type FullWord = T.Text


styleAbbrevs :: [FullWord] -> [FullWord]
styleAbbrevs fws = let abbrevs   = mkAbbrevs fws
                       helper fw = let [(_, (abbrev, rest))] = filter ((fw ==) . fst) abbrevs
                                   in bracketQuote . T.concat $ [ abbrevColorANSI
                                                                , abbrev
                                                                , dfltColorANSI
                                                                , rest ]
                   in map helper fws


type Abbrev         = T.Text
type Rest           = T.Text
type PrevWordInList = T.Text


mkAbbrevs :: [FullWord] -> [(FullWord, (Abbrev, Rest))]
mkAbbrevs = helper "" . sort . nub
  where
    helper :: PrevWordInList -> [FullWord] -> [(FullWord, (Abbrev, Rest))]
    helper _    []       = []
    helper ""   (en:ens) = (en, over _1 T.singleton $ headTail' $ en) : helper en ens
    helper prev (en:ens) = let abbrev = calcAbbrev en prev
                           in (en, (abbrev, fromJust $ abbrev `T.stripPrefix` en)) : helper  en ens


calcAbbrev :: T.Text -> T.Text -> T.Text
calcAbbrev (T.uncons -> Just (x, _ )) ""                                  = T.singleton x
calcAbbrev (T.uncons -> Just (x, xs)) (T.uncons -> Just (y, ys)) | x == y = T.singleton x <> calcAbbrev xs ys
                                                                 | x /= y = T.singleton x
calcAbbrev x                          y                                   = patternMatchFail "calcAbbrev" [ x, y ]


-----


withoutArgs :: Action -> ActionParams -> MudStack ()
withoutArgs act p = ignore p >> act p { args = [] }


ignore :: Action
ignore (Ignoring mq cols as) = send mq . wrapUnlines cols . parensQuote $ "Ignoring " <> as <> "..."
ignore p                     = patternMatchFail "ignore" [ showText p ]
