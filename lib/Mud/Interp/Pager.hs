{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Pager ( interpPager
                        , sendPagerPrompt ) where

import Mud.ANSI
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Pager"


-- ==================================================


type PageLen      = Int
type EntireTxtLen = Int
type RemainingTxt = [T.Text]


interpPager :: PageLen -> EntireTxtLen -> RemainingTxt -> Interp
interpPager pageLen txtLen rs (T.toLower -> cn) (NoArgs i mq cols) =
    case cn of
      "q" -> (prompt mq . nl' $ dfltPrompt) >> (void . modifyPla i interp $ Nothing)
      ""  -> if length rs + 3 <= pageLen
               then do
                   send mq . nl . T.unlines $ rs
                   prompt mq dfltPrompt
                   void . modifyPla i interp $ Nothing
               else let (T.unlines -> page, rest) = splitAt (pageLen - 2) rs in do
                   send mq page
                   sendPagerPrompt mq (pageLen - 2) txtLen
                   void . modifyPla i interp . Just $ interpPager pageLen txtLen rest
      _   -> promptRetry mq cols
interpPager _ _ _ _ (WithArgs _ mq cols _) = promptRetry mq cols
interpPager pageLen txtLen rs cn p = patternMatchFail "interpPager" [ showText pageLen
                                                                    , showText txtLen
                                                                    , showText rs
                                                                    , cn
                                                                    , showText p ]


sendPagerPrompt :: MsgQueue -> PageLen -> EntireTxtLen -> MudStack ()
sendPagerPrompt mq pageLen txtLen =
    prompt mq . T.concat $ [ pagerPromptColorANSI
                           , " [ "
                           , showText pageLen
                           , " of "
                           , showText txtLen
                           , " lines ("
                           , let txtLen'  = realToFrac txtLen
                                 pageLen' = realToFrac pageLen
                                 (l, r)   = T.break (== '.') . showText $ pageLen' / txtLen' * 100
                             in l <> T.take 2 r
                           , "%) ] "
                           , dfltColorANSI ]


promptRetry :: MsgQueue -> Cols -> MudStack ()
promptRetry mq cols = send mq . wrapUnlines cols $ "Enter a blank line to continue reading, or " <> dblQuote "q" <> " \
                                                   \to stop."
