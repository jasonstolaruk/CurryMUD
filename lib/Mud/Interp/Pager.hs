{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Pager ( interpPager
                        , sendPagerPrompt ) where

import Mud.ANSI
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.State
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Pla
import Mud.TopLvlDefs.Misc
import Mud.Util.Misc hiding (patternMatchFail)
import Mud.Util.Quoting
import Mud.Util.Wrapping
import qualified Mud.Util.Misc as U (patternMatchFail)

import Control.Lens (both, over)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Data.Text as T


patternMatchFail :: T.Text -> [T.Text] -> a
patternMatchFail = U.patternMatchFail "Mud.Interp.Pager"


-- ==================================================


type PageLen      = Int
type EntireTxtLen = Int


interpPager :: PageLen -> EntireTxtLen -> ([T.Text], [T.Text]) -> Interp
interpPager pageLen txtLen (left, right) (T.toLower -> cn) (NoArgs i mq cols) =
    case cn of
      ""  -> next
      "b" -> prev
      "d" -> next
      "f" -> next
      "n" -> next
      "p" -> prev
      "q" -> (prompt mq . nl' $ dfltPrompt) >> (void . modifyPla i interp $ Nothing)
      "u" -> prev
      _   -> promptRetry mq cols
  where
    next = if length right + 3 <= pageLen
      then do
          send mq . nl . T.unlines $ right
          prompt mq dfltPrompt
          void . modifyPla i interp $ Nothing
      else let (page, right') = splitAt (pageLen - 2) right in do
          send mq . T.unlines $ page
          sendPagerPrompt mq (length left + pageLen - 2) txtLen
          void . modifyPla i interp . Just $ interpPager pageLen txtLen (left ++ page, right')
    prev | length left == pageLen - 2 = do
             send mq . T.unlines $ left
             sendPagerPrompt mq (pageLen - 2) txtLen
         | otherwise = let (reverse -> currPage, left') = splitAt (pageLen - 2) . reverse $ left
                           (prevPage, left'') = over both reverse . splitAt (pageLen - 2) $ left'
                       in do
                           send mq . T.unlines $ prevPage
                           sendPagerPrompt mq (length left'' + pageLen - 2) txtLen
                           void . modifyPla i interp . Just $ interpPager pageLen txtLen ( left'' ++ prevPage
                                                                                         , currPage ++ right )
interpPager _       _      _   _  (WithArgs _ mq cols _) = promptRetry mq cols
interpPager pageLen txtLen txt cn p                      = patternMatchFail "interpPager" [ showText pageLen
                                                                                          , showText txtLen
                                                                                          , showText txt
                                                                                          , cn
                                                                                          , showText p ]


sendPagerPrompt :: MsgQueue -> PageLen -> EntireTxtLen -> MudStack ()
sendPagerPrompt mq pageLen txtLen =
    prompt mq . T.concat $ [ pagerPromptColor
                           , " [ "
                           , showText pageLen
                           , " of "
                           , showText txtLen
                           , " lines ("
                           , let txtLen'  = realToFrac txtLen
                                 pageLen' = realToFrac pageLen
                                 (l, r)   = T.breakOn "." . showText $ pageLen' / txtLen' * 100
                             in l <> T.take 2 r
                           , "%) ] "
                           , dfltColor ]


promptRetry :: MsgQueue -> Cols -> MudStack ()
promptRetry mq cols = send mq . wrapUnlines cols $ p
  where
    p = T.concat [ "Enter a blank line or "
                 , dblQuote "n"
                 , " for the next page, "
                 , dblQuote "b"
                 , " for the previous page, or "
                 , dblQuote "q"
                 , " to stop reading." ]
