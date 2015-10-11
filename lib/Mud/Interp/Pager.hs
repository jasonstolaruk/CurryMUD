{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

module Mud.Interp.Pager ( interpPager
                        , sendPagerPrompt ) where

import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Data.State.Util.Set
import Mud.Interp.Prompt
import Mud.Misc.ANSI
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text
import Mud.Util.Wrapping

import Control.Arrow (second)
import Control.Lens (both)
import Control.Lens.Operators ((%~), (&))
import Data.Monoid ((<>))
import qualified Data.Text as T


type PageLen      = Int
type EntireTxtLen = Int


interpPager :: PageLen -> EntireTxtLen -> ([T.Text], [T.Text]) -> Interp
interpPager pageLen txtLen (left, right) (T.toLower -> cn) (NoArgs i mq cols) = getState >>= \ms ->
    let next = if length right + 3 <= pageLen
                 then do
                     send mq . nl . T.unlines $ right
                     prompt mq . mkPrompt i $ ms
                     setInterp i Nothing
                 else let (page, right') = splitAt (pageLen - 2) right in do
                     send mq . T.unlines $ page
                     sendPagerPrompt mq (length left + pageLen - 2) txtLen
                     setInterp i . Just $ interpPager pageLen txtLen (left ++ page, right')
    in case cn of ""  -> next
                  "b" -> prev
                  "d" -> next
                  "f" -> next
                  "n" -> next
                  "p" -> prev
                  "q" -> (prompt mq . nlPrefix . mkPrompt i $ ms) >> setInterp i Nothing
                  "u" -> prev
                  _   -> promptRetry mq cols
  where
    prev | length left == pageLen - 2 = (send mq . T.unlines $ left) >> sendPagerPrompt mq (pageLen - 2) txtLen
         | (reverse -> currPage, left') <- splitAt (pageLen - 2) . reverse $ left
         , (prevPage, left'')           <- splitAt (pageLen - 2) left' & both %~ reverse = do
             send mq . T.unlines $ prevPage
             sendPagerPrompt mq (length left'' + pageLen - 2) txtLen
             setInterp i . Just $ interpPager pageLen txtLen (left'' ++ prevPage, currPage ++ right)
interpPager _ _ _ _ (ActionParams { plaMsgQueue, plaCols }) = promptRetry plaMsgQueue plaCols


sendPagerPrompt :: MsgQueue -> PageLen -> EntireTxtLen -> MudStack ()
sendPagerPrompt mq pageLen txtLen =
    prompt mq . T.concat $ [ pagerPromptColor
                           , " [ "
                           , showText pageLen
                           , " of "
                           , showText txtLen
                           , " lines ("
                           , uncurry (<>) . second (T.take 2) . T.breakOn "." . showText $ pageLen `divide` txtLen * 100
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
