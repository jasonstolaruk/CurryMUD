{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

module Mud.Interp.Pager ( interpPager
                        , sendPagerPrompt ) where

import Mud.Cmds.Msgs.Sorry
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MsgQueue
import Mud.Data.State.MudData
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Interp.Misc
import Mud.Misc.ANSI
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Control.Arrow (second)
import Control.Lens (both)
import Control.Lens.Operators ((%~), (&))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T


type PageLen      = Int
type EntireTxtLen = Int


interpPager :: Maybe Fun -> PageLen -> EntireTxtLen -> ([Text], [Text]) -> Interp
interpPager mf pageLen txtLen (left, right) (T.toLower -> cn) (NoArgs i mq cols) = getState >>= \ms ->
    let next = if length right + 3 <= pageLen
                 then do send mq . nl . T.unlines $ right
                         fromMaybe (sendDfltPrompt mq i >> setInterp i Nothing) mf
                 else let (page, right') = splitAt (pageLen - 2) right in do
                     send mq . T.unlines $ page
                     sendPagerPrompt mq (length left + pageLen - 2) txtLen
                     setInterp i . Just . interpPager mf pageLen txtLen $ (left ++ page, right')
    in case cn of ""  -> next
                  "b" -> prev
                  "n" -> next
                  "q" -> fromMaybe (sequence_ [ sendPrompt mq . nlPrefix . mkDfltPrompt i $ ms, resetInterp i]) mf
                  "u" -> prev
                  _   -> promptRetry mq cols
  where
    prev | length left == pageLen - 2 = sequence_ [ send mq . T.unlines $ left, sendPagerPrompt mq (pageLen - 2) txtLen ]
         | (reverse -> currPage, left') <- splitAt (pageLen - 2) . reverse $ left
         , (prevPage, left'')           <- splitAt (pageLen - 2) left' & both %~ reverse = do
             send mq . T.unlines $ prevPage
             sendPagerPrompt mq (length left'' + pageLen - 2) txtLen
             setInterp i . Just . interpPager mf pageLen txtLen $ (left'' ++ prevPage, currPage ++ right)
interpPager _ _ _ _ _ ActionParams { plaMsgQueue, plaCols } = promptRetry plaMsgQueue plaCols


sendPagerPrompt :: MsgQueue -> PageLen -> EntireTxtLen -> MudStack ()
sendPagerPrompt mq pageLen txtLen =
    let txt = T.concat [ showText pageLen, " of ", showText txtLen, " lines ", parensQuote $ per <> "%" ]
    in sendPrompt mq . colorWith pagerPromptColor . spaced . bracketQuote . spaced $ txt
  where
    per = uncurry (<>) . second (T.take 2) . T.breakOn "." . showText $ pageLen `divide` txtLen * 100


promptRetry :: MsgQueue -> Cols -> MudStack ()
promptRetry mq cols = wrapSend1Nl mq cols sorryInterpPager
