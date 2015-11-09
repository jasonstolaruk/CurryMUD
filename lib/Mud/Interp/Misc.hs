{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module Mud.Interp.Misc where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Misc.ANSI
import Mud.Util.Misc
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Monoid ((<>))
import qualified Data.Text as T


mkPrompt :: Id -> MudState -> T.Text
mkPrompt i ms = let (hps, mps, pps, fps) = getXps i ms
                    indent               = quoteWith' (promptIndentColor, dfltColor) "Curry"
                in indent <> " " <> spaces [ f "h" hps
                                           , f "m" mps
                                           , f "p" pps
                                           , f "f" fps ]
  where
    f a (x, y) = let c   = if | x == y    -> green
                              | per > 67  -> cyan
                              | per > 33  -> yellow
                              | per > 10  -> red
                              | otherwise -> magenta
                     per = round $ x `divide` y * 100
                 in quoteWith' (c, dfltColor) a <> showText x
