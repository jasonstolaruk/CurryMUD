{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module Mud.Interp.Misc where

import Mud.Data.Misc
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Misc.ANSI
import Mud.Util.Misc
import Mud.Util.Text

import Data.Monoid ((<>))
import qualified Data.Text as T


findActionHelper :: CmdName -> [Cmd] -> MudStack (Maybe Action)
findActionHelper cn cmds = return $ action . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds ]


mkPrompt :: Id -> MudState -> T.Text
mkPrompt i ms = let (hps, mps, pps, fps) = getXps i ms
                    marker               = colorWith promptIndentColor " "
                in marker <> " " <> spaces [ f "h" hps
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
                 in colorWith c a <> showText x
