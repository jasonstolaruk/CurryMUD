{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, NamedFieldPuns, OverloadedStrings #-}

module Mud.Interp.Misc where

import Mud.Data.Misc
import Mud.Data.State.ActionParams.ActionParams
import Mud.Data.State.MudData
import Mud.Data.State.Util.Get
import Mud.Data.State.Util.Misc
import Mud.Data.State.Util.Output
import Mud.Misc.ANSI
import Mud.Util.Misc
import Mud.Util.Operators
import Mud.Util.Text

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T


type FindActionFun = Id -> MudState -> CmdName -> MudStack (Maybe Action)


dispatch :: FindActionFun -> Interp
dispatch f cn p@(ActionParams { myId, plaMsgQueue }) = do
    getState >>= \ms -> maybe (send plaMsgQueue . nlnl $ "What?") (p |&|) =<< f myId ms cn
    getState >>= \ms -> when (isNothing . getInterp myId $ ms) . prompt plaMsgQueue . mkPrompt myId $ ms


findActionHelper :: CmdName -> [Cmd] -> MudStack (Maybe Action)
findActionHelper cn cmds = return $ action . fst <$> findFullNameForAbbrev cn [ (cmd, cmdName cmd) | cmd <- cmds ]


mkPrompt :: Id -> MudState -> T.Text
mkPrompt i ms = let (hps, mps, pps, fps) = getXps i ms
                    marker               = colorWith indentColor " "
                in marker <> " " <> spaces [ f "h" hps
                                           , f "m" mps
                                           , f "p" pps
                                           , f "f" fps ]
  where
    indentColor | isNpc i ms = toNpcColor
                | otherwise  = promptIndentColor
    f a (x, y) = let c   = if | x == y    -> green
                              | per > 67  -> cyan
                              | per > 33  -> yellow
                              | per > 10  -> red
                              | otherwise -> magenta
                     per = round $ x `divide` y * 100
                 in colorWith c a <> showText x
