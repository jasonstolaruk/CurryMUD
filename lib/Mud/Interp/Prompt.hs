module Mud.Interp.Prompt where

import Mud.Data.State.MudData
import Mud.Util.Quoting
import Mud.Data.State.Util.Get
import Mud.Util.Text

import Control.Lens.Operators ((^.))
import qualified Data.Text as T


mkPrompt :: Id -> MudState -> T.Text
mkPrompt i ms = let m   = getMob i ms
                    chp = m^.curHp
                    cmp = m^.curMp
                    cpp = m^.curPp
                    cfp = m^.curFp
                in angleBracketQuote . quoteWith " " . spaces $ [ f "H" chp
                                                                , f "M" cmp
                                                                , f "P" cpp
                                                                , f "F" cfp ]
  where
    f x y = T.concat [ x
                     , "="
                     , showText y ]
