{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Msgs where

import Mud.Util.Quoting

import qualified Data.Text as T


dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


dfltShutdownMsg :: T.Text
dfltShutdownMsg = "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


inOutOrOnOff :: T.Text
inOutOrOnOff = T.concat [ dblQuote "in"
                        , "/"
                        , dblQuote "out"
                        , " or "
                        , dblQuote "on"
                        , "/"
                        , dblQuote "off" ]
