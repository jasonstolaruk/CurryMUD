{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs.Msgs where

import qualified Data.Text as T


dbErrorMsg :: T.Text
dbErrorMsg = "There was an error when reading the database."


dfltBootMsg :: T.Text
dfltBootMsg = "You have been booted from CurryMUD. Goodbye!"


dfltShutdownMsg :: T.Text
dfltShutdownMsg = "CurryMUD is shutting down. We apologize for the inconvenience. See you soon!"


genericErrorMsg :: T.Text
genericErrorMsg = "Unfortunately, an error occured while executing your command."


motdErrorMsg :: T.Text
motdErrorMsg = "Unfortunately, the message of the day could not be retrieved."
