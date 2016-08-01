{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Dude where

          import Data.Text (Text)


          {-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


          dudeYou'reNaked :: Text
          dudeYou'reNaked = "You don't have anything readied. You're naked!"


          dudeYou'reScrewed :: Text
          dudeYou'reScrewed = "You aren't carrying anything, and you don't have anything readied. You're naked!"


          dudeYourHandsAreEmpty :: Text
          dudeYourHandsAreEmpty = "You aren't carrying anything."
