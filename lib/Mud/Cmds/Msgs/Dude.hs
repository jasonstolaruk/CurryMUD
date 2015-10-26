{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.Dude where

import qualified Data.Text as T


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- ==================================================


dudeYou'reNaked :: T.Text
dudeYou'reNaked = "You don't have anything readied. You're naked!"


dudeYou'reScrewed :: T.Text
dudeYou'reScrewed = "You aren't carrying anything, and you don't have anything readied. You're naked!"


dudeYourHandsAreEmpty :: T.Text
dudeYourHandsAreEmpty = "You aren't carrying anything."
