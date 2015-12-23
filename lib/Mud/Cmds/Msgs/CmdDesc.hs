{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.CmdDesc where

import qualified Data.Text as T


cmdDescBars :: T.Text
cmdDescBars = "Display one or more status bars."


cmdDescClear :: T.Text
cmdDescClear = "Clear the screen."


cmdDescDispCmdList :: T.Text
cmdDescDispCmdList = "Display or search this command list."


cmdDescDrop :: T.Text
cmdDescDrop = "Drop one or more items."


cmdDescEmote :: T.Text
cmdDescEmote = "Freely describe an action."


cmdDescEquip :: T.Text
cmdDescEquip = "Display your readied equipment, or examine one or more items in your readied equipment."


cmdDescExits :: T.Text
cmdDescExits = "Display obvious exits."


cmdDescExpCmdList :: T.Text
cmdDescExpCmdList = "Display or search a list of available expressive commands and their results."


cmdDescGet :: T.Text
cmdDescGet = "Pick up one or more items."


cmdDescGoDown :: T.Text
cmdDescGoDown = "Go down."


cmdDescGoEast :: T.Text
cmdDescGoEast = "Go east."


cmdDescGoNorth :: T.Text
cmdDescGoNorth = "Go north."


cmdDescGoNortheast :: T.Text
cmdDescGoNortheast = "Go northeast."


cmdDescGoNorthwest :: T.Text
cmdDescGoNorthwest = "Go northwest."


cmdDescGoSouth :: T.Text
cmdDescGoSouth = "Go south."


cmdDescGoSoutheast :: T.Text
cmdDescGoSoutheast = "Go southeast."


cmdDescGoSouthwest :: T.Text
cmdDescGoSouthwest = "Go southwest."


cmdDescGoUp :: T.Text
cmdDescGoUp = "Go up."


cmdDescGoWest :: T.Text
cmdDescGoWest = "Go west."


cmdDescInv :: T.Text
cmdDescInv = "Display your inventory, or examine one or more items in your inventory."


cmdDescLook :: T.Text
cmdDescLook = "Display a description of your current room, or examine one or more things in your current room."


cmdDescPut :: T.Text
cmdDescPut = "Put one or more items into a container."


cmdDescRemove :: T.Text
cmdDescRemove = "Remove one or more items from a container."


cmdDescStats :: T.Text
cmdDescStats = "Display your stats."
