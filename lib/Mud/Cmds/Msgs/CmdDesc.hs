{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.CmdDesc where

import Data.Text (Text)


cmdDescBars :: Text
cmdDescBars = "Display one or more status bars."


cmdDescClear :: Text
cmdDescClear = "Clear the screen."


cmdDescDispCmdList :: Text
cmdDescDispCmdList = "Display or search this command list."


cmdDescDrink :: Text
cmdDescDrink = "Drink a given amount from a vessel."


cmdDescDrop :: Text
cmdDescDrop = "Drop one or more items."


cmdDescEmote :: Text
cmdDescEmote = "Freely describe an action."


cmdDescEquip :: Text
cmdDescEquip = "Display your readied equipment, or examine one or more items in your readied equipment."


cmdDescExits :: Text
cmdDescExits = "Display obvious exits."


cmdDescExpCmdList :: Text
cmdDescExpCmdList = "Display or search a list of available expressive commands and their results."


cmdDescGet :: Text
cmdDescGet = "Pick up one or more items."


cmdDescGive :: Text
cmdDescGive = "Give one or more items to a person."


cmdDescGoDown :: Text
cmdDescGoDown = "Go down."


cmdDescGoEast :: Text
cmdDescGoEast = "Go east."


cmdDescGoNorth :: Text
cmdDescGoNorth = "Go north."


cmdDescGoNortheast :: Text
cmdDescGoNortheast = "Go northeast."


cmdDescGoNorthwest :: Text
cmdDescGoNorthwest = "Go northwest."


cmdDescGoSouth :: Text
cmdDescGoSouth = "Go south."


cmdDescGoSoutheast :: Text
cmdDescGoSoutheast = "Go southeast."


cmdDescGoSouthwest :: Text
cmdDescGoSouthwest = "Go southwest."


cmdDescGoUp :: Text
cmdDescGoUp = "Go up."


cmdDescGoWest :: Text
cmdDescGoWest = "Go west."


cmdDescInv :: Text
cmdDescInv = "Display your inventory, or examine one or more items in your inventory."


cmdDescLook :: Text
cmdDescLook = "Display a description of your current room, or examine one or more things in your current room."


cmdDescPut :: Text
cmdDescPut = "Put one or more items into a container."


cmdDescRead :: Text
cmdDescRead = "Read the text that is written on an item in your inventory or a fixture of your current room."


cmdDescReady :: Text
cmdDescReady = "Ready one or more items."


cmdDescRemove :: Text
cmdDescRemove = "Remove one or more items from a container."


cmdDescSay :: Text
cmdDescSay = "Say something out loud."


cmdDescShow :: Text
cmdDescShow = "Show one or more items in your inventory and/or readied equipment to another person."


cmdDescStats :: Text
cmdDescStats = "Display your stats."


cmdDescUnready :: Text
cmdDescUnready = "Unready one or more items."
