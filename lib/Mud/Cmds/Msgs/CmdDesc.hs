{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.CmdDesc where

import Mud.Data.State.MudData
import Mud.Util.Lang

import Data.Monoid ((<>))
import Data.Text (Text)


cmdDescBars :: Text
cmdDescBars = "Display one or more status bars."


cmdDescClear :: Text
cmdDescClear = "Clear the screen."


cmdDescDispCmdList :: Text
cmdDescDispCmdList = "Display or search this command list."


cmdDescDrink :: Text
cmdDescDrink = "Drink a given number of mouthfuls from a vessel."


cmdDescDrop :: Text
cmdDescDrop = "Drop one or more items."


cmdDescEmote :: Text
cmdDescEmote = "Freely describe an action."


cmdDescEmpty :: Text
cmdDescEmpty = "Empty the liquid contents of one or more vessels."


cmdDescEquip :: Text
cmdDescEquip = "Display your readied equipment, or examine one or more items in your readied equipment."


cmdDescExits :: Text
cmdDescExits = "Display obvious exits."


cmdDescExpCmdList :: Text
cmdDescExpCmdList = "Display or search a list of available expressive commands and their results."


cmdDescFeeling :: Text
cmdDescFeeling = "Check how you're feeling."


cmdDescFill :: Text
cmdDescFill = "Fill one or more vessels with a) the contents of another vessel, or b) a source of liquid in your \
              \current room."


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
cmdDescPut = "Put one or more items into a) a container in your inventory, or b) a container in your current room."


cmdDescRead :: Text
cmdDescRead = "Read the text that is written on a) an item in your inventory, or b) a fixture of your current room."


cmdDescReady :: Text
cmdDescReady = "Ready one or more items in your inventory."


cmdDescRemove :: Text
cmdDescRemove = "Remove one or more items from a) a container in your inventory, or b) a container in your current \
                \room."


cmdDescSay :: Lang -> Text
cmdDescSay l = "Say something out loud" <> mkInLangTxtForLang l <> "."


cmdDescShow :: Text
cmdDescShow = "Show one or more items in your inventory and/or readied equipment to another person."


cmdDescSmell :: Text
cmdDescSmell = "Smell a) the air, b) an item in your inventory, c) an item in your readied equipment, or d) a being or \
               \fixture in your current room."


cmdDescStats :: Text
cmdDescStats = "Display your stats."


cmdDescStop :: Text
cmdDescStop = "Stop moving, eating, drinking, or attacking."


cmdDescTaste :: Text
cmdDescTaste = "Taste a) an item in your inventory or b) an item in your readied equipment." -- TODO: a) b) ?


cmdDescUnready :: Text
cmdDescUnready = "Unready one or more items in your readied equipment."


cmdDescWhisper :: Text
cmdDescWhisper = "Whisper something to someone in your current room."
