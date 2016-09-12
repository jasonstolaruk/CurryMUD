{-# LANGUAGE OverloadedStrings #-}

module Mud.Cmds.Msgs.CmdDesc where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Lang
import Mud.Util.Quoting
import Mud.Util.Text

import Data.Monoid ((<>))
import Data.Text (Text)


cmdDescAbout :: Text
cmdDescAbout = "About CurryMUD."


cmdDescAdmin :: Text
cmdDescAdmin = "Display a list of administrators, or send a message to an administrator."


cmdDescBars :: Text
cmdDescBars = "Display one or more status bars."


cmdDescBonus :: Text
cmdDescBonus = "Give another player bonus experience points for outstanding role-playing."


cmdDescBug :: Text
cmdDescBug = "Report a bug."


cmdDescClear :: Text
cmdDescClear = "Clear the screen."


cmdDescColor :: Text
cmdDescColor = "Perform a color test."


cmdDescDescription :: Text
cmdDescDescription = "Change your character description."


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


cmdDescHelp :: Text
cmdDescHelp = "Get help on one or more commands or topics."


cmdDescInv :: Text
cmdDescInv = "Display your inventory, or examine one or more items in your inventory."


cmdDescLink :: Text
cmdDescLink = "Display a list of the people with whom you have established a telepathic link, or establish a \
              \telepathic link with one or more people."


cmdDescListen :: Text
cmdDescListen = "Display a description of what you hear in your current room."


cmdDescLook :: Text
cmdDescLook = "Display a description of your current room, or examine one or more things in your current room."


cmdDescLookSelf :: Text
cmdDescLookSelf = "Verify what others see when they " <> dblQuote "look" <> " at your character."


cmdDescMotd :: Text
cmdDescMotd = "Display the message of the day."


cmdDescPut :: Text
cmdDescPut = "Put one or more items into a) a container in your inventory, or b) a container in your current room."


cmdDescQuit :: Text
cmdDescQuit = "Quit playing CurryMUD."


cmdDescRead :: Text
cmdDescRead = "Read the text that is written on a) an item in your inventory, or b) a fixture of your current room."


cmdDescReady :: Text
cmdDescReady = "Ready one or more items in your inventory."


cmdDescRemove :: Text
cmdDescRemove = "Remove one or more items from a) a container in your inventory, or b) a container in your current \
                \room."


cmdDescRoomDesc :: Text
cmdDescRoomDesc = "Specify a temporary description to appear next to your name in the description of your current room."


cmdDescSay :: Lang -> Text
cmdDescSay l = prd $ "Say something out loud" <> mkInLangTxtForLang l


cmdDescSet :: Text
cmdDescSet = "View or change settings."


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
cmdDescTaste = "Taste a) an item in your inventory, or b) an item in your readied equipment."


cmdDescTelepathy :: Text
cmdDescTelepathy = "Send a private message to a person with whom you have established a two-way telepathic link."


cmdDescTempDesc :: Text
cmdDescTempDesc = "Specify a temporary character description visible when someone looks at you."


cmdDescTune :: Text
cmdDescTune = "Display a list of your telepathic connections, or tune in/out one or more telepathic connections."


cmdDescTypo :: Text
cmdDescTypo = "Report a typo."


cmdDescUnlink :: Text
cmdDescUnlink = "Sever one or more telepathic links."


cmdDescUnready :: Text
cmdDescUnready = "Unready one or more items in your readied equipment."


cmdDescUptime :: Text
cmdDescUptime ="Display how long CurryMUD has been running."


cmdDescWhisper :: Text
cmdDescWhisper = "Whisper something to someone in your current room."


cmdDescWho :: Text
cmdDescWho = "Display or search a list of who is currently awake."


cmdDescWhoAmI :: Text
cmdDescWhoAmI = "Confirm your name, sex, and race."
