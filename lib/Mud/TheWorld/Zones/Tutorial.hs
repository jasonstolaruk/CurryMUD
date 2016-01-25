{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.Tutorial (createTutorial) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Put
import Mud.TheWorld.Zones.TutorialIds
import qualified Mud.Misc.Logging as L (logNotice)

import Data.Bits (zeroBits)
import Data.Text (Text)
import qualified Data.Map.Lazy as M (empty)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Tutorial"


-- ==================================================


createTutorial :: MudStack ()
createTutorial = do
  logNotice "createTutorial" "creating the tutorial."

  putRm iTutWelcome
        []
        mempty
        (Rm "Welcome to the tutorial"
            "Hello!"
            zeroBits
            []
            M.empty [])

  putRmTeleName iTutWelcome "tutorial"
