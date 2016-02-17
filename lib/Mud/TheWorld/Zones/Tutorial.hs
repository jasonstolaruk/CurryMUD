{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.Tutorial ( createTutorial
                                   , tutorialHooks
                                   , tutorialRmActionFuns ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Put
import Mud.TheWorld.Misc
import Mud.TheWorld.Zones.TutorialIds
import qualified Mud.Misc.Logging as L (logNotice)

import Data.Bits (zeroBits)
import Data.Text (Text)
import qualified Data.Map.Lazy as M (fromList)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.Tutorial"


-- ==================================================
-- Hooks:


tutorialHooks :: [(HookName, HookFun)]
tutorialHooks = []


-- ==================================================
-- Room action functions:


tutorialRmActionFuns :: [(FunName, RmActionFun)]
tutorialRmActionFuns = []


-- ==================================================
-- Zone definition:


createTutorial :: MudStack ()
createTutorial = do
  logNotice "createTutorial" "creating the tutorial."

  putRm iTutWelcome
        []
        mempty
        (Rm "Welcome to the tutorial"
            "Hello!\n\
            \There is a trash bin here."
            zeroBits
            []
            (M.fromList [ ("look", [ lookTrashHook ])
                        , ("put",  [ putTrashHook  ]) ])
            [ trashRmAction ]
            [] [])

  putRmTeleName iTutWelcome "tutorial"
