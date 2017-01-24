{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.Tutorial ( createTutorial
                                   , tutorialHooks
                                   , tutorialRmActionFuns ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Put
import Mud.TheWorld.Misc
import Mud.TheWorld.Zones.TutorialIds
import qualified Mud.Misc.Logging as L (logNotice)

import Data.Bits (zeroBits)
import Data.Text (Text)
import qualified Data.Map.Strict as M (fromList)


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
        (mkRm (RmTemplate "Welcome to the tutorial"
            "Hello!\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0)
            InsideEnv
            (Just "Entrance")
            (M.fromList [ ("look", [ lookTrashHook ])
                        , ("put",  [ putTrashHook  ]) ])
            [ trashRmAction ]
            []))

  putRmTeleName iTutWelcome "tutorial"
