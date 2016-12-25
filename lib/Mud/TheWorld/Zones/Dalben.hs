{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld.Zones.Dalben ( createDalben
                                 , dalbenHooks
                                 , dalbenRmActionFuns ) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Make
import Mud.Data.State.Util.Put
import Mud.TheWorld.Misc
import Mud.TheWorld.Zones.DalbenIds
import qualified Mud.Misc.Logging as L (logNotice)

import Data.Bits (zeroBits)
import Data.Text (Text)
import qualified Data.Map.Lazy as M (fromList)


logNotice :: Text -> Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Zones.Dalben"


-- ==================================================
-- Hooks:


dalbenHooks :: [(HookName, HookFun)]
dalbenHooks = []


-- ==================================================
-- Room action functions:


dalbenRmActionFuns :: [(FunName, RmActionFun)]
dalbenRmActionFuns = []


-- ==================================================
-- Zone definition:


createDalben :: MudStack ()
createDalben = do
  logNotice "createDalben" "creating Dalben."

  putRm iDalbenWelcome
        []
        mempty
        (mkRm (RmTemplate "Welcome to Dalben"
            "Hello!\n\
            \There is a trash bin here."
            Nothing
            Nothing
            zeroBits
            []
            (0, 0, 0)
            OutsideEnv
            (Just "Welcome")
            (M.fromList [ ("look", [ lookTrashHook ])
                        , ("put",  [ putTrashHook  ]) ])
            [ trashRmAction ]
            []))

  putRmTeleName iDalbenWelcome "dalben"
