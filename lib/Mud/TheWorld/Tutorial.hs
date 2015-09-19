module Mud.TheWorld.Tutorial (createTutorial) where

import Mud.Data.State.MudData
import Mud.Data.State.Util.Put
import Mud.TheWorld.TutorialIds
import qualified Mud.Misc.Logging as L (logNotice)

-- import qualified Data.Map.Lazy as M (empty, fromList)
import Data.Bits (zeroBits)
import qualified Data.Text as T


logNotice :: T.Text -> T.Text -> MudStack ()
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
            [])

  putRmTeleName iTutWelcome "tutorial"
