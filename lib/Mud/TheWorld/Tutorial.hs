module Mud.TheWorld.Tutorial (createTutorial) where

--import Mud.Data.State.Util.Put
--import Mud.TheWorld.TutorialIds
import Mud.Data.State.MudData
import qualified Mud.Misc.Logging as L (logNotice)

-- import Data.Bits (zeroBits)
-- import qualified Data.Map.Lazy as M (empty, fromList)
import qualified Data.Text as T


logNotice :: T.Text -> T.Text -> MudStack ()
logNotice = L.logNotice "Mud.TheWorld.Tutorial"


-- ==================================================


createTutorial :: MudStack ()
createTutorial = logNotice "createTutorial" "creating the tutorial."
