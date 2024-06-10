{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CameraScreen.Handler where 

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (liftFlow)
import Prelude
import PrestoDOM.Core (getPushFn, terminateUI)
import PrestoDOM.Core.Types.Language.Flow (runScreen, showScreenWithNameSpace, initUIWithNameSpace)
import Presto.Core.Types.Language.Flow (Flow(..), getState)
import Screens.CameraScreen.View as CameraScreen
import Screens.CameraScreen.Controller
import Types.App (FlowBT, CAMERA_SCREEN_OUTPUT, GlobalState(..))
import Debug

cameraScreen :: Flow GlobalState Unit
cameraScreen = do
    (GlobalState state) <- getState
    liftFlow $ initUIWithNameSpace "CameraScreen" Nothing
    void $ showScreenWithNameSpace $ CameraScreen.screen state.cameraScreen
    liftFlow $ terminateUI $ Just "CameraScreen"
