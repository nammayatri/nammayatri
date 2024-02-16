{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.FollowRideScreen.Handler where

import Prelude
import Control.Monad.Trans.Class (lift)
import Engineering.Helpers.BackTrack (getState)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.FollowRideScreen.View as FollowRideScreen
import Screens.FollowRideScreen.Controller (ScreenOutput(..))
import Types.App (FOLLOW_RIDE_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Control.Transformers.Back.Trans as App
import ModifyScreenState (modifyScreenState)

followRideScreen :: FlowBT String FOLLOW_RIDE_SCREEN_OUTPUT
followRideScreen = do
  (GlobalState globalState) <- getState
  out <- lift $ lift $ runScreen $ FollowRideScreen.screen globalState.followRideScreen (GlobalState globalState)
  case out of
    Exit updatedState -> do
      modifyScreenState $ FollowRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HS_FROM_FOLLOW_RIDE)
    RestartTracking updatedState -> do
      modifyScreenState $ FollowRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RESTART_TRACKING)
    OpenNavigation updatedState -> do
      modifyScreenState $ FollowRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ OPEN_GOOGLE_MAPS_FOLLOW_RIDE updatedState)
    
