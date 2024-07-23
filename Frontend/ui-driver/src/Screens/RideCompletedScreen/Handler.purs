{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideCompletedScreen.Handler where

import Prelude (discard, bind, ($), pure, (<$>))
import Control.Monad.Except.Trans (lift)
import Types.App (FlowBT, GlobalState(..), RIDE_COMPLETED_SCREEN_OUTPUT(..), ScreenType(..))
import Engineering.Helpers.BackTrack (getState)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideCompletedScreen.View as RideCompletedScreen
import Screens.RideCompletedScreen.Controller (ScreenOutput(..))
import Control.Transformers.Back.Trans as App
import Types.ModifyScreenState (modifyScreenState)

rideCompletedScreen :: FlowBT String RIDE_COMPLETED_SCREEN_OUTPUT
rideCompletedScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RideCompletedScreen.screen state.rideCompletedScreen
  case action of
    GoToRideDetailsScreen updatedState -> do
      modifyScreenState $ RideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDE_DETAILS_SCREEN_FROM_RIDE_COMPLETED_SCREEN)
    GoToHomeScreen updatedState -> do 
      modifyScreenState $ RideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> ( pure $ GO_TO_HOME_SCREEN_FROM_RIDE_COMPLETED_SCREEN updatedState )