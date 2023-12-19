{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.VehicleDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($),(<$>))
import Screens.VehicleDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.VehicleDetailsScreen.View as VehicleDetailsScreen
import Types.App (GlobalState(..), FlowBT, VEHICLE_DETAILS_SCREEN_OUTPUT(..))
import React.Navigation.Navigate (navigateToScreen)

vehicleDetailsScreen :: FlowBT String VEHICLE_DETAILS_SCREEN_OUTPUT
vehicleDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ VehicleDetailsScreen.screen state.vehicleDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    UpdateVehicleDetails updateState -> App.BackT $ App.BackPoint <$> pure (UPDATE_VEHICLE_INFO updateState)