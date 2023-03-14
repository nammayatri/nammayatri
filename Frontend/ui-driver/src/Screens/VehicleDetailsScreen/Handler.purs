module Screens.VehicleDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($),(<$>))
import Screens.VehicleDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.VehicleDetailsScreen.View as VehicleDetailsScreen
import Types.App (GlobalState(..), FlowBT, VEHICLE_DETAILS_SCREEN_OUTPUT(..))

vehicleDetailsScreen :: FlowBT String VEHICLE_DETAILS_SCREEN_OUTPUT
vehicleDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ VehicleDetailsScreen.screen state.vehicleDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    UpdateVehicleDetails updateState -> App.BackT $ App.BackPoint <$> pure (UPDATE_VEHICLE_INFO updateState)