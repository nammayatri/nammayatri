module Screens.RideBookingFlow.MeterSelectDestinationScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.RideBookingFlow.MeterSelectDestinationScreen.Controller (ScreenOutput(..))
import Screens.RideBookingFlow.MeterSelectDestinationScreen.View as MeterSelectDestinationScreen
import Types.App (FlowBT, GlobalState(..), METER_SELECT_DESTINATION_SCREEN_OUTPUT(..), ScreenType(..))


meterSelectDestinationScreen :: FlowBT String METER_SELECT_DESTINATION_SCREEN_OUTPUT
meterSelectDestinationScreen = do 
  (GlobalState state') <- getState
  act <- lift $ lift $ runScreen $ MeterSelectDestinationScreen.screen state'.meterSelectDestinationScreen
  case act of
    GoBack state -> do
      modifyScreenState $ MeterSelectDestinationScreenType (\_ -> state)
      App.BackT $ App.NoBack <$> (pure $ GO_BACK_TO_METER_RIDE state)
    SearchPlace input updatedState -> do
          modifyScreenState $ MeterSelectDestinationScreenType (\_ -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SEARCH_LOCATION_DESTINATION input updatedState)
    UpdatedLocationName state lat lng -> do
      modifyScreenState $ MeterSelectDestinationScreenType (\_ -> state)
      App.BackT $ App.NoBack <$> pure (UPDATE_LOCATION_NAME_DESTINATION state lat lng)
    GetPlaceName state placeId -> do
      modifyScreenState $ MeterSelectDestinationScreenType (\_ -> state)
      App.BackT $ App.NoBack <$> (pure $ GET_PLACE_NAME_DESTINATION state placeId)
    GoToMeterRideScreen state -> do
      modifyScreenState $ MeterSelectDestinationScreenType (\_ -> state)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_METER_RIDE state)
