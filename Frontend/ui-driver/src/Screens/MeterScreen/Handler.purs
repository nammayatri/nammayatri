module Screens.MeterScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Types.ModifyScreenState (modifyScreenState)
import Screens.MeterScreen.Controller (ScreenOutput(..))
import Screens.MeterScreen.View as MeterScreen
import Types.App (FlowBT, GlobalState(..), METER_SCREEN_OUTPUT(..), ScreenType(..))


meterScreen :: FlowBT String METER_SCREEN_OUTPUT
meterScreen = do 
  (GlobalState state') <- getState
  act <- lift $ lift $ runScreen $ MeterScreen.screen state'.meterScreen
  case act of
    GoToHomeScreen state -> do
      modifyScreenState $ MeterScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_METER state)
    SearchPlace input updatedState -> do
          modifyScreenState $ MeterScreenStateType (\_ -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SEARCH_LOCATION input updatedState)
    GoToMeterMapScreen state -> do
          modifyScreenState $ MeterScreenStateType (\_ -> state)
          App.BackT $ App.BackPoint <$> (pure $ GO_TO_METER_MAP_FROM_METER state)
    UpdatedState screenState saveToCurrLocs -> do
       modifyScreenState $ MeterScreenStateType (\_ → screenState)
       App.BackT $ App.BackPoint <$> (pure $ RELOAD_STATE saveToCurrLocs)
    UpdatedLocationName state lat lng -> do
      modifyScreenState $ MeterScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> pure (UPDATE_LOCATION_NAME state lat lng)
    GetPlaceName state placeId -> do
      modifyScreenState $ MeterScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ GET_PLACE_NAME_METER_SCREEN state placeId)
