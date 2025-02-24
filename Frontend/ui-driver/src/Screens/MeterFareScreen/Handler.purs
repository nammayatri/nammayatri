module Screens.MeterFareScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Types.ModifyScreenState (modifyScreenState)
import Screens.MeterFareScreen.Controller (ScreenOutput(..))
import Screens.MeterFareScreen.View as MeterFareScreen
import Types.App (FlowBT, GlobalState(..), METER_FARE_SCREEN_OUTPUT(..), ScreenType(..))


meterFareScreen :: FlowBT String METER_FARE_SCREEN_OUTPUT
meterFareScreen = do 
  (GlobalState state') <- getState
  act <- lift $ lift $ runScreen $ MeterFareScreen.screen state'.meterFareScreen
  case act of
    GoToHomeScreen state -> do
      modifyScreenState $ MeterFareScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_METER_FARE state)
