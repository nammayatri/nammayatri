module Screens.MeterRideScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.Commons (markPerformance)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Events as Events
import Prelude (($), (<$>), discard, bind, pure)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.MeterRideScreen.View as MeterRideScreen
import Screens.MeterRideScreen.Controller (ScreenOutput(..))
import Screens.Types (MeterRideScreenState)
import Types.App (FlowBT, GlobalState(..), METER_RIDE_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


meterRideScreen :: FlowBT String METER_RIDE_SCREEN_OUTPUT
meterRideScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MeterRideScreen.screen state.meterRideScreen
  case action of
    GoBack state -> do 
      modifyScreenState $ MeterRideScreenStateType (\_ -> state)
      App.BackT $ pure App.GoBack
    GoToEnterDestination state -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ ENTER_DESTINATION state)
    GoToDriverProfile state -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_DRIVER_PROFILE state)
    StartMeterRide state -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ START_METER_RIDE state)
    EndMeterRide state -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ END_METER_RIDE state)
    UpdatePrice updatedState val -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_RATE_CARD_API updatedState val)
    TriggerGlobalEvents updatedState -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ TRIGGER_GLOBAL_EVENTS)
    RefreshTimeOut updatedState -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFRESH_TRIPS)
    GoToHelpAndSupportScreen updatedState -> do
      modifyScreenState $ MeterRideScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HELP_AND_SUPPORT_FROM_METER)