module Screens.MeterMapScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.MeterMapScreen.Controller (ScreenOutput(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.MeterMapScreen.View as MeterMapScreen
import Types.App (FlowBT, GlobalState(..), METER_MAP_SCREEN_OUTPUT(..), ScreenType(..))


meterMapScreen :: FlowBT String METER_MAP_SCREEN_OUTPUT
meterMapScreen = do 
  (GlobalState state') <- getState
  act <- lift $ lift $ runScreen $ MeterMapScreen.screen state'.meterMapScreen
  case act of
    GoToMeterScreen state -> do
        modifyScreenState $ MeterMapScreenStateType (\_ -> state)
        App.BackT $ App.BackPoint <$> (pure $ GO_TO_METER_SCREEN_FROM_METER_MAP state)
    SearchPlace input updatedState -> do
          modifyScreenState $ MeterMapScreenStateType (\_ -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SEARCH_LOCATION_MAP_SCREEN input updatedState)
    OTPEntered state -> do
          modifyScreenState $ MeterMapScreenStateType (\_ -> state {props {enableOtpModal = false}})
          App.BackT $ App.NoBack <$> (pure $ OTP_ENTERED state.props.alternateMobileOtp)
