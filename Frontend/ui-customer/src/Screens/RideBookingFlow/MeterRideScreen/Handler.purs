module Screens.RideBookingFlow.MeterRideScreen.Handler where

import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideBookingFlow.MeterRideScreen.Controller (ScreenOutput(..))
import Screens.RideBookingFlow.MeterRideScreen.View as MeterRideScreen
import Types.App (FlowBT, GlobalState(..), METER_RIDE_SCREEN_OP(..), ScreenType(..))
import ModifyScreenState (modifyScreenState)

meterRideScreen :: FlowBT String Unit
meterRideScreen = do
  (GlobalState allState) <- getState
  action <- lift $ lift $ runScreen $ MeterRideScreen.screen allState.meterRideScreen
  case action of
    GoBack state -> do
      modifyScreenState $ MeterRideScreenType (\_ -> state)
      App.BackT $ pure App.GoBack
    _ -> App.BackT $ pure App.GoBack