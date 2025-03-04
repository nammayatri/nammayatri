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
