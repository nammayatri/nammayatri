module Screens.RiderDriverProfileScreen.Handler where

import Screens.DriverProfileScreenCommon.View as DriverProfileScreenCommon
import Screens.DriverProfileScreenCommon.Controller
import Types.App (FlowBT, GlobalState(..), DRIVER_PROFILE_SCREEN(..), ScreenType(..))
import Engineering.Helpers.BackTrack (getState)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App

import Prelude

driverProfileScreen :: FlowBT String DRIVER_PROFILE_SCREEN
driverProfileScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DriverProfileScreenCommon.screen state.riderDriverProfileScreen
  case action of
    GoToBack -> App.BackT $ pure App.GoBack
    _ -> App.BackT $ pure App.GoBack
