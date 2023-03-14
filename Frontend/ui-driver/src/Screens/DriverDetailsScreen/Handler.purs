module Screens.DriverDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), discard)
import Screens.DriverDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverDetailsScreen.View as DriverDetailsScreen
import Types.App (GlobalState(..), DRIVER_DETAILS_SCREEN_OUTPUT, FlowBT,  ScreenType(..))
import Types.ModifyScreenState(modifyScreenState)

driverDetailsScreen :: FlowBT String DRIVER_DETAILS_SCREEN_OUTPUT
driverDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DriverDetailsScreen.screen state.driverDetailsScreen
  case action of
    GoBack updatedState -> do
      modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState)
      App.BackT $ pure App.GoBack