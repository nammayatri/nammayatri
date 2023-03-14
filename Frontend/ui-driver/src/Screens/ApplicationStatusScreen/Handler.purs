module Screens.ApplicationStatusScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.ApplicationStatusScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ApplicationStatusScreen.View as ApplicationStatusScreen
import Types.App (FlowBT, GlobalState(..), APPLICATION_STATUS_SCREENOUTPUT(..))


applicationStatus :: String -> FlowBT String APPLICATION_STATUS_SCREENOUTPUT
applicationStatus screenType = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ ApplicationStatusScreen.screen state.applicationStatusScreen screenType
  case action of
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_HOME_FROM_APPLICATION_STATUS
    GoToDlScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_UPLOAD_DL_SCREEN
    GoToVehicleDetailScreen ->  App.BackT $ App.BackPoint <$> pure GO_TO_VEHICLE_DETAIL_SCREEN
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_ACCOUT