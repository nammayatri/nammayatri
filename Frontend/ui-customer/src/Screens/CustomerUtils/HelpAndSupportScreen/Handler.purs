module Screens.HelpAndSupportScreen.Handler where

import Prelude (bind, discard, ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.HelpAndSupportScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HelpAndSupportScreen.View as HelpAndSupportScreen
import Components.SettingSideBar.Controller as SettingSideBar
import ModifyScreenState (modifyScreenState)
import Types.App (FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), ScreenType(..))

helpAndSupportScreen :: FlowBT String HELP_AND_SUPPORT_SCREEN_OUTPUT
helpAndSupportScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ HelpAndSupportScreen.screen state.helpAndSupportScreen
  case act of
    GoBack -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_HELP)
    GoHome -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBar.CLOSED}}}) 
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_HELP)
    GoToSupportScreen bookingId-> App.BackT $ App.BackPoint <$> (pure $ GO_TO_SUPPORT_SCREEN bookingId)
    GoToTripDetails updatedState-> App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRIP_DETAILS updatedState)
    GoToMyRides -> App.BackT $ App.BackPoint <$> (pure $ VIEW_RIDES)
    UpdateState updatedState -> App.BackT $ App.BackPoint <$> (pure $ UPDATE_STATE updatedState)
