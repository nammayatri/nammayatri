module Screens.HelpAndSupportScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.HelpAndSupportScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HelpAndSupportScreen.View as HelpAndSupportScreen
import Types.App (FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..))

helpAndSupportScreen :: FlowBT String HELP_AND_SUPPORT_SCREEN_OUTPUT
helpAndSupportScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ HelpAndSupportScreen.screen state.helpAndSupportScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToWriteToUsScreen -> App.BackT $ App.BackPoint <$> pure WRITE_TO_US_SCREEN
    GoToTripDetailsScreen updatedState -> App.BackT $ App.BackPoint <$> pure (TRIP_DETAILS_SCREEN updatedState)
    GoToMyRidesScreen -> App.BackT $ App.BackPoint <$> pure MY_RIDES_SCREEN