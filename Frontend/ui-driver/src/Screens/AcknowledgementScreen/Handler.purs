module Screens.AcknowledgementScreen.Handler where

import Prelude (bind, ($), pure , (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.AcknowledgementScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AcknowledgementScreen.View as AcknowledgementScreen
import Types.App (FlowBT, GlobalState(..),ACKNOWLEDGEMENT_SCREEN_OUTPUT(..))
import React.Navigation.Navigate (navigateToScreen)


acknowledgementScreen :: FlowBT String ACKNOWLEDGEMENT_SCREEN_OUTPUT
acknowledgementScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ navigateToScreen $ AcknowledgementScreen.screen state.acknowledgementScreen
  case act of
    HomeScreen -> App.BackT $ App.NoBack <$> (pure $ EXIT_TO_HOME_SCREEN)
    RetryPayment -> App.BackT $ App.NoBack <$> (pure $ RETRY_PAYMENT)