module Screens.TicketBookingFlow.MetroTicketStatus.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketBookingFlow.MetroTicketStatus.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingFlow.MetroTicketStatus.View as MetroTicketStatusView
import Types.App 
import Screens.TicketBookingFlow.MetroTicketStatus.ScreenData as MetroTicketStatusScreenData
import Screens.Types as ST

metroTicketStatusScreen :: FlowBT String METRO_TICKET_STATUS_SCREEN_OUTPUT
metroTicketStatusScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MetroTicketStatusView.screen state.metroTicketStatusScreen
  case action of
    NoOutput -> pure NO_OUTPUT_METRO_TICKET_STATUS_SCREEN
    GoToMetroTicketDetails updatedState -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\_ ->updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_TICKET_DETAILS updatedState)
    RefreshPaymentStatus updatedState -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\_ ->updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_STATUS_AC updatedState)
    GoToTryAgainPayment updatedState -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\_ ->updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRY_AGAIN_PAYMENT updatedState)