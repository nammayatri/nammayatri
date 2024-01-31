module Screens.TicketBookingFlow.TicketStatus.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketBookingFlow.TicketStatus.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingFlow.TicketStatus.View as TicketBookingScreen
import Types.App 
import Screens.TicketBookingFlow.TicketStatus.ScreenData as TicketStatusScreenData
import Screens.TicketBookingFlow.TicketBooking.ScreenData as TicketBookingScreenData
import Screens.Types as ST

ticketStatusScreen :: FlowBT String TICKET_STATUS_SCREEN_OUTPUT
ticketStatusScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ TicketBookingScreen.screen state.ticketStatusScreen
  case action of
    GoToHomeScreen updatedState -> do
      modifyScreenState $ TicketStatusScreenStateType (\_ -> TicketStatusScreenData.initData)
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_TICKET_STATUS updatedState)
    GoToGetBookingInfo updatedState bookingStatus -> do
      modifyScreenState $ TicketStatusScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GET_BOOKING_INFO_SCREEN_FROM_TICKET_STATUS updatedState bookingStatus)
    RefreshPaymentStatus updatedState -> do
      modifyScreenState $ TicketStatusScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFRESH_PAYMENT_STATUS_FROM_TICKET_STATUS_SCREEN updatedState)
    GoBack updatedState -> do
      modifyScreenState $ TicketStatusScreenStateType (\_ -> updatedState)
      modifyScreenState $ 
        TicketBookingScreenStateType (\ticketBookingState -> 
          ticketBookingState {
            props {
              currentStage = updatedState.props.currentStage
            }
          }
        )
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TICKET_LIST_FROM_STATUS_SCREEN updatedState)

