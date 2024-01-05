module Screens.TicketBookingFlow.TicketStatus.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketBookingFlow.TicketStatus.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingFlow.TicketStatus.View as TicketBookingScreen
import Types.App (FlowBT, GlobalState(..), TICKET_BOOKING_SCREEN_OUTPUT(..), ScreenType(..), TICKET_BOOKING_SCREEN_OUTPUT(..))
import Screens.TicketBookingFlow.TicketStatus.ScreenData (initData) as TicketBookingScreenData
import Screens.Types as ST

ticketStatusScreen :: FlowBT String TICKET_BOOKING_SCREEN_OUTPUT
ticketStatusScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ TicketBookingScreen.screen state.ticketBookingScreen
  case action of
    GoToHomeScreen updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> TicketBookingScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING updatedState)
    GoToGetBookingInfo updatedState bookingStatus -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GET_BOOKING_INFO_SCREEN updatedState bookingStatus)
    RefreshPaymentStatus updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFRESH_PAYMENT_STATUS updatedState)
    GoBack updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TICKET_LIST updatedState)
