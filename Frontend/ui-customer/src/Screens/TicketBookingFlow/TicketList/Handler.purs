module Screens.TicketBookingFlow.TicketList.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketBookingFlow.TicketList.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingFlow.TicketList.View as TicketBookingScreen
import Types.App (FlowBT, GlobalState(..), TICKET_BOOKING_SCREEN_OUTPUT(..), ScreenType(..), TICKET_BOOKING_SCREEN_OUTPUT(..))
import Screens.TicketBookingFlow.TicketList.ScreenData (initData) as TicketBookingScreenData
import Screens.Types as ST

ticketListScreen :: FlowBT String TICKET_BOOKING_SCREEN_OUTPUT
ticketListScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ TicketBookingScreen.screen state.ticketBookingScreen
  case action of
    GoToHomeScreen updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> TicketBookingScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING updatedState)
    GoToTicketPayment state -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      App.BackT $ App.NoBack <$> (pure (GO_TO_TICKET_PAYMENT state))
    GoToOpenGoogleMaps state lat2 long2 -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      App.BackT $ App.BackPoint <$> (pure (GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW lat2 long2))
    GoToGetBookingInfo updatedState bookingStatus -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GET_BOOKING_INFO_SCREEN updatedState bookingStatus)
    BookTickets updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> updatedState{props{navigateToHome = false}})
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING updatedState{props{navigateToHome = false}})