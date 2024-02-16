module Screens.TicketBookingFlow.PlaceDetails.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketBookingFlow.PlaceDetails.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingFlow.PlaceDetails.View as PlaceDetails
import Types.App (FlowBT, GlobalState(..), ScreenType(..), TICKET_BOOKING_SCREEN_OUTPUT(..))
import Screens.TicketBookingFlow.PlaceDetails.ScreenData (initData) as PlaceDetailsScreenData
import Screens.Types as ST

placeDetailsScreen :: FlowBT String TICKET_BOOKING_SCREEN_OUTPUT
placeDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ PlaceDetails.screen state.ticketBookingScreen
  case action of
    GoToHomeScreen updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> PlaceDetailsScreenData.initData)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING updatedState)
    GoToTicketPayment state -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      App.BackT $ App.NoBack <$> (pure (GO_TO_TICKET_PAYMENT state))
    GoToOpenGoogleMaps state lat2 long2 -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      App.BackT $ App.BackPoint <$> (pure (GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW lat2 long2))
    BookTickets updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> updatedState{props{navigateToHome = false}})
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING updatedState{props{navigateToHome = false}})