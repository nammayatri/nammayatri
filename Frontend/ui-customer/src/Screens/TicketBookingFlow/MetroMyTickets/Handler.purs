module Screens.TicketBookingFlow.MetroMyTickets.Handler where

import Prelude


import Engineering.Helpers.BackTrack 
import Prelude 
import Screens.TicketBookingFlow.MetroMyTickets.Controller 
import Control.Monad.Except.Trans 
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow
import ModifyScreenState 
import Screens.TicketBookingFlow.MetroMyTickets.View as MetroMyTickets
import Types.App 
import Screens.Types as ST


metroMyTicketsScreen :: FlowBT String METRO_MY_TICKETS_SCREEN_OUTPUT
metroMyTicketsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MetroMyTickets.screen state.metroMyTicketsScreen
  case action of
    GoToMetroTicketDetailsFlow bookingStatusApiResp -> do 
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_TICKET_DETAILS_FLOW bookingStatusApiResp)
    GoToMetroTicketStatusFlow bookingStatusApiResp -> do 
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_TICKET_STAUS_FLOW bookingStatusApiResp)
    _  -> App.BackT $ App.NoBack <$> (pure $ METRO_MY_TICKETS_SCREEN_OUTPUT_NO_OUTPUT)
