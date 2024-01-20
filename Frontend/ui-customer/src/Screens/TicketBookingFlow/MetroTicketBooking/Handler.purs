module Screens.TicketBookingFlow.MetroTicketBooking.Handler where

import Prelude

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.TicketBookingFlow.MetroTicketBooking.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.TicketBookingFlow.MetroTicketBooking.View as MetroTicketBooking
import Types.App

metroTicketBookingScreen :: FlowBT String METRO_TICKET_SCREEN_OUTPUT
metroTicketBookingScreen = do
    (GlobalState state) <- getState
    action <- lift $ lift $ runScreen $ MetroTicketBooking.screen state.metroTicketBookingScreen
    case action of
        GoBack updatedState -> do
            App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_METRO_TICKET updatedState)
        UpdateAction updatedState -> do
            App.BackT $ App.NoBack <$> (pure $ GO_TO_METRO_STATION_SEARCH updatedState)
        MyMetroTicketScreen updatedState -> do
            App.BackT $ App.NoBack <$> (pure $ GO_TO_MY_METRO_TICKET_SCREEN updatedState)
        GoToMetroRouteMap -> do
            App.BackT $ App.NoBack <$> (pure $ GO_TO_METRO_ROUTE_MAP)