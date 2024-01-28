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
import ModifyScreenState (modifyScreenState)

metroTicketBookingScreen :: FlowBT String METRO_TICKET_SCREEN_OUTPUT
metroTicketBookingScreen = do
    (GlobalState state) <- getState
    action <- lift $ lift $ runScreen $ MetroTicketBooking.screen state.metroTicketBookingScreen
    case action of
        GoBack updatedState -> do
            App.BackT $ pure App.GoBack
            -- App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN_FROM_METRO_TICKET updatedState)
        UpdateAction updatedState -> do
            void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ METRO_FARE_AND_PAYMENT updatedState)
        MyMetroTicketScreen -> do
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_MY_METRO_TICKET_SCREEN)
        GoToMetroRouteMap -> do
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_ROUTE_MAP)
        SelectSrcDest srcdest -> do
            App.BackT $ App.NoBack <$> (pure $ GO_TO_METRO_STATION_SEARCH srcdest)
        Refresh updatedState -> do
            void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ REFRESH_METRO_TICKET_SCREEN updatedState)