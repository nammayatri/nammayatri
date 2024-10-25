{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import LocalStorage.Cache (setInCache)
import Data.Function.Uncurried (runFn2)
import Storage (KeyStore(..))

metroTicketBookingScreen :: FlowBT String METRO_TICKET_SCREEN_OUTPUT
metroTicketBookingScreen = do
    (GlobalState state) <- getState
    action <- lift $ lift $ runScreen $ MetroTicketBooking.screen state.metroTicketBookingScreen
    let _ = runFn2 setInCache (show METRO_PAYMENT_SDK_POLLING) false
    case action of
        GoBack updatedState -> do
            App.BackT $ pure App.GoBack
        GoToHome -> do
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_METRO_TICKET)
        UpdateAction updatedState -> do
            void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ METRO_FARE_AND_PAYMENT updatedState)
        MyMetroTicketScreen -> do
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_MY_METRO_TICKET_SCREEN)
        GoToMetroRouteMap -> do
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_ROUTE_MAP)
        SelectSrcDest srcdest updatedState -> do
            void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ GO_TO_METRO_STATION_SEARCH srcdest updatedState)
        -- SearchRoute updatedState -> do
        --     void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> updatedState)
        --     App.BackT $ App.NoBack <$> (pure $ GO_TO_BUS_STATION_SEARCH updatedState)
        Refresh updatedState -> do
            void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> updatedState)
            App.BackT $ App.NoBack <$> (pure $ REFRESH_METRO_TICKET_SCREEN updatedState)
        GotoPaymentPage orderResp bookingId-> 
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_PAYMENT_PAGE orderResp bookingId)
        GotoPreviosStopScreen  updatedState-> 
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_PRIVIOUS_SEARCH_SCREEN updatedState)
        GotoSearchScreen updatedState->
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_SEARCH_SCREEN updatedState)
        GoToBusSearchScreen state -> 
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_BUS_SEARCH_SCREEN state)
        -- GET_ROUTES state -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_ROUTE_SEARCH_METRO_SCREEN state)
        AadhaarVerificationSO state offerType -> do
            void $ modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> state)
            App.BackT $ App.BackPoint <$> (pure $ GO_TO_AADHAAR_VERIFICATION_SCREEN state offerType)
        
