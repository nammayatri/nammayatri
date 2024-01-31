{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketDetails.Handler where

import Engineering.Helpers.BackTrack 
import Prelude 
import Screens.TicketBookingFlow.MetroTicketDetails.Controller 
import Control.Monad.Except.Trans 
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow
import ModifyScreenState 
import Screens.TicketBookingFlow.MetroTicketDetails.View as MetroTicketDetails
import Types.App 
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as PlaceDetailsScreenData
import Screens.Types as ST


metroTicketDetailsScreen :: FlowBT String METRO_TICKET_DETAILS_SCREEN_OUTPUT
metroTicketDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MetroTicketDetails.screen state.metroTicketDetailsScreen
  case action of
    GoBack ->  App.BackT $ pure App.GoBack 
    BackToSearchMetroLocation -> App.BackT $ App.NoBack <$> (pure $ BACK_TO_SEARCH_METRO_LOCATION)
    _  -> App.BackT $ App.NoBack <$> (pure $ METRO_TICKET_DETAILS_SCREEN_OUTPUT_NO_OUTPUT)
