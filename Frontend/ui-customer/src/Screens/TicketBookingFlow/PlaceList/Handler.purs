{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.PlaceList.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.TicketBookingFlow.PlaceList.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.TicketBookingFlow.PlaceList.View as TicketingScreen
import Types.App (FlowBT, GlobalState(..), TICKETING_SCREEN_SCREEN_OUTPUT(..),ScreenType(..))
import ModifyScreenState (modifyScreenState)
import Data.Maybe (Maybe(..))

placeListScreen :: FlowBT String TICKETING_SCREEN_SCREEN_OUTPUT
placeListScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ TicketingScreen.screen state.ticketingScreen
  case action of
    ExitToHomeScreen updatedState -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ EXIT_TO_HOME updatedState)
    ExitToMyTicketsScreen updatedState -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EXIT_TO_MY_TICKETS updatedState)
    BookTickets updatedState selectedPlace -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { data { totalAmount = 0, placeInfo = Just selectedPlace}})
      App.BackT $ App.BackPoint <$> (pure $ BOOK_TICKETS updatedState)
    _ -> App.BackT $ pure App.GoBack 
