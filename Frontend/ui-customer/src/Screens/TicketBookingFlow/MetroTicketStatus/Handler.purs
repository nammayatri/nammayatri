{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketStatus.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.TicketBookingFlow.MetroTicketStatus.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.TicketBookingFlow.MetroTicketStatus.View as MetroTicketStatusView
import Types.App 
import Screens.TicketBookingFlow.MetroTicketStatus.ScreenData as MetroTicketStatusScreenData
import Screens.Types as ST

metroTicketStatusScreen :: FlowBT String METRO_TICKET_STATUS_SCREEN_OUTPUT
metroTicketStatusScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MetroTicketStatusView.screen state.metroTicketStatusScreen
  case action of
    GoBack ->  App.BackT $ pure App.GoBack 
    NoOutput -> pure NO_OUTPUT_METRO_TICKET_STATUS_SCREEN
    GoToMetroTicketDetails updatedState resp -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\_ ->updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_TICKET_DETAILS updatedState resp)
    RefreshPaymentStatus updatedState -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\_ ->updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_STATUS_AC updatedState)
    GoToTryAgainPayment updatedState -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\_ ->updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRY_AGAIN_PAYMENT updatedState)