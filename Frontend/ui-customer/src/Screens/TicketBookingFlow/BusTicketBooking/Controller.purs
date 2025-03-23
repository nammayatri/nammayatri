{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.BusTicketBooking.Controller where

import Common.Types.App as App
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PopUpModal.Controller as PopUpModalController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Screens.TicketBookingFlow.MetroMyTickets.ScreenData as MetroMyTicketsScreenData
import Screens.TicketBookingFlow.MetroMyTickets.Transformer (metroTicketListApiToMyTicketsTransformer)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Constants.Configs (getPolylineAnimationConfig)
import Data.Array as DA
import Data.Maybe( Maybe(..), fromMaybe, maybe)
import Data.Tuple
import Debug (spy)
import Effect.Aff (launchAff)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Helpers.Utils as HU
import Prelude
import PrestoDOM (class Loggable, Eval, update, continue, exit, continueWithCmd, updateAndExit)
import Services.API as API
import Screens.Types as ST
import Services.Backend as Remote
import Types.App (GlobalState(..), defaultGlobalState, FlowBT, ScreenType(..))
import Helpers.Utils (emitTerminateApp, isParentView)
import Common.Types.App (LazyCheck(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action 
  = NoAction
  | EditAddress Boolean
  | GoBack
  | GenericHeaderAC GenericHeaderController.Action
  | SearchButtonClick
  | TicketIconClicked
  | SourceToDestinationAC SourceToDestinationController.Action
  | BusTicketBookingListRespAC (Array API.FRFSTicketBookingStatusAPIRes)
  | TicketPressed API.FRFSTicketBookingStatusAPIRes
  | RepeatRideClicked API.FRFSTicketBookingStatusAPIRes
  | ViewMoreClicked

data ScreenOutput
  = GoToHomeScreen ST.BusTicketBookingState
  | RefreshScreen ST.BusTicketBookingState
  | GoToMyTicketsScreen ST.BusTicketBookingState
  | GoToChooseYourRide ST.BusTicketBookingState
  | GoToConfirmgDelivery ST.BusTicketBookingState
  | GoToSearchLocationScreenForRoutes ST.BusTicketBookingState ST.LocationActionId
  | GoToMetroTicketDetailsFlow String
  | GoToMetroTicketDetailsScreen API.FRFSTicketBookingStatusAPIRes

eval :: Action -> ST.BusTicketBookingState -> Eval Action ScreenOutput ST.BusTicketBookingState

eval GoBack state =
  if isParentView FunctionCall 
    then do 
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else exit $ GoToHomeScreen state

eval SearchButtonClick state = updateAndExit state $ GoToSearchLocationScreenForRoutes state ST.Src

eval (BusTicketBookingListRespAC bookingList) state = 
  continue $ state {data {ticketDetailsState = Just $ metroTicketListApiToMyTicketsTransformer bookingList $ fromMaybe MetroMyTicketsScreenData.initData state.data.ticketDetailsState}}

eval TicketIconClicked state = updateAndExit state $ GoToMyTicketsScreen state

eval (TicketPressed (API.FRFSTicketBookingStatusAPIRes ticketApiResp)) state = do 
  updateAndExit state $ GoToMetroTicketDetailsFlow ticketApiResp.bookingId

eval (RepeatRideClicked ticketApiResp) state = 
  updateAndExit state $ GoToMetroTicketDetailsScreen ticketApiResp

eval (ViewMoreClicked) state =
  continue state { props { showAllTickets = not state.props.showAllTickets }}

eval _ state = continue state