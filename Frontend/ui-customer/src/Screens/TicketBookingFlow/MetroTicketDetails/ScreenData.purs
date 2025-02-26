{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroTicketDetails.ScreenData where

import Prelude

import Common.Types.App as Common
import ConfigProvider
import Screens.Types
import Data.Maybe (Maybe(..))

initData :: MetroTicketDetailsScreenState
initData = {
  data : {
    dummyData : ""
  , bookingId : ""
  , city : Common.AnyCity
  , bookingUpdatedAt : ""
  , metroRoute : []
  , ticketsInfo : []
  , ticketType : ""
  , noOfTickets : 0
  , ticketPrice : 0.0
  , vehicleType : ""
  , route : Nothing
  , transactionId : ""
  }
, props :  {
    dummyProps : ""
  , showLoader : false
  , stage : MetroTicketDetailsStage
  , currentTicketIndex : 0
  , previousScreenStage : MetroMyTicketsStage 
  , isBookingCancellable : Nothing
  , cancellationCharges : Nothing
  , refundAmount : Nothing
  , fromScreen : Nothing
  , paymentDetailsExpanded : false
  }
}