{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingScreen.ScreenData where

import Common.Types.App as Common
import Data.Maybe (Maybe(..))
import ConfigProvider
import Screens.Types (TicketBookingScreenState(..), TicketBookingScreenStage(..), TicketBookings(..), TicketItem(..))
import Services.API (BookingStatus(..))

initData :: TicketBookingScreenState
initData =
  { data:
      { placeInfo: Nothing
      , servicesInfo: []
      , servicesAvailing: []
      , shortOrderId: ""
      , dateOfVisit: ""
      , keyValArray: []
      , bookedForArray: []
      , zooName: ""
      , transactionId: ""
      , totalAmount: 0
      }
  , props:
      { currentStage: DescriptionStage
      , termsAndConditionsSelected: true
      , validDate: true
      , showShimmer: true
      , paymentStatus: Common.Scheduled
      , previousStage: DescriptionStage
      , ticketBookingList: dummyData
      , selectedBookingId: ""
      , selectedBookingInfo: { shortId: "", ticketPlaceId: "", ticketPlaceName: "", personId: "", amount: 500.0, visitDate: "2023-10-23", status: Booked, services: [ { ticketServiceShortId: "", ticketServiceName: "Videography", amount: 100.0, status: "Pending", verificationCount: 0, expiryDate: Nothing, prices: [ { pricePerUnit: 2.0, numberOfUnits: 3, attendeeType: "Adults" }, { pricePerUnit: 2.0, numberOfUnits: 2, attendeeType: "Mobile" } ] }, { ticketServiceShortId: "afdj;jkja", ticketServiceName: "Entrance", amount: 100.0, status: "Confirmed", verificationCount: 0, expiryDate: Nothing, prices: [ { pricePerUnit: 2.0, numberOfUnits: 3, attendeeType: "Adults" }, { pricePerUnit: 2.0, numberOfUnits: 2, attendeeType: "Mobile" } ] }, { ticketServiceShortId: "afdjasdf ;a", ticketServiceName: "Aquarium", amount: 100.0, status: "Pending", verificationCount: 0, expiryDate: Nothing, prices: [ { pricePerUnit: 2.0, numberOfUnits: 3, attendeeType: "Adults" }, { pricePerUnit: 2.0, numberOfUnits: 2, attendeeType: "Mobile" } ] } ] }
      , activeListItem: { ticketServiceShortId: "", ticketServiceName: "Videography", amount: 100.0, status: "Pending", verificationCount: 0, expiryDate: Nothing, prices: [ { pricePerUnit: 2.0, numberOfUnits: 3, attendeeType: "Adults" }, { pricePerUnit: 2.0, numberOfUnits: 2, attendeeType: "Mobile" } ] }
      , activeIndex: 0
      , rightButtonDisable: false
      , leftButtonDisable: true
      }
  }

dummyData :: TicketBookings
dummyData =
  { booked: [ { shortId: "kjdfk;a", ticketPlaceName: "Zoological Garden, AliPore", amount: 500.0, visitDate: "10-10-2023", status: Booked, ticketPlaceId: "", personId: "" } ]
  , pendingBooking: [ { shortId: "kjdfadfk;a", ticketPlaceName: "Zoological Garden, AliPore", amount: 500.0, visitDate: "10-10-2023", status: Pending, ticketPlaceId: "", personId: "" } ]
  }
