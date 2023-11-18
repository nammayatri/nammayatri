{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketInfoScreen.ScreenData where

import MerchantConfig.DefaultConfig as DC
import Screens.Types (TicketInfoScreenState(..), TicketBookingScreenStage(..), TicketBookings(..), TicketItem(..))
import Data.Maybe (Maybe(..))
import Services.API (BookingStatus(..))

initData :: TicketInfoScreenState
initData = 
  { data : {
      selectedBookingInfo : {shortId : "", ticketPlaceId : "", ticketPlaceName : "", personId : "" , amount : 500.0, visitDate : "2023-10-23", status : Booked, services : [{ ticketServiceShortId : "", ticketServiceName : "VideoPhotography", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,attendeeType: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,attendeeType: "Mobile"}]}, { ticketServiceShortId : "afdj;jkja", ticketServiceName : "Entrance", amount : 100.0, status : "Confirmed", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,attendeeType: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,attendeeType: "Mobile"}]}, { ticketServiceShortId : "afdjasdf ;a", ticketServiceName : "Aquarium", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,attendeeType: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,attendeeType: "Mobile"}]}] }
  }
  , props : {
        activeListItem : { ticketServiceShortId : "afdjasdf ;a", ticketServiceName : "VideoPhotography", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,attendeeType: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,attendeeType: "Mobile"}]}
      , activeIndex : 0
      , rightButtonDisable : false
      , leftButtonDisable : true
    }
  }