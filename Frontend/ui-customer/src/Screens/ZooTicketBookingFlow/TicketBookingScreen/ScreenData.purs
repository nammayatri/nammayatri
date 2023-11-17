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
import MerchantConfig.DefaultConfig as DC
import Screens.Types (TicketBookingScreenState(..), TicketBookingScreenStage(..), TicketBookings(..), TicketItem(..))
import Data.Maybe (Maybe(..))
import Services.API (BookingStatus(..))

initData :: TicketBookingScreenState
initData = 
  { data : {
      placeInfo : Nothing,
      servicesInfo : Nothing,
      servicesAvailing : [],
      shortOrderId: "",
      dateOfVisit : "",
      keyValArray : 
        [ {key : "Date", val : "10th Nov, 23"}
        , {key : "Booking for", val : "dvkm"}
        , {key : "Total Paid", val : "₹300"}
        , {key : "BookingId", val : "1234345356"}
        , {key : "Transaction ID", val : "OR8192BY29213292149824UI"}
        , {key : "Valid until", val : "5 PM, 10th Nov 2023"}
        ],
      bookedForArray : ["Zoo", "Aquarium", "Video"],
      zooName : "Zoological Garden, Alipore",
      transactionId : "",
      zooEntry : {
        id : "b73378dc-427f-4efa-9b55-8efe7e3352c2",
        availed : true,
        adult : 0,
        child : 0,
        ticketPerAdult : 200,
        ticketPerChild : 50 
      },
      aquariumEntry : {
        id : "a7eba6ed-99f7-442f-a9d8-00c8b380657b",
        availed : false,
        adult : 0,
        child : 0,
        ticketPerAdult : 20,
        ticketPerChild : 10  
      },
      photoOrVideoGraphy : {
        id : "d8f47b42-50a5-4a97-8dda-e80a3633d7ab",
        availed : false,
        noOfDevices : 0,
        ticketPerDevice : 250
      },
      totalAmount : 0
  }
  , props : {
      currentStage : DescriptionStage,
      termsAndConditionsSelected : true,
      validDate : true,
      showShimmer : true,
      paymentStatus : Common.Success
      , ticketBookingList : dummyData
      , selectedBookingId : "adfadf"
      , selectedBookingInfo : {shortId : "afda", ticketPlaceId : "aksdfjl;a", ticketPlaceName : "aksdfj;la", personId : "" , amount : 500.0, visitDate : "2023-10-23", status : Booked, services : [{ ticketServiceShortId : "afdjasdf ;a", ticketServiceName : "VideoPhotography", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : []}, { ticketServiceShortId : "afdj;jkja", ticketServiceName : "Entrance", amount : 100.0, status : "Confirmed", verificationCount : 0, expiryDate : Nothing,  prices : []}] }
      , activeListItem : { ticketServiceShortId : "afdjasdf ;a", ticketServiceName : "VideoPhotography", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : []}
      , activeIndex : 0
      , rightButtonDisable : false
      , leftButtonDisable : true
    }
  }

dummyData :: TicketBookings
dummyData = { booked : [{shortId : "kjdfk;a", ticketPlaceName : "Zoological Garden, AliPore", amount : 500.0, visitDate : "10-10-2023", status : Booked, ticketPlaceId : "", personId : "" }],
              pendingBooking : [{shortId : "kjdfadfk;a", ticketPlaceName : "Zoological Garden, AliPore", amount : 500.0, visitDate : "10-10-2023", status : Pending, ticketPlaceId : "", personId : ""}]
            }