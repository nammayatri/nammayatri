{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketInfoScreen.ScreenData
  ( dummyCategory
  , dummyServiceDetails
  , initData
  )
  where

import MerchantConfig.DefaultConfig as DC
import Screens.Types (TicketInfoScreenState(..), TicketBookingScreenStage(..), TicketBookings(..), TicketItem(..), TicketBookingPeopleCategoryDetails(..), TicketBookingCategoryDetails(..), TicketBookingServiceDetails(..))
import Data.Maybe (Maybe(..))
import Services.API (BookingStatus(..))

initData :: TicketInfoScreenState
initData = 
  { data : {
      -- selectedBookingInfo : {shortId : "", ticketPlaceId : "", ticketPlaceName : "", personId : "" , amount : 500.0, visitDate : "2023-10-23", status : Booked, services : [{ ticketServiceShortId : "", ticketServiceName : "VideoPhotography", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,peopleCategoryName: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,peopleCategoryName: "Mobile"}]}, { ticketServiceShortId : "afdj;jkja", ticketServiceName : "Entrance", amount : 100.0, status : "Confirmed", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,peopleCategoryName: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,peopleCategoryName: "Mobile"}]}, { ticketServiceShortId : "afdjasdf ;a", ticketServiceName : "Aquarium", amount : 100.0, status : "Pending", verificationCount : 0, expiryDate : Nothing,  prices : [{pricePerUnit: 2.0,numberOfUnits: 3,peopleCategoryName: "Adults"}, {pricePerUnit: 2.0,numberOfUnits: 2,peopleCategoryName: "Mobile"}]}] }
      selectedBookingInfo : {
          shortId : "String",
          ticketPlaceId : "ticketPlcaId",
          ticketPlaceName : "ticketPlaceName",
          personId : "personId",
          amount : 66.0,
          visitDate : "2023-11-30",
          status : Pending,
          services : [dummyServiceDetails, dummyServiceDetails2]
      }
  }
  , props : {
        activeListItem : dummyServiceDetails
      , activeIndex : 0
      , rightButtonDisable : false
      , leftButtonDisable : true
    }
  }

dummyServiceDetails :: TicketBookingServiceDetails
dummyServiceDetails =
  { amount : 55.0,
    status : "dkfj;a",
    verificationCount : 5,
    expiryDate : Just "akfjk;asd",
    ticketServiceName : "dkj;akl",
    categories : [dummyCategory, dummyCategory],
    ticketServiceShortId : "dkajf;a",
    slot : Just "13:00:00"
  }

dummyServiceDetails2 :: TicketBookingServiceDetails
dummyServiceDetails2 =
  { amount : 55.0,
    status : "dkfj;a",
    verificationCount : 5,
    expiryDate : Just "akfjk;asd",
    ticketServiceName : "dkj;ak asdfasfd  afasd l",
    categories : [dummyCategory, dummyCategory],
    ticketServiceShortId : "dkajf;a asdfasd ",
    slot : Just "13:00:00"
  }

dummyCategory :: TicketBookingCategoryDetails
dummyCategory =
  { amount : 53.0,
    bookedSeats : 5,
    name : "catName",
    peopleCategories : [pc, pc]
  }

pc :: TicketBookingPeopleCategoryDetails
pc =
 {  name : "Adult",
    numberOfUnits : 5,
    pricePerUnit : 55.0
 }