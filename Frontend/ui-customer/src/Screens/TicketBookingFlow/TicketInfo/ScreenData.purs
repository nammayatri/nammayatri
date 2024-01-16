{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketInfoScreen.ScreenData where

import ConfigProvider
import Screens.Types (TicketInfoScreenState(..), TicketBookingScreenStage(..), TicketBookings(..), TicketItem(..), TicketBookingPeopleCategoryDetails(..), TicketBookingCategoryDetails(..), TicketBookingServiceDetails(..))
import Data.Maybe (Maybe(..))
import Services.API (BookingStatus(..))

initData :: TicketInfoScreenState
initData = 
  { data : {
      selectedBookingInfo : {
          shortId : "String",
          ticketPlaceId : "ticketPlcaId",
          ticketPlaceName : "ticketPlaceName",
          personId : "personId",
          amount : 66.0,
          visitDate : "2023-11-30",
          status : Pending,
          services : []
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

dummyCategory :: TicketBookingCategoryDetails
dummyCategory =
  { amount : 53.0,
    bookedSeats : 5,
    name : "catName",
    peopleCategories : [peopleCategory, peopleCategory]
  }

peopleCategory :: TicketBookingPeopleCategoryDetails
peopleCategory =
 {  name : "Adult",
    numberOfUnits : 5,
    pricePerUnit : 55.0
 }