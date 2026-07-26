{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketStatus.ScreenData where

import Common.Types.App as Common
import Data.Maybe 
import ConfigProvider
import Screens.Types 
import Services.API 
import Domain.Payments as PP
import Foreign.Object (empty)

initData :: MetroTicketStatusScreenState
initData = 
  { 
    data : { 
      shortOrderId: "",
      keyValArray : [],
      validUntil : "",
      bookingId : "",
      resp : dummyMetroBookingStatus,
      timerId : "",
      quoteId : "",
      logField : empty
  }
  , props : {
      showShimmer : true,
      paymentStatus : PP.Success,
      entryPoint : HomescreenToMetroTicketStatus
    }
  }

dummyMetroBookingStatus :: FRFSTicketBookingStatusAPIRes
dummyMetroBookingStatus = 
  FRFSTicketBookingStatusAPIRes {
    _type: "SingleJourney",
    createdAt : "",
    bookingId : "",
    city : "",
    updatedAt : "",
    payment : Nothing,
    price: 1.0,
    quantity: 2,
    stations: [],
    status: "CONFIRMED",
    tickets: [],
    vehicleType: "Metro_",
    validTill: "2024-01-26T19:37:12.516Z",
    routeStations : Nothing
    }