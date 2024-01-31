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

initData :: MetroTicketStatusScreenState
initData = 
  { 
    data : { 
      shortOrderId: "",
      keyValArray : [],
      ticketName : "",
      validUntil : "",
      bookingId : "",
      resp : dummyMetroBookingStatus,
      timerId : "",
      quoteId : ""
  }
  , props : {
      showShimmer : true,
      paymentStatus : Common.Success  
    }
  }

dummyMetroBookingStatus :: MetroTicketBookingStatus
dummyMetroBookingStatus = 
  MetroTicketBookingStatus {
    _type: "SingleJourney",
    createdAt : "",
    bookingId : "d663387c-b2b1-4e1b-9dd5-269c777fe5c1",
    payment : Nothing,
    price: 1,
    quantity: 2,
    stations: [],
    status: "CONFIRMED",
    tickets: [
        FRFSTicketAPI {
          status : "ACTIVE"
        , qrData : "Namma Yatri"
        , validTill : "30 Jan 2024, 22:00"
        , ticketNumber : "JASF98234324"
        }
      , FRFSTicketAPI {
          status : "ACTIVE"
        , qrData : "Namma Yatri PARTNER"
        , validTill : "30 Jan 2024, 22:00"
        , ticketNumber : "324F98223984923"
        }
      , FRFSTicketAPI {
          status : "ACTIVE"
        , qrData : "Namma Yatri CHENNAI"
        , validTill : "30 Jan 2024, 22:00"
        , ticketNumber : "KJQEDF98234324"
        }
    ],
    vehicleType: "Metro_",
    validTill: "2024-01-26T19:37:12.516Z"
    }