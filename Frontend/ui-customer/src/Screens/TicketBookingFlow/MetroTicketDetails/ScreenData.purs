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

initData :: MetroTicketDetailsScreenState
initData = {
  data : {
    dummyData : ""
  , metroRoute : dummyMetroRoute
  , ticketsInfo : []-- dummyTicketInfo
  , ticketType : ""
  , noOfTickets : 1
  }
, props :  {
    dummyProps : ""
  , stage : MetroTicketDetailsStage
  , currentTicketIndex : 0
  , previousScreenStage : MetroMyTicketsStage
  }
}


-- dummyTicketInfo :: Array MetroTicketInfo
-- dummyTicketInfo = 
--   [
--     {qrString : "Hello", ticketNumber: "12345", validUntil : "12:00 PM 5 May23", status : "ACTIVE" }
--   , {qrString : "world", ticketNumber: "12dasf5", validUntil : "12:00 PM 4 Jan 24", status : "ACTIVE" }
--   , {qrString : "fasd", ticketNumber: "adsfasd", validUntil : "3:15 PM 16 Dec 23", status : "ACTIVE" }
--   ]


dummyMetroRoute :: Array MetroRoute
dummyMetroRoute = 
  [
    {
      name : "Anna Nagar East"
    , line : GreenLine 
    , stops : [
        {name : "dummy stop 1"}
      , {name : "dummy stop 2"}
      , {name : "dummy stop 3"}
      ]
    , listExpanded : false
    },
    {
      name : "Anna Nagar East"
    , line : BlueLine 
    , stops : [
        {name : "dummy stop 1"}
      , {name : "dummy stop 3"}
      ]
    , listExpanded : false
    },
    {
      name : "Anna Nagar East"
    , line : RedLine 
    , stops : []
    , listExpanded : false
    }
  ]

