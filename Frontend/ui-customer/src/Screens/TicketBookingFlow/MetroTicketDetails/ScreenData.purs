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
  , ticketsInfo : dummyTicketInfo
  }
, props :  {
    dummyProps : ""
  , stage : MetroTicketDetailsStage
  , currentTicketIndex : 0
  }
}


dummyTicketInfo :: Array MetroTicketInfo
dummyTicketInfo = 
  [
    {qrString : "Hello", ticketNumber: "12345", validUntil : "12:00 PM 5 May23", status : "ACTIVE" }
  , {qrString : "world", ticketNumber: "12dasf5", validUntil : "12:00 PM 4 Jan 24", status : "ACTIVE" }
  , {qrString : "fasd", ticketNumber: "adsfasd", validUntil : "3:15 PM 16 Dec 23", status : "ACTIVE" }
  ]


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














-- module Screens.TicketBookingFlow.MetroTicketDetails.ScreenData where

-- import Prelude

-- import Common.Types.App as Common
-- import ConfigProvider
-- import Screens.Types 



-- initData :: MetroTicketDetailsScreenState
-- initData = {
--   data : {
--     dummyData : ""
--   }
-- , props :  {
--     dummyProps : ""
--   }
-- }