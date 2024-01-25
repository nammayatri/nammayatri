module Screens.TicketBookingFlow.MetroMyTickets.ScreenData where

import Prelude



import Common.Types.App as Common
import ConfigProvider
import Screens.Types 



initData :: MetroMyTicketsScreenState
initData = {
  data : {
      activeTickets : []
    , pastTickets : []
    }
  , props :  {
      dummyProps : ""
    , showShimmer : true
    }
  }

-- type MetroTicketCardData = {
--   sourceName :: String
--   , destinationName :: String
--   , createdAt :: String
--   , noOfTickets :: Int
--   , metroTicketStatusApiResp :: MetroTicketBookingStatus
-- }