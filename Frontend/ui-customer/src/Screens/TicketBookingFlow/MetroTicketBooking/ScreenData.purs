module Screens.TicketBookingFlow.MetroTicketBooking.ScreenData where

import Prelude
import Screens.Types as ST

initData :: ST.MetroTicketBookingScreenState
initData = {
  data: {
    ticketType : ST.ONE_WAY
  , ticketCount : 1
  },
  props: {
    isLimitExceeded : false
    , termsAndConditionsSelected : true
  }
}