module Screens.TripDetailsScreen.ScreenData where

import Screens.Types (TripDetailsScreenState, PaymentMode(..))


initData :: TripDetailsScreenState
initData = {
  data: {
      tripId : "",
      distance: "",
      message : "",
      timeTaken : "",
      rider : "",
      date : "",
      time : "" ,
      source : "" ,
      destination : "",
      totalAmount : 0,
      paymentMode : CASH,
      status : ""
  },
  props: {
    rating : 4,
    reportIssue : false,
    issueReported : false
  }
}

