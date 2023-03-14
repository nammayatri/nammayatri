module Screens.HelpAndSupportScreen.ScreenData where

import Screens.Types (HelpAndSupportScreenState)

initData :: HelpAndSupportScreenState
initData = {
  data: {
    source : "",
    destination : "",
    date : "",
    time : "",
    rating : 0,
    driverName : "",
    totalAmount : "",
    isNull : true,
    status : "",
    rideStartTime : "",
    rideEndTime : "",
    vehicleNumber : "",
    rideId : "",
    tripId : "",
    bookingId : ""
  },
  props:{
    apiFailure : false
  , isCallConfirmation : false
  }

}
