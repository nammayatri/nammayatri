module Screens.RideDetailScreen.ScreenData where


import Screens.Types (RideDetailScreenState)

initData :: RideDetailScreenState
initData = {
  data: {
    sourceAddress : 
    { place : "",
      lat : 0.0,
      lon : 0.0
    },
    destAddress : 
    { place : "",
      lat : 0.0,
      lon : 0.0
    },
    rideStartTime : "",
    rideEndTime : "",
    bookingDateAndTime : "",
    totalAmount : 0,
    customerName : ""
  },
  props: { cashCollectedButton : false }
}
