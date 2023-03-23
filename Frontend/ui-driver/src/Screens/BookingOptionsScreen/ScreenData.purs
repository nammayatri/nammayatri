module Screens.BookingOptionsScreen.ScreenData where

import Screens.Types (BookingOptionsScreenState)

initData :: BookingOptionsScreenState
initData = {
  data : {
    vehicleType : "",
    vehicleNumber : "",
    vehicleName : "",
    vehicleCapacity : ""
  },
  props: { }
}