module Screens.BookingOptionsScreen.ScreenData where

import Screens.Types (BookingOptionsScreenState)

initData :: BookingOptionsScreenState
initData = {
  data : {
    vehicleType : "",
    vehicleNumber : "",
    vehicleName : "",
    vehicleCapacity : 0,
    downgradeOptions : []
  },
  props: { 
    isBtnActive : false,
    downgraded : false
  }
}