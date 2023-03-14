module Screens.InvoiceScreen.ScreenData where

import Screens.Types (InvoiceScreenState)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)

initData :: InvoiceScreenState
initData = {
  data: {
    tripCharges : "",
    promotion : 0.0,
    gst : 0.0,
    totalAmount : "",
    date : "wed,10,45",
    selectedItem : dummyIndividualCard
  },
  props: {
    paymentMode : "Cash"
  , fromHomeScreen : false
  }
}