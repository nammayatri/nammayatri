module Screens.TripDetailsScreen.ScreenData where

import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Screens.Types (TripDetailsScreenState, PaymentMode(..))


initData :: TripDetailsScreenState
initData = {
  data: {
      message : "",
      driverName : "",
      date : "",
      time : "" ,
      source : "",
      destination : "",
      totalAmount : "",
      paymentMode : CASH,
      rating : 0,
      tripId : "",
      selectedItem : dummyIndividualCard
  },
  props: {
    reportIssue : true,
    issueReported : false,
    activateSubmit : false,
    fromMyRides : false,
    showConfirmationPopUp : false,
    canConnectWithDriver : true
  }
}

