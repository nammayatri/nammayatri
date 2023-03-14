module Screens.HomeScreen.ScreenData where

import Screens.Types
import Prelude(negate)
import Services.APITypes(Status(..))
import Data.Maybe

initData :: HomeScreenState
initData = {
    data: {
        driverName : "",
        vehicleType : "",
        activeRide : {
          id : "",
          source : "",
          destination : "",
          src_lat : 0.0,
          src_lon : 0.0, 
          dest_lat : 0.0,
          dest_lon : 0.0,
          actualRideDistance : 0.0,
          status : NOTHING,
          distance : 0.0,
          duration : 0,
          riderName : "",
          estimatedFare : 0,
          isDriverArrived : false,
          notifiedCustomer : false
        },
        cancelRideModal : {
          cancelRideReasons : [],
          activeIndex : Nothing,
          selectedReasonCode : "",
          selectedReasonDescription : "",
          isMandatoryTextHidden : false,
          isCancelButtonActive : false
        },
        currentDriverLat : 0.0,
        currentDriverLon : 0.0,
        locationLastUpdatedTime : "",
        totalRidesOfDay : 2,
        totalEarningsOfDay : 2,
        route : [],
        cancelRideConfirmationPopUp : {
          delayInSeconds : 5,
          timerID : "",
          continueEnabled : false,
          enableTimer : true
        }
    },
    props: {
        statusOnline : true,
        goOfflineModal : false,
        screenName : "Home",
        rideActionModal : false,
        enterOtpModal : false,
        rideOtp : "",
        enterOtpFocusIndex : 0,
        time : 0,
        otpIncorrect : false,
        endRidePopUp : false,
        cancelRideModalShow : false,
        routeVisible : false,
        otpAttemptsExceeded : false,
        refreshAnimation : false,
        showDottedRoute : true,
        currentStage : HomeScreen,
        mapRendered : false,
        cancelConfirmationPopup : false
    }
}

navData :: BottomNavBarState
navData = {
   activeIndex: 0,
   screenName : "driver homeScreen screen",
   navButton: [
    {
      activeIcon: "ny_ic_home_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_active.png",
      defaultIcon: "ny_ic_home_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_home_inactive.png",
      text: "Home"
    },
    {
      activeIcon: "ny_ic_cab_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_active.png",
      defaultIcon: "ny_ic_cab_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_cab_inactive.png",
      text: "Rides"
    },
    {
      activeIcon: "ny_ic_account_active,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_active.png",
      defaultIcon: "ny_ic_account_inactive,https://assets.juspay.in/nammayatri/images/driver/ny_ic_account_inactive.png",
      text: "Profile"
    }
  ]
}

