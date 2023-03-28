{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
        cancelConfirmationPopup : false,
        zoneRideBooking : true
    }
}