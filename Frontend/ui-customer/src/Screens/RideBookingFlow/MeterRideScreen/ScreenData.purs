module Screens.RideBookingFlow.MeterRideScreen.ScreenData where

import Prelude
import Data.Maybe(Maybe(..))
import Screens.Types as ST

initData :: ST.MeterRideScreenState
initData = {
    data : {
        isMeterRideSynced : false,
        distance : 0.0,
        timeSec : 0,
        destinationAddress : "",
        destinationLat : 0.0,
        destinationLng : 0.0,
        rateCard : Nothing,
        lastUpdatedTime: ""
    }
    , props : {
        showInfoCard : false
        , otp : ""
        , invalidOTP : false
        , isOTPLoading : false
        , isFocussed : false
        , rateCardConfig : {
            sliderMaxValue : 70,
            sliderMinValue : 1,
            sliderDefVal : 10,
            incrementUnit : 1,
            sliderVal : 10,
            ratePerKM : 0.0,
            sliderFare : 280
        }
        , showRateCard : false
        , isRateCardLoading : false
        , isMeterClockRunning : false
        , refreshAnimation : false
        , meterFare : 0
    }
}
