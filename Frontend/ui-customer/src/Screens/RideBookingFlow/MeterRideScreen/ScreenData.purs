module Screens.RideBookingFlow.MeterRideScreen.ScreenData where

import Prelude
import Screens.Types as ST

initData :: ST.MeterRideScreenState
initData = {
    data : {

    }
    , props : {
        showInfoCard : false
        , otp : {
            one : ""
            , two : ""
            , three : ""
            , four : ""
        }
        , invalidOTP : false
        , isOTPLoading : false
        , isFocussed : false
    }
}
