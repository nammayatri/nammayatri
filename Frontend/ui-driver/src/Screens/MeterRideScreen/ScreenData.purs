module Screens.MeterRideScreen.ScreenData where

import Services.API (RidesInfo(..))
import Screens.Types (MeterRideScreenState)
import Data.Maybe
import Prelude

initData :: MeterRideScreenState
initData = {
    data : {
        distance : 0.0,
        timeSec : 0,
        destinationAddress : "",
        destinationLat : 0.0,
        destinationLng : 0.0,
        ridesInfo : Nothing,
        rateCard : Nothing,
        lastUpdatedTime: ""
    }
    , props : {
        meterFare : 0,
        showRateCard : false,
        startButtonCountDown : 5,
        rateCardConfig : {
            sliderMaxValue : 70,
            sliderMinValue : 1,
            sliderDefVal : 10,
            incrementUnit : 1,
            sliderVal : 10,
            ratePerKM : 0.0,
            sliderFare : 280
        },
        isMeterRideStarted : false,
        isMeterClockRunning : false,
        confirmMeterRideStop : false,
        isRateCardLoading : false,
        refreshAnimation : false,
        rideStartingLoader : false
    }
}
