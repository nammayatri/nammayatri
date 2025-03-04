module Screens.MeterRideScreen.ScreenData where

import Screens.Types (MeterRideScreenState)

initData :: MeterRideScreenState
initData = {
    data : {
        distance : 0.0,
        timeMin : 0,
        timeSec : 0
    }
    , props : {
        meterFare : 0,
        showRateCard : false,
        startButtonCountDown : 5,
        sliderMaxValue : 70,
        sliderMinValue : 1,
        sliderDefVal : 10,
        incrementUnit : 1,
        sliderVal : 10,
        ratePerKM : 28,
        isMeterRideStarted : false
    }
}
