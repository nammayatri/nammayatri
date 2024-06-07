
module Screens.RateCardScreen.ScreenData where

import Prelude
import Screens.Types(RateCardScreenState)
import ConfigProvider
import Screens.BookingOptionsScreen.ScreenData as BOP

initData :: RateCardScreenState
initData = { 
    data: { 
        ridePreferences : [],
        rateCard : BOP.dummyRateCard
     }, 
    props: { 
        sliderVal : 4,
        showRateCard : false,
        sliderDefVal : 4,
        incrementUnit : 1,
        sliderMinValue :1,
        sliderMaxValue : 50
     }
    }