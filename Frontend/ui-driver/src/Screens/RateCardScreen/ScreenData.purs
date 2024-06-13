
module Screens.RateCardScreen.ScreenData where

import Prelude
import Screens.Types(RateCardScreenState)
import ConfigProvider
import Screens.BookingOptionsScreen.ScreenData as BOP
import MerchantConfig.DefaultConfig as DC
import Resource.Constants as RC

initData :: RateCardScreenState
initData = { 
    data: { 
        ridePreferences : [],
        rateCard : BOP.dummyRateCard,
        cityConfig : DC.dummyCityConfig
     }, 
    props: { 
        sliderVal : RC.defaultSliderDist,
        showRateCard : false,
        sliderDefVal : RC.defaultSliderDist,
        incrementUnit : 1,
        sliderMinValue :1,
        sliderMaxValue : 50,
        sliderLoading : false
     }
    }