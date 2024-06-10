
module Screens.RateCardScreen.ScreenData where

import Prelude
import ConfigProvider
import Screens.Types(RateCardScreenState)
import ConfigProvider (getAppConfig, appConfig)
import Screens.BookingOptionsScreen.ScreenData as BOP
import MerchantConfig.DefaultConfig (defaultCityConfig)
import Resource.Constants as RC
import MerchantConfig.DefaultConfig as DC

initData :: RateCardScreenState
initData = { 
    data: { 
        ridePreferences : [],
        rateCard : BOP.dummyRateCard,
        cityConfig : defaultCityConfig,
        config : getAppConfig appConfig
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