module Screens.CustomerUtils.AboutUsScreen.ScreenData where

import Screens.Types (AboutUsScreenState)
import MerchantConfig.DefaultConfig as DC

initData :: AboutUsScreenState
initData = { 
    appConfig : DC.config
}