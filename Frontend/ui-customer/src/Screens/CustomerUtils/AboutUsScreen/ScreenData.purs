module Screens.CustomerUtils.AboutUsScreen.ScreenData where

import Screens.Types (AboutUsScreenState)
import ConfigProvider
import Constants

initData :: AboutUsScreenState
initData = { 
    appConfig : getAppConfig appConfig
}