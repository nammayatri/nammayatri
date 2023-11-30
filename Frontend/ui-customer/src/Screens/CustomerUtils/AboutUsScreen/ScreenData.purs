module Screens.CustomerUtils.AboutUsScreen.ScreenData where

import Screens.Types (AboutUsScreenState)
import ConfigProvider

initData :: AboutUsScreenState
initData = { 
    appConfig : getAppConfig appConfig
}