module Screens.CustomerUtils.AboutUsScreen.ScreenData where

import Screens.Types (AboutUsScreenState)
import ConfigProvider

initData :: AboutUsScreenState
initData = { 
    appConfig : getAppConfig appConfig,
    props : {
        enableDemoModeCount: 0,
        demoModePopup: false
    }
}
