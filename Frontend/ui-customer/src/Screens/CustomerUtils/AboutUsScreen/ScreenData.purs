module Screens.CustomerUtils.AboutUsScreen.ScreenData where

import Screens.Types (AboutUsScreenState)
import Config.DefaultConfig as DC

initData :: AboutUsScreenState
initData = { 
    appConfig : DC.config
}