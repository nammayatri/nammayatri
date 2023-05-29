module Screens.OnBoardingFlow.PermissionScreen.ScreenData where

import Screens.Types (PermissionScreenState)
import Config.DefaultConfig as DC

initData :: PermissionScreenState
initData = { 
    appConfig : DC.config
}