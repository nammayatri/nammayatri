module Screens.OnBoardingFlow.PermissionScreen.ScreenData where

import Screens.Types (PermissionScreenState)
import Config.DefaultConfig as DC
import Foreign.Object (empty)

initData :: PermissionScreenState
initData = { 
    appConfig : DC.config,
    logField : empty
}