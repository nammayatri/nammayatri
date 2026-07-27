module Screens.OnBoardingFlow.PermissionScreen.ScreenData where

import Screens.Types (PermissionScreenState, PermissionScreenStage(..))
import ConfigProvider
import Foreign.Object (empty)

initData :: PermissionScreenState
initData = { 
    appConfig : getAppConfig appConfig,
    logField : empty,
    stage : NORMAL
}