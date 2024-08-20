module Screens.CancellationRateScreen.ScreenData where

import Screens.Types(CancellationRateScreenState)
import ConfigProvider (getAppConfig, appConfig)
import Data.Maybe

initData :: CancellationRateScreenState
initData = {
  appConfig : getAppConfig appConfig,
  data:  { 
    cancellationRate : 69,
    assignedRides : 100,
    cancelledRides : 69,
    cancellationWindow : Nothing,
    missedEarnings : 123456
  }
}
