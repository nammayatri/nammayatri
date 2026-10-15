module Screens.CancellationRateScreen.ScreenData where

import Screens.Types(CancellationRateScreenState)
import ConfigProvider (getAppConfig, appConfig)
import Data.Maybe
import ConfigProvider

initData :: CancellationRateScreenState
initData = {
  appConfig : getAppConfig appConfig,
  data:  { 
    cancellationRate : 0,
    assignedRides : 10,
    cancelledRides : 0,
    cancellationWindow : Nothing,
    missedEarnings : 0
  }
}
