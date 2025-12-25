module Screens.ScheduledRideAcceptedScreen.ScreenData where

import Prelude
import Prelude
import Screens.Types as S
import Services.API
import Common.Types.App as CTA
import Data.Maybe
import ConfigProvider
import MerchantConfig.Types (AppConfig)

type ScheduleRideAcceptedScreenState = {
  data :: ScheduleRideAcceptedScreenData,
  props :: ScheduleRideAcceptedScreenProps
}

type ScheduleRideAcceptedScreenData = {
  config :: AppConfig
}

type ScheduleRideAcceptedScreenProps ={

}

initData ::ScheduleRideAcceptedScreenState 
initData  = {
 data :{
  config : getAppConfig appConfig
 },
 props :{}
}


