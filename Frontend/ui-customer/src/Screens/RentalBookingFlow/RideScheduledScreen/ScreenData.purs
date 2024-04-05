module Screens.RentalBookingFlow.RideScheduledScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (RideScheduledScreenState)
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)
import ConfigProvider
import Screens.Types (FareProductType(..)) as FPT

initData :: RideScheduledScreenState
initData =
  { data : { 
      primaryButtonText : ""
    , source : dummyLocationInfo
    , destination : Nothing
    , startTime : ""
    , startDate : ""
    , finalPrice : ""
    , baseDuration : ""
    , baseDistance : ""
    , bookingId : ""
    , cancellationReasons : []
    , config : getAppConfig appConfig
    , fareProductType : FPT.RENTAL
    , fromScreen : ""
    }
  , props : {
      isCancelRide : false
    , cancelRideActiveIndex : Nothing
    , cancelDescription : "" 
    , cancelReasonCode : "" 
    , driverAllocationTime : ""
    }
  }