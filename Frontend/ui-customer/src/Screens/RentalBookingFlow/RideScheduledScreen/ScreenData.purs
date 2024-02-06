module Screens.RentalBookingFlow.RideScheduledScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (RideScheduledScreenState)
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)
import ConfigProvider

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
    , config : getAppConfig appConfig}
  , props : {
      isCancelRide : false
    , cancelRideActiveIndex : Nothing
    , cancelDescription : "" 
    , cancelReasonCode : "" 
    , driverAllocationTime : ""
    }
  }