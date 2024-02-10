module Screens.RentalBookingFlow.RideScheduledScreen.ScreenData where

import Common.Types.App (RideType(..))
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
    , config : getAppConfig appConfig
    , rideType : NORMAL_RIDE
    }
  , props : {
      isCancelRide : false
    , cancelRideActiveIndex : Nothing
    , cancelDescription : "" 
    , cancelReasonCode : "" 
    , driverAllocationTime : ""
    }
  }