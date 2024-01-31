module Screens.RentalBookingFlow.RideScheduledScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (RideScheduledScreenState)
import Screens.SearchLocationScreen.ScreenData (dummyLocationInfo)

initData :: RideScheduledScreenState
initData =
  { primaryButtonText : ""
  , source : dummyLocationInfo
  , destination : Nothing
  , startTime : ""
  , startDate : ""
  , finalPrice : ""
  , baseDuration : ""
  , baseDistance : ""
  , driverAllocationTime : ""
  }