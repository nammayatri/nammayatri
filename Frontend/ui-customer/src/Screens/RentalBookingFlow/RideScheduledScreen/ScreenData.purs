module Screens.RentalBookingFlow.RideScheduledScreen.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types (RideScheduledScreenState)

initData :: RideScheduledScreenState
initData =
  { primaryButtonText : ""
  , source : ""
  , destination : Nothing
  , startTime : ""
  , startDate : ""
  , finalPrice : ""
  , baseDuration : ""
  , baseDistance : ""
  , driverAllocationTime : ""
  }