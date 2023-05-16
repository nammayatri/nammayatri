module SharedLogic.SimulatedFlow.Booking where

import qualified Domain.Types.Booking.BookingLocation as SLoc
import qualified Domain.Types.SearchRequest.SearchReqLocation as SSRL
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import SharedLogic.Booking

mkSimulatedBookingAPIDetails :: SSRL.SearchReqLocation -> HighPrecMeters -> BookingAPIDetails
mkSimulatedBookingAPIDetails toLocation distance = OneWayAPIDetails $ do
  OneWayBookingAPIDetails
    { toLocation = SLoc.makeSimulatedBookingLocationAPIEntity toLocation,
      estimatedDistance = distance
    }
