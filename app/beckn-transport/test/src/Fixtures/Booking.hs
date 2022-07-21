module Fixtures.Booking where

import Beckn.Types.Id
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude
import qualified Fixtures.BaseUrl as Fixtures
import qualified Fixtures.Time as Fixtures

defaultBooking :: SRB.Booking
defaultBooking = do
  let details =
        SRB.OneWayBookingDetails
          { toLocationId = Id "",
            estimatedDistance = 0
          }
  SRB.Booking
    { id = Id "1",
      status = SRB.CONFIRMED,
      providerId = Id "",
      bapId = "",
      bapUri = Fixtures.defaultUrl,
      startTime = Fixtures.defaultTime,
      riderId = Just $ Id "",
      fromLocationId = Id "",
      vehicleVariant = Veh.SUV,
      estimatedFare = 0,
      discount = Nothing,
      estimatedTotalFare = 0,
      reallocationsCount = 0,
      bookingDetails = SRB.OneWayDetails details,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
