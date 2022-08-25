module Fixtures.Booking where

import Beckn.Types.Id
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.BookingLocation as Loc
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude
import qualified Fixtures.BaseUrl as Fixtures
import qualified Fixtures.Time as Fixtures

defaultBooking :: SRB.Booking
defaultBooking = do
  let details =
        SRB.OneWayBookingDetails
          { toLocation = defaultBookingLocation,
            estimatedDistance = 0,
            estimatedFinishTime = Fixtures.defaultTime
          }
  SRB.Booking
    { id = Id "1",
      status = SRB.CONFIRMED,
      providerId = Id "",
      bapId = "",
      bapUri = Fixtures.defaultUrl,
      startTime = Fixtures.defaultTime,
      riderId = Just $ Id "",
      fromLocation = defaultBookingLocation,
      vehicleVariant = Veh.SUV,
      estimatedFare = 0,
      discount = Nothing,
      estimatedTotalFare = 0,
      reallocationsCount = 0,
      bookingDetails = SRB.OneWayDetails details,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }

defaultBookingLocation :: Loc.BookingLocation
defaultBookingLocation =
  Loc.BookingLocation
    { id = "1",
      lat = 0.0,
      lon = 0.0,
      address = defaultLocationAddress,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }

defaultLocationAddress :: Loc.LocationAddress
defaultLocationAddress =
  Loc.LocationAddress
    { street = Nothing,
      door = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      areaCode = Nothing,
      area = Nothing
    }
