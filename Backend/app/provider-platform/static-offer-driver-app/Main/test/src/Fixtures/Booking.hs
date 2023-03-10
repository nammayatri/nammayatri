{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Fixtures.Booking where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.BookingLocation as Loc
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude
import qualified Fixtures.BaseUrl as Fixtures
import qualified Fixtures.Person as Fixtures
import qualified Fixtures.Time as Fixtures
import Kernel.Types.Id

defaultBooking :: SRB.Booking
defaultBooking = do
  let details =
        SRB.OneWayBookingDetails
          { toLocation = bookingStopLocation,
            estimatedDistance = 20000,
            estimatedFinishTime = Fixtures.defaultTime,
            estimatedDuration = 1200
          }
  SRB.Booking
    { id = Id "1",
      transactionId = "",
      status = SRB.CONFIRMED,
      providerId = Fixtures.defaultMerchantId,
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
      riderName = Just "John",
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }

defaultBookingLocation :: Loc.BookingLocation
defaultBookingLocation =
  Loc.BookingLocation
    { id = "1",
      lat = 9.96,
      lon = 9.96,
      address = defaultLocationAddress,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }

bookingStopLocation :: Loc.BookingLocation
bookingStopLocation =
  defaultBookingLocation
    { Loc.lat = 10.02,
      Loc.lon = 10.02
    }

defaultLocationAddress :: Loc.LocationAddress
defaultLocationAddress =
  Loc.LocationAddress
    { street = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      areaCode = Nothing,
      area = Nothing
    }
