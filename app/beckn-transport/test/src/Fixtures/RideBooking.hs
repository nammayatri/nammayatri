module Fixtures.RideBooking where

import Beckn.External.Encryption (Encrypted (Encrypted))
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.Vehicle as Veh

defaultRideBooking :: SRB.RideBooking
defaultRideBooking =
  SRB.RideBooking
    { id = Id "1",
      transactionId = "",
      requestId = Id "1",
      quoteId = "1",
      status = SRB.CONFIRMED,
      providerId = Id "",
      bapId = "",
      startTime = Fixtures.defaultTime,
      requestorId = Id "",
      requestorMobileNumber = Encrypted "",
      fromLocationId = Id "",
      toLocationId = Id "",
      vehicleVariant = Veh.SUV,
      estimatedFare = 0,
      discount = Nothing,
      estimatedTotalFare = 0,
      distance = 0,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
