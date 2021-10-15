module Fixtures.RideBooking where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.Id
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchRequest as SearchRequest
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
      fromLocationId = Id "",
      toLocationId = Id "",
      vehicleVariant = Veh.SUV,
      price = 0,
      estimatedTotalFare = 0,
      distance = 0,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
