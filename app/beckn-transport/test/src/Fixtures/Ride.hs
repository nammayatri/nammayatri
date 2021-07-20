module Fixtures.Ride where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.Id
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.SearchRequest as SearchRequest

defaultRide :: Ride.Ride
defaultRide =
  Ride.Ride
    { id = Id "1",
      requestId = Id "1",
      productId = Id "1",
      personId = Just $ Id "1",
      personUpdatedAt = Nothing,
      shortId = "",
      entityType = Ride.VEHICLE,
      entityId = Nothing,
      quantity = 0,
      price = Just . Amount.Amount $ 100 % 1,
      actualPrice = Nothing,
      status = Ride.COMPLETED,
      startTime = Fixtures.defaultTime,
      endTime = Nothing,
      validTill = Fixtures.defaultTime,
      fromLocation = Nothing,
      toLocation = Nothing,
      organizationId = "",
      quoteId = "",
      distance = 0,
      udf1 = Nothing,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      info = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
