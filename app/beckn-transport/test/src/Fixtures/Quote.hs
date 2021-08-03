module Fixtures.Quote where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.Id
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.SearchRequest as SearchRequest

defaultQuote :: Quote.Quote
defaultQuote =
  Quote.Quote
    { id = Id "1",
      requestId = Id "1",
      productId = Id "1",
      personId = Just $ Id "1",
      personUpdatedAt = Nothing,
      shortId = "",
      entityType = Quote.VEHICLE,
      entityId = Nothing,
      quantity = 0,
      actualPrice = Nothing,
      status = Quote.COMPLETED,
      startTime = Fixtures.defaultTime,
      endTime = Nothing,
      validTill = Fixtures.defaultTime,
      fromLocation = Nothing,
      toLocation = Nothing,
      providerId = "",
      distanceToNearestDriver = 0,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      info = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
