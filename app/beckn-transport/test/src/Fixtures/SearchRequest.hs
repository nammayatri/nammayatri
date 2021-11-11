module Fixtures.SearchRequest (defaultSearchRequest) where

import Beckn.Types.Id
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as Veh

defaultSearchRequest :: SearchRequest.SearchRequest
defaultSearchRequest =
  SearchRequest.SearchRequest
    { id = Id "1",
      transactionId = "",
      startTime = Fixtures.defaultTime,
      validTill = Fixtures.defaultTime,
      providerId = Id "",
      requestorId = Id "",
      fromLocationId = "",
      toLocationId = "",
      vehicleVariant = Just Veh.SUV,
      bapId = "",
      bapUri = "",
      createdAt = Fixtures.defaultTime
    }
