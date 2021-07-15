module Fixtures.SearchRequest (defaultSearchRequest) where

import Beckn.Types.Id
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.SearchRequest as SearchRequest

defaultSearchRequest :: SearchRequest.SearchRequest
defaultSearchRequest =
  SearchRequest.SearchRequest
    { id = Id "1",
      name = Nothing,
      description = Nothing,
      shortId = "",
      industry = SearchRequest.MOBILITY,
      _type = SearchRequest.RIDESEARCH,
      exchangeType = SearchRequest.FULFILLMENT,
      status = SearchRequest.COMPLETED,
      startTime = Fixtures.defaultTime,
      endTime = Nothing,
      validTill = Fixtures.defaultTime,
      provider = Nothing,
      providerType = Nothing,
      requestor = Nothing,
      requestorType = Nothing,
      fromLocationId = "",
      toLocationId = "",
      udf1 = Nothing,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      info = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
