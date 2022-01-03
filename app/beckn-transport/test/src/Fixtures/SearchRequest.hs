module Fixtures.SearchRequest (defaultSearchRequest) where

import Beckn.Types.Id
import qualified Fixtures.Time as Fixtures
import Servant.Client
import qualified Types.Storage.SearchRequest as SearchRequest

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
      bapId = "",
      bapUri = BaseUrl {baseUrlScheme = Http, baseUrlHost = "localhost", baseUrlPort = 8013, baseUrlPath = ""},
      createdAt = Fixtures.defaultTime
    }
