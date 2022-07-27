module Fixtures.SearchRequest (defaultSearchRequest) where

import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Servant.Client

defaultSearchRequest :: SearchRequest.SearchRequest
defaultSearchRequest =
  SearchRequest.SearchRequest
    { id = Id "1",
      messageId = "",
      startTime = Fixtures.defaultTime,
      validTill = Fixtures.defaultTime,
      providerId = Id "",
      fromLocationId = "",
      toLocationId = Just "",
      bapId = "",
      bapUri = BaseUrl {baseUrlScheme = Http, baseUrlHost = "localhost", baseUrlPort = 8013, baseUrlPath = ""},
      createdAt = Fixtures.defaultTime
    }
