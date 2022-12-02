module Fixtures.SearchRequest (defaultSearchRequest) where

import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as Loc
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
      fromLocation = defaultSearchReqLocation,
      toLocation = Just defaultSearchReqLocation,
      bapId = "",
      bapUri = BaseUrl {baseUrlScheme = Http, baseUrlHost = "localhost", baseUrlPort = 8013, baseUrlPath = ""},
      createdAt = Fixtures.defaultTime
    }

defaultSearchReqLocation :: Loc.SearchReqLocation
defaultSearchReqLocation =
  Loc.SearchReqLocation
    { id = "1",
      lat = 0.0,
      lon = 0.0,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
