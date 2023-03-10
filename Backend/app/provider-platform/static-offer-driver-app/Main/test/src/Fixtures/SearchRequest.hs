{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Fixtures.SearchRequest (defaultSearchRequest) where

import qualified Domain.Types.SearchRequest as SearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as Loc
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Kernel.Types.Id
import Servant.Client

defaultSearchRequest :: SearchRequest.SearchRequest
defaultSearchRequest =
  SearchRequest.SearchRequest
    { id = Id "1",
      transactionId = "",
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
