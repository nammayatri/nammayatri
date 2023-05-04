{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SearchRetry.OneWayRetry
  ( SearchRetryReq (..),
    DSearch.SearchReqLocation (..),
    oneWaySearchRetry,
  )
where

import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Action.UI.Search.OneWay as AUSO
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.SearchRetry as DSearchRetry
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error (SearchRequestError (SearchRequestNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.SearchRetry as QSearchRetry
import Tools.Metrics
import qualified Tools.Metrics as Metrics

newtype SearchRetryReq = SearchRetryReq
  { retryType :: Select.RetryType
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

oneWaySearchRetry ::
  ( HasCacheConfig r,
    EncFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m,
    HasBAPMetrics m r
  ) =>
  Person.Person ->
  Merchant.Merchant ->
  SearchRetryReq ->
  Id SearchRequest ->
  m AUSO.OneWaySearchRes
oneWaySearchRetry person merchant req parentSearchId = do
  searchRequestRow <- QSearchRequest.findById parentSearchId >>= fromMaybeM (SearchRequestNotFound parentSearchId.getId)
  let destination = (searchRequestRow.toLocation <&>) $ \toLocation -> do
        DSearch.SearchReqLocation
          { gps =
              LatLong
                { lat = toLocation.lat,
                  lon = toLocation.lon
                },
            address = toLocation.address
          }
  let justDestination = fromMaybe (error "Destination is Nothing") destination
  let origin =
        DSearch.SearchReqLocation
          { gps =
              LatLong
                { lat = searchRequestRow.fromLocation.lat,
                  lon = searchRequestRow.fromLocation.lon
                },
            address = searchRequestRow.fromLocation.address
          }
  fromLocation <- DSearch.buildSearchReqLoc origin
  toLocation <- mapM DSearch.buildSearchReqLoc destination
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation toLocation searchRequestRow.maxDistance searchRequestRow.distance now searchRequestRow.bundleVersion searchRequestRow.clientVersion searchRequestRow.device searchRequestRow.estimatedRideDuration
  let searchRetry =
        DSearchRetry.SearchRetry
          { id = cast searchRequest.id,
            parentSearchId = parentSearchId,
            retryCreatedAt = now,
            retryType = req.retryType
          }
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  DB.runTransaction $ do
    QSearchRetry.create searchRetry
    QSearchRequest.create searchRequest
    QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  QPFS.clearCache person.id
  let dSearchRes =
        AUSO.OneWaySearchRes
          { origin = origin,
            destination = justDestination,
            searchId = searchRequest.id,
            now = now,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill,
            city = merchant.city
          }
  return dSearchRes
