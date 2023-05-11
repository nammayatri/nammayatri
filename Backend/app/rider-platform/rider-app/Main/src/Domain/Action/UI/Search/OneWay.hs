{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.UI.Search.OneWay
  ( OneWaySearchReq (..),
    OneWaySearchRes (..),
    DSearch.SearchReqLocation (..),
    CommonSearchReq (..),
    SearchRetryReq (..),
    DistanceAndDuration (..),
    oneWaySearch,
    search,
  )
where

import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRetry as DSearchRetry
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import Storage.Queries.Geometry
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.SearchRetry as QSearchRetry
import Tools.Error
import Tools.Metrics
import qualified Tools.Metrics as Metrics

data OneWaySearchReq = OneWaySearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype SearchRetryReq = SearchRetryReq
  { retryType :: Select.RetryType
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data DistanceAndDuration = DistanceAndDuration
  { shortdistance :: Maybe Meters,
    duration :: Maybe Seconds,
    longestditance :: Maybe Meters
  }
  deriving (Generic, ToJSON, FromJSON)

data SerchRetryRequest = SerchRetryRequest
  { originAndDestination :: OneWaySearchReq,
    shortdistance :: Maybe HighPrecMeters,
    duration :: Maybe Seconds,
    longestDitance :: Maybe HighPrecMeters
  }

data CommonSearchReq = SearchRetry SearchRetryReq | Search OneWaySearchReq

data OneWaySearchRes = OneWaySearchRes
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    city :: Text
  }

search ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBAPMetrics m r,
    HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]
  ) =>
  Person.Person ->
  Merchant.Merchant ->
  CommonSearchReq ->
  Maybe (Id DSearchReq.SearchRequest) ->
  Maybe Version ->
  Maybe Version ->
  Maybe Meters ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  m OneWaySearchRes
search person merchant req parentSearchId mbBundleVersion mbClientVersion longestRouteDistance device distance duration = do
  case req of
    Search searchRequest -> oneWaySearch person merchant searchRequest mbBundleVersion mbClientVersion (metersToHighPrecMeters <$> longestRouteDistance) device (metersToHighPrecMeters <$> distance) duration Nothing Nothing
    SearchRetry searchRequestRetry -> do
      result <- originandDestination parentSearchId
      oneWaySearch person merchant result.originAndDestination mbBundleVersion mbClientVersion (result.longestDitance) device (result.shortdistance) result.duration parentSearchId (Just searchRequestRetry.retryType)

oneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m,
    HasBAPMetrics m r
  ) =>
  Person.Person ->
  Merchant.Merchant ->
  OneWaySearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe HighPrecMeters ->
  Maybe Text ->
  Maybe HighPrecMeters ->
  Maybe Seconds ->
  Maybe (Id DSearchReq.SearchRequest) ->
  Maybe Select.RetryType ->
  m OneWaySearchRes
oneWaySearch person merchant req bundleVersion clientVersion longestRouteDistance device distance duration parentSearchId retryType = do
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation (Just toLocation) longestRouteDistance distance now bundleVersion clientVersion device duration
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  mbSearchRetry <- forM parentSearchId $ buildSearchRetry (searchRequest.id) now retryType
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
    whenJust mbSearchRetry QSearchRetry.create
    QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  QPFS.clearCache person.id
  let dSearchRes =
        OneWaySearchRes
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill,
            city = merchant.city
          }
  return dSearchRes
  where
    validateServiceability geoConfig =
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps (Just req.destination.gps)) $
        throwError RideNotServiceable

originandDestination :: (MonadFlow m, EsqDBFlow m r) => Maybe (Id DSearchReq.SearchRequest) -> m SerchRetryRequest
originandDestination mbparentSearchId = do
  parentSearchId <- fromMaybeM (InternalError "parentSearchId Not Found") mbparentSearchId
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
  justDestimation <- destination & fromMaybeM (InternalError "Destination is Nothing")
  let origin =
        DSearch.SearchReqLocation
          { gps =
              LatLong
                { lat = searchRequestRow.fromLocation.lat,
                  lon = searchRequestRow.fromLocation.lon
                },
            address = searchRequestRow.fromLocation.address
          }
  let searchReq =
        SerchRetryRequest
          { originAndDestination =
              OneWaySearchReq
                { origin = origin,
                  destination = justDestimation
                },
            shortdistance = searchRequestRow.distance,
            duration = searchRequestRow.estimatedRideDuration,
            longestDitance = searchRequestRow.maxDistance
          }

  return searchReq

buildSearchRetry :: (MonadThrow m, Log m) => Id DSearchReq.SearchRequest -> UTCTime -> Maybe Select.RetryType -> Id DSearchReq.SearchRequest -> m DSearchRetry.SearchRetry
buildSearchRetry retrySearchid now retryType parentSearchId = do
  justRetryType <- fromMaybeM (InternalError "RetryType is Nothing") retryType
  return $
    DSearchRetry.SearchRetry
      { id = retrySearchid,
        parentSearchId = parentSearchId,
        retryCreatedAt = now,
        retryType = justRetryType
      }
