{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Search.OneWay
  ( OneWaySearchReq (..),
    OneWaySearchRes (..),
    DSearch.SearchReqLocation (..),
    oneWaySearch,
  )
where

import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import Storage.Queries.Geometry
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import Tools.Metrics
import qualified Tools.Metrics as Metrics

data OneWaySearchReq = OneWaySearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWaySearchRes = OneWaySearchRes
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    --TODO: This supposed to be temporary solution. Check if we still need it
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime
  }

oneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m,
    HasBAPMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Person.Person ->
  Merchant.Merchant ->
  OneWaySearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Meters ->
  m OneWaySearchRes
oneWaySearch person merchant req bundleVersion clientVersion distance = do
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation (Just toLocation) (metersToHighPrecMeters <$> distance) now bundleVersion clientVersion
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
    QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  let dSearchRes =
        OneWaySearchRes
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill
          }
  return dSearchRes
  where
    validateServiceability geoConfig =
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps (Just req.destination.gps)) $
        throwError RideNotServiceable
