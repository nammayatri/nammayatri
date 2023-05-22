module SharedLogic.SimulatedFlow.OneWay where

import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import qualified SharedLogic.Search.Common as DSearch
import SharedLogic.Search.OneWay
import qualified SharedLogic.Types.Merchant as Merchant
import qualified SharedLogic.Types.Person as Person
import qualified SharedLogic.Types.Person.PersonFlowStatus as DPFS
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import Storage.CachedQueries.SimulatedFlow.SearchRequest
import Storage.Queries.Geometry
import Tools.Error
import Tools.Metrics
import qualified Tools.Metrics as Metrics

oneWaySimulatedSearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m,
    EsqDBReplicaFlow m r,
    SimluatedCacheFlow m r,
    HasBAPMetrics m r
  ) =>
  Person.Person ->
  Merchant.Merchant ->
  OneWaySearchReq ->
  Maybe Version ->
  Maybe Version ->
  Maybe Meters ->
  Maybe Text ->
  Maybe Meters ->
  Maybe Seconds ->
  m OneWaySearchRes
oneWaySimulatedSearch person merchant req bundleVersion clientVersion longestRouteDistance device distance duration = do
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation (Just toLocation) (metersToHighPrecMeters <$> longestRouteDistance) (metersToHighPrecMeters <$> distance) now bundleVersion clientVersion device duration
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  DB.runTransaction $ do
    QPFS.updateStatus person.id DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  cacheSearchRequest searchRequest person
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
