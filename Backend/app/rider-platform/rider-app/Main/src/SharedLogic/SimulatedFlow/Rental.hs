module SharedLogic.SimulatedFlow.Rental where

import Kernel.Prelude
import Kernel.Serviceability
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import qualified SharedLogic.Search.Common as DSearch
import SharedLogic.Search.Rental
import qualified SharedLogic.Types.Person as Person
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.CachedQueries.SimulatedFlow.SearchRequest
import Storage.Queries.Geometry
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Metrics
import qualified Tools.Metrics as Metrics

simulatedRentalSearch ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r,
    SimluatedCacheFlow m r
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  RentalSearchReq ->
  m RentalSearchRes
simulatedRentalSearch personId bundleVersion clientVersion device req = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <-
    QMerchant.findById person.merchantId
      >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  now <- getCurrentTime
  searchRequest <- DSearch.buildSearchRequest person fromLocation Nothing Nothing Nothing now bundleVersion clientVersion device Nothing
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  cacheSearchRequest searchRequest person
  let dSearchRes =
        RentalSearchRes
          { origin = req.origin,
            searchId = searchRequest.id,
            startTime = req.startTime,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill,
            city = merchant.city
          }
  return dSearchRes
  where
    validateServiceability geoConfig = do
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps Nothing) $
        throwError RideNotServiceable
