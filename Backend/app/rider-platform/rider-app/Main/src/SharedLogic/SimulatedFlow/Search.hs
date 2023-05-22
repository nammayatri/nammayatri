{-# LANGUAGE OverloadedLists #-}

module SharedLogic.SimulatedFlow.Search where

import Environment
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Streaming.Kafka.Topic.PublicTransportSearch
import Kernel.Streaming.MonadProducer
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import SharedLogic.Search
import qualified SharedLogic.Search.OneWay as DOneWaySearch
import qualified SharedLogic.Search.Rental as DRentalSearch
import SharedLogic.SimulatedFlow.OneWay as DOneWaySearch
import qualified SharedLogic.SimulatedFlow.Rental as DRentalSearch
import qualified SharedLogic.Types.Person as Person
import SharedLogic.Types.SearchRequest (SearchRequest)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.CachedQueries.SimulatedFlow.SearchRequest
import qualified Storage.Queries.Person as Person
import qualified Tools.Maps as Maps
import Tools.Metrics

interceptWhenSimulated ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    SimluatedCacheFlow m r,
    EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r,
    MonadProducer PublicTransportSearch m
  ) =>
  Bool ->
  SearchReq ->
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  m SearchRes ->
  m SearchRes
interceptWhenSimulated isSimulated req personId mbBundleVersion mbClientVersion device nonStimulatedAction =
  if isSimulated
    then do
      (searchId, searchExpiry, routeInfo) <- case req of
        OneWaySearch oneWay -> simulatedOneWaySearch personId mbBundleVersion mbClientVersion device oneWay
        RentalSearch rental -> simulatedRentalSearch personId mbBundleVersion mbClientVersion device rental
      return $ SearchRes searchId searchExpiry routeInfo
    else nonStimulatedAction

simulatedOneWaySearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r,
    HedisFlow m r,
    SimluatedCacheFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r,
    MonadProducer PublicTransportSearch m
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  DOneWaySearch.OneWaySearchReq ->
  m (Id SearchRequest, UTCTime, Maybe Maps.RouteInfo)
simulatedOneWaySearch personId bundleVersion clientVersion device req = do
  person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  let sourceLatlong = req.origin.gps
  let destinationLatLong = req.destination.gps
  let request =
        Maps.GetRoutesReq
          { waypoints = [sourceLatlong, destinationLatLong],
            calcPoints = True,
            mode = Just Maps.CAR
          }
  routeResponse <- Maps.getSimulatedRoutes person.merchantId request
  let shortestRouteInfo = getRouteInfoWithShortestDuration routeResponse
  let longestRouteDistance = (.distance) =<< getLongestRouteDistance routeResponse
  let shortestRouteDistance = (.distance) =<< shortestRouteInfo
  let shortestRouteDuration = (.duration) =<< shortestRouteInfo
  dSearchRes <- DOneWaySearch.oneWaySimulatedSearch person merchant req bundleVersion clientVersion longestRouteDistance device shortestRouteDistance shortestRouteDuration
  whenJust shortestRouteInfo $ \routeInfo -> cacheRouteInfo dSearchRes.searchId routeInfo
  return (dSearchRes.searchId, dSearchRes.searchRequestExpiry, shortestRouteInfo)

simulatedRentalSearch ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasHttpClientOptions r c,
    SimluatedCacheFlow m r,
    EsqDBReplicaFlow m r,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  DRentalSearch.RentalSearchReq ->
  m (Id SearchRequest, UTCTime, Maybe Maps.RouteInfo)
simulatedRentalSearch personId bundleVersion clientVersion device req = do
  dSearchRes <- DRentalSearch.simulatedRentalSearch personId bundleVersion clientVersion device req
  pure (dSearchRes.searchId, dSearchRes.searchRequestExpiry, Nothing)
