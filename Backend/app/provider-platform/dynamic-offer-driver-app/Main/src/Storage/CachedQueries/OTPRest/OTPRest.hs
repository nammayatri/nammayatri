module Storage.CachedQueries.OTPRest.OTPRest where

import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.Domain.Types.RouteStopMapping as RSM
import qualified Lib.GtfsDataServer.Flow as Flow
import Lib.GtfsDataServer.Types
import SharedLogic.IntegratedBPPConfig (getGimsBaseUrl)

-- Route Queries

getRouteByRouteId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe RouteInfoNandi)
getRouteByRouteId integratedBPPConfig routeId = IM.withInMemCache ["RouteByRouteId", integratedBPPConfig.id.getId, routeId] 43200 $ do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  route <- Flow.getRouteByRouteId baseUrl (integratedBPPConfig.feedKey) routeId
  case route of
    Just route' -> pure $ Just route'
    Nothing -> do
      logError $ "Route not found in OTPRest: " <> show routeId
      pure Nothing

getRouteStopMappingByRouteCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m [RSM.RouteStopMapping]
getRouteStopMappingByRouteCode routeCode integratedBPPConfig = IM.withInMemCache ["RSM", routeCode, integratedBPPConfig.id.getId] 3600 $ do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl (integratedBPPConfig.feedKey) (Just routeCode) Nothing
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig (integratedBPPConfig.merchantId) (integratedBPPConfig.merchantOperatingCityId)
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

getExampleTrip ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe TripDetails)
getExampleTrip integratedBPPConfig routeId = IM.withInMemCache ["ExampleTrip", integratedBPPConfig.id.getId, routeId] 1800 $ do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  Flow.getExampleTrip baseUrl (integratedBPPConfig.feedKey) routeId

getBusTripSchedule ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  Int ->
  Text ->
  m BusScheduleDetails
getBusTripSchedule integratedBPPConfig waybillNo tripNumber routeId = IM.withInMemCache ["BusTripSchedule", integratedBPPConfig.id.getId, waybillNo, show tripNumber, routeId] 300 $ do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  Flow.getBusTripSchedule baseUrl (integratedBPPConfig.feedKey) waybillNo tripNumber routeId

-- Parse Functions

parseRouteStopMappingInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [RouteStopMappingInMemoryServer] ->
  IntegratedBPPConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m [RSM.RouteStopMapping]
parseRouteStopMappingInMemoryServer routeStopMappingInMemoryServer integratedBPPConfig merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  mapM
    ( \mapping -> do
        let routeCode = last $ T.splitOn ":" mapping.routeCode
            stopCode = last $ T.splitOn ":" mapping.stopCode
        return $
          RSM.RouteStopMapping
            { estimatedTravelTimeFromPreviousStop = mapping.estimatedTravelTimeFromPreviousStop,
              integratedBppConfigId = integratedBPPConfig.id.getId,
              merchantId = merchantId.getId,
              merchantOperatingCityId = merchantOperatingCityId.getId,
              providerCode = if mapping.providerCode == "GTFS" then stopCode else mapping.providerCode,
              routeCode = routeCode,
              sequenceNum = mapping.sequenceNum,
              stopCode = stopCode,
              stopName = mapping.stopName,
              stopPoint = mapping.stopPoint,
              timeBounds = Unbounded,
              vehicleType = mapping.vehicleType,
              createdAt = now,
              updatedAt = now
            }
    )
    routeStopMappingInMemoryServer
