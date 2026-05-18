module Storage.CachedQueries.OTPRest.OTPRest where

import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RouteStopMapping as RSM
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as Flow
import SharedLogic.External.Nandi.Types
import Tools.MultiModal as MM

-- Route Queries

getRouteByRouteId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe RouteInfoNandi)
getRouteByRouteId integratedBPPConfig routeId = IM.withInMemCache ["RouteByRouteId", integratedBPPConfig.id.getId, routeId] 43200 $ do
  baseUrl <- MM.getOTPRestServiceReq (integratedBPPConfig.merchantId) (integratedBPPConfig.merchantOperatingCityId)
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
  baseUrl <- MM.getOTPRestServiceReq (integratedBPPConfig.merchantId) (integratedBPPConfig.merchantOperatingCityId)
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
  baseUrl <- MM.getOTPRestServiceReq (integratedBPPConfig.merchantId) (integratedBPPConfig.merchantOperatingCityId)
  Flow.getExampleTrip baseUrl (integratedBPPConfig.feedKey) routeId

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
              integratedBppConfigId = integratedBPPConfig.id,
              merchantId,
              merchantOperatingCityId,
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
