module Storage.CachedQueries.OTPRest.OTPRest where

import Data.List (groupBy)
import Data.Text (splitOn)
import qualified Domain.Types.GTFSFeedInfo as GTFSFeedInfo
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopMapping
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as Flow
import SharedLogic.External.Nandi.Types
import Storage.CachedQueries.GTFSFeedInfo (findByVehicleTypeAndCity)
import Storage.CachedQueries.RouteStopTimeTable (castVehicleType)
import Tools.MultiModal as MM

getFeedInfoVehicleTypeAndBaseUrl ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m (GTFSFeedInfo.GTFSFeedInfo, BaseUrl)
getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig = do
  vehicleType <- castVehicleType integratedBPPConfig.vehicleCategory
  feedInfo <- findByVehicleTypeAndCity vehicleType integratedBPPConfig.merchantOperatingCityId integratedBPPConfig.merchantId
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  return (feedInfo, baseUrl)

getRouteStopMappingByRouteCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByRouteCode routeCode integratedBPPConfig = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl feedInfo.feedId.getId (Just routeCode) Nothing
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

getRouteStopMappingByStopCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByStopCode stopCode integratedBPPConfig = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl feedInfo.feedId.getId Nothing (Just stopCode)
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

getRouteStopMappingByStopCodeAndRouteCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByStopCodeAndRouteCode stopCode routeCode integratedBPPConfig = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl feedInfo.feedId.getId (Just routeCode) (Just stopCode)
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

findAndCache ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, EsqDBFlow m r, HasShortDurationRetryCfg r c, Log m, CacheFlow m r) =>
  IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [RouteStopMapping]
findAndCache integratedBPPConfig merchantId merchantOperatingCityId = do
  vehicleType' <- castVehicleType integratedBPPConfig.vehicleCategory
  feedInfo <- findByVehicleTypeAndCity vehicleType' merchantOperatingCityId merchantId
  now <- getCurrentTime
  baseUrl <- MM.getOTPRestServiceReq merchantId merchantOperatingCityId
  logDebug $ "baseUrl: " <> showBaseUrl baseUrl
  routeStopMapping' <- Flow.getRouteStopMapping baseUrl feedInfo.feedId.getId
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  let routeStopMapping = parseRouteStopMapping routeStopMapping' integratedBPPConfig.id merchantId merchantOperatingCityId now
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  fork "caching route stop mapping info" $ cacheAllRouteStopMapping routeStopMapping
  return routeStopMapping

parseRouteStopMapping ::
  [RouteStopMappingNandi] ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  UTCTime ->
  [RouteStopMapping]
parseRouteStopMapping routeStopMappingNandi integratedBppConfig merchantId merchantOperatingCityId now =
  map
    ( \mapping ->
        RouteStopMapping
          { estimatedTravelTimeFromPreviousStop = mapping.estimatedTravelTimeFromPreviousStop,
            integratedBppConfigId = integratedBppConfig,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            providerCode = "NANDI", -- Hardcoding provider code as NANDI since it's not in RouteStopMappingNandi
            routeCode = last $ splitOn ":" mapping.routeCode,
            sequenceNum = mapping.sequenceNum,
            stopCode = last $ splitOn ":" mapping.stopCode,
            stopName = mapping.stopName,
            stopPoint = mapping.stopPoint,
            timeBounds = Unbounded,
            vehicleType = mapping.vehicleType,
            createdAt = now,
            updatedAt = now
          }
    )
    routeStopMappingNandi

parseRouteStopMappingInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, EsqDBFlow m r, HasShortDurationRetryCfg r c, Log m, CacheFlow m r) =>
  [RouteStopMappingInMemoryServer] ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [RouteStopMapping]
parseRouteStopMappingInMemoryServer routeStopMappingInMemoryServer integratedBppConfig merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  return $
    map
      ( \mapping ->
          RouteStopMapping
            { estimatedTravelTimeFromPreviousStop = mapping.estimatedTravelTimeFromPreviousStop,
              integratedBppConfigId = integratedBppConfig,
              merchantId,
              merchantOperatingCityId,
              providerCode = "NANDI", -- Hardcoding provider code as NANDI since it's not in RouteStopMappingNandi
              routeCode = last $ splitOn ":" mapping.routeCode,
              sequenceNum = mapping.sequenceNum,
              stopCode = last $ splitOn ":" mapping.stopCode,
              stopName = mapping.stopName,
              stopPoint = mapping.stopPoint,
              timeBounds = Unbounded,
              vehicleType = mapping.vehicleType,
              createdAt = now,
              updatedAt = now
            }
      )
      routeStopMappingInMemoryServer

-- TODO: Optimize this currently taking too much space
cacheAllRouteStopMapping :: (CacheFlow m r) => [RouteStopMapping] -> m ()
cacheAllRouteStopMapping routeStopMapping = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)

  Hedis.setExp allRouteStopMappingKey routeStopMapping expTime
  let routeStopMappingByRouteId = groupBy (\a b -> a.routeCode == b.routeCode) routeStopMapping
  forM_ routeStopMappingByRouteId $ \mappings -> do
    case mappings of
      (mapping : _) -> Hedis.setExp (routeStopMappingByRouteIdKey mapping.routeCode) mappings expTime
      _ -> pure ()
  let routeStopMappingByStopCode = groupBy (\a b -> a.stopCode == b.stopCode) routeStopMapping
  forM_ routeStopMappingByStopCode $ \mappings -> do
    case mappings of
      (mapping : _) -> Hedis.setExp (routeStopMappingByStopCodeKey mapping.stopCode) mappings expTime
      _ -> pure ()

-- TODO: Optimize this currently taking too much space
cacheByRouteId :: (CacheFlow m r) => [RouteStopMapping] -> m ()
cacheByRouteId routeStopMapping = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp allRouteStopMappingKey routeStopMapping expTime

routeStopMappingByRouteIdKey :: Text -> Text
routeStopMappingByRouteIdKey routeId = "routeStopMappingByRouteId:" <> routeId

allRouteStopMappingKey :: Text
allRouteStopMappingKey = "allRouteStopMapping"

routeStopMappingByStopCodeKey :: Text -> Text
routeStopMappingByStopCodeKey stopCode = "routeStopMappingByStopCode:" <> stopCode
