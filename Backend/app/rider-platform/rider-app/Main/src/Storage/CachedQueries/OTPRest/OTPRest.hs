module Storage.CachedQueries.OTPRest.OTPRest where

import BecknV2.FRFS.Enums
import Data.List (groupBy)
import Data.Text (splitOn)
import qualified Domain.Types.GTFSFeedInfo as GTFSFeedInfo
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route as Route
import Domain.Types.RouteStopMapping
import qualified Domain.Types.Station as Station
import GHC.Num (integerFromInt)
import Kernel.Beam.Functions as B
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
import qualified Storage.CachedQueries.Station as CQStation
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RoutePolylines as QRoutePolylines
import qualified Storage.Queries.Station as QStation
import qualified Storage.Queries.StationsExtraInformation as QStationsExtraInformation
import Tools.Error
import Tools.MultiModal as MM

getRouteByRouteId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe Route.Route)
getRouteByRouteId integratedBPPConfig routeId = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  route <- Flow.getRouteByRouteId baseUrl feedInfo.feedId.getId routeId
  case route of
    Just route' -> Just <$> parseRouteFromInMemoryServer route' integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
    Nothing -> do
      logError $ "Route not found in OTPRest: " <> show routeId
      pure Nothing

getRouteByFuzzySearch ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m [Route.Route]
getRouteByFuzzySearch integratedBPPConfig query = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  routes <- Flow.getRouteByFuzzySearch baseUrl feedInfo.feedId.getId query
  mapM (\route -> parseRouteFromInMemoryServer route integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId) routes

getRoutesByGtfsId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m [Route.Route]
getRoutesByGtfsId integratedBPPConfig = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  routes <- Flow.getRoutesByGtfsId baseUrl feedInfo.feedId.getId
  mapM (\route -> parseRouteFromInMemoryServer route integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId) routes

getRoutesByVehicleType ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  VehicleCategory ->
  m [Route.Route]
getRoutesByVehicleType integratedBPPConfig vehicleType = do
  routes <- getRoutesByGtfsId integratedBPPConfig
  return $ filter (\route -> route.vehicleType == vehicleType) routes

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

fromMaybe' :: Int -> Maybe Int -> Integer
fromMaybe' a b = fromMaybe (integerFromInt a) (integerFromInt <$> b)

getStationsByGtfsId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m [Station.Station]
getStationsByGtfsId integratedBPPConfig = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  stations <- Flow.getStationsByGtfsId baseUrl feedInfo.feedId.getId
  parseStationsFromInMemoryServer stations integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId

getStationsByGtfsIdAndStopCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe Station.Station)
getStationsByGtfsIdAndStopCode integratedBPPConfig stopCode = do
  (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
  stations <- try @_ @SomeException (Flow.getStationsByGtfsIdAndStopCode baseUrl feedInfo.feedId.getId stopCode)
  case stations of
    Right stations' -> do
      listToMaybe <$> parseStationsFromInMemoryServer [stations'] integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
    Left err -> do
      logError $ "Error getting stations by gtfs id and stop code: " <> show err
      pure Nothing

findByStationCodeAndIntegratedBPPConfigId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Text -> IntegratedBPPConfig -> m (Maybe Station.Station)
findByStationCodeAndIntegratedBPPConfigId stationCode integratedBPPConfig = do
  station <- getStationsByGtfsIdAndStopCode integratedBPPConfig stationCode
  case station of
    Nothing -> CQStation.findByStationCodeAndIntegratedBPPConfigId stationCode integratedBPPConfig.id
    _ -> pure station

findAllByBppConfigId :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => IntegratedBPPConfig -> m [Station.Station]
findAllByBppConfigId integratedBPPConfig = do
  stations <- try @_ @SomeException (getStationsByGtfsId integratedBPPConfig)
  case stations of
    Right stations' -> pure stations'
    Left err -> do
      logError $ "Error getting stations by gtfs id: " <> show err
      QStation.findAllByBppConfigId integratedBPPConfig.id

findAllByVehicleType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> VehicleCategory -> IntegratedBPPConfig -> m [Station.Station]
findAllByVehicleType limit offset vehicleType integratedBPPConfig = do
  stations <- try @_ @SomeException (getStationsByGtfsId integratedBPPConfig)
  case stations of
    Right stations' -> pure $ take (fromMaybe (length stations') limit) $ drop (fromMaybe 0 offset) $ filter (\station -> station.vehicleType == vehicleType) stations'
    Left err -> do
      logError $ "Error getting stations by gtfs id: " <> show err
      QStation.findAllByVehicleType limit offset vehicleType integratedBPPConfig.id

findAllMatchingStations :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Int -> Maybe Int -> Id MerchantOperatingCity -> VehicleCategory -> IntegratedBPPConfig -> m [Station.Station]
findAllMatchingStations mbSearchStr mbLimit mbOffset merchantOperatingCityId vehicle integratedBPPConfig = do
  if isNothing mbSearchStr
    then return []
    else do
      (feedInfo, baseUrl) <- getFeedInfoVehicleTypeAndBaseUrl integratedBPPConfig
      stations <- try @_ @SomeException (Flow.getStationsByGtfsIdFuzzySearch baseUrl feedInfo.feedId.getId (fromMaybe "" mbSearchStr))
      case stations of
        Right stations' -> do
          parsedStations <- parseStationsFromInMemoryServer stations' integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
          pure $ take (fromMaybe (length parsedStations) mbLimit) $ drop (fromMaybe 0 mbOffset) $ filter (\station -> station.vehicleType == vehicle) parsedStations
        Left err -> do
          logError $ "Error getting stations by gtfs id fuzzy search: " <> show err
          QStation.findAllMatchingStations mbSearchStr (integerFromInt <$> mbLimit) (integerFromInt <$> mbOffset) merchantOperatingCityId vehicle integratedBPPConfig.id

parseStationsFromInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, EsqDBFlow m r, HasShortDurationRetryCfg r c, Log m, CacheFlow m r) =>
  [RouteStopMappingInMemoryServer] ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [Station.Station]
parseStationsFromInMemoryServer stations integratedBPPConfig merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  mapM
    ( \station -> do
        stationsExtraInformation <- QStationsExtraInformation.findByStationIdAndCity station.stopCode merchantOperatingCityId
        return $
          Station.Station
            { address = stationsExtraInformation >>= (.address),
              code = station.stopCode,
              hindiName = Nothing,
              id = Id station.stopCode,
              integratedBppConfigId = integratedBPPConfig,
              lat = Just station.stopPoint.lat,
              lon = Just station.stopPoint.lon,
              merchantId = merchantId,
              merchantOperatingCityId = merchantOperatingCityId,
              name = station.stopName,
              possibleTypes = Nothing,
              regionalName = Nothing,
              suggestedDestinations = Nothing,
              timeBounds = Unbounded,
              vehicleType = station.vehicleType,
              createdAt = now,
              updatedAt = now
            }
    )
    stations

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

parseRouteFromInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  RouteInfoNandi ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m Route.Route
parseRouteFromInMemoryServer routeInfoNandi integratedBppConfig merchantId merchantOperatingCityId = do
  routePolyline <- QRoutePolylines.findByRouteIdAndCity routeInfoNandi.id merchantOperatingCityId
  now <- getCurrentTime
  return $
    Route.Route
      { code = routeInfoNandi.id,
        color = Nothing,
        dailyTripCount = routeInfoNandi.tripCount,
        endPoint = routeInfoNandi.endPoint,
        startPoint = routeInfoNandi.startPoint,
        id = Id $ routeInfoNandi.id,
        integratedBppConfigId = integratedBppConfig,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        polyline = routePolyline >>= (.polyline),
        longName = fromMaybe "" routeInfoNandi.longName,
        shortName = fromMaybe "" routeInfoNandi.shortName,
        vehicleType = routeInfoNandi.mode,
        stopCount = routeInfoNandi.stopCount,
        timeBounds = Unbounded,
        createdAt = now,
        updatedAt = now
      }

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
              Domain.Types.RouteStopMapping.providerCode = "NANDI", -- Hardcoding provider code as NANDI since it's not in RouteStopMappingNandi
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

getRouteByRouteCodeWithFallback ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m Route.Route
getRouteByRouteCodeWithFallback integratedBPPConfig routeCode = do
  try @_ @SomeException (getRouteByRouteId integratedBPPConfig routeCode) >>= \case
    Left err -> do
      logError $ "Error getting route by route code: " <> show err
      B.runInReplica $ QRoute.findByRouteCode routeCode integratedBPPConfig.id >>= maybe (QRoute.findByRouteId (Id routeCode)) (pure . Just) >>= fromMaybeM (RouteNotFound routeCode)
    Right route'' -> case route'' of
      Just route' -> pure route'
      Nothing -> B.runInReplica $ QRoute.findByRouteCode routeCode integratedBPPConfig.id >>= maybe (QRoute.findByRouteId (Id routeCode)) (pure . Just) >>= fromMaybeM (RouteNotFound routeCode)
