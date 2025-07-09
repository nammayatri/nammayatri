module Storage.CachedQueries.OTPRest.OTPRest (module OTPRestCommon, module Storage.CachedQueries.OTPRest.OTPRest) where

import BecknV2.FRFS.Enums
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy)
import Data.Text (splitOn)
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route as Route
import Domain.Types.RouteStopMapping
import qualified Domain.Types.Station as Station
import GHC.Num (integerFromInt)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as Flow
import SharedLogic.External.Nandi.Types
import Storage.CachedQueries.OTPRest.Common as OTPRestCommon
import Storage.CachedQueries.RouteStopTimeTable as GRQRSTT
import qualified Storage.Queries.RoutePolylines as QRoutePolylines
import qualified Storage.Queries.StationsExtraInformation as QStationsExtraInformation
import Tools.Error
import Tools.MultiModal as MM

-- Route Queries

getRouteByRouteId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe Route.Route)
getRouteByRouteId integratedBPPConfig routeId = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  route <- Flow.getRouteByRouteId baseUrl integratedBPPConfig.feedKey routeId
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
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routes <- Flow.getRouteByFuzzySearch baseUrl integratedBPPConfig.feedKey query
  parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId

findAllMatchingRoutes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Int -> Maybe Int -> VehicleCategory -> IntegratedBPPConfig -> m [Route.Route]
findAllMatchingRoutes mbSearchStr mbLimit mbOffset vehicle integratedBPPConfig = do
  if isNothing mbSearchStr
    then return []
    else do
      baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
      routes <- Flow.getRouteByFuzzySearch baseUrl integratedBPPConfig.feedKey (fromMaybe "" mbSearchStr)
      parsedRoutes <- parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
      pure $ take (fromMaybe (length parsedRoutes) mbLimit) $ drop (fromMaybe 0 mbOffset) $ filter (\route -> route.vehicleType == vehicle) parsedRoutes

getRoutesByGtfsId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m [Route.Route]
getRoutesByGtfsId integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routes <- Flow.getRoutesByGtfsId baseUrl integratedBPPConfig.feedKey
  parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId

getRoutesByVehicleType ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  VehicleCategory ->
  m [Route.Route]
getRoutesByVehicleType integratedBPPConfig vehicleType = do
  routes <- getRoutesByGtfsId integratedBPPConfig
  return $ filter (\route -> route.vehicleType == vehicleType) routes

-- Route Stop Mapping Queries

getRouteStopMappingByRouteCode ::
  ( MonadFlow m,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByRouteCode routeCode integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl integratedBPPConfig.feedKey (Just routeCode) Nothing
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

getRouteStopMappingByStopCode ::
  ( MonadFlow m,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByStopCode stopCode integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl integratedBPPConfig.feedKey Nothing (Just stopCode)
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

getRouteStopMappingByStopCodeAndRouteCode ::
  ( MonadFlow m,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Text ->
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByStopCodeAndRouteCode stopCode routeCode integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl integratedBPPConfig.feedKey (Just routeCode) (Just stopCode)
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

fromMaybe' :: Int -> Maybe Int -> Integer
fromMaybe' a = maybe (integerFromInt a) integerFromInt

-- Station Queries

getStationsByGtfsId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m [Station.Station]
getStationsByGtfsId integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  stations <- Flow.getStationsByGtfsId baseUrl integratedBPPConfig.feedKey
  parseStationsFromInMemoryServer stations integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId

getStationByGtfsIdAndStopCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m (Maybe Station.Station)
getStationByGtfsIdAndStopCode stopCode integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  stations <- Flow.getStationsByGtfsIdAndStopCode baseUrl integratedBPPConfig.feedKey stopCode
  listToMaybe <$> parseStationsFromInMemoryServer [stations] integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId

findAllStationsByVehicleType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> VehicleCategory -> IntegratedBPPConfig -> m [Station.Station]
findAllStationsByVehicleType limit offset vehicleType integratedBPPConfig = do
  stations <- getStationsByGtfsId integratedBPPConfig
  pure $ take (fromMaybe (length stations) limit) $ drop (fromMaybe 0 offset) $ filter (\station -> station.vehicleType == vehicleType) stations

findAllMatchingStations :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Int -> Maybe Int -> VehicleCategory -> IntegratedBPPConfig -> m [Station.Station]
findAllMatchingStations mbSearchStr mbLimit mbOffset vehicle integratedBPPConfig = do
  if isNothing mbSearchStr
    then return []
    else do
      baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
      stations <- Flow.getStationsByGtfsIdFuzzySearch baseUrl integratedBPPConfig.feedKey (fromMaybe "" mbSearchStr)
      parsedStations <- parseStationsFromInMemoryServer stations integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
      pure $ take (fromMaybe (length parsedStations) mbLimit) $ drop (fromMaybe 0 mbOffset) $ filter (\station -> station.vehicleType == vehicle) parsedStations

getStationsByVehicleType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => VehicleCategory -> IntegratedBPPConfig -> m [Station.Station]
getStationsByVehicleType vehicleType integratedBPPConfig = do
  stations <- getStationsByGtfsId integratedBPPConfig
  return $ filter (\station -> station.vehicleType == vehicleType) stations

-- Parse Queries

parseStationsFromInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, EsqDBFlow m r, HasShortDurationRetryCfg r c, Log m, CacheFlow m r) =>
  [RouteStopMappingInMemoryServer] ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [Station.Station]
parseStationsFromInMemoryServer stations integratedBPPConfig merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  stationsExtraInformation <- QStationsExtraInformation.getBystationIdsAndCity (map (.stopCode) stations) merchantOperatingCityId
  let stationAddressMap = HM.fromList $ map (\info -> (info.stationId, (info.address, info.suggestedDestinations))) stationsExtraInformation
  mapM
    ( \station -> do
        return $
          Station.Station
            { address = join (fst <$> HM.lookup station.stopCode stationAddressMap),
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
              suggestedDestinations = join (snd <$> HM.lookup station.stopCode stationAddressMap),
              geoJson = station.geoJson,
              gates = station.gates,
              timeBounds = Unbounded,
              vehicleType = station.vehicleType,
              createdAt = now,
              updatedAt = now
            }
    )
    stations

parseRoutesFromInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [RouteInfoNandi] ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [Route.Route]
parseRoutesFromInMemoryServer routes integratedBppConfigId merchantId merchantOperatingCityId = do
  let routeIds = map (.id) routes
  routePolylines <- QRoutePolylines.getByRouteIdsAndCity routeIds merchantOperatingCityId
  let polylineMap = HM.fromList $ map (\polyline -> (polyline.routeId, polyline.polyline)) routePolylines
  now <- getCurrentTime
  return $
    map
      ( \route ->
          Route.Route
            { code = route.id,
              color = Nothing,
              dailyTripCount = route.tripCount,
              endPoint = route.endPoint,
              startPoint = route.startPoint,
              id = Id $ route.id,
              integratedBppConfigId = integratedBppConfigId,
              merchantId = merchantId,
              merchantOperatingCityId = merchantOperatingCityId,
              polyline = join $ HM.lookup route.id polylineMap,
              longName = fromMaybe "" route.longName,
              shortName = fromMaybe "" route.shortName,
              vehicleType = route.mode,
              stopCount = route.stopCount,
              timeBounds = Unbounded,
              createdAt = now,
              updatedAt = now
            }
      )
      routes

parseRouteFromInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  RouteInfoNandi ->
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m Route.Route
parseRouteFromInMemoryServer routeInfoNandi integratedBppConfigId merchantId merchantOperatingCityId = do
  routes <- parseRoutesFromInMemoryServer [routeInfoNandi] integratedBppConfigId merchantId merchantOperatingCityId
  case routes of
    (route : _) -> pure route
    _ -> throwError $ InternalError "Failed to parse route"

parseRouteStopMappingInMemoryServer ::
  ( MonadFlow m,
    ServiceFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  [RouteStopMappingInMemoryServer] ->
  IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [RouteStopMapping]
parseRouteStopMappingInMemoryServer routeStopMappingInMemoryServer integratedBPPConfig merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  mapM
    ( \mapping -> do
        let routeCode = last $ splitOn ":" mapping.routeCode
            stopCode = last $ splitOn ":" mapping.stopCode
        routeStopTimeTables <- GRQRSTT.findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOperatingCityId [routeCode] stopCode
        return $
          RouteStopMapping
            { estimatedTravelTimeFromPreviousStop = mapping.estimatedTravelTimeFromPreviousStop,
              integratedBppConfigId = integratedBPPConfig.id,
              merchantId,
              merchantOperatingCityId,
              Domain.Types.RouteStopMapping.providerCode = fromMaybe stopCode $ ((listToMaybe routeStopTimeTables) >>= \routeStopTimeTable -> routeStopTimeTable.providerStopCode),
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

-- Vehicle Related Queries
getVehicleServiceType ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe VehicleServiceTypeResponse)
getVehicleServiceType integratedBPPConfig vehicleNumber = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getVehicleServiceType baseUrl vehicleNumber

-- Get Stop Code From Provider Code

getStopCodeFromProviderCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe Text)
getStopCodeFromProviderCode integratedBPPConfig providerStopCode = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  resp <- Flow.getStopCode baseUrl integratedBPPConfig.feedKey providerStopCode
  return (resp <&> (.stop_code))
