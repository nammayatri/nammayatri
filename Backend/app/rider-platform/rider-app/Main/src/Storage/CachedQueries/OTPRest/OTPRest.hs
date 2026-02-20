module Storage.CachedQueries.OTPRest.OTPRest (module OTPRestCommon, module Storage.CachedQueries.OTPRest.OTPRest) where

import BecknV2.FRFS.Enums
import qualified BecknV2.FRFS.Utils as BecknFRFSUtils
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy)
import Data.Text (splitOn)
import qualified Data.Text as T
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route as Route
import Domain.Types.RouteStopMapping
import qualified Domain.Types.Station as Station
import GHC.Num (integerFromInt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as Flow
import SharedLogic.External.Nandi.Types
import Storage.CachedQueries.OTPRest.Common as OTPRestCommon
import qualified Storage.CachedQueries.RoutePolylines as QRoutePolylines
import qualified Storage.Queries.StationsExtraInformation as QStationsExtraInformation
import Tools.Error
import Tools.MultiModal as MM

-- Route Queries

getRouteByRouteId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe Route.Route)
getRouteByRouteId integratedBPPConfig routeId = IM.withInMemCache ["RouteByRouteId", integratedBPPConfig.id.getId, routeId] 43200 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  route <- Flow.getRouteByRouteId baseUrl integratedBPPConfig.feedKey routeId
  case route of
    Just route' -> Just <$> parseRouteFromInMemoryServer route' integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
    Nothing -> do
      logError $ "Route not found in OTPRest: " <> show routeId
      pure Nothing

getRouteBusSchedule ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m BusScheduleDetails
getRouteBusSchedule routeId integratedBPPConfig = IM.withInMemCache ["getRouteBusSchedule", integratedBPPConfig.id.getId, routeId] 7200 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  schedules <- Flow.getRouteBusSchedule baseUrl integratedBPPConfig.feedKey routeId
  pure schedules

getRoutesByRouteIds ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  [Text] ->
  m [Route.Route]
getRoutesByRouteIds integratedBPPConfig routeIds = IM.withInMemCache ["RoutesByRouteIds", integratedBPPConfig.id.getId, show routeIds] 3600 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routes <- Flow.getRoutesByRouteIds baseUrl integratedBPPConfig.feedKey routeIds
  parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId False

getRouteByFuzzySearch ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m [Route.Route]
getRouteByFuzzySearch integratedBPPConfig query = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routes <- Flow.getRouteByFuzzySearch baseUrl integratedBPPConfig.feedKey query
  parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId False

findAllMatchingRoutes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Maybe Int -> Maybe Int -> VehicleCategory -> IntegratedBPPConfig -> m [Route.Route]
findAllMatchingRoutes mbSearchStr mbLimit mbOffset vehicle integratedBPPConfig = do
  if isNothing mbSearchStr
    then return []
    else do
      baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
      routes <- Flow.getRouteByFuzzySearch baseUrl integratedBPPConfig.feedKey (fromMaybe "" mbSearchStr)
      parsedRoutes <- parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId False
      pure $ take (fromMaybe (length parsedRoutes) mbLimit) $ drop (fromMaybe 0 mbOffset) $ filter (\route -> route.vehicleType == vehicle) parsedRoutes

getRoutesByGtfsId ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m [Route.Route]
getRoutesByGtfsId integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routes <- Flow.getRoutesByGtfsId baseUrl integratedBPPConfig.feedKey
  parseRoutesFromInMemoryServer routes integratedBPPConfig.id integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId False

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
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMapping]
getRouteStopMappingByRouteCode routeCode integratedBPPConfig = IM.withInMemCache ["RSM", routeCode, integratedBPPConfig.id.getId] 3600 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl integratedBPPConfig.feedKey (Just routeCode) Nothing
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  routeStopMapping <- parseRouteStopMappingInMemoryServer routeStopMapping' integratedBPPConfig integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  logDebug $ "routeStopMapping from rest api after parsing: " <> show routeStopMapping
  return routeStopMapping

getRouteStopMappingByRouteCodeInMem ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m [RouteStopMappingInMemoryServer]
getRouteStopMappingByRouteCodeInMem routeCode integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routeStopMapping' <- Flow.getRouteStopMappingInMemoryServer baseUrl integratedBPPConfig.feedKey (Just routeCode) Nothing
  logDebug $ "routeStopMapping from rest api: " <> show routeStopMapping'
  return routeStopMapping'

getRouteStopMappingByStopCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
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
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
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

getRouteStopMappingByStopCodes ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  [Text] ->
  m [RouteStopMapping]
getRouteStopMappingByStopCodes integratedBPPConfig stopCodes = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  routeStopMapping' <- Flow.postRouteStopMappingByStopCodes baseUrl integratedBPPConfig.feedKey stopCodes
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
  parseStationsFromInMemoryServerWithPublicData stations integratedBPPConfig True

getStationByGtfsIdAndStopCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m (Maybe Station.Station)
getStationByGtfsIdAndStopCode stopCode integratedBPPConfig = IM.withInMemCache ["SBSC", stopCode, integratedBPPConfig.id.getId] 3600 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  stations <- Flow.getStationsByGtfsIdAndStopCode baseUrl integratedBPPConfig.feedKey stopCode
  listToMaybe <$> parseStationsFromInMemoryServer [stations] integratedBPPConfig False

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
      parsedStations <- parseStationsFromInMemoryServer stations integratedBPPConfig False
      pure $ take (fromMaybe (length parsedStations) mbLimit) $ drop (fromMaybe 0 mbOffset) $ filter (\station -> station.vehicleType == vehicle) parsedStations

getStationsByVehicleType :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => VehicleCategory -> IntegratedBPPConfig -> m [Station.Station]
getStationsByVehicleType vehicleType integratedBPPConfig = do
  stations <- getStationsByGtfsId integratedBPPConfig
  return $ filter (\station -> station.vehicleType == vehicleType) stations

-- Parse Queries
parseStationsFromInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, EsqDBFlow m r, HasShortDurationRetryCfg r c, Log m, CacheFlow m r) =>
  [RouteStopMappingInMemoryServer] ->
  IntegratedBPPConfig ->
  Bool ->
  m [Station.Station]
parseStationsFromInMemoryServer stations integratedBPPConfig needExtraInformation = do
  let routeStopMappingInMemoryServerWithPublicData = map (\RouteStopMappingInMemoryServer {..} -> RouteStopMappingInMemoryServerWithPublicData estimatedTravelTimeFromPreviousStop providerCode routeCode sequenceNum stopCode stopName stopPoint vehicleType Nothing gates hindiName regionalName parentStopCode) stations
  parseStationsFromInMemoryServerWithPublicData routeStopMappingInMemoryServerWithPublicData integratedBPPConfig needExtraInformation

parseStationsFromInMemoryServerWithPublicData ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, EsqDBFlow m r, HasShortDurationRetryCfg r c, Log m, CacheFlow m r) =>
  [RouteStopMappingInMemoryServerWithPublicData] ->
  IntegratedBPPConfig ->
  Bool ->
  m [Station.Station]
parseStationsFromInMemoryServerWithPublicData stations integratedBPPConfig needExtraInformation = do
  now <- getCurrentTime
  allStationsExtraInformation <- if needExtraInformation then IM.withInMemCache ["StationsExtraInformation", integratedBPPConfig.id.getId] 3600 $ QStationsExtraInformation.getAllStationsByCity integratedBPPConfig.merchantOperatingCityId else pure []
  let stationsExtraInformation = filter (\info -> info.stationId `elem` map (.stopCode) stations) allStationsExtraInformation
  let stationAddressMap = HM.fromList $ map (\info -> (info.stationId, (info.address, info.suggestedDestinations))) stationsExtraInformation
  mapM
    ( \station -> do
        return $
          Station.Station
            { address = join (fst <$> HM.lookup station.stopCode stationAddressMap),
              code = station.stopCode,
              hindiName = station.hindiName,
              id = Id station.stopCode,
              integratedBppConfigId = integratedBPPConfig.id,
              lat = Just station.stopPoint.lat,
              lon = Just station.stopPoint.lon,
              merchantId = integratedBPPConfig.merchantId,
              merchantOperatingCityId = integratedBPPConfig.merchantOperatingCityId,
              name = station.stopName,
              possibleTypes = Nothing,
              regionalName = station.regionalName,
              suggestedDestinations = join (snd <$> HM.lookup station.stopCode stationAddressMap),
              geoJson = station.geoJson,
              gates = station.gates,
              timeBounds = Unbounded,
              vehicleType = BecknFRFSUtils.becknVehicleCategoryToFrfsVehicleCategory integratedBPPConfig.vehicleCategory,
              parentStopCode = station.parentStopCode,
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
  Bool ->
  m [Route.Route]
parseRoutesFromInMemoryServer routes integratedBppConfigId merchantId merchantOperatingCityId isPolylineRequired = do
  let routeIds = map (.id) routes
  routePolylines <- if isPolylineRequired then QRoutePolylines.getByRouteIdsAndCity routeIds merchantOperatingCityId else pure []
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
  routes <- parseRoutesFromInMemoryServer [routeInfoNandi] integratedBppConfigId merchantId merchantOperatingCityId True
  case routes of
    (route : _) -> pure route
    _ -> throwError $ InternalError "Failed to parse route"

parseRouteStopMappingInMemoryServer ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
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
        return $
          RouteStopMapping
            { estimatedTravelTimeFromPreviousStop = mapping.estimatedTravelTimeFromPreviousStop,
              integratedBppConfigId = integratedBPPConfig.id,
              merchantId,
              merchantOperatingCityId,
              Domain.Types.RouteStopMapping.providerCode = if mapping.providerCode == "GTFS" then stopCode else mapping.providerCode,
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
  Maybe Bool ->
  m (Maybe VehicleServiceTypeResponse)
getVehicleServiceType integratedBPPConfig vehicleNumber mbPassVerifyReq = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getVehicleServiceType baseUrl integratedBPPConfig.feedKey vehicleNumber mbPassVerifyReq

getVehicleInfo ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe VehicleInfoResponse)
getVehicleInfo integratedBPPConfig vehicleNumber = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getVehicleInfo baseUrl integratedBPPConfig.feedKey vehicleNumber

getVehicleOperationInfo ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe VehicleOperationInfo)
getVehicleOperationInfo integratedBPPConfig fleetNo = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getVehicleOperationInfo baseUrl fleetNo

-- Get Stop Code From Provider Code

getStopCodeFromProviderCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe Text)
getStopCodeFromProviderCode integratedBPPConfig providerStopCode = IM.withInMemCache ["SCFPC", integratedBPPConfig.id.getId, providerStopCode] 3600 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  resp <- Flow.getStopCode baseUrl integratedBPPConfig.feedKey providerStopCode
  return (resp <&> (.stop_code))

getNandiTripInfo ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe TripInfoResponse)
getNandiTripInfo integratedBPPConfig tripId = IM.withInMemCache ["NandiTripInfo", integratedBPPConfig.id.getId, tripId] 3600 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  let updatedTripId = integratedBPPConfig.feedKey <> ":" <> tripId
  Flow.getNandiTripInfo baseUrl updatedTripId

getGtfsVersion ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  m Text
getGtfsVersion integratedBPPConfig = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getGtfsVersion baseUrl integratedBPPConfig.feedKey

getExampleTrip ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  IntegratedBPPConfig ->
  Text ->
  m (Maybe TripDetails)
getExampleTrip integratedBPPConfig routeId = IM.withInMemCache ["ExampleTrip", integratedBPPConfig.id.getId, routeId] 1800 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getExampleTrip baseUrl integratedBPPConfig.feedKey routeId

getAlternateStationsByGtfsIdAndStopCode ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  IntegratedBPPConfig ->
  m [Station.Station]
getAlternateStationsByGtfsIdAndStopCode stopCode integratedBPPConfig = IM.withInMemCache ["AlternateStops", stopCode, integratedBPPConfig.id.getId] 3600 $ do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  stations <- Flow.getAlternateStationsByGtfsIdAndStopCode baseUrl integratedBPPConfig.feedKey stopCode
  parseStationsFromInMemoryServer stations integratedBPPConfig False

-- Helper function to find a specific stop in TripDetails
findTripStopByStopCode :: TripDetails -> Text -> Maybe TripStopDetail
findTripStopByStopCode tripDetails stopCode =
  find (\stop -> stop.stopCode == stopCode) tripDetails.stops

-- Helper function to extract stage information from TripStopDetail
extractStageFromTripStop :: TripStopDetail -> Maybe Int
extractStageFromTripStop tripStop = do
  extraInfo <- tripStop.extraInfo
  fareStageText <- extraInfo.fareStageNumber
  readMaybe (T.unpack fareStageText)

-- Helper function to extract isStageStop from TripStopDetail
extractIsStageStopFromTripStop :: TripStopDetail -> Maybe Bool
extractIsStageStopFromTripStop tripStop = do
  extraInfo <- tripStop.extraInfo
  extraInfo.isStageStop

-- Helper function to extract providerStopCode from TripStopDetail
extractProviderStopCodeFromTripStop :: TripStopDetail -> Maybe Text
extractProviderStopCodeFromTripStop tripStop = do
  extraInfo <- tripStop.extraInfo
  extraInfo.providerStopCode

-- Helper function to extract platformCode from TripStopDetail
extractPlatformCodeFromTripStop :: TripStopDetail -> Maybe Text
extractPlatformCodeFromTripStop tripStop = tripStop.platformCode
