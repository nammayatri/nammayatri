module Domain.Action.UI.NearbyBuses (postNearbyBusBooking, getNextVehicleDetails, utcToIST, getTimetableStop, getSimpleNearbyBuses) where

import qualified API.Types.UI.NearbyBuses
import qualified BecknV2.FRFS.Enums as Spe
import qualified BecknV2.OnDemand.Enums
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as M
import qualified Data.Set as Set
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.RiderConfig as DomainRiderConfig
import Domain.Types.RouteStopTimeTable
import qualified Environment
import EulerHS.Prelude hiding (decodeUtf8, id)
import ExternalBPP.ExternalAPI.CallAPI as CallAPI
import qualified ExternalBPP.Flow as Flow
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Common.FRFS (getNearbyBusesFRFS)
import Lib.JourneyModule.Utils as JourneyUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.RouteStopTimeTable as GRSM
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RecentLocation as QRecentLocation
import Tools.Error

postNearbyBusBooking ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyBuses.NearbyBusesRequest ->
    Environment.Flow API.Types.UI.NearbyBuses.NearbyBusesResponse
  )
postNearbyBusBooking (mbPersonId, _) req = do
  riderId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound "No person found")
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  -- let radius :: Double = fromMaybe 0.5 riderConfig.nearbyDriverSearchRadius --TODO: To be moved to config.
  -- nearbyBuses <-
  --   if req.requireNearbyBuses
  --     then getNearbyBuses person.merchantOperatingCityId radius req
  --     else return []

  simpleNearbyBuses <-
    if req.requireNearbyBuses
      then getSimpleNearbyBuses person.merchantOperatingCityId riderConfig req
      else return []

  recentRides <-
    if req.requireRecentRide
      then getRecentRides person req
      else return []

  -- Return the complete response
  return $ API.Types.UI.NearbyBuses.NearbyBusesResponse simpleNearbyBuses (catMaybes recentRides)

castToEntityType :: Spe.VehicleCategory -> Domain.Types.RecentLocation.EntityType
castToEntityType Spe.BUS = Domain.Types.RecentLocation.BUS
castToEntityType Spe.METRO = Domain.Types.RecentLocation.METRO
castToEntityType Spe.SUBWAY = Domain.Types.RecentLocation.SUBWAY

castToOnDemandVehicleCategory :: Spe.VehicleCategory -> BecknV2.OnDemand.Enums.VehicleCategory
castToOnDemandVehicleCategory Spe.BUS = BecknV2.OnDemand.Enums.BUS
castToOnDemandVehicleCategory Spe.METRO = BecknV2.OnDemand.Enums.METRO
castToOnDemandVehicleCategory Spe.SUBWAY = BecknV2.OnDemand.Enums.SUBWAY

getSimpleNearbyBuses :: Id MerchantOperatingCity -> DomainRiderConfig.RiderConfig -> API.Types.UI.NearbyBuses.NearbyBusesRequest -> Environment.Flow [API.Types.UI.NearbyBuses.NearbyBus]
getSimpleNearbyBuses merchantOperatingCityId riderConfig req = do
  let vehicleCategory = castToOnDemandVehicleCategory req.vehicleType
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing merchantOperatingCityId vehicleCategory req.platformType
  buses <- getNearbyBusesFRFS (Maps.LatLong req.userLat req.userLon) riderConfig integratedBPPConfig
  logDebug $ "Nearby buses: " <> show buses
  let busesWithMostMatchingRouteStates = mapMaybe (\bus -> (bus,) <$> sortOnRouteMatchConfidence bus.routes_info) buses
  logDebug $ "Buses with most matching route states: " <> show busesWithMostMatchingRouteStates
  logDebug $ "Number of nearby buses: " <> show (length buses)

  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOperatingCityId vehicleCategory req.platformType

  let vehicleNumbers = Set.toList $ Set.fromList $ mapMaybe (.vehicle_number) buses
  logDebug $ "Vehicle numbers: " <> show vehicleNumbers
  logDebug $ "Number of unique vehicle numbers: " <> show (length vehicleNumbers)

  busRouteMapping <- forM vehicleNumbers $ \vehicleNumber -> do
    mbResult <- SIBC.fetchFirstIntegratedBPPConfigResult integratedBPPConfigs $ \config ->
      maybeToList <$> OTPRest.getVehicleServiceType config vehicleNumber Nothing
    pure $ Kernel.Prelude.listToMaybe mbResult

  let successfulMappings = catMaybes busRouteMapping

  serviceTypeMap :: HashMap.HashMap Text (Spe.ServiceTierType, Maybe Text) <-
    HashMap.fromList
      <$> mapM
        ( \m -> do
            frfsServiceTier <- SIBC.fetchFirstIntegratedBPPConfigMaybeResult integratedBPPConfigs $ \config -> do
              CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId m.service_type riderConfig.merchantOperatingCityId config.id
            return (m.vehicle_no, (m.service_type, frfsServiceTier <&> (.shortName)))
        )
        successfulMappings

  pure $
    map
      ( \(bus, (route_id, routeInfo)) ->
          let maybeServiceType = bus.vehicle_number >>= (`HashMap.lookup` serviceTypeMap)
           in API.Types.UI.NearbyBuses.NearbyBus
                { currentLocation = Maps.LatLong bus.latitude bus.longitude,
                  distance = Nothing,
                  routeCode = route_id,
                  routeState = Just routeInfo.route_state,
                  serviceType = fst <$> maybeServiceType,
                  serviceTierName = snd =<< maybeServiceType,
                  shortName = routeInfo.route_number,
                  vehicleNumber = bus.vehicle_number,
                  bearing = round <$> bus.bearing
                }
      )
      busesWithMostMatchingRouteStates
  where
    sortOnRouteMatchConfidence :: Maybe (M.Map CQMMB.BusRouteId CQMMB.BusRouteInfo) -> Maybe (CQMMB.BusRouteId, CQMMB.BusRouteInfo)
    sortOnRouteMatchConfidence mbRouteInfo = do
      let routeInfoList = M.toList (fromMaybe (M.empty) mbRouteInfo)
      Kernel.Prelude.listToMaybe $ sortOn (\(_, a) -> a.route_state) routeInfoList

getRecentRides :: Domain.Types.Person.Person -> API.Types.UI.NearbyBuses.NearbyBusesRequest -> Environment.Flow [Maybe API.Types.UI.NearbyBuses.RecentRide]
getRecentRides person req = do
  let entityType = castToEntityType req.vehicleType
  recentLocations <- QRecentLocation.findRecentLocationsByEntityType entityType person.id person.merchantOperatingCityId
  forM recentLocations $ \recentLoc -> do
    case (recentLoc.fromStopCode, recentLoc.toStopCode, recentLoc.routeCode) of
      (Just fromStopCode, Just toStopCode, Just routeCode) -> do
        let vehicleCategory = castToOnDemandVehicleCategory req.vehicleType
        SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId vehicleCategory req.platformType
          >>= \case
            [] -> return Nothing
            integratedBPPConfigs@(_ : _) -> do
              merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantDoesNotExist person.merchantId.getId)
              merchantOperatingCity <- QMerchantOperatingCity.findById person.merchantOperatingCityId >>= fromMaybeM (InternalError "No merchant operating city found")
              becknConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id "FRFS" vehicleCategory >>= fromMaybeM (InternalError "No beckn config found")
              mbFare <-
                Kernel.Prelude.listToMaybe
                  <$> ( SIBC.fetchFirstIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig -> do
                          let fareRouteDetails = fromList [CallAPI.BasicRouteDetail {routeCode, startStopCode = fromStopCode, endStopCode = toStopCode}]
                          snd <$> Flow.getFares person.id merchant merchantOperatingCity integratedBPPConfig becknConfig fareRouteDetails req.vehicleType Nothing Nothing
                      )
              return $
                (mbFare >>= (\fare -> find (\category -> category.category == ADULT) fare.categories)) <&> \fare ->
                  API.Types.UI.NearbyBuses.RecentRide
                    { fare = fare.price,
                      fromStopCode = fromStopCode,
                      routeCode = Just routeCode,
                      toStopCode = toStopCode
                    }
      _ -> return Nothing

getNextVehicleDetails ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Text ->
    Text ->
    Kernel.Prelude.Maybe Spe.VehicleCategory ->
    Environment.Flow JourneyUtils.UpcomingTripInfo
  )
getNextVehicleDetails (mbPersonId, mid) routeCode stopCode mbVehicleType = do
  riderId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound riderId.getId)
  now <- getCurrentTime
  let vehicleType = maybe BecknV2.OnDemand.Enums.BUS castToOnDemandVehicleCategory mbVehicleType
  JourneyUtils.findUpcomingTrips routeCode stopCode Nothing now mid person.merchantOperatingCityId vehicleType

fetchPlatformCodesFromRedis :: [Text] -> Text -> Environment.Flow (HashMap.HashMap Text Text)
fetchPlatformCodesFromRedis tripIds stopCode = do
  if null tripIds
    then do
      logDebug $ "fetchPlatformCodesFromRedis: No tripIds provided, returning empty map"
      return HashMap.empty
    else do
      let platformHashKey = "platform-codes-hashmap"
      let redisKeys = map (\tripId -> tripId <> ":" <> stopCode) tripIds
      logDebug $ "fetchPlatformCodesFromRedis: Fetching platform codes for " <> show (length redisKeys) <> " tripId:stopCode keys for stopCode: " <> stopCode
      platformCodes <- CQMMB.withCrossAppRedisNew $ Hedis.hmGet platformHashKey redisKeys
      let result = HashMap.fromList $ catMaybes $ zipWith (\tripId mbCode -> (tripId,) <$> mbCode) tripIds platformCodes
      logDebug $ "fetchPlatformCodesFromRedis: Retrieved " <> show (HashMap.size result) <> " platform codes"
      return result

fetchCancelledTrainsFromRedis :: Environment.Flow [Text]
fetchCancelledTrainsFromRedis = do
  cancelledTrains <- CQMMB.withCrossAppRedisNew $ Hedis.get "trains:cancelled"
  case cancelledTrains of
    Nothing -> do
      logDebug "fetchCancelledTrainsFromRedis: No cancelled trains data found in Redis"
      return []
    Just cancelledTrainsList -> do
      logDebug $ "fetchCancelledTrainsFromRedis: Retrieved " <> show (length cancelledTrainsList) <> " cancelled trains"
      return cancelledTrainsList

getTimetableStop ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Text ->
    Text ->
    Kernel.Prelude.Maybe Text ->
    Kernel.Prelude.Maybe (Spe.VehicleCategory) ->
    Environment.Flow API.Types.UI.NearbyBuses.TimetableResponse
  )
getTimetableStop (mbPersonId, mid) routeCode fromStopCode mbToCode mbVehicleType = do
  riderId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound riderId.getId)
  let vehicleType = maybe BecknV2.OnDemand.Enums.BUS castToOnDemandVehicleCategory mbVehicleType
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId vehicleType DIBC.MULTIMODAL
  currentTime <- getCurrentTime

  -- Fetch cancelled trains from Redis
  cancelledTrains <- fetchCancelledTrainsFromRedis

  routeStopTimeTables <-
    SIBC.fetchFirstIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig -> do
      routeCodes <-
        maybe
          (pure [routeCode])
          (\toCode -> JourneyUtils.getRouteCodesFromTo fromStopCode toCode integratedBPPConfig)
          mbToCode
      staticTimetable <- GRSM.findByRouteCodeAndStopCode integratedBPPConfig mid person.merchantOperatingCityId routeCodes fromStopCode False False
      liveSubWayTimings <- JourneyUtils.fetchLiveSubwayTimings routeCodes fromStopCode currentTime integratedBPPConfig mid person.merchantOperatingCityId
      let liveSubWayTrips = map (.tripId.getId) liveSubWayTimings
      let allTrips = liveSubWayTimings ++ filter (\trip -> trip.tripId.getId `notElem` liveSubWayTrips) staticTimetable
      -- Filter out cancelled trains
      let filteredTrips = filter (\trip -> trip.tripId.getId `notElem` cancelledTrains) allTrips
      return filteredTrips

  let tripIds = map (.tripId.getId) routeStopTimeTables
  platformCodeMap <- fetchPlatformCodesFromRedis tripIds fromStopCode

  return $ API.Types.UI.NearbyBuses.TimetableResponse $ map (convertToTimetableEntry platformCodeMap) routeStopTimeTables
  where
    convertToTimetableEntry :: HashMap.HashMap Text Text -> RouteStopTimeTable -> API.Types.UI.NearbyBuses.TimetableEntry
    convertToTimetableEntry platformCodeMap routeStopTimeTable = do
      let tripId = routeStopTimeTable.tripId.getId
      let updatedPlatformCode = HashMap.lookup tripId platformCodeMap <|> routeStopTimeTable.platformCode
      API.Types.UI.NearbyBuses.TimetableEntry
        { timeOfArrival = routeStopTimeTable.timeOfArrival,
          timeOfDeparture = routeStopTimeTable.timeOfDeparture,
          serviceTierType = routeStopTimeTable.serviceTierType,
          platformCode = updatedPlatformCode,
          tripId = tripId,
          delay = routeStopTimeTable.delay,
          source = routeStopTimeTable.source
        }
