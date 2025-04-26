module Domain.Action.UI.NearbyBuses (postNearbyBusBooking, getNextVehicleDetails) where

import qualified API.Types.UI.NearbyBuses
import qualified BecknV2.FRFS.Enums as Spe
import qualified BecknV2.OnDemand.Enums
import Data.List (nub)
import Data.Text.Encoding (decodeUtf8)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.VehicleRouteMapping as DTVRM
import qualified Environment
import EulerHS.Prelude hiding (decodeUtf8, id)
import qualified ExternalBPP.CallAPI as CallAPI
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyModule.Utils as JourneyUtils
import Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.BecknConfig as QBecknConfig
import qualified Storage.Queries.IntegratedBPPConfig as QIntegratedBPPConfig
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RecentLocation as QRecentLocation
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.VehicleRouteMapping as QVehicleRouteMapping
import Tools.Error

nearbyBusKey :: Text
nearbyBusKey = "bus_locations"

postNearbyBusBooking ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyBuses.NearbyBusesRequest ->
    Environment.Flow API.Types.UI.NearbyBuses.NearbyBusesResponse
  )
postNearbyBusBooking (mbPersonId, merchantId) req = do
  riderId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound "No person found")
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  let radius :: Double = fromMaybe 0.5 riderConfig.nearbyDriverSearchRadius --TODO: To be moved to config.

  -- Convert ByteString to Text after geo search
  busesBS :: [ByteString] <- CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat req.userLat req.userLon) (Hedis.ByRadius radius "km")

  logDebug $ "BusesBS: " <> show busesBS
  let buses'' = map decodeUtf8 busesBS
  logDebug $ "Buses: " <> show buses''
  busesBS' :: [ByteString] <- CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat req.userLon req.userLat) (Hedis.ByRadius radius "km")
  let buses = map decodeUtf8 busesBS'
  logDebug $ "BusesBS': " <> show busesBS'
  logDebug $ "Buses': " <> show buses

  busRouteMapping <- QVehicleRouteMapping.findAllByVehicleNumber buses
  let routeIds :: [Text] = nub $ map DTVRM.routeId busRouteMapping

  logDebug $ "Route IDs: " <> show routeIds

  recentLocations <- QRecentLocation.findRecentLocationsByRouteIds riderId routeIds

  -- Process recent locations to build RecentRide objects only if requireRecentRide is true
  recentRidesNested <-
    if req.requireRecentRide
      then forM recentLocations $ \recentLoc -> do
        if isJust recentLoc.fromStopCode && isJust recentLoc.routeCode && isValidEntityType recentLoc.entityType
          then do
            let (vehicleCategory, specVehicleCategory) = getVehicleCategories recentLoc.entityType
            integratedBPPConfig' <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" person.merchantOperatingCityId vehicleCategory req.platformType
            case integratedBPPConfig' of
              Just integratedBPPConfig -> do
                mbStopMapping <- Kernel.Prelude.listToMaybe <$> QRouteStopMapping.findByStopCode (fromMaybe "" recentLoc.stopCode) integratedBPPConfig.id
                case mbStopMapping of
                  Just stopMapping -> do
                    merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
                    becknConfig <- QBecknConfig.findByMerchantIdDomainAndVehicle (Just merchant.id) "FRFS" vehicleCategory >>= fromMaybeM (InternalError "No beckn config found")
                    merchantOperatingCity <- QMerchantOperatingCity.findById person.merchantOperatingCityId >>= fromMaybeM (InternalError "No merchant operating city found")
                    getFares <- Kernel.Prelude.listToMaybe <$> CallAPI.getFares riderId merchant merchantOperatingCity becknConfig (fromMaybe "" recentLoc.routeCode) (fromMaybe "" recentLoc.fromStopCode) (fromMaybe "" recentLoc.stopCode) specVehicleCategory req.platformType
                    case getFares of
                      Just fares ->
                        return
                          [ API.Types.UI.NearbyBuses.RecentRide
                              { fare = fares.price,
                                fromStopCode = fromMaybe "" recentLoc.fromStopCode,
                                fromStopName = fromMaybe "" recentLoc.fromStopName,
                                routeCode = recentLoc.routeCode,
                                toStopName = stopMapping.stopName,
                                toStopCode = fromMaybe "" recentLoc.stopCode
                              }
                          ]
                      Nothing -> return []
                  Nothing -> return []
              Nothing -> return []
          else return []
      else return []

  -- Flatten the nested list and prepare the response
  let recentRides = concat recentRidesNested

  -- Process nearby buses only if requireNearbyBuses is true
  nearbyBuses <-
    if req.requireNearbyBuses
      then do
        allBusesForRides <- mapM CQMMB.getRoutesBuses routeIds
        logDebug $ "All buses for rides: " <> show allBusesForRides
        let allBusesData =
              map
                ( \routeData -> do
                    let filteredBus = filter (\bus -> elem bus.vehicleNumber buses) routeData.buses
                    (routeData {buses = filteredBus})
                )
                allBusesForRides
        logDebug $ "All buses data: " <> show allBusesData

        -- Create nearby bus objects
        mapM
          ( \busData -> do
              mapM
                ( \bus -> do
                    let busEta = Kernel.Prelude.listToMaybe $ fromMaybe [] bus.busData.eta_data
                    return $
                      API.Types.UI.NearbyBuses.NearbyBus
                        { capacity = Nothing,
                          currentLocation = Maps.LatLong bus.busData.latitude bus.busData.longitude,
                          distance = Nothing,
                          eta = busEta >>= (\etaD -> Just etaD.arrivalTime),
                          nextStop = busEta >>= (\etaD -> Just etaD.stopName),
                          occupancy = Nothing,
                          routeCode = busData.routeId,
                          serviceType = Nothing,
                          vehicleNumber = Just $ bus.vehicleNumber
                        }
                )
                busData.buses
          )
          allBusesData
      else return []

  -- Return the complete response
  return $ API.Types.UI.NearbyBuses.NearbyBusesResponse (concat nearbyBuses) recentRides
  where
    isValidEntityType entityType = entityType `elem` [Domain.Types.RecentLocation.BUS, Domain.Types.RecentLocation.METRO, Domain.Types.RecentLocation.SUBWAY]

    getVehicleCategories :: Domain.Types.RecentLocation.EntityType -> (BecknV2.OnDemand.Enums.VehicleCategory, Spe.VehicleCategory)
    getVehicleCategories Domain.Types.RecentLocation.BUS = (BecknV2.OnDemand.Enums.BUS, Spe.BUS)
    getVehicleCategories Domain.Types.RecentLocation.METRO = (BecknV2.OnDemand.Enums.METRO, Spe.METRO)
    getVehicleCategories Domain.Types.RecentLocation.SUBWAY = (BecknV2.OnDemand.Enums.SUBWAY, Spe.SUBWAY)
    getVehicleCategories _ = (BecknV2.OnDemand.Enums.BUS, Spe.BUS) -- This case will never be hit due to isValidEntityType check

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
  integratedBPPConfig <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" person.merchantOperatingCityId vehicleType DIBC.MULTIMODAL >>= fromMaybeM (InternalError "No integrated bpp config found")
  JourneyUtils.findUpcomingTrips [routeCode] stopCode Nothing now integratedBPPConfig.id mid person.merchantOperatingCityId
  where
    castToOnDemandVehicleCategory :: Spe.VehicleCategory -> BecknV2.OnDemand.Enums.VehicleCategory
    castToOnDemandVehicleCategory Spe.BUS = BecknV2.OnDemand.Enums.BUS
    castToOnDemandVehicleCategory Spe.METRO = BecknV2.OnDemand.Enums.METRO
    castToOnDemandVehicleCategory Spe.SUBWAY = BecknV2.OnDemand.Enums.SUBWAY
