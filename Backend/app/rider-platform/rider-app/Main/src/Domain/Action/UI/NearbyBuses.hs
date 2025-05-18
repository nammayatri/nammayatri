module Domain.Action.UI.NearbyBuses (postNearbyBusBooking, getNextVehicleDetails, utcToIST) where

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
import qualified ExternalBPP.Flow as Flow
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
postNearbyBusBooking (mbPersonId, _) req = do
  riderId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound "No person found")
  riderConfig <- QRiderConfig.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let radius :: Double = fromMaybe 0.5 riderConfig.nearbyDriverSearchRadius --TODO: To be moved to config.
  nearbyBuses <-
    if req.requireNearbyBuses
      then getNearbyBuses radius req
      else return []

  recentRides <-
    if req.requireRecentRide
      then getRecentRides person req
      else return []

  -- Return the complete response
  return $ API.Types.UI.NearbyBuses.NearbyBusesResponse (concat nearbyBuses) (catMaybes recentRides)

castToEntityType :: Spe.VehicleCategory -> Domain.Types.RecentLocation.EntityType
castToEntityType Spe.BUS = Domain.Types.RecentLocation.BUS
castToEntityType Spe.METRO = Domain.Types.RecentLocation.METRO
castToEntityType Spe.SUBWAY = Domain.Types.RecentLocation.SUBWAY

castToOnDemandVehicleCategory :: Spe.VehicleCategory -> BecknV2.OnDemand.Enums.VehicleCategory
castToOnDemandVehicleCategory Spe.BUS = BecknV2.OnDemand.Enums.BUS
castToOnDemandVehicleCategory Spe.METRO = BecknV2.OnDemand.Enums.METRO
castToOnDemandVehicleCategory Spe.SUBWAY = BecknV2.OnDemand.Enums.SUBWAY

getNearbyBuses :: Double -> API.Types.UI.NearbyBuses.NearbyBusesRequest -> Environment.Flow [[API.Types.UI.NearbyBuses.NearbyBus]]
getNearbyBuses nearbyDriverSearchRadius req = do
  busesBS :: [ByteString] <- CQMMB.withCrossAppRedisNew $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat req.userLon req.userLat) (Hedis.ByRadius nearbyDriverSearchRadius "km")
  let buses = map decodeUtf8 busesBS
  logDebug $ "BusesBS: " <> show busesBS
  logDebug $ "Buses: " <> show buses
  busRouteMapping <- QVehicleRouteMapping.findAllByVehicleNumber buses
  let routeIds :: [Text] = nub $ map DTVRM.routeId busRouteMapping
  logDebug $ "Route IDs: " <> show routeIds
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
                    eta = busEta >>= (\etaD -> Just (CQMMB.utcToIST etaD.arrivalTime)),
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

getRecentRides :: Domain.Types.Person.Person -> API.Types.UI.NearbyBuses.NearbyBusesRequest -> Environment.Flow [Maybe API.Types.UI.NearbyBuses.RecentRide]
getRecentRides person req = do
  let entityType = castToEntityType req.vehicleType
  recentLocations <- QRecentLocation.findRecentLocationsByEntityType entityType person.id person.merchantOperatingCityId
  forM recentLocations $ \recentLoc -> do
    case (recentLoc.fromStopCode, recentLoc.toStopCode, recentLoc.routeCode) of
      (Just fromStopCode, Just toStopCode, Just routeCode) -> do
        let vehicleCategory = castToOnDemandVehicleCategory req.vehicleType
        mbIntegratedBPPConfig <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" person.merchantOperatingCityId vehicleCategory req.platformType
        case mbIntegratedBPPConfig of
          Just integratedBPPConfig -> do
            merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantDoesNotExist person.merchantId.getId)
            becknConfig <- QBecknConfig.findByMerchantIdDomainAndVehicle (Just merchant.id) "FRFS" vehicleCategory >>= fromMaybeM (InternalError "No beckn config found")
            merchantOperatingCity <- QMerchantOperatingCity.findById person.merchantOperatingCityId >>= fromMaybeM (InternalError "No merchant operating city found")
            mbFare <- Kernel.Prelude.listToMaybe <$> Flow.getFares person.id merchant merchantOperatingCity integratedBPPConfig becknConfig routeCode fromStopCode toStopCode req.vehicleType
            return $
              mbFare <&> \fare -> do
                API.Types.UI.NearbyBuses.RecentRide
                  { fare = fare.price,
                    fromStopCode = fromStopCode,
                    routeCode = Just routeCode,
                    toStopCode = toStopCode
                  }
          Nothing -> return Nothing
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
  integratedBPPConfig <- QIntegratedBPPConfig.findByDomainAndCityAndVehicleCategory "FRFS" person.merchantOperatingCityId vehicleType DIBC.MULTIMODAL >>= fromMaybeM (InternalError "No integrated bpp config found")
  JourneyUtils.findUpcomingTrips [routeCode] stopCode Nothing now integratedBPPConfig mid person.merchantOperatingCityId vehicleType
