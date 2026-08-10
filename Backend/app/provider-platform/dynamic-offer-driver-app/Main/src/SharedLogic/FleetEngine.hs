{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Mirrors the BPP ride lifecycle into Google Fleet Engine trips. Best-effort:
-- a no-op when the city has no Fleet Engine service config, and callers must
-- 'fork' entry points so failures never disturb the ride flow.
--
-- tripId = BPP rideId (1:1); vehicleId = driverId (1:1).
module SharedLogic.FleetEngine
  ( FleetEngineFlow,
    mkDriverToken,
    notifyDriverOnline,
    notifyTripCreated,
    notifyDriverArrived,
    notifyRideStarted,
    notifyRideCompleted,
    notifyTripCancelled,
    notifyDropoffChanged,
    notifyPickupChanged,
    notifyStopsChanged,
    notifyStopArrived,
    notifyStopDeparted,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as DVV
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.FleetEngine.Auth as FEAuth
import qualified Kernel.External.FleetEngine.Client as FEClient
import Kernel.External.FleetEngine.Config (FleetEngineCfg)
import qualified Kernel.External.FleetEngine.Config as FEConfig
import Kernel.External.FleetEngine.Types (Trip (..))
import qualified Kernel.External.FleetEngine.Types as FETypes
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Vehicle as QVeh

type FleetEngineFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r
  )

getFleetEngineCfg ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m (Maybe FleetEngineCfg)
getFleetEngineCfg merchantOpCityId = do
  mbServiceConfig <- QOMSC.findByServiceAndCity (DOSC.FleetEngineService DOSC.GoogleFleetEngine) merchantOpCityId
  pure $ case mbServiceConfig of
    Just sc -> case sc.serviceConfig of
      DOSC.FleetEngineServiceConfig cfg | cfg.enabled == Just True -> Just cfg
      _ -> Nothing
    Nothing -> Nothing

-- | Returns 'Nothing' when Fleet Engine is off for the city, or on config/token
-- errors (logged, not thrown) — callers decide how to surface each case.
mkDriverToken ::
  FleetEngineFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m (Maybe (Text, Text, Text))
mkDriverToken merchantOpCityId driverId = do
  mbCfg <- getFleetEngineCfg merchantOpCityId
  case mbCfg of
    Nothing -> pure Nothing -- no config for this city
    Just cfg | cfg.enabled /= Just True -> pure Nothing -- config present but kill switch off (Nothing / Just False both = off)
    Just cfg -> do
      saText <- decrypt cfg.driverServiceAccountJson
      case FEAuth.parseServiceAccount saText of
        Left err -> do
          logError $ "FleetEngine: invalid driver service account for city " <> merchantOpCityId.getId <> ": " <> T.pack err
          pure Nothing
        Right sa -> do
          let vehicleId = driverId.getId -- Fleet Engine vehicleId is the driverId (driver<->vehicle is 1:1)
              ttl = fromMaybe FEConfig.defaultDriverTokenTtl cfg.driverTokenTtlSeconds
          eToken <- liftIO $ FEAuth.mintFleetEngineToken sa (FEAuth.DriverToken vehicleId) ttl
          case eToken of
            Left err -> do
              logError $ "FleetEngine: driver token mint failed for city " <> merchantOpCityId.getId <> ": " <> T.pack err
              pure Nothing
            Right token -> pure $ Just (token, vehicleId, cfg.providerId)

-- | Registers the vehicle so a pool trip landing before this driver's first ride finds it.
notifyDriverOnline ::
  FleetEngineFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m ()
notifyDriverOnline merchantOpCityId driverId =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    logInfo $ "FleetEngine: driverOnline provider=" <> providerId <> " driverId=" <> driverId.getId
    mbVeh <- QVeh.findById driverId
    ensureVehicleExists baseUrl providerId token driverId.getId mbVeh

-- | Silent skip when Fleet Engine is off; logs (never throws) on config/token errors.
withFleetEngine ::
  FleetEngineFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  (Text -> Text -> BaseUrl -> m ()) -> -- providerId -> serverToken -> baseUrl
  m ()
withFleetEngine merchantOpCityId action = do
  mbCfg <- getFleetEngineCfg merchantOpCityId
  case mbCfg of
    Nothing -> pure () -- no config for this city
    Just cfg | cfg.enabled /= Just True -> pure () -- config present but kill switch off (Nothing / Just False both = off)
    Just cfg -> do
      saText <- decrypt cfg.serverServiceAccountJson
      case FEAuth.parseServiceAccount saText of
        Left err -> logError $ "FleetEngine: invalid server service account for city " <> merchantOpCityId.getId <> ": " <> T.pack err
        Right sa -> do
          let ttl = fromMaybe FEConfig.defaultServerTokenTtl cfg.serverTokenTtlSeconds
          eToken <- liftIO $ FEAuth.mintFleetEngineToken sa FEAuth.ServerToken ttl
          case eToken of
            Left err -> logError $ "FleetEngine: server token mint failed for city " <> merchantOpCityId.getId <> ": " <> T.pack err
            Right token -> action cfg.providerId token (fromMaybe FEClient.defaultFleetEngineBaseUrl cfg.fleetEngineUrl)

-- | Ensure the vehicle exists, then create the trip and assign it (NEW -> ENROUTE_TO_PICKUP).
notifyTripCreated :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyTripCreated booking ride =
  withFleetEngine booking.merchantOperatingCityId $ \providerId token baseUrl -> do
    let tripId = ride.id.getId
        vehicleId = ride.driverId.getId -- driver<->vehicle is 1:1
        pickupPoint = Just (FETypes.LatLng booking.fromLocation.lat booking.fromLocation.lon)
        dropoffPoint = (\loc -> FETypes.LatLng loc.lat loc.lon) <$> booking.toLocation
    mbVeh <- QVeh.findById ride.driverId
    ensureVehicleExists baseUrl providerId token vehicleId mbVeh
    FEClient.createTrip baseUrl providerId token tripId $
      FETypes.mkCreateTripBody (bookingToTripType booking) pickupPoint dropoffPoint (bookingToNumberOfPassengers booking)
    FEClient.assignVehicleAndStart baseUrl providerId token tripId vehicleId

-- | GET-first: don't overwrite state the Driver SDK may set once mobile integration lands.
ensureVehicleExists :: FleetEngineFlow m r => BaseUrl -> Text -> Text -> Text -> Maybe DVeh.Vehicle -> m ()
ensureVehicleExists baseUrl providerId token vehicleId mbVeh = do
  mbFleetVeh <- FEClient.getVehicle baseUrl providerId token vehicleId
  when (isNothing mbFleetVeh) $
    FEClient.createVehicle baseUrl providerId token vehicleId (mkFleetEngineVehicle mbVeh)

mkFleetEngineVehicle :: Maybe DVeh.Vehicle -> FETypes.Vehicle
mkFleetEngineVehicle mbVeh =
  FETypes.Vehicle
    { vehicleState = FETypes.OFFLINE,
      supportedTripTypes = [FETypes.EXCLUSIVE, FETypes.SHARED],
      maximumCapacity = fromMaybe 4 (mbVeh >>= (.capacity)),
      vehicleType = FETypes.VehicleType {category = mapVehicleCategory mbVeh}
    }

mapVehicleCategory :: Maybe DVeh.Vehicle -> FETypes.VehicleCategory
mapVehicleCategory Nothing = FETypes.UNKNOWN_VEHICLE_CATEGORY
mapVehicleCategory (Just veh) = case DVV.castVehicleVariantToVehicleCategory veh.variant of
  DVC.CAR -> FETypes.TAXI
  DVC.MOTORCYCLE -> FETypes.TWO_WHEELER
  DVC.AUTO_CATEGORY -> FETypes.AUTO
  DVC.TRUCK -> FETypes.TRUCK
  DVC.TOTO -> FETypes.AUTO
  _ -> FETypes.UNKNOWN_VEHICLE_CATEGORY

bookingToTripType :: DRB.Booking -> FETypes.TripType
bookingToTripType _booking = FETypes.EXCLUSIVE

bookingToNumberOfPassengers :: DRB.Booking -> Maybe Int
bookingToNumberOfPassengers _booking = Nothing

notifyTripStatus :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> FETypes.TripStatus -> m ()
notifyTripStatus merchantOpCityId rideId status =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    logInfo $ "FleetEngine: updateTripStatus provider=" <> providerId <> " tripId=" <> rideId.getId <> " status=" <> show status
    FEClient.updateTripStatus baseUrl providerId token rideId.getId status

notifyDriverArrived :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyDriverArrived booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.ARRIVED_AT_PICKUP

-- Trips with intermediates must transition ENROUTE_TO_INTERMEDIATE_DESTINATION first (per FE multi-destination docs).
notifyRideStarted :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyRideStarted booking ride =
  withFleetEngine booking.merchantOperatingCityId $ \providerId token baseUrl -> do
    stops <- QLM.getLatestStopsByEntityId ride.id.getId
    if null stops
      then do
        logInfo $ "FleetEngine: rideStarted (enroute to dropoff) provider=" <> providerId <> " tripId=" <> ride.id.getId
        FEClient.updateTripStatus baseUrl providerId token ride.id.getId FETypes.ENROUTE_TO_DROPOFF
      else do
        mbVersion <- Redis.safeGet (intermediateDestinationsVersionKey ride.id)
        logInfo $ "FleetEngine: rideStarted (enroute to intermediate, index=0) provider=" <> providerId <> " tripId=" <> ride.id.getId
        void . FEClient.updateTrip baseUrl providerId token ride.id.getId "tripStatus,intermediateDestinationIndex" $
          FETypes.emptyTrip
            { tripStatus = Just FETypes.ENROUTE_TO_INTERMEDIATE_DESTINATION,
              intermediateDestinationIndex = Just 0,
              intermediateDestinationsVersion = mbVersion
            }

notifyRideCompleted :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyRideCompleted booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.COMPLETE

notifyTripCancelled :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> m ()
notifyTripCancelled merchantOpCityId rideId = notifyTripStatus merchantOpCityId rideId FETypes.CANCELED

notifyDropoffChanged :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> LatLong -> m ()
notifyDropoffChanged merchantOpCityId rideId newDrop =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    logInfo $ "FleetEngine: updateDropoff provider=" <> providerId <> " tripId=" <> rideId.getId
    let newDropTL = FETypes.TerminalLocation {point = FETypes.LatLng {latitude = newDrop.lat, longitude = newDrop.lon}}
    void . FEClient.updateTrip baseUrl providerId token rideId.getId "dropoffPoint" $
      FETypes.emptyTrip {dropoffPoint = Just newDropTL}

-- | Only valid while trip is ENROUTE_TO_PICKUP; Fleet Engine rejects after ARRIVED_AT_PICKUP.
notifyPickupChanged :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> LatLong -> m ()
notifyPickupChanged merchantOpCityId rideId newPickup =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    logInfo $ "FleetEngine: updatePickup provider=" <> providerId <> " tripId=" <> rideId.getId
    let newPickupTL = FETypes.TerminalLocation {point = FETypes.LatLng {latitude = newPickup.lat, longitude = newPickup.lon}}
    void . FEClient.updateTrip baseUrl providerId token rideId.getId "pickupPoint" $
      FETypes.emptyTrip {pickupPoint = Just newPickupTL}

-- | Cached Fleet Engine version token for intermediateDestinations optimistic-lock.
intermediateDestinationsVersionKey :: Id SRide.Ride -> Text
intermediateDestinationsVersionKey rideId = "fleetengine:trip:" <> rideId.getId <> ":iDestVersion"

notifyStopsChanged :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> [LatLong] -> m ()
notifyStopsChanged merchantOpCityId rideId stops =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    mbVersion <- Redis.safeGet (intermediateDestinationsVersionKey rideId)
    let stopsTL = map (\ll -> FETypes.TerminalLocation {point = FETypes.LatLng {latitude = ll.lat, longitude = ll.lon}}) stops
    logInfo $ "FleetEngine: updateIntermediateDestinations provider=" <> providerId <> " tripId=" <> rideId.getId <> " count=" <> show (length stops)
    mbResp <-
      FEClient.updateTrip baseUrl providerId token rideId.getId "intermediateDestinations" $
        FETypes.emptyTrip
          { intermediateDestinations = Just stopsTL,
            intermediateDestinationsVersion = mbVersion
          }
    whenJust (mbResp >>= (.intermediateDestinationsVersion)) $ \newVersion ->
      Redis.setExp (intermediateDestinationsVersionKey rideId) newVersion (24 * 60 * 60)

-- Version goes in body but not in updateMask (per FE docs).
notifyStopArrived :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> Int -> m ()
notifyStopArrived merchantOpCityId rideId stopIndex =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    mbVersion <- Redis.safeGet (intermediateDestinationsVersionKey rideId)
    logInfo $ "FleetEngine: stopArrived tripId=" <> rideId.getId <> " index=" <> show stopIndex
    void . FEClient.updateTrip baseUrl providerId token rideId.getId "tripStatus,intermediateDestinationIndex" $
      FETypes.emptyTrip
        { tripStatus = Just FETypes.ARRIVED_AT_INTERMEDIATE_DESTINATION,
          intermediateDestinationIndex = Just stopIndex,
          intermediateDestinationsVersion = mbVersion
        }

-- Nothing = no more stops (drop is next); Just idx = enroute to the next intermediate at that FE index.
notifyStopDeparted :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> Maybe Int -> m ()
notifyStopDeparted merchantOpCityId rideId mbNextIdx =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> case mbNextIdx of
    Nothing -> do
      logInfo $ "FleetEngine: stopDeparted (enroute to dropoff) tripId=" <> rideId.getId
      FEClient.updateTripStatus baseUrl providerId token rideId.getId FETypes.ENROUTE_TO_DROPOFF
    Just nextIdx -> do
      mbVersion <- Redis.safeGet (intermediateDestinationsVersionKey rideId)
      logInfo $ "FleetEngine: stopDeparted (enroute to intermediate) tripId=" <> rideId.getId <> " nextIndex=" <> show nextIdx
      void . FEClient.updateTrip baseUrl providerId token rideId.getId "tripStatus,intermediateDestinationIndex" $
        FETypes.emptyTrip
          { tripStatus = Just FETypes.ENROUTE_TO_INTERMEDIATE_DESTINATION,
            intermediateDestinationIndex = Just nextIdx,
            intermediateDestinationsVersion = mbVersion
          }
