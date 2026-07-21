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
  ( mkDriverToken,
    notifyTripCreated,
    notifyDriverArrived,
    notifyRideStarted,
    notifyRideCompleted,
    notifyTripCancelled,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as SRide
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.FleetEngine.Auth as FEAuth
import qualified Kernel.External.FleetEngine.Client as FEClient
import Kernel.External.FleetEngine.Config (FleetEngineCfg)
import qualified Kernel.External.FleetEngine.Config as FEConfig
import qualified Kernel.External.FleetEngine.Types as FETypes
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC

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
      DOSC.FleetEngineServiceConfig cfg -> Just cfg
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
    Nothing -> pure Nothing -- feature off for this city
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

-- | Silent skip when Fleet Engine is off; logs (never throws) on config/token errors.
withFleetEngine ::
  FleetEngineFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  (Text -> Text -> BaseUrl -> m ()) -> -- providerId -> serverToken -> baseUrl
  m ()
withFleetEngine merchantOpCityId action = do
  mbCfg <- getFleetEngineCfg merchantOpCityId
  case mbCfg of
    Nothing -> pure () -- feature off for this city
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
    ensureVehicleExists baseUrl providerId token vehicleId
    FEClient.createTrip baseUrl providerId token tripId (FETypes.mkCreateTripBody FETypes.EXCLUSIVE pickupPoint dropoffPoint Nothing)
    FEClient.assignVehicleAndStart baseUrl providerId token tripId vehicleId

-- | GET-first: don't overwrite state the Driver SDK may set once mobile integration lands.
ensureVehicleExists :: FleetEngineFlow m r => BaseUrl -> Text -> Text -> Text -> m ()
ensureVehicleExists baseUrl providerId token vehicleId = do
  mbVeh <- FEClient.getVehicle baseUrl providerId token vehicleId
  when (isNothing mbVeh) $
    FEClient.createVehicle baseUrl providerId token vehicleId defaultVehicle

-- | Placeholder body — Driver SDK overwrites state and vehicleType at online-time.
defaultVehicle :: FETypes.Vehicle
defaultVehicle =
  FETypes.Vehicle
    { vehicleState = FETypes.OFFLINE,
      supportedTripTypes = [FETypes.EXCLUSIVE],
      maximumCapacity = 4,
      vehicleType = FETypes.VehicleType {category = FETypes.AUTO}
    }

notifyTripStatus :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> FETypes.TripStatus -> m ()
notifyTripStatus merchantOpCityId rideId status =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl -> do
    logInfo $ "FleetEngine: updateTripStatus provider=" <> providerId <> " tripId=" <> rideId.getId <> " status=" <> show status
    FEClient.updateTripStatus baseUrl providerId token rideId.getId status

notifyDriverArrived :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyDriverArrived booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.ARRIVED_AT_PICKUP

notifyRideStarted :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyRideStarted booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.ENROUTE_TO_DROPOFF

notifyRideCompleted :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyRideCompleted booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.COMPLETE

notifyTripCancelled :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> m ()
notifyTripCancelled merchantOpCityId rideId = notifyTripStatus merchantOpCityId rideId FETypes.CANCELED
