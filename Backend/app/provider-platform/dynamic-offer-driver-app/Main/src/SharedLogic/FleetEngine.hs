{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Mirrors the BPP ride lifecycle into Google Fleet Engine (On-demand Rides &
-- Deliveries) trips, so the rider Consumer SDK and driver Driver SDK can share
-- the journey. Every entry point is best-effort and a no-op when the merchant
-- operating city has no Fleet Engine service config (i.e. the feature is off):
-- it must never affect the ride lifecycle, so callers should additionally
-- 'fork' these actions.
--
-- Fleet Engine @tripId@ is the (1:1) BPP ride id; @vehicleId@ is derived from
-- the driver id. The server JWT is minted on the fly from the city's encrypted
-- service-account JSON via 'Kernel.External.FleetEngine.Auth'.
module SharedLogic.FleetEngine
  ( mkFleetEngineVehicleId,
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

-- | Stable Fleet Engine vehicle id for a driver.
mkFleetEngineVehicleId :: Id DP.Person -> Text
mkFleetEngineVehicleId driverId = "driver-" <> driverId.getId

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

-- | Resolve config, mint a server token and run the action. Silently skips when
-- the feature is off for the city; logs (without throwing) on config/token
-- errors so the caller's ride flow is never disturbed.
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
      saText <- decrypt cfg.serviceAccountJson
      case FEAuth.parseServiceAccount saText of
        Left err -> logError $ "FleetEngine: invalid service account for city " <> merchantOpCityId.getId <> ": " <> T.pack err
        Right sa -> do
          let ttl = fromMaybe FEConfig.defaultServerTokenTtl cfg.serverTokenTtlSeconds
          eToken <- liftIO $ FEAuth.mintFleetEngineToken sa FEAuth.ServerToken ttl
          case eToken of
            Left err -> logError $ "FleetEngine: server token mint failed for city " <> merchantOpCityId.getId <> ": " <> T.pack err
            Right token -> action cfg.providerId token (fromMaybe FEClient.defaultFleetEngineBaseUrl cfg.fleetEngineUrl)

-- | Create the trip and assign the driver's vehicle (NEW -> ENROUTE_TO_PICKUP).
notifyTripCreated :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyTripCreated booking ride =
  withFleetEngine booking.merchantOperatingCityId $ \providerId token baseUrl -> do
    let tripId = ride.id.getId
        vehicleId = mkFleetEngineVehicleId ride.driverId
        -- Waypoints let Fleet Engine compute a meaningful ETA to pickup/dropoff;
        -- toLocation can be absent (e.g. open-destination rides).
        pickupPoint = Just (FETypes.LatLng booking.fromLocation.lat booking.fromLocation.lon)
        dropoffPoint = (\loc -> FETypes.LatLng loc.lat loc.lon) <$> booking.toLocation
    FEClient.createTrip baseUrl providerId token tripId (FETypes.mkCreateTripBody FETypes.EXCLUSIVE pickupPoint dropoffPoint Nothing)
    FEClient.assignVehicleAndStart baseUrl providerId token tripId vehicleId

notifyTripStatus :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> FETypes.TripStatus -> m ()
notifyTripStatus merchantOpCityId rideId status =
  withFleetEngine merchantOpCityId $ \providerId token baseUrl ->
    FEClient.updateTripStatus baseUrl providerId token rideId.getId status

notifyDriverArrived :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyDriverArrived booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.ARRIVED_AT_PICKUP

notifyRideStarted :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyRideStarted booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.ENROUTE_TO_DROPOFF

notifyRideCompleted :: FleetEngineFlow m r => DRB.Booking -> SRide.Ride -> m ()
notifyRideCompleted booking ride = notifyTripStatus booking.merchantOperatingCityId ride.id FETypes.COMPLETE

notifyTripCancelled :: FleetEngineFlow m r => Id DMOC.MerchantOperatingCity -> Id SRide.Ride -> m ()
notifyTripCancelled merchantOpCityId rideId = notifyTripStatus merchantOpCityId rideId FETypes.CANCELED
