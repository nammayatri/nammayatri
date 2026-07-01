module SharedLogic.Association.Change
  ( guardNoLiveRideByDriver,
    guardNoLiveRideByRC,
    guardNoLiveRideInFleet,
    withAssociation,
  )
where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, throwError)
import qualified Storage.Queries.DriverRCAssociation as QDriverRC
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.Ride as QRide

-- Throws if the driver currently has a NEW/UPCOMING/INPROGRESS ride.
guardNoLiveRideByDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m ()
guardNoLiveRideByDriver driverId = do
  mbRide <- QRide.getUpcomingOrActiveByDriverId driverId
  when (isJust mbRide) $ throwError (InvalidRequest "Driver has a live ride, cannot change association")

-- Vehicle-scoped: resolves the RC to its active driver(s), then checks their live rides.
guardNoLiveRideByRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DVRC.VehicleRegistrationCertificate -> m ()
guardNoLiveRideByRC rcId = do
  mbAssoc <- QDriverRC.findActiveAssociationByRC rcId True
  mbLiveRide <- maybe (pure Nothing) (\assoc -> QRide.findFirstUpcomingOrActiveByDriverIds [assoc.driverId]) mbAssoc
  when (isJust mbLiveRide) $ throwError (InvalidRequest "Vehicle has a live ride, cannot change association")

-- Fleet-wide: blocks if ANY active driver under the fleet has a live ride.
guardNoLiveRideInFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
guardNoLiveRideInFleet fleetOwnerId = do
  driverIds <- QFleetDriver.getActiveDriverIdsByFleetOwnerId fleetOwnerId
  mbLiveRide <- QRide.findFirstUpcomingOrActiveByDriverIds driverIds
  when (isJust mbLiveRide) $ throwError (InvalidRequest "A driver in this fleet has a live ride, cannot change association")

-- Sequencing seam for a change flow: run the guard, then the body
-- (deleteById old -> create new). NOTE: not a DB transaction -- these
-- association tables use the KV (`*WithKV`) layer which cannot be wrapped in
-- Esqueleto `runTransaction` (see SharedLogic.DeleteDriver, where the
-- `Esq.runTransaction` over the same deletes is commented out). The delete of
-- the old row streams to ClickHouse (drainer -> Kafka) as the ended event;
-- full association history lives in ClickHouse, not Postgres.
withAssociation :: Monad m => m () -> m a -> m a
withAssociation runGuard body = runGuard >> body
