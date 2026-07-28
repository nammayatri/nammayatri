module SharedLogic.Association.Change
  ( guardNoLiveRideByDriver,
    guardNoLiveRideByRC,
    guardNoLiveRideInFleet,
    guardRCNotOwnedByAnotherFleet,
    guardRCNotActiveWithAnotherDriver,
    withAssociation,
  )
where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.External.Encryption (EncFlow, EncKind (AsEncrypted), EncryptedHashedField, decrypt)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, throwError)
import qualified Storage.Queries.DriverRCAssociation as QDriverRC
import qualified Storage.Queries.FleetDriverAssociation as QFleetDriver
import qualified Storage.Queries.FleetRCAssociation as QFleetRC
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

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

-- RC ownership guard, driver side: block a driver from linking an RC that's actively
-- claimed by a fleet they aren't a member of (e.g. hijacking another fleet's vehicle by
-- re-verifying its RC number through their own app). Safe for the fleet's own driver:
-- createFleetRCAssociationIfPossible always creates the fleet_rc_association before this
-- runs in initiateRCCreation's driver branch, so FleetDriverAssociation membership already
-- exists by the time a legitimate fleet driver reaches here.
guardRCNotOwnedByAnotherFleet :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Id DVRC.VehicleRegistrationCertificate -> m ()
guardRCNotOwnedByAnotherFleet driverId rcId = do
  guardNoLiveRideByRC rcId
  activeFleetAssocs <- QFleetRC.findAllActiveAssociationByRCId rcId
  unless (null activeFleetAssocs) $ do
    membershipChecks <- mapM (\assoc -> isJust <$> QFleetDriver.findByDriverIdAndFleetOwnerId driverId assoc.fleetOwnerId.getId True) activeFleetAssocs
    unless (or membershipChecks) $ throwError VehicleBelongsToAnotherFleet

-- RC ownership guard, fleet side: a fleet claiming an RC must not silently take over a another's active RC.
guardRCNotActiveWithAnotherDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id DVRC.VehicleRegistrationCertificate -> EncryptedHashedField 'AsEncrypted Text -> m ()
guardRCNotActiveWithAnotherDriver rcId encCertificateNumber = do
  guardNoLiveRideByRC rcId
  mbActiveDriverAssoc <- QDriverRC.findActiveAssociationByRC rcId True
  whenJust mbActiveDriverAssoc $ \_ -> throwError RCActiveOnOtherAccount
  -- No active driver association: clean up any orphan vehicle row that still
  -- references this RC's registration number (can happen on stale state).
  registrationNo <- decrypt encCertificateNumber
  mbVehicle <- QVehicle.findByRegistrationNo registrationNo
  whenJust mbVehicle $ \vehicle -> QVehicle.deleteById vehicle.driverId

  linkedDriverAssocs <- QDriverRC.findAllActiveAssociationByRCId rcId
  forM_ linkedDriverAssocs $ \assoc -> QDriverRC.endAssociationForRC assoc.driverId rcId
